import * as ts from 'typescript';
import * as fs from 'fs';

if (process.argv.length != 3)
{
    console.log("usage: " + process.argv[1] + " somefile.hs");
    process.exit(1);
}


function parseMethodSig(sig:string)
{
    let pieces = sig.split('->');
    let preparams = (pieces[0] == 'void' ? [] : pieces[0].split(','));
    let rettype = pieces[1];

    let params:Array<string> = [];
    for (let i = 0; i < preparams.length; ++i)
    {
        if (preparams[i] == 'rb' || preparams[i] == 'wb')
        {
            params.push('i32');
            params.push('i32');
        }
        else
            params.push(preparams[i]);
    }

    return {
        params_with_buf: preparams,
        params: params,
        rettype: rettype,
        typekey: (params.length == 0 ? 'void' : params.join(',')) + '->' + rettype
    }
}


const hookapi:any = 
(():void=>
{
    const hookapiRaw = fs.readFileSync('hookapi.txt').toString('utf-8').
        replace(/\n/g,'').
        replace(/  *|\t\t*/g, ' ').
        replace(/uint64_t/g, 'i64').
        replace(/uint32_t/g, 'i32').
        replace(/int64_t/g, 'i64').
        replace(/int32_t/g, 'i32').
        replace(/DECLARE_HOOK_FUNCTION/g, '').
        replace(/DECLARE_HOOK_FUNCNARG/g, '')


    const def = /\([^\)]*\) *;?/g;

    let ret:any = {};
    let m;
    do
    {
        m = def.exec(hookapiRaw);
        if (m)
        {
            let entry = m[0];

            const rawEntry = entry;

            entry = entry.
                replace(/;/g,'').
                replace(/[\(\)]/g, '').
                replace(/, */g, ',');


            //console.log("b-entry", entry)

            entry = entry.
                replace(/i32 ([^_]*write)_ptr,i32 [^_]*write_len/g, 'wb $1');
            
            entry = entry.
                replace(/i32 ([^_]*read)_ptr,i32 [^_]*read_len/g, 'rb $1');


            entry = entry.
                replace(/  *$/g, '').
                replace(/ [^,]+(,|$)/g, '$1');

            let pieces = entry.split(',');
            const retType = pieces.shift();
            const apiName = pieces.shift();
            let params = pieces.join(',');
            if (params == '')
                params = 'void';
            const sig = params + '->' + retType;
            ret['' + apiName] = 
            {
                raw: rawEntry,
                ... parseMethodSig(sig)
            };
            ret['$' + apiName] = 
            {
                raw: rawEntry,
                ... parseMethodSig(sig)
            };
        }
    } while (m);
    return ret;
})();

const filename = process.argv[2];
const rawfile = Buffer.from(fs.readFileSync(filename)).toString('utf-8');

const srcfile = ts.createSourceFile(
    filename, rawfile, ts.ScriptTarget.Latest
);


const markers:any =
{
    "FirstAssignment": "EqualsToken",
    "LastAssignment": "CaretEqualsToken",
    "FirstCompoundAssignment": "PlusEqualsToken",
    "LastCompoundAssignment": "CaretEqualsToken",
    "FirstReservedWord": "BreakKeyword",
    "LastReservedWord": "WithKeyword",
    "FirstKeyword": "BreakKeyword",
    "LastKeyword": "OfKeyword",
    "FirstFutureReservedWord": "ImplementsKeyword",
    "LastFutureReservedWord": "YieldKeyword",
    "FirstTypeNode": "TypePredicate",
    "LastTypeNode": "ImportType",
    "FirstPunctuation": "OpenBraceToken",
    "LastPunctuation": "CaretEqualsToken",
    "FirstToken": "Unknown",
    "LastToken": "LastKeyword",
    "FirstTriviaToken": "SingleLineCommentTrivia",
    "LastTriviaToken": "ConflictMarkerTrivia",
    "FirstLiteralToken": "NumericLiteral",
    "LastLiteralToken": "NoSubstitutionTemplateLiteral",
    "FirstTemplateToken": "NoSubstitutionTemplateLiteral",
    "LastTemplateToken": "TemplateTail",
    "FirstBinaryOperator": "LessThanToken",
    "LastBinaryOperator": "CaretEqualsToken",
    "FirstStatement": "VariableStatement",
    "LastStatement": "DebuggerStatement",
    "FirstNode": "QualifiedName",
    "FirstJSDocNode": "JSDocTypeExpression",
    "LastJSDocNode": "JSDocPropertyTag",
    "FirstJSDocTagNode": "JSDocTag",
    "LastJSDocTagNode": "JSDocPropertyTag",
    "FirstContextualKeyword": "AbstractKeyword",
    "LastContextualKeyword": "OfKeyword"
};
function printRecursiveFrom(
    node: ts.Node, indentLevel: number, srcfile: ts.SourceFile
) {
    const indentation = "-".repeat(indentLevel);
    let syntaxKind = ts.SyntaxKind[node.kind];
    if (markers[syntaxKind] != undefined)
        syntaxKind = markers[syntaxKind];

    const nodeText = node.getText(srcfile);
    console.log(`${indentation}${syntaxKind}: ${nodeText}`);

    node.forEachChild(child =>
        printRecursiveFrom(child, indentLevel + 1, srcfile)
    );
}

function walk(
    node: ts.Node, srcfile: ts.SourceFile, findKind: any, visit: (node:ts.Node)=>void ) : void
{
    if (node === undefined || node.kind === undefined)
        return;

    if (findKind && Array.isArray(findKind))
    {
        let tmp:any = {};
        for (let i = 0; i < findKind.length; ++i)
        {
            let kind:number = findKind[i];
            tmp[kind] = true;
        }
        findKind = tmp;
    }

    if (findKind === undefined || findKind[node.kind])
        visit(node);
    
    node.forEachChild(child =>
    {
        walk(child, srcfile, findKind, visit);
    });
}

function d(node:any, depth:number = 2):void
{
    console.dir(node, {depth:depth});
}

function die(ctx:any, node:ts.Node, msg:string):void
{
    const pos = node.pos;
    const end = node.end;
    const lineno = (ctx.rawfile.substr(0, pos).match(/\n/g) || []).length + 1;
    console.error(
        "Error: " +
        ctx.filename + ":" + lineno + " " +
        msg + " " +
        "Near: `" + node.getText(ctx.srcfile) + "`",
    );
    process.exit(1);
}

function inferImports(ctx: any) : void
{
    walk(ctx.srcfile, ctx.srcfile, [ts.SyntaxKind.CallExpression], 
    (node:any):void=> 
    {
        //console.log("walked to: ", node.getText(srcfile));
        const funcname = node.expression.escapedText;

        if (!hookapi[funcname])
            return;

        const typekey = hookapi[funcname].typekey;
        if (ctx.types[typekey] == undefined)
        {
            ctx.types[typekey] = ctx.type_count;
            ctx.types_map[ctx.type_count++] = typekey;
        }

        const typeidx = ctx.types[typekey];

        if (ctx.imports[funcname] == undefined)
        {
            ctx.imports[funcname] = {
                idx: ctx.import_count,
                typeidx: typeidx
            };
            ctx.imports_map[ctx.import_count++] = funcname;
        }
        //console.log("function: `" + funcname + "`, sig: `" + hookapi[funcname].typekey + "`");
    });
}


const string_types: any =
{
    'string': true,
    'bigstring': true
}

const float_types: any =
{
    'xfl': true
}
const int_types: any =
{
    'i8' : true,
    'u8' : true,
    'i16': true,
    'u16': true,
    'i32': true,
    'u32': true,
    'i64': true,
    'u64': true
}

const obj_types: any =
{
    'Object': true,
    'Account': true,
    'Amount': true
}

const aliased_types : any = 
{
    'number': 'i32'
}

const allowed_types:any =
(():any=>
{
    let out:any = {};
    const addTypes = (types:any) : void =>
    {
        types = Object.keys(types);
        for (let i = 0; i < types.length; ++i)
        {
            if (out[types[i]] === undefined)
                out[types[i]] = true;
            if (out['Array<' + types[i] + '>'] === undefined)
                out['Array<' + types[i] + '>'] = true;

        }
    }

    addTypes(string_types);
    addTypes(float_types);
    addTypes(int_types);
    addTypes(obj_types);
    return out;
})();

d(allowed_types, 2);


function processFunctions(ctx: any) : void
{

    walk(ctx.srcfile, ctx.srcfile, [ts.SyntaxKind.FunctionDeclaration], 
    (node:any):void=> 
    {
        const funcname = node.name.escapedText;

        if (ctx.funcs[funcname] || ctx.macros[funcname])
            die(ctx, node, "Duplicate function definition: " + funcname); 

        const realfunc = funcname == 'hook' || funcname == 'cbak';

        let params = [];
        let param_types = [];
        for (let i = 0; i < node.parameters.length; ++i)
        {
            const param = node.parameters[i];
            const name = param.name.escapedText;
            const type = param.type.typeName.escapedText;
            const initializer = param.initializer;

            // parameter validation
            if (realfunc)
            {
                if (type != 'i32' && type != 'i64')
                    die(ctx, node, "hook and cbak must have method signature i32->i64. [1]");

                if (initializer != undefined)
                    die(ctx, node, "hook and cbak cannot have default value parameters.");
            }
            else
            {
                if (!allowed_types[type])
                    die(ctx, node, "in macro " + funcname + ", parameter type: " + type + " not supported.");
            }

            params.push(
            {
                name: name,
                type: type,
                init: initializer
            });

            param_types.push(type);
        }

        if (realfunc && params.length != 1)
            die(ctx, node, "hook and cbak must have method signature i32->i64. [2]");

        const rettype = (node.type === undefined ? 
                         "void" :
                         node.type.typeName.escapedText);

        if (realfunc)
        {
            if (rettype != 'i64')
                die(ctx, node, "hook and cbak must have method signature i32->i64. [3]");
        }
        else
        {   
            if (rettype != 'void' && !allowed_types[rettype])
                die(ctx, node, "in macro " + funcname + ", return type: " + rettype + " not supported.");
        }
        
        const sig = (param_types.length == 0 ? 'void' : param_types.join(',')) + '->' + rettype;

        if (realfunc)
        {
            ctx.funcs[funcname] =
            {
                idx: ctx.func_count,
                name: funcname,
                type: parseMethodSig(sig),
                body: node.body,
                typeidx: 0,         // hard coded always the hook/cbak type
            }
            ctx.funcs_map[ctx.func_count++] = funcname;
        }
        else
        {
            ctx.macros[funcname] =
            {
                idx: ctx.macro_count,
                name: funcname,
                type: parseMethodSig(sig),
                body: node.body
            }
            ctx.macros_map[ctx.macro_count++] = funcname;
        }
    });
}

function validateAndShakeFunctions(ctx: any) : void
{
    let touched: any = {};
    for (let i = 0; i < ctx.macro_count; ++i)
        touched[ctx.macros_map[i]] = false;

    let changed = false;

    const visitor =
    (node:any):void=> 
    {
        //console.log("walked to: ", node.getText(srcfile));
        const funcname = node.expression.escapedText;

        if (funcname == 'hook' || funcname == 'cbak')
            die(ctx, node, "hook and cbak cannot be called from inside the hook.");

        if (hookapi[funcname])
            return;

        if (!ctx.macros[funcname])
            die(ctx, node, "macro `" + funcname + "` is referenced but not defined.");

        if (!touched[funcname])
        {
            touched[funcname] = true;
            changed = true;
        }
    };

    for (let i = 0; i < ctx.func_count; ++i)
    {
        const funcname = ctx.funcs_map[i];
        const func = ctx.funcs[funcname];
        walk(func.body, ctx.srcfile, [ts.SyntaxKind.CallExpression], visitor);
    }

    do 
    {
        changed = false;
        for (let i = 0; i < ctx.macro_count; ++i)
        {
            const funcname = ctx.macros_map[i];
            const func = ctx.macros[funcname];

            // check this macro to see if it calls other macros
            if (touched[funcname])
                walk(func.body, ctx.srcfile, [ts.SyntaxKind.CallExpression], visitor);
        }
    } while (changed);

    // mark unused macros
    for (let i = 0; i < ctx.macro_count; ++i)
    {
        const funcname = ctx.macros_map[i];
        ctx.macros[funcname].unused = !touched[funcname];

        if (!touched[funcname])
            console.error("Warn: unused macro `" + funcname + "`");
    }
}

function processInitializers(ctx: any) : void
{
    // first process string literals
    walk(ctx.srcfile, ctx.srcfile, [ts.SyntaxKind.StringLiteral], 
    (node:any):void=> 
    {
        // RH UPTO
        // concat string literals together into a data blob to be emitted to wasm
        // concat (non-string) array literals in the samw way
//        d(node, 6);
    });
}


function getTypeName(ctx:any, node:any) : string
{
    if (!node)
        return "any";

    if (node.type)
        node = node.type;

    let tn = "any";

    if (node.typeName && node.typeName.escapedText)
        tn = node.typeName.escapedText;
    else
    if (node.type === undefined && node.initializer !== undefined)
    {
        const k = node.initializer.kind;
        if (k == ts.SyntaxKind.StringLiteral)
            tn = "string";
        else if (k == ts.SyntaxKind.NumericLiteral)
            tn = "number";
        else if (k == ts.SyntaxKind.ObjectLiteralExpression)
            tn = "Object";
        else if (k == ts.SyntaxKind.NewExpression)
        {
            d(node, 3);
            console.log(ts.SyntaxKind[k]);
        }
        // RH UPTO: array literal expression (and infer inner type)
    }

    tn = tn.trim();

    if (tn.match(/^Array.*/) &&
        node.typeArguments && node.typeArguments[0])
    {
        d(node, 3);
        if (node.typeArguments.length == 1)
        {
            const arg = node.typeArguments[0];

            let inner = "any";

            if (arg.typeName &&
                arg.typeName.escapedText)
                inner = arg.typeName.escapedText;
            else if (arg.typeName)
                inner = ctx.rawfile.substr(arg.typeName.pos, arg.typeName.end - arg.typeName.pos);
            else
                inner = ctx.rawfile.substr(arg.pos, arg.end - arg.pos);

            console.log("here");
            d(node, 3);
            tn = "Array<" + inner.trim() + ">";
        }
    }

    return tn.trim();
}

function getArrayInnerType(tn: string) : string | undefined
{
    const tn2 = tn.replace(/^Array<([^>]+)>$/g, '$1');
    if (tn2 == '')
        return undefined;
    if (tn2 != tn)
        return tn2;
    return undefined;
}

function validateTypes(ctx: any) : void
{
    walk(ctx.srcfile, ctx.srcfile,
         [ts.SyntaxKind.VariableDeclaration], 
    (node:any):void=> 
    {
        const k = node.kind;
        
        // VariableDeclaration
        const tn = getTypeName(ctx, node);
        if (tn === undefined || !(tn in allowed_types))
        {
            if (tn === undefined)
                die(ctx, node, "Type any / unspecified types are not supported.");
            else
                die(ctx, node, "Type `" + tn + "` is not supported.");
        }

        const validateLiteral = (node:any, inner:string) : void =>
        {
            const k = node.kind;
            if (inner == 'string' || inner == 'bigstring')
            {
                if (k != ts.SyntaxKind.StringLiteral)
                    die(ctx, node, "Type " + inner + " may only be initialized using string literals.");
                return;
            }

            if (inner in int_types)
            {
               if (k != ts.SyntaxKind.NumericLiteral)
                   die(ctx, node, "Type " + inner + " may only be initialized using numeric literals.");
               return;
            }

            if (inner == 'xfl')
            {
                if (k == ts.SyntaxKind.NumericLiteral && node.text.match(/[0-9]+\.[0-9]+/g) && inner != 'xfl')
                    die(ctx, node, "Cannot initialize " + inner + " with fractional value.");
                return;
            }

            if (inner in obj_types)
            {
               if (k != ts.SyntaxKind.ObjectLiteralExpression)
                   die(ctx, node, "Object types can only be initalized with object literals.");
               return;
            }
        }

        // if it's an array we should check initializer args (if any)
        const inner = getArrayInnerType('' + tn);
        if (inner !== undefined && node.initializer && 
            node.initializer.elements && node.initializer.elements.length > 0)
        {
            const init = node.initializer.elements;
            for (let i = 0; i < init.length; ++i)
                validateLiteral(init[i], inner);
            return;
        }        
        
        // non-array, check initializer
        if (node.initializer)
            validateLiteral(node.initializer, tn);

        node.validedTypeName = tn;
    });

}

function validateTopLevelAST(ctx: any) : void
{
    // top level may contain only:
    // - functions
    // - variable declarations

    //VariableStatement
    //FunctionDeclaration

    const node : ts.Node = ctx.srcfile;
    node.forEachChild(child =>
    {
        const k = child.kind;
        if (k != ts.SyntaxKind.VariableStatement &&
            k != ts.SyntaxKind.FunctionDeclaration &&
            k != ts.SyntaxKind.EndOfFileToken)
        {
            die(ctx, child, 
                "Only variable declarations are allowed outside of a function. Found: " + 
                    ts.SyntaxKind[child.kind] + ".");
        }
    });
}

let ctx:any = 
{
    filename: filename,
    rawfile: rawfile,
    srcfile: srcfile,
    
    /* types as they will appear in the output wasm, with the first type prefilled for hook & cbak */
    types: { 
        "i32->i64" : 0 
    },
    types_map: {
        0: "i32->i64"
    },      // idx -> key
    type_count: 1,

    /* imports as they will appear in the output wasm */
    imports: {},
    imports_map: {},    // idx -> key
    import_count: 0,

    /* functions as they will appear in the output wasm */
    funcs: {},
    funcs_map: {},      // idx -> key
    func_count : 0,

    /* macros are user defined functions that are always inlined */
    macros: {},
    macros_map: {},     // idx -> key
    macro_count: 0,

    globals: {},
    globals_map: {},
    globals_count: 0
};

validateTopLevelAST(ctx);
validateTypes(ctx);

inferImports(ctx);
processFunctions(ctx);

validateAndShakeFunctions(ctx); // remove all unused/unreferenced macros

processInitializers(ctx); 

//processData(ctx);

//process.exit(0);

console.log('(module');

// output types
for (let i = 0; i < ctx.type_count; ++i)
{
    const typekey = ctx.types_map[i];
    const type = parseMethodSig(typekey);
    console.log("  " +
        "(type (;" + i + ";) " +
        "(func " +
            (type.params.length > 0 ? 
            "(param " + type.params.join(' ') + ") " : "") +
            "(result " + type.rettype + ")))");
}

// output imports
for (let i = 0; i < ctx.import_count; ++i)
{
    const importname = ctx.imports_map[i];
    const imp = ctx.imports[importname];
    console.log("  " +
        '(import "env" "' + importname + '" ' +
        '(func (;' + i + ';) '+
        '(type ' + imp.typeidx + ')))'
    );
}

// output funcs
for (let i = 0; i < ctx.func_count; ++i)
{
    const funcname = ctx.funcs_map[i];
    const func = ctx.funcs[funcname];
    const type = func.type;
    console.log("  " +
        "(func (;" + i + ";) " +
        "(type " + func.typeidx + ") " +
            (type.params.length > 0 ? 
            "(param " + type.params.join(' ') + ") " : "") +
            "(result " + type.rettype + ")");
    // code generation here
    console.log("  )");
}

// output memory
console.log('  (memory (;0;) 2)');


// output globals
// TODO

// output exports
for (let i = 0; i < ctx.func_count; ++i)
{
    const funcname = ctx.funcs_map[i];
    console.log(
        '  (export "' + funcname + '" (func ' + (i + ctx.import_count) + '))'
    )
}

//output data section
// TODO

console.log(')');


printRecursiveFrom(srcfile, 0, srcfile);
//d(ctx, 4);
/*
for (let s = 0; s < srcfile.statements.length; ++s)
{
    const stmt = srcfile.statements[s];
 //   console.log(ts.SyntaxKind[stmt.kind])
    walk(stmt, 1);
}
*/
//console.log(srcfile);
//console.dir(srcfile,{depth: 20});

