import * as ts from 'typescript';
import * as fs from 'fs';

if (process.argv.length != 3)
{
    console.log("usage: " + process.argv[1] + " somefile.hs");
    process.exit(1);
}

const Sym = ts.SyntaxKind;

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
    let syntaxKind = Sym[node.kind];
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
    walk(ctx.srcfile, ctx.srcfile, [Sym.CallExpression],
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
    'Amount': true,
    'BigObject': true
}

const aliased_types : any =
{
    'number': 'i32',
    'int' : 'i32'
}

// this can be queried to see if a type can go into a local/global variable as opposed to memory
const primitive_types : any = 
(():any=>{

    let out = {...int_types, ...float_types};
    for (let t in aliased_types)
    {
        if (aliased_types[t] in out)
            out[t] = true;
    }
    return out;
})();

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
    addTypes(aliased_types);
    return out;
})();

d(allowed_types, 2);


function processFunctions(ctx: any) : void
{

    walk(ctx.srcfile, ctx.srcfile, [Sym.FunctionDeclaration],
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
                node: node,
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
                node: node
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
        walk(func.body, ctx.srcfile, [Sym.CallExpression], visitor);
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
                walk(func.body, ctx.srcfile, [Sym.CallExpression], visitor);
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


function getTypeName(ctx:any, node:any) : string
{
    if (!node)
        return "any";

    if (node.type)
        node = node.type;

    let tn = "any";

    if (node.typeName && node.typeName.escapedText)
        tn = node.typeName.escapedText;

    tn = tn.trim();

    if (tn.match(/^Array.*/) &&
        node.typeArguments && node.typeArguments[0])
    {
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

function nodeToString(ctx: any, node: ts.Node) : string
{
    let s = ctx.rawfile.substr(node.pos, node.end - node.pos).trim();
    s = s.replace(/\/\/.*/, '').trim();
    s = s.replace(/\/\*.*?\*\//, '').trim();
    s = s.replace(/  */, ' ').replace(/\n\n*/, '\n').trim();
    return s;
}

function validateTypes(ctx: any) : void
{
    walk(ctx.srcfile, ctx.srcfile,
         [Sym.VariableStatement, Sym.VariableDeclaration, Sym.NewExpression, Sym.NewKeyword, Sym.ConstKeyword],
    (node:any):void=>
    {

        const k = node.kind;

        if (k != Sym.VariableDeclaration)
        {
            if (k == Sym.NewKeyword || k == Sym.NewExpression)
                die(ctx, node, "New keyword is not supported (try omitting it).");

            const rawnode = nodeToString(ctx, node);
            if (k == Sym.ConstKeyword ||
                rawnode.match(/(^| |\t)+const($| |\t)+/))
                die(ctx, node, "Const keyword is not supported. Use let instead.");
            return;
        }


        // VariableDeclaration
        const tn = getTypeName(ctx, node);
        if (tn === undefined || !(tn in allowed_types))
        {
            if (tn === undefined)
                die(ctx, node, "Type any / unspecified types are not supported.");
            else
                die(ctx, node, "Type `" + tn + "` is not supported.");
        }

        const validateLiteral = (node:any, inner:string, toplevel:any) : void =>
        {
            if (toplevel)
                walk(ctx.srcfile, node, [Sym.CallExpression], (node:any)=>
                {
                    die(ctx, node, "Call expressions are not supported in top level initializers. " +
                       "Use = [a,b,...] instead.");
                });

            const k = node.kind;
            if (inner == 'string' || inner == 'bigstring')
            {
                if (k != Sym.StringLiteral)
                    die(ctx, node, "Type " + inner + " may only be initialized using string literals.");
                return;
            }

            if (inner in int_types)
            {
               if (k != Sym.NumericLiteral)
                   die(ctx, node, "Type " + inner + " may only be initialized using numeric literals.");
               return;
            }

            if (inner == 'xfl')
            {
                if (k == Sym.NumericLiteral && node.text.match(/[0-9]+\.[0-9]+/g) && inner != 'xfl')
                    die(ctx, node, "Cannot initialize " + inner + " with fractional value.");
                return;
            }

            if (inner in obj_types)
            {
               if (k != Sym.ObjectLiteralExpression)
                   die(ctx, node, "Object types can only be initialized with object literals.");

               return;
            }

            
            if (toplevel && !(k >= Sym.FirstLiteralToken && k <= Sym.LastLiteralToken))
                die(ctx, node, "Top level variables must be initialized with literals.");
        }

        if (!node.initializer)
            die(ctx, node, "Variables must be initialized.");


        // if it's an array we should check initializer args (if any)
        const inner = getArrayInnerType('' + tn);
        if (inner && node.initializer)
        {
            // array semantics: var = [..., ...]
            if (node.initializer.elements && node.initializer.elements.length > 0)
            {
                const init = node.initializer.elements;
                for (let i = 0; i < init.length; ++i)
                    validateLiteral(init[i], inner, node._toplevel);
            }
            // or call type semantics: var = Array<x>(..., ...)
            else
            if (node.initializer.arguments && node.initializer.arguments.length > 0)
            {
                if (node.initializer.arguments.length == 1 &&
                    node.initializer.arguments[0].kind == Sym.NumericLiteral)
                {
                    // pass, this is like Array<string>(10);
                    // i.e. blank init
                }
                else
                {
                    const init = node.initializer.arguments;
                    for (let i = 0; i < init.length; ++i)
                        validateLiteral(init[i], inner, node._toplevel);
                }
            }
            else
                die(ctx, node, "Array types must be initialized");
        }
        
        if (node.initializer.kind == Sym.CallExpression)
        {
            // call expression *must* be a constructor of the same type
            let ctn:string = node.initializer.expression.escapedText.trim();
            if (node.initializer.typeArguments.length == 1)
               ctn += '<' + nodeToString(ctx, node.initializer.typeArguments) + '>';

            if (ctn != tn)
                die(ctx, node, "Variables can only be node.initializerialized via a constructor of their own type.");
        }

        // non-array, check initializer
        if (inner === undefined)
            validateLiteral(node.initializer, tn, node._toplevel);

        node._typename = tn;
        node._inner = inner;
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
        if (k != Sym.VariableStatement &&
            k != Sym.FunctionDeclaration &&
            k != Sym.EndOfFileToken)
        {
            die(ctx, child,
                "Only variable declarations are allowed outside of a function. Found: " +
                    Sym[child.kind] + ".");
        }

        (child as any)._toplevel = true;
        walk((child as ts.Node), ctx.srcfile, undefined, (node:any)=>{
            node._toplevel = true;
        });
    });
}

let dataseg : any = {};

/*
function processIninitializers(ctx: any) : void
{
    walk(ctx.srcfile, ctx.srcfile,
         [Sym.VariableDeclaration],
    (node:any):void=>
    {
        if (node.initializer === undefined)
            die(ctx, node, "Variables must be initialized.");

        const init = node.initializer;



        const isTopLevel = !!((node as any)['_toplevel']);
        const tn = node._typename;

        if (tn === undefined)
            die(ctx, node, "Could not determine type of variable.");

        if (init.kind == Sym.CallExpression)
        {
            // RHTODO: move this to validateTypes
            // RHTODO: initialize on demand not ahead of time, remove this function
            // call expression *must* be a constructor of the same type
            let ctn:string = init.expression.escapedText.trim();
            if (init.typeArguments.length == 1)
               ctn += '<' + nodeToString(ctx, init.typeArguments) + '>'.trim();

            if (ctn != tn)
                die(ctx, node, "Variables can only be initialized via a constructor of their own type.");

            //d(node, 4);
            if (node._inner)
            {

                // array
                let count = 0;
                let fill = [];

                if (init.arguments.length == 1 && init.arguments[0].kind == Sym.NumericLiteral &&
                   !init.arguments[0].text.match(/\./))
                    count = parseInt(init.arguments[0].text);
                else
                {
                    // = Array<X>(1,2,3); // prefilled array
                    count = init.arguments.length;
                    // RHUPTO   how are we handling Array<STObject>({},{...}, ...)
                }

            }
        }
        else
        {
            // non-call expression, must be some sort of literal
        }

        if (node._inner)
        {
            // array type
        }
        else
        {
            // non-array type
        }
//        console.log(tn);


    });
}
*/

function tagAll(ctx:any) : void
{
    walk(ctx.srcfile, ctx.srcfile, undefined,
    (node:any):void=>
    {
        node._raw = nodeToString(ctx, node);
        if (node.kind !== undefined)
            node._tn = Sym[node.kind];
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
    macro_count: 0
};

function processVariableStatement(ctx: any, node: any, varmap: any, suppress_output:boolean): void
{
    if (!node.declarationList || 
        !node.declarationList.declarations || 
         node.declarationList.declarations.length <= 0)
    {
        console.log("Warn: variable statement without declaration list");
        return;
    }

    const decl = node.declarationList.declarations;

    for (let i = 0; i < decl.length; ++i)
    {
        const name = decl[i].name.escapedText;
        if (varmap[name] !== undefined)
            die(ctx, node, "Variable " + name + " declared more than once.");
        varmap[decl[i].name.escapedText.trim()] = decl[i];
        if (suppress_output)
            continue;

        
    }
}

function collectVariablesAtTopLevel(ctx:any):void
{
    let varmap:any = {};

    (ctx.srcfile as ts.Node).forEachChild(child =>
    {
        const node:any = (child as ts.Node);
    
        if (node.kind != Sym.VariableStatement)
            return;

        processVariableStatement(ctx, node, varmap, true);
    });

    return varmap;
}

tagAll(ctx);
validateTopLevelAST(ctx);
validateTypes(ctx);

inferImports(ctx);
processFunctions(ctx);

validateAndShakeFunctions(ctx); // remove all unused/unreferenced macros

//processIninitializers(ctx);


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

function processExpression(ctx:any, func:any, expr:any, varmap:any):void
{
    const k = expr.kind; 
//    console.log("=>> processExpression " + Sym[k] + "<<=");
    switch (k)
    {
        case Sym.NumericLiteral:
        {
            let rettype = "i64";
            if (func)
                rettype = func.type.rettype;
            console.log("    " + rettype + ".const " + expr.text.trim());
            return;
        }
        case Sym.BigIntLiteral:
        case Sym.StringLiteral:
        case Sym.RegularExpressionLiteral:
        case Sym.NoSubstitutionTemplateLiteral:
        case Sym.TypeLiteral:
        case Sym.LiteralType:
        //case Sym.TemplateLiteralType:
        //case Sym.TemplateLiteralTypeSpan:
        case Sym.ArrayLiteralExpression:
        case Sym.ObjectLiteralExpression:
        case Sym.JSDocTypeLiteral:
        default:
            die(ctx, expr, "Unknown expression type: " + Sym[k]);
    }
}

function processStatement(ctx:any, func:any, stmt:any, varmap:any):void
{
    const k = stmt.kind;
//    console.log("=>> processStatement " + Sym[k] + "<<=");
    switch (k)
    {
        case Sym.EmptyStatement:
            return;

        case Sym.VariableStatement:
        {
            processVariableStatement(ctx, stmt, varmap, false);
            return;
        }
        case Sym.ExpressionStatement:
        {
            return;
        }
        case Sym.IfStatement:
        case Sym.DoStatement:
        case Sym.WhileStatement:
        case Sym.ForStatement:
        case Sym.ForInStatement:
        case Sym.ForOfStatement:
        case Sym.ContinueStatement:
        case Sym.BreakStatement:
        {
            die(ctx, stmt, "Unknown statement type: " + Sym[k]);
        }
        case Sym.ReturnStatement:
        {
            if (stmt.expression)
                processExpression(ctx, func, stmt.expression, varmap);
            console.log("    return");
            return;
        }

        case Sym.WithStatement:
        case Sym.SwitchStatement:
        case Sym.LabeledStatement:
        case Sym.ThrowStatement:
        case Sym.TryStatement:
        case Sym.DebuggerStatement:
        case Sym.NotEmittedStatement:
        default:
            die(ctx, stmt, "Unknown statement type: " + Sym[k]);
    }
}

// output funcs
for (let i = 0; i < ctx.func_count; ++i)
{

    // setup a variable map
    let varmap : any =
        collectVariablesAtTopLevel(ctx);

    const funcname = ctx.funcs_map[i];
    const func = ctx.funcs[funcname];
    const type = func.type;
    console.log("  " +
        "(func (;" + i + ";) " +
        "(type " + func.typeidx + ") " +
            (type.params.length > 0 ?
            "(param " + type.params.join(' ') + ") " : "") +
            "(result " + type.rettype + ")");
    // collect local parameters

    let localidx = 0;
    let paramcount = 0;
    if (func.node.parameters && func.node.parameters.length)
    {
        const params = func.node.parameters;
        for (let i = 0; i < params.length; ++i)
        {
            const param : any = params[i];
            varmap[param.name.escapedText.trim()] = param;
            param._localidx = localidx++;
        }
        paramcount = params.length;
    }

    // tag locals
    let localsout = "";
    walk(func.node.body, ctx.srcfile, [Sym.VariableStatement], (node:any):void=>
    {
        if (node.declarationList && node.declarationList.declarations &&
            node.declarationList.declarations.length >= 0)
        {
            const decls = node.declarationList.declarations;
            for (let i = 0; i < decls.length; ++i)
            {
                let tn = getTypeName(ctx, decls[i]);
                if (primitive_types[tn])
                {
                    if (aliased_types[tn])
                        tn = aliased_types[tn];
                    localsout += (localsout == "" ? "" : " ") + tn;
                    decls[i]._localidx = localidx++;
                }
            }
        }
    });

    console.log("    (local " + localsout + ")");

    // statement iteration
    if (!func.node.body || !func.node.body.statements || func.node.body.statements.length <= 0)
        die(ctx, func.node, "Empty functions are not supported.");

    const stmts = func.node.body.statements;

    for (let i = 0; i < stmts.length; ++i)
        processStatement(ctx, func, stmts[i], varmap);
    
    //d(func.node, 2);

    console.log("  )");
}

// output memory
console.log('  (memory (;0;) 2)');


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


//printRecursiveFrom(srcfile, 0, srcfile);
//d(ctx, 4);
/*
for (let s = 0; s < srcfile.statements.length; ++s)
{
    const stmt = srcfile.statements[s];
 //   console.log(Sym[stmt.kind])
    walk(stmt, 1);
}
*/
//console.log(srcfile);
//console.dir(srcfile,{depth: 20});

