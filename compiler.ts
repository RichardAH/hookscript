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
        }
    } while (m);
    return ret;
})();

const filename = process.argv[2];
const code = Buffer.from(fs.readFileSync(filename)).toString('utf-8');

const srcfile = ts.createSourceFile(
    filename, code, ts.ScriptTarget.Latest
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
    node: ts.Node, srcfile: ts.SourceFile, findKind: number, visit: (node:ts.Node)=>void ) : void
{
    if (node.kind == undefined)
        return;

    if (node.kind == findKind)
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
    console.error("Error: " + msg, node.getText(ctx.srcfile));
    process.exit(1);
}

function processImports(ctx: any) : void
{
    walk(ctx.srcfile, ctx.srcfile, ts.SyntaxKind.CallExpression, 
    (node:any):void=> 
    {
        //console.log("walked to: ", node.getText(srcfile));
        const funcname = node.expression.escapedText;

        if (hookapi[funcname] == undefined)
            die(ctx, node, "function " + funcname + " is not a HookApi");

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

const allowed_macro_types:any =
{
    'i32': true,
    'i64': true,
    'string': true,
    'bigstring': true,
    'xfl': true
}

function processFunctions(ctx: any) : void
{

    walk(ctx.srcfile, ctx.srcfile, ts.SyntaxKind.FunctionDeclaration, 
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
                if (!allowed_macro_types[type])
                    die(ctx, node, "in macro " + funcname + ", parameter type: " + type + " not allowed.");
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
            if (rettype != 'void' && !allowed_macro_types[rettype])
                die(ctx, node, "in macro " + funcname + ", return type: " + rettype + " not allowed.");
        }

        if (realfunc)
        {
            ctx.funcs[funcname] =
            {
                idx: ctx.func_count,
                name: funcname,
                rettype: rettype,
                params: params,
                body: node.body,
                typeidx: 0,         // hard coded always the hook/cbak type
            }
            ctx.funcs_map[ctx.func_count++] = funcname;
        }
        else
        {
            const sig = (param_types.length == 0 ? 'void' : param_types.join(',')) + '->' + rettype;
            ctx.macros[funcname] =
            {
                idx: ctx.macro_count,
                name: funcname,
                rettype: rettype,
                params: params,
                body: node.body,
                sig: sig
            }
            ctx.macros_map[ctx.macro_count++] = funcname;
        }
    });
}

let ctx:any = 
{
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
};

processImports(ctx);
processFunctions(ctx);


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

console.log(')');

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

