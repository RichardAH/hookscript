import * as ts from 'typescript';
import * as fs from 'fs';

if (process.argv.length != 3)
{
    console.error("usage: " + process.argv[1] + " somefile.hs");
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

function walk(
    node: ts.Node, srcfile: ts.SourceFile, findKind: any, visit: any ) : void
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
    {
        const cont = visit(node);
        if (cont === true)
            return;
    }

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
        msg + ". " +
        "Near: `" + node.getText(ctx.srcfile) + "`.",
    );
    process.exit(1);
}

function inferImports(ctx: any) : void
{
    walk(ctx.srcfile, ctx.srcfile, [Sym.CallExpression],
    (node:any) =>
    {
        const funcname = node.expression.escapedText;

        if (funcname == 'hook' || funcname == 'cbak')
            die(ctx, node, "hook and cbak cannot be called within the hook.");

        if (hookapi[funcname])
        {

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
            return;
        }

        // macro type


    });
}


const string_types: any =
{
    'string': true,
    'bigstring': true
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
    'int' : 'i32',
    'xfl' : 'i64',
    'string' : 'i32,i32',
    'bigstring': 'i32,i32'
}

// this can be queried to see if a type can go into a local/global variable as opposed to memory
const primitive_types : any = 
(():any=>{

    let out = {...int_types};
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
    addTypes(int_types);
    addTypes(obj_types);
    addTypes(aliased_types);
    return out;
})();

function processFunctions(ctx: any) : void
{

    walk(ctx.srcfile, ctx.srcfile, [Sym.FunctionDeclaration],
    (node:any):void=>
    {
        const funcname = node.name.escapedText;

        if (ctx.funcs[funcname] || ctx.macros[funcname])
            die(ctx, node, "Duplicate function definition: " + funcname);

        if (hookapi[funcname])
            die(ctx, node, "Cannot redefine Hook APIs: " + funcname);

        const realfunc = funcname == 'hook' || funcname == 'cbak';

        let params = [];
        let param_types = [];           // the types in hook script: string, Object, int, etc.
        let param_types_wasm = [];      // wasm types only (i32, i64)

        for (let i = 0; i < node.parameters.length; ++i)
        {
            const param = node.parameters[i];
            const name = param.name.escapedText;
            const type = 
                param.type.typeName && param.type.typeName.escapedText
                ? param.type.typeName.escapedText 
                : nodeToString(ctx, param.type);

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

            param_types_wasm.push(getTypeName(ctx, param, {noalias:true}));
        }

        if (realfunc && params.length != 1)
            die(ctx, node, "hook and cbak must have method signature i32->i64. [2]");

        const rettype = 
             node.type === undefined
                ? "void"
                 : ( node.type.typeName && node.type.typeName.escapeText 
                     ? node.type.typeName.escapedText
                     : nodeToString(ctx, node.type));

        const rettype_wasm = getTypeName(ctx, node, {noalias:true});

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

        const sig_wasm = (param_types_wasm.length == 0 ? 'void' : param_types_wasm.join(',')) + '->' + rettype_wasm;

        if (realfunc)
        {
            ctx.funcs[funcname] =
            {
                idx: ctx.func_count,
                name: funcname,
                type: parseMethodSig(sig),
                node: node,
                typeidx: 0,         // hard coded always the hook/cbak type
                type_wasm: parseMethodSig(sig_wasm)
            }
            ctx.funcs_map[ctx.func_count++] = funcname;
        }
        else
        {
            // include a type so macro can be used in a block with a typeidx
            let typeidx = ctx.types[sig_wasm];
            if (typeidx === undefined)
            {
                typeidx = ctx.type_count++;
                ctx.types[sig_wasm] = typeidx;
                ctx.types_map[typeidx] = sig_wasm;
            }

            ctx.macros[funcname] =
            {
                idx: ctx.macro_count,
                name: funcname,
                type: parseMethodSig(sig),
                node: node,
                typeidx: typeidx,
                type_wasm: parseMethodSig(sig_wasm)
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


function getTypeName(ctx:any, node:any, extra:any = {}) : string
{
    if (!node)
        return "any";

    let tn = "any";
   
//    d(node, 3);

    if (node.type)
    {
        if (node.type.typeName && node.type.typeName.escapedText)
            tn = node.type.typeName.escapedText.trim();
        else
            tn = nodeToString(ctx, node.type);
        
        node = node.type;
    }


    if (tn.match(/^Array.*/) &&
        node.typeArguments && node.typeArguments[0])
    {
        if (node.typeArguments.length == 1)
        {
            const arg = node.typeArguments[0];

            let inner = "any";

            if (arg.typeName &&
                arg.typeName.escapedText)
                inner = arg.typeName.escapedText.trim();
            else if (arg.typeName)
                inner = ctx.rawfile.substr(arg.typeName.pos, arg.typeName.end - arg.typeName.pos).trim();
            else
                inner = ctx.rawfile.substr(arg.pos, arg.end - arg.pos).trim();;

           
            if (aliased_types[inner])
                inner = aliased_types[inner];

            tn = "Array<" + inner + ">";
        }
    }


    if (aliased_types[tn] && extra.noalias)
        tn = aliased_types[tn];

    if (tn == 'xfl' && extra.noalias)
        tn = 'i64';

    return tn;
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
            if (child.kind == Sym.FunctionDeclaration)
                return true;
            node._toplevel = true;
        });
    });
}

let dataseg : any = {};

function tagAll(ctx:any) : void
{
    walk(ctx.srcfile, ctx.srcfile, undefined,
    (node:any):void=>
    {
        node._raw = nodeToString(ctx, node);
        if (node.kind !== undefined)
            node._sym = Sym[node.kind];
    });
}
let ctx:any =
{
    filename: filename,
    rawfile: rawfile,
    srcfile: srcfile,

    block_depth: 0,

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

function setLocal(ctx:any, func:any, decl:any):void
{
    console.log("    local.set " + decl._localidx);
}

function processVariableStatement(ctx: any, func:any, node: any, varmap: any, suppress_output:boolean): void
{
    if (!node.declarationList || 
        !node.declarationList.declarations || 
         node.declarationList.declarations.length <= 0)
    {
        console.error("Warn: variable statement without declaration list");
        return;
    }

    const decls = node.declarationList.declarations;

    for (let i = 0; i < decls.length; ++i)
    {
        const decl = decls[i];

        const name = decl.name.escapedText.trim();
        const tn = getTypeName(ctx, decl, {noalias:true});
        
        if (varmap[name] != decl)
        {
            if (varmap[name] !== undefined)
                die(ctx, node, "Variable " + name + " declared more than once.");

            decl._tn = tn;
            varmap[name] = decl;
        }

        if (decl.initializer === undefined)
            die(ctx, decl, "Initializer missing. All variables must be initialized.");

        if (suppress_output)
            continue;

        if (primitive_types[tn])
        {
            //function processExpression(ctx:any, func:any, expr:any, varmap:any, extra:any = {}):void
            processExpression(ctx, func, decl.initializer, varmap, {rettype: tn});
            setLocal(ctx, func, decl);

        }
        else
        {
        }
        // RH TODO: processExpression onto stack and store in variable iff local
        // processExpression onto stack and store in memory if not local
        
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

        processVariableStatement(ctx, {}, node, varmap, true);
    });

    return varmap;
}

tagAll(ctx);
validateTopLevelAST(ctx);
validateTypes(ctx);

inferImports(ctx);
processFunctions(ctx);

validateAndShakeFunctions(ctx); // remove all unused/unreferenced macros

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
            "(result " + type.rettype.replace(',', ' ') + ")))");
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

// returns the type of the expression if available
// extra.tee    -- use local.tee instead of local.get
// extra.noout  -- do not generate code
function processExpression(ctx:any, func:any, expr:any, varmap:any, extra:any = {}):any
{
    const k = expr.kind; 
    console.error("=>> processExpression " + Sym[k] + "<<=");
    switch (k)
    {
        case Sym.Identifier:
        {
            const name = expr.escapedText.trim();
            if (!varmap[name])
                die(ctx, expr, "Identifier " + name + " not understood. Missing variable declaration?");

            const node:any = varmap[name];
            if (node._localidx)
            {
                // it's a local variable
                console.error("==>variable: " + name);

                // todo: is it a get or a set operation??
                const instr = (extra.tee ? "local.tee" : "local.get");
                if (!extra.noout)
                    console.log("    " + instr + " " + node._localidx);
                return node._tn;
            }
            else
            {
                console.log("    >>todo load memory var: " + name);
                // it's a memory variable
            }
            return;
        }
        case Sym.NumericLiteral:
        {
            let rettype = "i64";
            if (extra.rettype)
                rettype = extra.rettype;
            let nl:any;
            try
            {
                nl = parseInt(''+ expr.text.trim());
            }
            catch (e)
            {
                die(ctx, expr, "NumericalLiteral was not parsable.");
            }

            if (rettype.substr(0,1) == 'i')
            {
                const bits = Math.ceil(Math.log(nl)/Math.log(2));
                if (bits > parseInt(rettype.substr(1)))
                    die(ctx, expr, "Numeric literal was larger than " + rettype + " can hold.");
            }

            if (!extra.noout)
                console.log("    " + rettype + ".const " + expr.text.trim());
            return;
        }
        case Sym.BigIntLiteral:
        case Sym.StringLiteral:
        case Sym.RegularExpressionLiteral:
        case Sym.NoSubstitutionTemplateLiteral:
        case Sym.TypeLiteral:
        case Sym.LiteralType:
        case Sym.ArrayLiteralExpression:
        case Sym.ObjectLiteralExpression:
        case Sym.JSDocTypeLiteral:

        case Sym.ArrayLiteralExpression:
        case Sym.ObjectLiteralExpression:
        case Sym.PropertyAccessExpression:
        case Sym.ElementAccessExpression:
        {
            die(ctx, expr, "not impl");
        }
        case Sym.CallExpression:
        {
            if (expr.expression.kind != Sym.Identifier)
                die(ctx, expr, "Functions must be called using literals. Function pointers are not supported.");

            const id = expr.expression.escapedText.trim();

            // RH TODO: check argument types
            const args = expr.arguments;
            for (let i = 0; i < args.length; ++i)
            {
                const arg = args[i];
                processExpression(ctx, func, arg, varmap, {});
            }

            if (ctx.funcs[id])
                die(ctx, expr, "Calling hook/cbak is not supported.");
            else
            if (ctx.macros[id])
            {
                // inline the macro
                //
                // RH TODO: check params / types
               
                const macro = ctx.macros[id];
                const stmts = macro.node.body.statements;
                if (stmts.length > 0)
                {
                    // output start of block
                    console.log("    block ;" + macro.typeidx + ";");
                    ctx.block_depth++;
                    for (let i = 0; i < stmts.length; ++i)
                       processStatement(ctx, func, stmts[i], varmap, {branch_returns: true});
                    console.log("    end");
                    ctx.block_depth--;
                }
            }
            else
            if (ctx.imports[id])
            {
                const imp:any = ctx.imports[id];
                console.log('    call ' + imp.idx);
            }
            else
                die(ctx, expr, "Unknown function: `" + id + "`");

            //if (!extra.noout)
            
            // RH UPTO
            return;
        }
        case Sym.NewExpression:
        {
            die(ctx, expr, "New keyword is not supported.");
        }
        case Sym.TaggedTemplateExpression:
        case Sym.TypeAssertionExpression:
        {
            die(ctx, expr, "Unknown expression type: " + Sym[k]);

        }
        case Sym.ParenthesizedExpression:
        {
            processExpression(ctx, func, expr.expression, varmap, extra);
            return;
        }
        case Sym.FunctionExpression:
        case Sym.DeleteExpression:
        case Sym.TypeOfExpression:
        case Sym.VoidExpression:
        case Sym.AwaitExpression:
        case Sym.PrefixUnaryExpression:
        case Sym.PostfixUnaryExpression:
        {

            die(ctx, expr, "Unknown expression type: " + Sym[k]);
        }
        case Sym.BinaryExpression:
        {
            const op = expr.operatorToken.kind;

            if (op == Sym.EqualsToken)
            {   
                //assignment operator requires a tee here
                //and operations need to be done in reverse order
                const tn = processExpression(ctx, func, expr.left, varmap, {noout:true});
                processExpression(ctx, func, expr.right, varmap, {rettype:tn});
                processExpression(ctx, func, expr.left, varmap, {tee: true});

                if (!extra.rettype)
                    return;

                const havebits = tn.substr(1);
                const wantbits = extra.rettype.substr(1);

                const s = tn.substr(0,1) == 's' || extra.rettype.substr(0,1) == 's' ? 's' : 'u';
            
                if (havebits != wantbits)
                {
                    if (havebits == '32')
                        console.log('    i64.extend_i32_' + s);
                    else
                        console.log('    i32.trunc_i64_' + s);
                }
                return;
            }

            processExpression(ctx, func, expr.left, varmap, extra);
            processExpression(ctx, func, expr.right, varmap);
//            processOperator(ctx, func, expr, op, varmap);
            return;
        }
        case Sym.ConditionalExpression:
        case Sym.TemplateExpression:
        case Sym.YieldExpression:
        case Sym.ClassExpression:
        case Sym.OmittedExpression:
        case Sym.ExpressionWithTypeArguments:
        case Sym.AsExpression:
        case Sym.NonNullExpression:
        case Sym.SyntheticExpression:
        case Sym.ExpressionStatement:
        case Sym.JsxExpression:
        case Sym.JSDocTypeExpression:
        case Sym.PartiallyEmittedExpression:
        case Sym.CommaListExpression:
        case Sym.SyntheticReferenceExpression:

        default:
            die(ctx, expr, "Unknown expression type: " + Sym[k]);
    }
}

function processStatement(ctx:any, func:any, stmt:any, varmap:any, extra:any = {}):void
{
    const k = stmt.kind;
    console.error("=>> processStatement " + Sym[k] + "<<=");
    switch (k)
    {
        case Sym.EmptyStatement:
            return;

        case Sym.VariableStatement:
        {
            processVariableStatement(ctx, func, stmt, varmap, false);
            return;
        }
        case Sym.ExpressionStatement:
        {
            const tn = processExpression(ctx, func, stmt.expression, varmap);
            // if there's a type left over on the stack, drop it
            if (tn)
                console.log("    drop");
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
                processExpression(ctx, func, stmt.expression, varmap, {rettype: func.type.rettype});
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
            param._tn = getTypeName(ctx, param, {noalias:true});
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
                let tn = getTypeName(ctx, decls[i], {noalias:true});
                if (primitive_types[tn])
                {
                    localsout += (localsout == "" ? "" : " ") + tn;
                    decls[i]._localidx = localidx++;
                    decls[i]._tn = tn;
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


function printRecursiveFrom(
    node: ts.Node, indentLevel: number, srcfile: ts.SourceFile
) {
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

    const indentation = "-".repeat(indentLevel);
    let syntaxKind = Sym[node.kind];
    if (markers[syntaxKind] != undefined)
        syntaxKind = markers[syntaxKind];

    const nodeText = node.getText(srcfile);
    console.error(`${indentation}${syntaxKind}: ${nodeText}`);

    node.forEachChild(child =>
        printRecursiveFrom(child, indentLevel + 1, srcfile)
    );
}

//printRecursiveFrom(ctx.srcfile, 0, ctx.srcfile);
