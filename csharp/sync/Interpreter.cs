using System.Numerics;
using System.Text.Json.Nodes;

namespace sync
{
    public record FnDesc(
        string Owner,
        string Package,
        string Module,
        string Function,
        int Version)
    {
        public static FnDesc StdFnDesc(string module, string function, int version)
            => new("dark", "stdlib", module, function, version);
    }

    public enum ExpType
    {
        Unknown,
        EInt,
        EString,
        ELet,
        EVariable,
        EFnCall,
        EBinOp,
        ELambda,
        EIf,
    }

    public abstract record Expr(ExpType Type)
    {
        public static EFnCall Sfn(string module, string function, int version, IReadOnlyList<Expr> args)
            => new(new FnDesc("dark", "stdlib", module, function, version), args);

        public static EBinOp BinOp(Expr arg1, string module, string function, int version, Expr arg2)
            => new(arg1, new FnDesc("dark", "stdlib", module, function, version), arg2);
    }
    public record EInt(BigInteger Value) : Expr(ExpType.EInt);
    public record EString(string Value) : Expr(ExpType.EString);
    public record ELet(string Lhs, Expr Rhs, Expr Body) : Expr(ExpType.ELet);
    public record EVariable(string Name) : Expr(ExpType.EVariable);
    public record EFnCall(FnDesc FnDesc, IReadOnlyList<Expr> Args) : Expr(ExpType.EFnCall);
    public record EBinOp(Expr Arg1, FnDesc FnDesc, Expr Arg2) : Expr(ExpType.EBinOp);
    public record ELambda(IReadOnlyList<string> Vars, Expr Expr) : Expr(ExpType.ELambda);
    public record EIf(Expr Cond, Expr ThenBody, Expr ElseBody) : Expr(ExpType.EIf);

    public enum DValType
    {
        Unknown,
        DInt,
        DString,
        DSpecial,
        DList,
        DBool,
        DLambda,
    }

    public abstract record DVal(DValType Type)
    {
        public bool IsSpecial => Type == DValType.DSpecial;

        public DVal ToDList(IReadOnlyList<DVal> list)
            => list.FirstOrDefault(x => x.IsSpecial, new DList(list));

        public JsonNode ToJson()
            => this switch
            {
                DInt i => JsonValue.Create(i.Value)!,
                DString str => JsonValue.Create(str.Value)!,
                DList l => new JsonArray(l.Values.Select(x => x.ToJson()).ToArray())!,
                DBool b => JsonValue.Create(b.Value)!,
                DLambda _ => JsonValue.Create<string>(null)!, // TODO: should this just be "null"?
                DSpecial s when s.Value is DError e =>
                    new JsonObject(new[] { KeyValuePair.Create("error", (JsonNode?)JsonValue.Create(e.ToString())) }),
                _ => throw new InvalidOperationException(),
            };

        public static DVal Err(RuntimeError e) => new DSpecial(new DError(e));
    }
    public record DInt(BigInteger Value) : DVal(DValType.DInt);
    public record DString(string Value) : DVal(DValType.DString);
    public record DSpecial(Special Value) : DVal(DValType.DSpecial);
    public record DList(IReadOnlyList<DVal> Values) : DVal(DValType.DList);
    public record DBool(bool Value) : DVal(DValType.DBool);
    public record DLambda(IReadOnlyDictionary<string, DVal> Symtable, IReadOnlyList<string> Vars, Expr Expr) : DVal(DValType.DLambda);

    public static class Symtable
    {
        public static IReadOnlyDictionary<string, DVal> Empty => new Dictionary<string, DVal>();
        public static DVal Get(this IReadOnlyDictionary<string, DVal> st, string name)
            => st.TryGetValue(name, out var value) ? value : DVal.Err(new ErrUndefinedVariable(name));
    }

    public enum DTypeType
    {
        Unknown,
        TString,
        TInt,
        TBool,
        TList,
        TVariable,
        TFn,
    }

    public abstract record DType(DTypeType Type);
    public record TString() : DType(DTypeType.TString);
    public record TInt() : DType(DTypeType.TInt);
    public record TBool() : DType(DTypeType.TBool);
    public record TList(DType ValuesType) : DType(DTypeType.TList);
    public record TVariable(string Name) : DType(DTypeType.TVariable);
    public record TFn(IReadOnlyList<DType> ArgTypes, DType ReturnType) : DType(DTypeType.TFn);

    public record Param(string Name, DType Type, string Doc);

    public enum RuntimeErrorType
    {
        Unknown,
        NotAFunction,
        CondWithNonBool,
        FnCalledWithWrongTypes,
        UndefinedVariable,
    }

    public abstract record RuntimeError(RuntimeErrorType Type);
    public record ErrNotAFunction(FnDesc FnDesc) : RuntimeError(RuntimeErrorType.NotAFunction);
    public record ErrCondWithNonBool(DVal Cond) : RuntimeError(RuntimeErrorType.CondWithNonBool);
    public record ErrFnCalledWithWrongTypes(FnDesc FnDesc, IReadOnlyList<DVal> Values, IReadOnlyList<Param> Params) : RuntimeError(RuntimeErrorType.FnCalledWithWrongTypes);
    public record ErrUndefinedVariable(string Name) : RuntimeError(RuntimeErrorType.UndefinedVariable);

    public enum SpecialType
    {
        Unknown,
        DError,
    }

    public abstract record Special(SpecialType Type);
    public record DError(RuntimeError Error) : Special(SpecialType.DError);

    public record RetVal(DType Type, string Doc);

    public record Environment(IReadOnlyDictionary<FnDesc, BuiltInFn> Functions);

    public abstract record Result<T>
    {
        protected Result() { }
    }
    public record Ok<T>(T Value) : Result<T>;
    /// <summary>
    /// No Error arg because we currently don't need it.
    /// </summary>
    public record Error<T>() : Result<T>;

    public record BuiltInFn(
        string Name,
        IReadOnlyList<Param> Parameters,
        RetVal ReturnVal,
        Func<Environment, IReadOnlyList<DVal>, Result<DVal>> Fn);

    public static class Interpreter
    {
        public static readonly Expr FizzBuzz =
            new ELet(
                "range",
                Expr.Sfn("Int", "range", 0, new[] { new EInt(1), new EInt(100) }),
                Expr.Sfn(
                    "List",
                    "map",
                    0,
                    new Expr[]
                    {
                        new EVariable("range"),
                        new ELambda(
                            new[] { "i" },
                            new EIf(
                                Expr.BinOp(Expr.BinOp(new EVariable("i"), "Int", "%", 0, new EInt(15)),"Int", "==", 0, new EInt(0)),
                                new EString("fizzbuzz"),
                                new EIf(
                                    Expr.BinOp(Expr.BinOp(new EVariable("i"), "Int", "%", 0, new EInt(5)), "Int", "==", 0, new EInt(0)),
                                    new EString("buzz"),
                                    new EIf(
                                        Expr.BinOp(Expr.BinOp(new EVariable("i"), "Int", "%", 0, new EInt(3)), "Int", "==", 0, new EInt(0)),
                                        new EString("fizz"),
                                        Expr.Sfn("Int", "toString", 0, new [] { new EVariable("i") })
                                    )
                                )
                            )
                        )
                    }
                )
            );
    }
}
