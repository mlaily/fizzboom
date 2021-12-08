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

        public JsonNode? ToJson()
            => this switch
            {
                DInt i => JsonValue.Create(i.Value),
                DString str => JsonValue.Create(str.Value),
                DList l => new JsonArray(l.Values.Select(x => x.ToJson()).ToArray()),
                DBool b => JsonValue.Create(b.Value),
                DLambda _ => null,
                DSpecial s when s.Value is DError e =>
                    new JsonObject(new[] { KeyValuePair.Create("error", (JsonNode?)JsonValue.Create(e.ToString())) }),
                _ => throw new InvalidOperationException(),
            };

        public static DVal ToDList(IReadOnlyList<DVal> list)
            => list.FirstOrDefault(x => x.IsSpecial, new DList(list));

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
        // TODO: how do we make this more efficient? (maybe we could use FSharp Map instead?)
        public static IReadOnlyDictionary<string, DVal> CloneAdd(IReadOnlyDictionary<string, DVal> st, string name, DVal val)
        {
            var copy = st.ToDictionary(kvp => kvp.Key, kvp => kvp.Value);
            copy.Add(name, val);
            return copy;
        }
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
        FnDesc Name,
        IReadOnlyList<Param> Parameters,
        RetVal ReturnVal,
        Func<Environment, IReadOnlyList<DVal>, Result<DVal>> Fn);

    public static class Interpreter
    {
        public static Expr FizzBuzz { get; } =
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

        public static DVal Eval(Environment env, IReadOnlyDictionary<string, DVal> st, Expr e)
        {
            switch (e)
            {
                case EInt i: return new DInt(i.Value);
                case EString s: return new DString(s.Value);
                case ELet let:
                    {
                        var rhs = Eval(env, st, let.Rhs);
                        var newSt = Symtable.CloneAdd(st, let.Lhs, rhs);
                        return Eval(env, newSt, let.Body);
                    }
                case EFnCall call:
                    {
                        if (env.Functions.TryGetValue(call.FnDesc, out var fn))
                        {
                            var args = call.Args.Select(x => Eval(env, st, x));
                            return CallFn(env, fn, args.ToList());
                        }
                        else
                            return DVal.Err(new ErrNotAFunction(call.FnDesc));
                    }
                case EBinOp binOp:
                    {
                        if (env.Functions.TryGetValue(binOp.FnDesc, out var fn))
                        {
                            var arg1 = Eval(env, st, binOp.Arg1);
                            var arg2 = Eval(env, st, binOp.Arg2);
                            return CallFn(env, fn, new[] { arg1, arg2 });
                        }
                        else
                            return DVal.Err(new ErrNotAFunction(binOp.FnDesc));
                    }
                case ELambda lambda: return new DLambda(st, lambda.Vars, lambda.Expr);
                case EVariable variable: return Symtable.Get(st, variable.Name);
                case EIf eif:
                    {
                        var cond = Eval(env, st, eif.Cond);
                        return cond switch
                        {
                            DBool x when x.Value == true => Eval(env, st, eif.ThenBody),
                            DBool x when x.Value == false => Eval(env, st, eif.ElseBody),
                            _ => DVal.Err(new ErrCondWithNonBool(cond)),
                        };
                    }
                default: throw new InvalidOperationException();
            }
        }

        private static DVal CallFn(Environment env, BuiltInFn fn, IReadOnlyList<DVal> args)
        {
            var firstSpecial = args.FirstOrDefault(x => x.IsSpecial);
            if (firstSpecial != null)
                return firstSpecial;
            else
            {
                var result = fn.Fn(env, args); ;
                return result switch
                {
                    Ok<DVal> ok => ok.Value,
                    Error<DVal> error => DVal.Err(new ErrFnCalledWithWrongTypes(fn.Name, args, fn.Parameters)),
                    _ => throw new InvalidOperationException(),
                };
            }
        }

        public static IReadOnlyDictionary<FnDesc, BuiltInFn> Functions()
        {
            var list = new List<BuiltInFn>()
            {
                new BuiltInFn(
                    Name: FnDesc.StdFnDesc("Int", "range", 0),
                    Parameters: new[]
                    {
                        new Param("lowerBound", new TInt(), "Range lower bound (inclusive)"),
                        new Param("upperBound", new TInt(), "Range upper bound (inclusive)"),
                    },
                    ReturnVal: new RetVal(new TList(new TInt()), "List of ints between lowerBound and upperBound"),
                    Fn: (env, args) =>
                    {
                        if (args.Count == 2 && args[0] is DInt lower && args[1] is DInt upper)
                        {
                            // TODO: the range should support BigInteger properly
                            var range = Enumerable.Range((int)lower.Value, (int)(upper.Value - lower.Value + 1)).Select(x => new DInt(x));
                            return new Ok<DVal>(new DList(range.ToList()));
                        }
                        else
                            return new Error<DVal>();
                    }),
                new BuiltInFn(
                    Name: FnDesc.StdFnDesc("List", "map", 0),
                    Parameters: new[]
                    {
                        new Param("list", new TList(new TVariable("a")), "The list to be operated on"),
                        new Param("fn", new TFn(new[] { new TVariable("a") }, new TVariable("b")), "Function to be called on each member"),
                    },
                    ReturnVal: new RetVal(new TList(new TVariable("b")), "A list created by the elelemnts of `list` with `fn` called on each of them in order"),
                    Fn: (env, args) =>
                    {
                        if (args.Count == 2 && args[0] is DList l && args[1] is DLambda lambda && lambda.Vars.Count == 1)
                        {
                            var result = l.Values.Select(dv =>
                            {
                                var st = Symtable.CloneAdd(lambda.Symtable, lambda.Vars[0], dv);
                                return Eval(env, st, lambda.Expr);
                            });
                            return new Ok<DVal>(DVal.ToDList(result.ToList()));
                        }
                        else
                            return new Error<DVal>();
                    }),
                new BuiltInFn(
                    Name: FnDesc.StdFnDesc("Int", "%", 0),
                    Parameters: new[]
                    {
                        new Param("a", new TInt(), "Numerator"),
                        new Param("b", new TInt(), "Denominator"),
                    },
                    ReturnVal: new RetVal(new TInt(), "Returns the modulus of a / b"),
                    Fn: (env, args) =>
                    {
                        if (args.Count == 2 && args[0] is DInt a && args[1] is DInt b)
                        {
                            try
                            {
                                return new Ok<DVal>(new DInt(a.Value % b.Value));
                            }
                            catch
                            {
                                return new Ok<DVal>(new DInt(0));
                            }
                        }
                        else
                            return new Error<DVal>();
                    }),
                new BuiltInFn(
                    Name: FnDesc.StdFnDesc("Int", "==", 0),
                    Parameters: new[]
                    {
                        new Param("a", new TInt(), "a"),
                        new Param("b", new TInt(), "b"),
                    },
                    ReturnVal: new RetVal(new TBool(), "True if equal"),
                    Fn: (env, args) =>
                    {
                        if (args.Count == 2 && args[0] is DInt a && args[1] is DInt b)
                            return new Ok<DVal>(new DBool(a == b));
                        else
                            return new Error<DVal>();
                    }),
                new BuiltInFn(
                    Name: FnDesc.StdFnDesc("Int", "toString", 0),
                    Parameters: new[]
                    {
                        new Param("a", new TInt(), "value"),
                    },
                    ReturnVal: new RetVal(new TString(), "Stringified version of a"),
                    Fn: (env, args) =>
                    {
                        if (args.Count == 1 && args[0] is DInt a)
                            return new Ok<DVal>(new DString(a.Value.ToString()));
                        else
                            return new Error<DVal>();
                    }),
                //new BuiltInFn(
                //    Name: FnDesc.StdFnDesc("HttpClient", "get", 0),
                //    Parameters: new[]
                //    {
                //        new Param("url", new TString(), "URL to fetch"),
                //    },
                //    ReturnVal: new RetVal(new TString(), "Body of response"),
                //    Fn: (env, args) =>
                //    {
                //        if (args.Count == 1 && args[0] is DString url)
                //            return new Ok<DVal>(new DString(a.Value.ToString()));
                //        else
                //            return new Error<DVal>();
                //    }),
            };
            return list.ToDictionary(x => x.Name);
        }

        public static Environment Env { get; } = new Environment(Functions());

        public static string RunJson(Expr e)
        {
            var result = Eval(Env, Symtable.Empty, e);
            var json = result.ToJson();
            return json.ToString();
        }
    }
}
