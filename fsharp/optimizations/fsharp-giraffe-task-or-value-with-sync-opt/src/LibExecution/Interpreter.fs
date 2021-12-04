module LibExecution.Interpreter

open System.Threading.Tasks
open FSharp.Data
open FSharp.Data.JsonExtensions
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine.Unsafe

// fsharplint:disable FL0039

type TaskOrValue<'a> =
    | Value of 'a
    | Task of Task<'a>

// It seems from the docs that Delay needs to return a TaskOrValue<'T>, and
// that other functions take a TaskOrValue<'T>. However, the truth is that
// those functions (Run, Combine, While, at least), actually take the return
// type of Delay, which can be anything.
// https://fsharpforfunandprofit.com/posts/computation-expressions-builder-part3/
type Delayed<'T> = unit -> TaskOrValue<'T>

module TaskOrValue =
    let rec toTask (v: TaskOrValue<'T>) : Task<'T> =
        match v with
        | Task t -> t
        | Value v -> Task.FromResult v


// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions
type TaskOrValueBuilder() =
    member x.Bind(tv: TaskOrValue<'a>, f: 'a -> TaskOrValue<'b>) : TaskOrValue<'b> =
        match tv with
        | Value v -> f v
        | Task t ->
            Task(
                task {
                    let! v = t
                    let result = f v
                    return! TaskOrValue.toTask result
                }
            )

    member x.Bind(t: Task<'a>, f: 'a -> TaskOrValue<'b>) : TaskOrValue<'b> =
        Task(
            task {
                let! v = t
                let result = f v
                return! TaskOrValue.toTask result
            }
        )

    member x.Bind(async: Async<'a>, f: 'a -> TaskOrValue<'b>) : TaskOrValue<'b> =
        // This isn't called in the fizzbuzz benchmark, and I couldnt make the types work, so just hack it for now.
        failwith "TODO"


    member x.Bind(t: Task, f: unit -> TaskOrValue<'b>) : TaskOrValue<'b> =
        Task(
            task {
                do! t
                let result = f ()
                return! TaskOrValue.toTask result
            }
        )

    member x.Return(v: 'a) : TaskOrValue<'a> = Value v
    member x.ReturnFrom(tv: TaskOrValue<'a>) : TaskOrValue<'a> = tv
    member x.Zero() : TaskOrValue<unit> = Value()

    // These lets us use try
    member x.TryWith(tv: Delayed<'a>, f: exn -> TaskOrValue<'a>) : TaskOrValue<'a> =
        try
            match tv () with
            | Value v -> Value v
            | Task t ->
                Task(
                    task {
                        try
                            let! x = t
                            return x
                        with
                        | e -> return! TaskOrValue.toTask (f e)
                    }
                )
        with
        | e -> f e

    member x.Delay(f: unit -> TaskOrValue<'a>) : Delayed<'a> = f

    member x.Run(tv: Delayed<'a>) : TaskOrValue<'a> =
        match tv () with
        | Value v -> Value v
        | Task t -> x.Bind(t, (fun v -> Value v))

    member x.While(cond: unit -> bool, body: Delayed<'a>) : TaskOrValue<unit> =
        if not (cond ()) then
            // exit loop
            x.Zero()
        else
            // evaluate the body function, and call recursively
            x.Bind(body (), (fun _ -> x.While(cond, body)))

    member x.Combine(v0: TaskOrValue<unit>, v1: Delayed<'a>) : TaskOrValue<'a> = x.Bind(v0, (fun () -> v1 ()))


let taskv = TaskOrValueBuilder()


module FnDesc =
    type T =
        { owner: string
          package: string
          module_: string
          function_: string
          version: int }

    let fnDesc (owner: string) (package: string) (module_: string) (function_: string) (version: int) : T =
        { owner = owner
          package = package
          module_ = module_
          function_ = function_
          version = version }


    let stdFnDesc (module_: string) (function_: string) (version: int) : T =
        fnDesc "dark" "stdlib" module_ function_ version


type Expr =
    | EInt of bigint
    | EString of string
    | ELet of string * Expr * Expr
    | EVariable of string
    | EFnCall of FnDesc.T * List<Expr>
    | EBinOp of Expr * FnDesc.T * Expr
    | ELambda of List<string> * Expr
    | EIf of Expr * Expr * Expr

exception InternalException of string

type Dval =
    | DInt of bigint
    | DString of string
    | DSpecial of Special
    | DList of List<Dval>
    | DBool of bool
    | DLambda of Symtable * List<string> * Expr

    member this.isSpecial: bool =
        match this with
        | DSpecial _ -> true
        | _ -> false

    member this.toJSON() : FSharp.Data.JsonValue =
        match this with
        | DInt i -> JsonValue.Number(decimal i)
        | DString str -> JsonValue.String(str)

        | DList l ->
            l
            |> List.map (fun dv -> dv.toJSON ())
            |> List.toArray
            |> JsonValue.Array

        | DBool b -> JsonValue.Boolean b
        | DLambda _ -> JsonValue.Null
        | DSpecial (DError (e)) -> JsonValue.Record [| "error", JsonValue.String(e.ToString()) |]

    static member toDList(list: List<Dval>) : Dval =
        List.tryFind (fun (dv: Dval) -> dv.isSpecial) list
        |> Option.defaultValue (DList list)


and Symtable = Map<string, Dval>

and Param =
    { name: string
      tipe: DType
      doc: string }

(* Runtime errors can be things that happen relatively commonly (such as calling
   a function with an incorrect type), or things that aren't supposed to happen
   but technically can (such as accessing a variable which doesn't exist)
*)
and RuntimeError =
    | NotAFunction of FnDesc.T
    | CondWithNonBool of Dval
    | FnCalledWithWrongTypes of FnDesc.T * List<Dval> * List<Param>
    | FnCalledWhenNotSync of FnDesc.T * List<Dval> * List<Param>
    | UndefinedVariable of string


and Special = DError of RuntimeError

and DType =
    | TString
    | TInt
    | TBool
    | TList of DType
    (* A named variable, eg `a` in `List<a>` *)
    | TVariable of string
    | TFn of List<DType> * DType

let err (e: RuntimeError) : Dval = (DSpecial(DError(e)))

module Symtable =
    type T = Symtable
    let empty: T = Map []

    let get (st: T) (name: string) : Dval =
        st.TryFind(name)
        |> Option.defaultValue (err (UndefinedVariable name))

module Environment =
    type RetVal = { tipe: DType; doc: string }

    type BuiltInFn =
        | Sync of SyncFn
        | Async of AsyncFn

    and SyncFn =
        { name: FnDesc.T
          parameters: List<Param>
          returnVal: RetVal
          fn: (T * List<Dval>) -> Result<Dval, unit> }

    and AsyncFn =
        { name: FnDesc.T
          parameters: List<Param>
          returnVal: RetVal
          fn: (T * List<Dval>) -> TaskOrValue<Result<Dval, unit>> }

    and T = { functions: Map<FnDesc.T, BuiltInFn> }

    let envWith (functions: Map<FnDesc.T, BuiltInFn>) : T = { functions = functions }

let param (name: string) (tipe: DType) (doc: string) : Param = { name = name; tipe = tipe; doc = doc }
let retVal (tipe: DType) (doc: string) : Environment.RetVal = { tipe = tipe; doc = doc }


let sfn (module_: string) (function_: string) (version: int) (args: List<Expr>) : Expr =
    EFnCall(FnDesc.fnDesc "dark" "stdlib" module_ function_ version, args)

let binOp (arg1: Expr) (module_: string) (function_: string) (version: int) (arg2: Expr) : Expr =
    EBinOp(arg1, FnDesc.fnDesc "dark" "stdlib" module_ function_ version, arg2)

let fizzbuzz: Expr =
    ELet(
        "range",
        (sfn "Int" "range" 0 [ EInt(bigint 1); EInt(bigint 100) ]),
        (sfn
            "List"
            "map"
            0
            [ EVariable "range"
              (ELambda(
                  [ "i" ],
                  EIf(
                      (binOp (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 15))) "Int" "==" 0 (EInt(bigint 0))),
                      EString "fizzbuzz",
                      EIf(
                          binOp (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 5))) "Int" "==" 0 (EInt(bigint 0)),
                          EString "buzz",
                          EIf(
                              binOp (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 3))) "Int" "==" 0 (EInt(bigint 0)),
                              EString "fizz",
                              sfn "Int" "toString" 0 [ EVariable "i" ]
                          )
                      )
                  )
              )) ])
    )


let map_s (list: List<'a>) (f: 'a -> TaskOrValue<'b>) : TaskOrValue<List<'b>> =
    taskv {
        let! result =
            taskv {
                let! (accum: List<'b>) =
                    List.fold
                        (fun (accum: TaskOrValue<List<'b>>) (arg: 'a) ->
                            taskv {
                                // Ensure the previous computation is done first
                                let! (accum: List<'b>) = accum
                                let! result = f arg
                                return result :: accum
                            })
                        (Value [])
                        list

                return List.rev accum
            }

        return (result |> Seq.toList)
    }


let rec evalAsync (env: Environment.T) (st: Symtable.T) (e: Expr) : TaskOrValue<Dval> =
    taskv {
        let tryFindFn desc = env.functions.TryFind(desc)

        match e with
        | EInt i -> return DInt i
        | EString s -> return DString s
        | ELet (lhs, rhs, body) ->
            let! rhs = evalAsync env st rhs
            let st = st.Add(lhs, rhs)
            return! evalAsync env st body
        | EFnCall (desc, exprs) ->
            match tryFindFn desc with
            | Some fn ->
                let! args = map_s exprs (evalAsync env st)
                return! (call_fn_async env fn (Seq.toList args))
            | None -> return (err (NotAFunction desc))
        | EBinOp (arg1, desc, arg2) ->
            match tryFindFn desc with
            | Some fn ->
                let! t1 = evalAsync env st arg1
                let! t2 = evalAsync env st arg2
                return! call_fn_async env fn [ t1; t2 ]
            | None -> return err (NotAFunction desc)
        | ELambda (vars, expr) -> return DLambda(st, vars, expr)
        | EVariable (name) -> return Symtable.get st name
        | EIf (cond, thenbody, elsebody) ->
            match! (evalAsync env st cond) with
            | DBool (true) -> return! evalAsync env st thenbody
            | DBool (false) -> return! evalAsync env st elsebody
            | cond -> return err (CondWithNonBool cond)
    }

and call_fn_async (env: Environment.T) (fn: Environment.BuiltInFn) (args: List<Dval>) : TaskOrValue<Dval> =
    taskv {
        match List.tryFind (fun (dv: Dval) -> dv.isSpecial) args with
        | Some special -> return special
        | None ->
            match fn with
            | Environment.BuiltInFn.Async fn ->
                match! fn.fn (env, args) with
                | Ok result -> return result
                | Error () -> return err (FnCalledWithWrongTypes(fn.name, args, fn.parameters))
            | Environment.BuiltInFn.Sync fn ->
                match fn.fn (env, args) with
                | Ok result -> return result
                | Error () -> return err (FnCalledWithWrongTypes(fn.name, args, fn.parameters))
    }



module StdLib =
    let functions () : Map<FnDesc.T, Environment.BuiltInFn> =
        let fns: List<Environment.BuiltInFn> =
            [ Environment.BuiltInFn.Sync
                { name = (FnDesc.stdFnDesc "Int" "range" 0)
                  parameters =
                      [ param "list" (TList(TVariable("a"))) "The list to be operated on"
                        param "fn" (TFn([ TVariable("a") ], TVariable("b"))) "Function to be called on each member" ]
                  returnVal = retVal (TList(TInt)) "List of ints between lowerBound and upperBound"
                  fn =
                      (function
                      | _, [ DInt lower; DInt upper ] -> List.map DInt [ lower .. upper ] |> DList |> Ok
                      | _ -> (Error())) }
              Environment.BuiltInFn.Async
                  { name = (FnDesc.stdFnDesc "List" "map" 0)
                    parameters =
                        [ param "list" (TList(TVariable("a"))) "The list to be operated on"
                          param "fn" (TFn([ TVariable("a") ], TVariable("b"))) "Function to be called on each member" ]
                    returnVal =
                        (retVal
                            (TList(TVariable("b")))
                            "A list created by the elements of `list` with `fn` called on each of them in order")
                    fn =
                        (function
                        | env, [ DList l; DLambda (st, [ var ], body) ] ->
                            (taskv {
                                let! result =
                                    map_s
                                        l
                                        (fun dv ->
                                            let st = st.Add(var, dv)
                                            evalAsync env st body)

                                return Ok(result |> Dval.toDList)
                             })
                        | _ -> Value(Error())) }
              Environment.BuiltInFn.Sync
                  { name = (FnDesc.stdFnDesc "Int" "%" 0)
                    parameters =
                        [ param "a" TInt "Numerator"
                          param "b" TInt "Denominator" ]
                    returnVal = (retVal TInt "Returns the modulus of a / b")
                    fn =
                        (function
                        | env, [ DInt a; DInt b ] ->
                            try
                                Ok(DInt(a % b))
                            with
                            | _ -> Ok(DInt(bigint 0))
                        | _ -> Error()) }
              Environment.BuiltInFn.Sync
                  { name = (FnDesc.stdFnDesc "Int" "==" 0)
                    parameters =
                        [ param "a" TInt "a"
                          param "b" TInt "b" ]
                    returnVal =
                        (retVal
                            TBool
                            "True if structurally equal (they do not have to be the same piece of memory, two dicts or lists or strings with the same value will be equal), false otherwise")
                    fn =
                        (function
                        | env, [ DInt a; DInt b ] -> Ok(DBool(a = b))
                        | _ -> Error()) }
              Environment.BuiltInFn.Sync
                  { name = (FnDesc.stdFnDesc "Int" "toString" 0)
                    parameters = [ param "a" TInt "value" ]
                    returnVal = (retVal TString "Stringified version of a")
                    fn =
                        (function
                        | env, [ DInt a ] -> Ok(DString(a.ToString()))
                        | _ -> Error()) }
              Environment.BuiltInFn.Async
                  { name = (FnDesc.stdFnDesc "HttpClient" "get" 0)
                    parameters = [ param "url" TString "URL to fetch" ]
                    returnVal = (retVal TString "Body of response")
                    fn =
                        (function
                        | env, [ DString url ] ->
                            try
                                taskv {
                                    let! response = Http.AsyncRequestString(url)
                                    let info = JsonValue.Parse(response)
                                    return Ok(DString(info?data.AsString()))
                                }
                            with
                            | e ->
                                printfn "error in HttpClient::get: %s" (e.ToString())
                                Value(Error())
                        | _ -> Value(Error())) } ]

        fns
        |> List.map
            (fun fn ->
                match fn with
                | Environment.Async f -> (f.name), fn
                | Environment.Sync f -> (f.name), fn)
        |> Map

let env =
    Environment.envWith (StdLib.functions ())

let runAsync (e: Expr) : Task<Dval> =
    TaskOrValue.toTask (evalAsync env Symtable.empty e)

let runJSONAsync (e: Expr) : Task<string> =
    task {
        let! result = (runAsync e)
        let json = result.toJSON ()
        return json.ToString()
    }
