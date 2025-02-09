﻿module LibExecution.Interpreter

open System.Threading.Tasks
open FSharp.Data
open FSharp.Data.JsonExtensions
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine.Unsafe

// fsharplint:disable FL0039

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
    | DTask of Ply.Ply<Dval>

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
        | DTask _ -> raise (InternalException "Stringifying a task is a no-no")
        | DSpecial (DError (e)) -> JsonValue.Record [| "error", JsonValue.String(e.ToString()) |]

    static member toDList(list: List<Dval>) : Dval =
        List.tryFind (fun (dv: Dval) -> dv.isSpecial) list
        |> Option.defaultValue (DList list)


    member dv.toTask() : Ply.Ply<Dval> =
        match dv with
        | DTask t -> t
        | _ -> uply { return dv }


    member dv.map(f: Dval -> Dval) : Dval =
        match dv with
        | DTask t ->
            DTask(
                uply {
                    let! dv = t
                    return (f dv)
                }
            )
        | _ -> dv

    member dv.bind(f: Dval -> Dval) : Dval =
        match dv with
        | DTask t ->
            DTask(
                uply {
                    let! dv = t
                    // If `f` returns a task, don't wrap it
                    return! (f dv).toTask ()
                }
            )
        | _ -> f dv

    member dv1.bind2 (dv2: Dval) (f: Dval -> Dval -> Dval) : Dval =
        match dv1, dv2 with
        | _, DTask _
        | DTask _, _ ->
            DTask(
                uply {
                    let! t1 = dv1.toTask ()
                    let! t2 = dv2.toTask ()
                    // If `f` returns a task, don't wrap it
                    return! (f t1 t2).toTask ()
                }
            )
        | _ -> f dv1 dv2



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

    and BuiltInFn =
        { name: FnDesc.T
          parameters: List<Param>
          returnVal: RetVal
          fn: (T * List<Dval>) -> Result<Dval, unit> }

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

let map_s (list: List<'a>) (f: 'a -> Dval) : Ply.Ply<List<Dval>> =
    uply {
        let! result =
            uply {
                let! (accum: List<Dval>) =
                    List.fold
                        (fun (accum: Ply.Ply<List<Dval>>) (arg: 'a) ->
                            uply {
                                // Ensure the previous computation is done first
                                let! (accum: List<Dval>) = accum
                                let! result = (f arg).toTask ()
                                return result :: accum
                            })
                        (uply { return [] })
                        list

                return List.rev accum
            }

        return (result |> Seq.toList)
    }


let rec evalAsync (env: Environment.T) (st: Symtable.T) (e: Expr) : Dval =
    let tryFindFn desc = env.functions.TryFind(desc)

    match e with
    | EInt i -> DInt i
    | EString s -> DString s
    | ELet (lhs, rhs, body) ->
        let rhs = evalAsync env st rhs

        rhs.bind
            (fun rhs ->
                let st = st.Add(lhs, rhs)
                evalAsync env st body)

    | EFnCall (desc, exprs) ->
        (match tryFindFn desc with
         | Some fn ->
             DTask(
                 uply {
                     let! args = map_s exprs (evalAsync env st)
                     return! (call_fn_async env fn (Seq.toList args)).toTask ()
                 }
             )
         | None -> err (NotAFunction desc))
    | EBinOp (arg1, desc, arg2) ->
        (match tryFindFn desc with
         | Some fn ->
             let t1 = (evalAsync env st arg1)
             let t2 = (evalAsync env st arg2)

             t1.bind2 t2 (fun arg1 arg2 -> call_fn_async env fn [ arg1; arg2 ])
         | None -> err (NotAFunction desc))
    | ELambda (vars, expr) -> DLambda(st, vars, expr)
    | EVariable (name) -> Symtable.get st name
    | EIf (cond, thenbody, elsebody) ->
        let cond = (evalAsync env st cond)

        cond.bind
            (function
            | DBool (true) -> evalAsync env st thenbody
            | DBool (false) -> evalAsync env st elsebody
            | cond -> err (CondWithNonBool cond))

and call_fn_async (env: Environment.T) (fn: Environment.BuiltInFn) (args: List<Dval>) : Dval =
    match List.tryFind (fun (dv: Dval) -> dv.isSpecial) args with
    | Some special -> special
    | None ->
        match fn.fn (env, args) with
        | Ok result -> result
        | Error () -> err (FnCalledWithWrongTypes(fn.name, args, fn.parameters))


module StdLib =
    let functions () : Map<FnDesc.T, Environment.BuiltInFn> =
        let fns: List<Environment.BuiltInFn> =
            [ { name = (FnDesc.stdFnDesc "Int" "range" 0)
                parameters =
                    [ param "list" (TList(TVariable("a"))) "The list to be operated on"
                      param "fn" (TFn([ TVariable("a") ], TVariable("b"))) "Function to be called on each member" ]
                returnVal = retVal (TList(TInt)) "List of ints between lowerBound and upperBound"
                fn =
                    (function
                    | _, [ DInt lower; DInt upper ] -> List.map DInt [ lower .. upper ] |> DList |> Ok
                    | _ -> Error()) }
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
                        Ok(
                            DTask(
                                uply {
                                    let! result =
                                        map_s
                                            l
                                            (fun dv ->
                                                let st = st.Add(var, dv)
                                                evalAsync env st body)

                                    return (result |> Dval.toDList)
                                }
                            )
                        )

                    | _ -> Error()) }
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
              { name = (FnDesc.stdFnDesc "Int" "toString" 0)
                parameters = [ param "a" TInt "value" ]
                returnVal = (retVal TString "Stringified version of a")
                fn =
                    (function
                    | env, [ DInt a ] -> Ok(DString(a.ToString()))
                    | _ -> Error()) }
              { name = (FnDesc.stdFnDesc "HttpClient" "get" 0)
                parameters = [ param "url" TString "URL to fetch" ]
                returnVal = (retVal TString "Body of response")
                fn =
                    (function
                    | env, [ DString url ] ->
                        try
                            Ok(
                                DTask(
                                    uply {
                                        let! response = Http.AsyncRequestString(url)
                                        let info = JsonValue.Parse(response)
                                        return (DString(info?data.AsString()))
                                    }
                                )
                            )
                        with
                        | e ->
                            printfn "error in HttpClient::get: %s" (e.ToString())
                            Error()
                    | _ -> Error()) } ]

        fns |> List.map (fun fn -> (fn.name), fn) |> Map


let env =
    Environment.envWith (StdLib.functions ())

let runAsync (e: Expr) : Task<Dval> =
    Ply.TplPrimitives.runPlyAsTask ((evalAsync env Symtable.empty e).toTask ())

let runJSONAsync (e: Expr) : Task<string> =
    uply {
        let! result = (runAsync e)
        let json = result.toJSON ()
        return json.ToString()
    }
    |> Ply.TplPrimitives.runPlyAsTask
