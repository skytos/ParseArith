type Expr =
    | Add of Expr * Expr
    | Mul of Expr * Expr
    | Int of int

exception BadExpression of string * string List

let combine(v1, v2, f) =
    match v2 with
    | Some(v2) -> f(v1, v2)
    | None -> v1

let maybeInt(str: string): int Option =
    try
        Some(int str)
    with
        | :? System.FormatException -> None
        | ex -> reraise()

let rec expr_prime (tokens: string List) : (Expr Option * string List) =
    match tokens with
    | [] -> (None, tokens)
    | "+" :: rest ->
        let v1, ts1 = term rest
        let v2, ts2 = expr_prime ts1
        (Some(combine(v1, v2, Add)), ts2)
    | _ :: rest -> (None, tokens)

and expr (tokens: string List) : (Expr * string List) =
    let v1, ts1 = term tokens
    let v2, ts2 = expr_prime ts1
    (combine(v1, v2, Add), ts2)

and term_prime (tokens: string List) : (Expr Option * string List) =
    match tokens with
    | [] -> (None, tokens)
    | "*" :: rest ->
        let v1, ts1 = fact rest
        let v2, ts2 = term_prime ts1
        (Some(combine(v1, v2, Mul)), ts2)
    | _ :: rest -> (None, tokens)

and term (tokens: string List) : (Expr * string List) =
    let v1, ts1 = fact tokens
    let v2, ts2 = term_prime ts1
    (combine(v1, v2, Mul), ts2)

and fact (tokens: string List) : (Expr * string List) =
    match tokens with
    | [] -> raise (BadExpression("empty fact", tokens))
    | "(" :: rest ->
        let v1, ts1 = expr rest
        match ts1 with
        | [] -> raise (BadExpression("no closing parens", tokens))
        | ")" :: ts2 -> (v1, ts2)
        | _ :: ts2 -> raise (BadExpression("no closing parens", tokens))
    | t :: rest ->
        match maybeInt t with
        | Some(i) -> (Int(i), rest)
        | None -> raise (BadExpression("bad fact", tokens))

let str_expr = "1 * ( 2 + ( 3 + 4 ) ) + 5"
let e, r = expr(Array.toList(str_expr.Split([|' '|])))

printfn $"{str_expr}"
printfn $"{e} {r}"