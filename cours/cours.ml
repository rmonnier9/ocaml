let main () =
        print_endline "Hello world !"

let () = main ()


Types
int 10 float 3.14 char '1' string "po" couplet 3,14
booleen true unit ()
(function) int -> int > int let x y = x + y
rlwrap ocaml
( + );; gives the type of the function +
pas de casts implicites
Operators
+
+.
^
> 'a 'a bool
<>
lsr
= structurelle
== adresse
begin
print_string "Hello" ;
print_int 42
end ;;
read_line ()

let a = 42 in a + 42
let a = 20 in
let b = 21 in
a + b
;;
let a = 20
and let b = 21
a + b
;;

if 42 = 42
then print_endline "yes"
else print_endline "no"

let f x =
        x * x
in

let f = fun x -> x * x in

Day2

let dice x = match x with
        | 1 -> "Un"
        | 2 -> "Deux"
        | _ -> "Error"

let isalpha c = match c with
        | 'a' .. 'z' | 'A' .. 'Z' -> "True"
        | _ -> "False"

let inbound x = match x with
        | y when y > 0 && y < 100 -> "True"
        | _ -> "False"

type foo = int
let x:foo = 3

type bar = int -> int
let plusUn:bar = (+) 1

let pair = (2, 3)  (int * int)
let foo = ("Hello", 1, true) (string * int * bool)

let f x = match x with
        | ("Hello", _, true) -> print_endline("1")
        | (_, _, _) -> print_endline("2")

let a = 1 :: 2 :: 3 :: []
let b = 1 :: 2 :: [3]
let c = 1 :: [2: 3]
let d = [1: 2: 3]

let l = [1, 2, 3]

let rec is_in_list x list = match liste with
        | [] -> False
        | tete::queue -> (x = tete) || (is_in_list x queue)


Variants

type card = As | King | Queen

let cardvalue c = match c with
        | As -> 14
        | King -> 13
        | Queen -> 12

type expr = Literal of int
        | Prod of expr * expr
        | Sum of expr * expr

let rec eval_expr e = match e with
        | Literal n -> n
        | Prod (lhsm rhs) -> (eval_expr lhs) * (eval_expr rhs)
        | Sum (lhsm rhs) -> (eval_expr lhs) + (eval_expr rhs)

let () =
        let e1 = Literal 42 in
        let e2 = Sum (Literal 40, Literal 2) in (* 40 + 2 *)
        let e3 = Prod (Literal 21, Literal 2) in (* 21 * 2 *)
        let e4 = Prod (
                Sum (
                        Literal 1,
                        Literal 2 ),
                Sum (
                        Literal 3,
                        Literal 4 ),
        ) in (* (1+2) * (3+4) *)

> contient au moins les constructeurs suivants
`A
[> `A ] = `A
(`B 4)
[> `B of int ] = `B 4

< contient au plus les constructeurs suivants
let f = function
        | `Foo a -> 1
        | `Bar -> 2
var f : [< `Foo of `a | `Bar ] -> int = <fun>

Records

type etudiant = {
        nom : string;
        login : string;
        etat
}

let marvin = {
        nom = "Marvin";
        login = "marvin";
        etat = "deprime"
}

let () =
        print_endline marvin.nom

let print_record e = match e with
        | {login: _} when login = "marvin" -> e.nom
        | {nom; etat; _} -> "Other"

let () =
        let marvin = {
                nom = "Marvin";
                login = "thor";
                etat = "deprime"
        } in
        print_endline (print_record marvin)

Option type

type int_option = Some of int | None

let a = Some 42

let is_nothin x = match x with
        | None -> "Nothing"
        | Some n -> "Something " ^ (string_of_int n)

let extract x = match x with
        | None -> 0
        | Some x -> x

Parametric type

type `a indexed = int * `a
type `a option = Some of `a
                | None
type (`a, `b) pair = {
        first : `a;
        second : `b
}
type (`a, `b) succes_or_error = Succes of `a
                                | Error of `b

type (`a, `b) btree = Leaf of `b
                        | Btree of (`a, `b) node
and (`a, `b) node = {
        op:a;
        son1:(`a,`b) btree;
        son2:(`a,`b) btree
}

let () =
        let t = Btree {
                op = "+";
                son1 = Leaf 1;
                son2 = Leaf 2
        } in
        match t 


Functions

let f1 = function x -> function y -> x + y
let f2 = fun x y -> x + y
let f3 x y = x + y

let f = function
        | 0 -> 0
        | _ -> 1

let g = fun x y -> x + y
