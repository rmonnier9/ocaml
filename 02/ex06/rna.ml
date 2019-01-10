(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   helix.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/10 16:58:55 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/10 16:58:57 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | U | None

type nucleotide = {
    p : phosphate;
    d : deoxyribose;
    n : nucleobase
}

type helix = nucleotide list

type rna = nucleotide list

let random_nucleobase () = match (Random.int 4) with
    | 0 -> A
    | 1 -> T
    | 2 -> C
    | 3 -> G
    | _ -> G

let random_nucleotide () =
    {
        p = "phosphate";
        d = "deoxyribose";
        n = random_nucleobase ();
    }

let rec generate_helix n = 
    let h:helix = match n with
        | i when i <= 0 -> []
        | i -> [random_nucleotide ()] @ generate_helix (i - 1)
    in
    h

let generate_nucleotide c = 
    let n = match c with
        | 'A' -> A
        | 'T' -> T
        | 'U' -> U
        | 'C' -> C
        | 'G' -> G
        | _ -> None
    in
    {
        p = "phosphate";
        d = "deoxyribose";
        n = n;
    }

let nucleobase_to_string n = match n with
    | A -> "A"
    | T -> "T"
    | U -> "U"
    | C -> "C"
    | G -> "G"
    | None -> "_"

let rec helix_to_string list = 
    let input_helix:helix = list in
    match input_helix with
        | [] -> ""
        | hd :: tl -> (nucleobase_to_string hd.n) ^ (helix_to_string tl)

let rec rna_to_string list = 
    let input_rna:rna = list in
    match input_rna with
        | [] -> ""
        | hd :: tl -> (nucleobase_to_string hd.n) ^ (rna_to_string tl)

let complementary_nucleotide n = match n with
    | { n ; _ } when n = A -> generate_nucleotide 'T'
    | { n ; _ } when n = T -> generate_nucleotide 'A'
    | { n ; _ } when n = C -> generate_nucleotide 'G'
    | { n ; _ } when n = G -> generate_nucleotide 'C'
    | _ -> generate_nucleotide ' '

let rec complementary_helix list =
    let complementary:helix = match list with
        | [] -> []
        | hd :: tl -> (complementary_nucleotide hd) :: (complementary_helix tl)
    in
    complementary

let generate_rna h =
    let rna_complementary_nucleotide n = match n with
        | { n ; _ } when n = A -> generate_nucleotide 'U'
        | { n ; _ } when n = T -> generate_nucleotide 'A'
        | { n ; _ } when n = C -> generate_nucleotide 'G'
        | { n ; _ } when n = G -> generate_nucleotide 'C'
        | _ -> generate_nucleotide ' '
    in
    let rec rna_complementary_helix list =
        let rna_complementary:rna = match list with
            | [] -> []
            | hd :: tl -> (rna_complementary_nucleotide hd) :: (rna_complementary_helix tl)
        in
        rna_complementary
    in 
    let input_helix:helix = h in
    rna_complementary_helix input_helix


let main () =
    let a = generate_helix 10 in
    let b = generate_helix 8 in
    let c = generate_helix 20 in
    print_endline (helix_to_string a);
    print_endline (helix_to_string (complementary_helix a));
    print_endline (rna_to_string (generate_rna a));
    print_endline (helix_to_string b);
    print_endline (helix_to_string (complementary_helix b));
    print_endline (rna_to_string (generate_rna b));
    print_endline (helix_to_string c);
    print_endline (helix_to_string (complementary_helix c));
    print_endline (rna_to_string (generate_rna c))

let () = main ()
