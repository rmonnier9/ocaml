(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   nucleotides.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/10 15:45:09 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/10 15:45:11 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = {
    p : phosphate;
    d : deoxyribose;
    n : nucleobase
}

let generate_nucleotide c = 
    let nucleobase = match c with
        | 'A' -> A
        | 'T' -> T
        | 'C' -> C
        | 'G' -> G
        | _ -> None
    in
    let n:nucleotide =
        {
            p = "phosphate";
            d = "deoxyribose";
            n = nucleobase;
        }
    in
    n
