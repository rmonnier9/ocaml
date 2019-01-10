(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sequence.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/10 13:52:34 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/10 13:52:36 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let describe_sequence list =
    let rec describe_sequence_aux list acc elem = match list with
        | [] -> [acc;elem]
        | hd :: tl when hd = elem -> describe_sequence_aux tl (acc + 1) elem
        | hd :: tl -> acc :: elem :: describe_sequence_aux tl 1 hd
    in
    match list with
    | [] -> []
    | hd :: tl -> describe_sequence_aux tl 1 hd 

let rec join list = match list with
    | [] -> ""
    | hd :: tl -> (string_of_int hd) ^ (join tl)

let sequence n =
    let rec sequence_aux i acc = match i with
    | i when i <= 0 -> []
    | 1 -> acc
    | i -> sequence_aux (i - 1) (describe_sequence acc)
    in
    join (sequence_aux n [1])

let main () =
    print_endline (sequence (-1));
    print_endline (sequence 0);
    print_endline (sequence 1);
    print_endline (sequence 2);
    print_endline (sequence 3);
    print_endline (sequence 4);
    print_endline (sequence 5);
    print_endline (sequence 6);
    print_endline (sequence 7);
    print_endline (sequence 8);
    print_endline (sequence 9)

let () = main ()
