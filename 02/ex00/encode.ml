(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   encode.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/10 11:13:13 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/10 11:13:15 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let encode list =
    let rec encode_aux list acc elem = match list with
        | [] -> [(acc, elem)] 
        | hd :: tl when hd = elem -> encode_aux tl (acc + 1) elem
        | hd :: tl -> (acc, elem) :: encode_aux tl 1 hd
    in
    match list with
    | [] -> []
    | hd :: tl -> encode_aux tl 1 hd

let rec print_list list = match list with
    | [] -> print_char '\n'
    | (a, b) :: tl -> print_string " ("; print_int a; print_string ", "; print_char b; print_string ");"; print_list tl

let main () =
    print_list (encode ['a'; 'a'; 'b'; 'a'; 'c'; 'c']);
    print_list (encode []);
    print_list (encode ['a'])

let () = main ()
