(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   crossover.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/10 11:38:49 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/10 11:38:52 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let crossover list1 list2 =
    let rec in_list elem list = match list with
        | [] -> false
        | hd :: tl -> hd = elem || in_list elem tl
    in
    let rec crossover_aux list1 list2 common = match list1 with
        | [] -> common
        | hd :: tl when not (in_list hd common) && in_list hd list2
            -> crossover_aux tl list2 (common @ [hd])
        | hd :: tl -> crossover_aux tl list2 common
    in
    crossover_aux list1 list2 []

let rec print_list list = match list with
    | [] -> print_string "\n"
    | hd :: tl -> print_int hd; print_string ";"; print_list tl

let main () =
    print_list (crossover [] []);
    print_list (crossover [] [1]);
    print_list (crossover [1] []);
    print_list (crossover [1] [1]);
    print_list (crossover [1] [2]);
    print_list (crossover [1;2;3] [1;3]);
    print_list (crossover [1;2;3] [4]);
    print_list (crossover [1;2;3;1;1] [1;4])

let () = main ()
