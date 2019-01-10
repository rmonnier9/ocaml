(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gray.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/10 11:40:25 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/10 11:40:27 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let concat_list list1 list2 =
    let rec concat_list_aux list1 = match list1 with
        | [] -> list2
        | hd :: tl -> hd :: concat_list_aux tl
    in
    concat_list_aux list1

let rec revert_list list = match list with
    | [] -> []
    | hd :: tl -> concat_list (revert_list tl) [hd]

let rec prepend str list = match list with
    | [] -> []
    | hd :: tl -> (str ^ hd) :: (prepend str tl)

let rec print_list list = match list with
    | [] -> print_string "\n"
    | hd :: [] -> print_string hd; print_string "\n"
    | hd :: tl -> print_string hd; print_string " "; print_list tl

let rec get_gray n = match n with
    | 0 -> []
    | 1 -> ["0";"1"]
    | i -> concat_list (prepend "0" (get_gray (i - 1))) (prepend "1" (revert_list (get_gray (i - 1))))

let gray n = print_list (get_gray n)

let main () =
    gray 0;
    gray 1;
    gray 2;
    gray 3;
    gray 4;
    gray 5

let () = main ()
