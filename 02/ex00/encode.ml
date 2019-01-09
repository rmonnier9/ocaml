(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_x.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/08 16:31:38 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/08 16:31:41 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let encode list =
    let rec number_of_times list elem = match list with
        | tete::queue when tete = elem -> 1 + (number_of_times queue elem)
        | _ -> 0
    in
    let rec loop list output = match list with
        | [] -> output
        | tete::queue when tete = output -> () 
    in

let main () =
    print_endline (string_of_int (fibonacci (-42)));
    print_endline (string_of_int (fibonacci 1));
    print_endline (string_of_int (fibonacci 3));
    print_endline (string_of_int (fibonacci 6))

let () = main ()
