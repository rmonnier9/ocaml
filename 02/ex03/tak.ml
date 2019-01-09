(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   tak.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/08 17:24:15 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/08 17:25:58 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec tak x y z =
    if y < x
    then z
    else
        tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)


let main () =
    print_endline (string_of_int (tak 1 2 3));
    print_endline (string_of_int (tak 5 23 7));
    print_endline (string_of_int (tak 9 1 0));
    print_endline (string_of_int (tak 1 1 1));
    print_endline (string_of_int (tak 0 42 0));
    print_endline (string_of_int (tak 23498 98734 98776));
    print_endline (string_of_int (tak 1 2 3))

let () = main ()
