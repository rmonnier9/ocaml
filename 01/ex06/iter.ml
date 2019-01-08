(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   iter.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/08 18:41:26 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/08 18:48:20 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let iter f x n =
    if n < 0
    then (-1)
    else if n = 0
    then 0
    else if n = 1
    then 1
    else
        begin
            let rec iterator i acc1 acc2 =
                if (i = 0)
                then acc2
                else iterator (i - 1) acc2 (acc1 + acc2)
            in
            iterator (n - 1) 0 1
        end

let main () =
    print_endline (string_of_int (fibonacci (-42)));
    print_endline (string_of_int (fibonacci 1));
    print_endline (string_of_int (fibonacci 3));
    print_endline (string_of_int (fibonacci 6))

let () = main ()
