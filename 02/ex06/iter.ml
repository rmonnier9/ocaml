(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   iter.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/08 18:41:26 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/09 10:35:39 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let iter f x n =
    if n < 0
    then (-1)
    else
        begin
            let rec iterator i acc =
                if (i = 0)
                then acc
                else iterator (i - 1) (f acc)
            in
            iterator n x
        end

let main () =
    print_endline (string_of_int (iter (fun x -> x * x) 2 4));
    print_endline (string_of_int (iter (fun x -> x * 2) 2 4))

let () = main ()
