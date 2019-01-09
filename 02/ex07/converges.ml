(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   converges.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/09 10:40:40 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/09 11:50:20 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let converges f x n =
    if n < 0
    then false
    else
        begin
            let rec iterator i acc =
                if (i = 0)
                then (acc = (f acc))
                else iterator (i - 1) (f acc)
            in
            iterator n x
        end

let main () =
    print_endline (string_of_bool (converges (( * ) 2) 2 5));
    print_endline (string_of_bool (converges (fun x -> x / 2) 2 0));
    print_endline (string_of_bool (converges (fun x -> x / 2) 2 1));
    print_endline (string_of_bool (converges (fun x -> x / 2) 2 2));
    print_endline (string_of_bool (converges (fun x -> x / 2) 2 3))

let () = main ()
