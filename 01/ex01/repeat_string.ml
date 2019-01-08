(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_string.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/08 16:37:33 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/08 16:37:36 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let repeat_string ?str:(str="x") n =
    let error = "Error" in
    if n < 0
    then error
    else
        begin
            let rec iterator output i =
                if (i > 0)
                then iterator (output ^ str) (i - 1)
                else output
            in
            iterator "" n
        end

let main () =
    print_endline (repeat_string ~str:"what" 10);
    print_endline (repeat_string 0);
    print_endline (repeat_string (-10))

let () = main ()
