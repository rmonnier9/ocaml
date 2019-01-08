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

let repeat_x n =
    let error = "Error" in
    if n < 0
    then error
    else
        begin
            let rec iterator str i =
                if (i > 0)
                then iterator (str ^ "x") (i - 1)
                else str
            in
            iterator "" n
        end

let main () =
    print_endline (repeat_x 10);
    print_endline (repeat_x 0);
    print_endline (repeat_x (-10))

let () = main ()
