(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   leibniz_pi.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/09 11:08:14 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/09 12:02:08 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let leibniz_pi delta =
    if delta < 0.
    then (-1)
    else
        begin
            let pi = 4. *. (atan 1.) in
            let f i =
               let output = (4. /. ((2. *. (float_of_int i)) +. 1.)) in
               if (i mod 2 = 0)
               then output
               else (-.output)
            in
            let rec iterator i acc =
                if (acc < (pi +. delta) && acc > (pi -. delta))
                then i
                else iterator (i + 1) (acc +. (f i))
            in
            iterator 0 0.
        end

let main () =
    print_endline (string_of_int (leibniz_pi 0.01));
    print_endline (string_of_int (leibniz_pi 0.001));
    print_endline (string_of_int (leibniz_pi 0.0001))

let () = main ()
