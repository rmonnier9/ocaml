(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_sum.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/09 10:57:39 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/09 11:07:33 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_sum f start n =
    if start > n
    then nan
    else
        begin
            let rec iterator i acc =
                if (i > n)
                then acc
                else iterator (i + 1) (acc +. (f i))
            in
            iterator start 0.
        end

let main () =
    print_endline (string_of_float (ft_sum (fun x -> float_of_int (x * x)) 1 10));
    print_endline (string_of_float (ft_sum (fun x -> float_of_int (x * x)) 11 10));
    print_endline (string_of_float (ft_sum (fun x -> float_of_int (x * x)) 1 5))

let () = main ()
