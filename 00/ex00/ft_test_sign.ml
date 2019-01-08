(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_test_sign.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/08 14:53:31 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/08 14:55:44 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_test_sign a =
        if a >= 0
        then print_endline "positive"
        else print_endline "negative"

let main () =
    ft_test_sign (42);
    ft_test_sign (0);
    ft_test_sign (-42);
    ft_test_sign (-0);
    ft_test_sign (0);
    ft_test_sign (10000);
    ft_test_sign (-100000)

let () = main ()
