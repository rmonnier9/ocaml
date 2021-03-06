(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_power.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/08 14:55:14 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/08 14:55:46 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_power a b =
        if b = 0
        then 1
        else a * (ft_power a (b - 1))

let main () =
    print_string "[2 4]: ";
    print_int(ft_power 2 4);
    print_char '\n';
    print_string "[3 0]: ";
    print_int(ft_power 3 0);
    print_char '\n';
    print_string "[0 5]: ";
    print_int(ft_power 0 5);
    print_char '\n';
    print_string "[10 2]: ";
    print_int(ft_power 10 2);
    print_char '\n'

let () = main ()
