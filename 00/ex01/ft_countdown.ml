(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_countdown.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/08 14:55:05 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/08 14:55:10 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_countdown a =
	if a <= 0
	then
		begin
			print_int 0;
			print_char '\n'
		end
	else
		begin
			print_int a;
			print_char '\n';
			ft_countdown (a - 1)
		end

let main () =
    ft_countdown 10;
    ft_countdown (-10);
    ft_countdown 0

let () = main ()
