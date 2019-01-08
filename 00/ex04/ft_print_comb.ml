(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/08 14:55:23 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/08 14:56:07 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_comb () =
	let rec third_loop first second third =
		if (third <= 9)
		then
		begin
			if (second < third) 
			then
			begin
				print_int first;
				print_int second;
				print_int third;
				print_string ", "
			end;
		third_loop first second (third + 1)
		end
	in
	let rec second_loop first second =
		if (second <= 9)
		then begin
			if (first < second)
			then third_loop first second 0;
			second_loop first (second + 1)
		end
	in
	let rec first_loop first =
		if (first <= 6)
		then begin
			second_loop first 0;
			first_loop (first + 1)
		end
	in
	first_loop 0;
	print_string "789\n"

let main () =
	ft_print_comb ()

let () = main ()
