(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_rev.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/08 14:55:27 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/08 14:55:58 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_rev str =
  let rec print_next_char str index =
    let length = String.length str in
    if (index < length)
    then
      begin 
        print_next_char str (index + 1);
        print_char (String.get str index)
      end
  in
  print_next_char str 0;
  print_char '\n'

let main () =
  ft_print_rev "coucou toi!";
  ft_print_rev "Robin"

let () = main ()
