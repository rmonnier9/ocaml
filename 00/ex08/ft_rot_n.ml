(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_rot_n.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmonnier <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/08 18:04:09 by rmonnier          #+#    #+#             *)
(*   Updated: 2019/01/08 18:04:35 by rmonnier         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_rot_n n str =
  let rot_char_n n c =
    let ascii_of_current = int_of_char c in
    if (ascii_of_current >= 65 && ascii_of_current <= 90)
    then (char_of_int (65 + (((ascii_of_current - 65) + n) mod 26)))
    else if (ascii_of_current >= 97 && ascii_of_current <= 122)
    then (char_of_int (97 + (((ascii_of_current - 97) + n) mod 26)))
    else c
  in
  String.map (rot_char_n n) str


let main () =
  print_string "3 Robin: ";
  print_endline (ft_rot_n 3 "Robin");
  print_string "26 Robin: ";
  print_endline (ft_rot_n 26 "Robin");
  print_string "2 poisson87: ";
  print_endline (ft_rot_n 2 "poisson87");
  print_string "1 azAZ[]{}(): ";
  print_endline (ft_rot_n 1 "azAZ[]{}()")

let () = main ()
