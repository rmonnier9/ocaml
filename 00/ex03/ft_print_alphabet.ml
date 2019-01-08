let ft_print_alphabet () =
		let ascii_of_a = int_of_char 'a' in
		let ascii_of_z = int_of_char 'z' in
		let rec loop current_ascii =
			print_char (char_of_int (current_ascii));
			if (current_ascii < ascii_of_z)
			then loop (current_ascii + 1)
			else print_char '\n'
		in
		loop (ascii_of_a)

let main () =
	ft_print_alphabet ()


(* ************ *)

let () = main ()
