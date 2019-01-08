let rec ft_countdown a =
	if a = 0
	then
		begin
			print_int 0;
			print_char '\n'
		end
	else
		begin
			print_int a;
			print_char '\n';
			(ft_countdown (a - 1))
		end

let main () =
	ft_countdown (10);
	ft_countdown (0)

(* ************ *)

let () = main ()
