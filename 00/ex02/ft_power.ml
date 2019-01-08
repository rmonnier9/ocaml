let rec ft_power a b =
        if b = 0
        then 1
        else a * (ft_power a (b - 1))

let main () =
        print_endline "3^0: "; print_int (ft_power 3 0); print_char '\n';
        print_endline "0^3: "; print_int (ft_power 0 3); print_char '\n';
        print_endline "2^2: "; print_int (ft_power 2 2); print_char '\n'


(* ************ *)

let () = main ()
