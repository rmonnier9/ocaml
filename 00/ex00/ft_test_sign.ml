let ft_test_sign a =
        if a >= 0
        then print_endline "positive"
        else print_endline "negative"

let main () =
        ft_test_sign 4;
        ft_test_sign 0;
        ft_test_sign (-10)

(* ************ *)

let () = main ()
