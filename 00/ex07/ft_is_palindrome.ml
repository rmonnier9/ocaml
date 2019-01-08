let ft_is_palindrome str =
  let rec iterate_match str index =
    let length = String.length str in
    let matching_index = (length - index) - 1 in
    if (index >= matching_index)
    then true
    else
      begin
        let current_char = String.get str index in
        let matching_char = String.get str matching_index in
        if (current_char = matching_char)
        then iterate_match str (index + 1)
        else false
      end
  in
  iterate_match str 0

let main () =
  print_string "Test with [radar]: ";
  print_endline (string_of_bool(ft_is_palindrome "radar"));
  print_string "Test with [madam]: ";
  print_endline (string_of_bool(ft_is_palindrome "madam"));
  print_string "Test with [AABB]: ";
  print_endline (string_of_bool(ft_is_palindrome "AABB"));
  print_string "Test with [\"\"]: ";
  print_endline (string_of_bool(ft_is_palindrome ""));
  print_string "Test with [ABBA]: ";
  print_endline (string_of_bool(ft_is_palindrome "ABBA"));
  print_string "Test with [car]: ";
  print_endline (string_of_bool(ft_is_palindrome "car"));
  print_string "Test with [aBcD]: ";
  print_endline (string_of_bool(ft_is_palindrome "aBcD"))
  
(* ************ *)

let () = main ()
