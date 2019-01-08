let ft_string_all predicate str =
  let rec iterate predicate str index =
    let length = String.length str in
    if (index < length)
    then
      begin
        let current = String.get str index in
        if (predicate current)
        then iterate predicate str (index + 1)
        else false
      end
    else true
  in
  iterate predicate str 0

let main () =
  let is_digit c = c >= '0' && c <= '9' in
  if (ft_string_all is_digit "0123456789")
  then print_string "This is true for 0123456789\n"
  else print_string "This is false for 0123456789\n";
  if (ft_string_all is_digit "allo123")
  then print_string "This is true for allo123\n"
  else print_string "This is false for allo123\n"
  
(* ************ *)

let () = main ()
