let loop player =
  display_game ();
  let (i, j) = prompt player in
  play i j player;
  if update_status = 0
  then game_end ()
  else
    match player with
      | "X" -> loop "O"
      | _   -> loop "X"

let main () =
  loop "O"