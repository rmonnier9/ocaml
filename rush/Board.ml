type cell = O | X | Undefined
type grid = {
  cells: cell list;
  status: cell
}
type board = grid list
type position = int * int

let init_list n ~f =
  let rec init_list' i n f =
    if i >= n then []
    else (f i) :: (init_list' (i+1) n f)
  in init_list' 0 n f

let newGrid () =
  { cells = (init_list (3 * 3) (fun _ -> Undefined)); status = Undefined }

let newBoard () =
  (init_list (3 * 3) (fun _ -> (newGrid ())))

let grid_status grid = match grid.cells with
  | elem1 :: _ :: _ :: elem2 :: _ :: _ :: elem3 :: _ :: _ :: [] when elem1 = (O | X) && elem1 = elem2 && elem2 = elem3  -> elem1
  | _ :: elem1 :: _ :: _ :: elem2 :: _ :: _ :: elem3 :: _ :: [] when elem1 = (O | X) && elem1 = elem2 && elem2 = elem3  -> elem1
  | _ :: _ :: elem1 :: _ :: _ :: elem2 :: _ :: _ :: elem3 :: [] when elem1 = (O | X) && elem1 = elem2 && elem2 = elem3  -> elem1
  | elem1 :: _ :: _ :: _ :: elem2 :: _ :: _ :: _ :: elem3 :: [] when elem1 = (O | X) && elem1 = elem2 && elem2 = elem3  -> elem1
  | _ :: _ :: elem1 :: _ :: elem2 :: _ :: elem3 :: _ :: _ :: [] when elem1 = (O | X) && elem1 = elem2 && elem2 = elem3  -> elem1
  | elem1 :: elem2 :: elem3 :: _ :: _ :: _ :: _ :: _ :: _ :: [] when elem1 = (O | X) && elem1 = elem2 && elem2 = elem3  -> elem1
  | _ :: _ :: _ :: elem1 :: elem2 :: elem3 :: _ :: _ :: _ :: [] when elem1 = (O | X) && elem1 = elem2 && elem2 = elem3  -> elem1
  | _ :: _ :: _ :: _ :: _ :: _ :: elem1 :: elem2 :: elem3 :: [] when elem1 = (O | X) && elem1 = elem2 && elem2 = elem3  -> elem1
  | _ -> Undefined

let testgrid = {
  cells = (Undefined :: Undefined :: Undefined ::
          Undefined :: Undefined :: Undefined ::
           O :: O :: O :: []);
  status = Undefined
}