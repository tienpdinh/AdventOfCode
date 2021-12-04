let data = "data.txt"

let get_result init_accum init_prev_val =
    let f = open_in data in
    let rec build_res accum prev =
        match input_line f with
        | line -> if int_of_string line > prev then build_res (accum + 1) (int_of_string line) else build_res accum (int_of_string line)
        | exception End_of_file -> close_in f; accum
    in
    build_res init_accum init_prev_val

(* Run this by `get_result 0 max_int` to get the answer *)