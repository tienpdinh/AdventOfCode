let data = "data.txt"

let move input (cur_depth, cur_horizontal) =
    match String.split_on_char ' ' input with
    | direction :: steps :: [] ->
        (match direction with
        | "up" -> (cur_depth - int_of_string steps, cur_horizontal)
        | "down" -> (cur_depth + int_of_string steps, cur_horizontal)
        | "forward" -> (cur_depth, cur_horizontal + int_of_string steps)
        | _ -> failwith "Invalid direction")
    | _ -> failwith "Invalid input"

let real_move input (cur_depth, cur_horizontal, cur_aim) =
    match String.split_on_char ' ' input with
    | direction :: steps :: [] -> 
        (match direction with
        | "down" -> (cur_depth, cur_horizontal, cur_aim + int_of_string steps)
        | "up" -> (cur_depth, cur_horizontal, cur_aim - int_of_string steps)
        | "forward" -> (cur_depth + cur_aim * int_of_string steps, cur_horizontal + int_of_string steps, cur_aim)
        | _ -> failwith "Invalid direction")
    | _ -> failwith "Invalid input"

let get_result init_depth init_horizontal =
    let f = open_in data
    in
    let rec build_res (cur_vertical, cur_horizontal) = 
        match input_line f with
        | line -> build_res (move line (cur_vertical, cur_horizontal))
        | exception End_of_file -> close_in f; (cur_vertical, cur_horizontal)
    in
    match build_res (init_depth, init_horizontal) with
    | (fin_vertical, fin_horizontal) -> fin_vertical * fin_horizontal

let get_real_result init_depth init_horizontal init_aim =
    let f = open_in data
    in
    let rec build_res (cur_vertical, cur_horizontal, cur_aim) = 
        match input_line f with
        | line -> build_res (real_move line (cur_vertical, cur_horizontal, cur_aim))
        | exception End_of_file -> close_in f; (cur_vertical, cur_horizontal, cur_aim)
    in
    match build_res (init_depth, init_horizontal, init_aim) with
    | (fin_vertical, fin_horizontal, _) -> fin_vertical * fin_horizontal