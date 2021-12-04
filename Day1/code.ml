let data = "data.txt"

let get_result_part_1 init_accum init_prev_val =
    let f = open_in data
    in
    let rec build_res accum prev =
        match input_line f with
        | line -> if int_of_string line > prev then build_res (accum + 1) (int_of_string line) else build_res accum (int_of_string line)
        | exception End_of_file -> close_in f; accum
    in
    build_res init_accum init_prev_val

let get_result_part_2 init_accum init_prev_val =
    let f = open_in data
    in
    let rec build_res accum prev count val1 val2 val3 =
        match input_line f with
        | line ->
            if count = 0 then build_res accum prev (count + 1) (int_of_string line) val2 val3
            else
            if count = 1 then build_res accum prev (count + 1) val1 (int_of_string line) val3
            else
            if count = 2 then build_res accum prev (count + 1) val1 val2 (int_of_string line)
            else
                let sum = val1 + val2 + val3
                in
                if sum > prev then build_res (accum + 1) sum 3 val2 val3 (int_of_string line) else build_res accum sum 3 val2 val3 (int_of_string line)
        | exception End_of_file -> close_in f; if (val1 + val2 + val3) > prev then accum + 1 else accum
    in
    build_res init_accum init_prev_val 0 0 0 0