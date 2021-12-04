(* Create a stream of lines from a channel *)
let line_stream_of_channel channel =
    Stream.from (fun _ -> try Some (input_line channel) with End_of_file -> None)

(* Create a stream of lines that result in a list of chars from a channel *)
let list_stream_of_channel channel =
    let rec grab lst =
        match (input_char channel) with
        | '\n' -> List.rev lst
        | x -> grab (x :: lst)
    in
    Stream.from (fun _ -> try Some (grab []) with End_of_file -> None)

(* Folding a stream *)
let stream_fold_right f stream init =
    let result = ref init in
    Stream.iter
        (fun x -> result := f x !result)
        stream;
    !result

(* Calculate pow(a, b) *)
let pow a b =
    let rec pow_tail a b accum =
        if b = 0 then accum else pow_tail a (b - 1) (a * accum)
    in
    pow_tail a b 1

(* Create a list of length length and fill with value *)
let make_list length value = List.init length (fun f -> value)

let st = list_stream_of_channel (open_in "data.txt")

(* Given a line and the current counts, increment the proper count *)
let process_elem elem count value =
    let combined = List.combine elem count 
    in
    List.map (fun (e, c) -> if e = value then c + 1 else c) combined

(* Process the entire stream to get the final tuple of counts *)
let process_stream =
    (* Create an initial list of counts for 1 and 0 at each element *)
    let init_count =
        match Stream.peek st with
        | Some x -> make_list (List.length x) 0
        | _ -> failwith "Stream is empty"
    in
    let f lst accum =
        match accum with
        | (z_ct, o_ct) -> (process_elem lst z_ct '0', process_elem lst o_ct '1')
    in
    match stream_fold_right f st (init_count, init_count) with
    | (x, y) -> List.combine x y

(* From a tuple of z_count * o_count, form a tuple of gamma * epsilon *)
let get_gamma_epsilon = 
    let f (z, o) (gamma, epsilon) =
        if z > o then (0 :: gamma, 1 :: epsilon) else (1 :: gamma, 0 :: epsilon)
    in
    List.fold_right f process_stream ([], [])

(* Get the decimal value from a list of binary char *)
let decimal count =
    let f b (count, result) =
        if b = 1 then (count + 1, result + pow 2 count) else (count + 1, result)
    in
    match List.fold_right f count (0, 0) with
    | (ct, res) -> res

let final_result =
    match get_gamma_epsilon with
    | (gamma, epsilon) -> (decimal gamma) * (decimal epsilon)

(* PART 2 *)
(* Construct the char list list from the stream *)
let list_of_data = stream_fold_right (fun a b -> a :: b) (list_stream_of_channel (open_in "data.txt")) []

(* Count the number of ones and zeros at the beginning of each elem in the list *)
let count_first lst =
    let f elem (z_count, o_count) =
        match elem with
        | x :: _ -> if x = '0' then (z_count + 1, o_count) else (z_count, o_count + 1)
        | _ -> failwith "None left to count"
    in
    List.fold_right f lst (0, 0)

(* Remove elements from the list that are not needed *)
let filter_by_count lst (z_count, o_count) most_freq =
    let f elem accum =
        match elem with
        | x :: xs ->
            if most_freq then
                if o_count >= z_count then
                    if x = '1' then xs :: accum else accum
                else
                    if x = '1' then accum else xs :: accum
            else
                if z_count > o_count then
                    if x = '0' then accum else xs :: accum
                else
                    if x = '0' then xs :: accum else accum
        | _ -> failwith "None left to filter"
    in
    List.fold_right f lst []

(* Given a list, filter out values that are least common *)
let rec get_oxygen_gen_rating lst =
    match lst with
    | x :: [] -> List.map (fun a -> (int_of_char a) - 48) x
    | _ ->
        let (z_ct, o_ct) = count_first lst
        in
        if o_ct >= z_ct then
            1 :: get_oxygen_gen_rating (filter_by_count lst (z_ct, o_ct) true)
        else
            0 :: get_oxygen_gen_rating (filter_by_count lst (z_ct, o_ct) true)

let rec get_oxygen_scr_rating lst =
    match lst with
    | x :: [] -> List.map (fun a -> (int_of_char a) - 48) x
    | _ ->
        let (z_ct, o_ct) = count_first lst
        in
        if z_ct > o_ct then
            1 :: get_oxygen_scr_rating (filter_by_count lst (z_ct, o_ct) false)
        else
            0 :: get_oxygen_scr_rating (filter_by_count lst (z_ct, o_ct) false)

let get_result_part_2 = (decimal (get_oxygen_gen_rating list_of_data)) * (decimal (get_oxygen_scr_rating list_of_data))