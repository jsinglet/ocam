open Format


let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let printProgram f = print_endline (sprintf "Input Program:\n\n %s\n\n" (load_file f))
