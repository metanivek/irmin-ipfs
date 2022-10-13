module On_output = struct
  let print = ((), fun _ s -> Printf.printf "%s\n" s)
  let nothing = ((), fun _ _ -> ())
end

let read_lines ic (acc, on_line) =
  let rec aux acc =
    match In_channel.input_line ic with
    | None -> acc
    | Some s ->
        let acc = on_line acc s in
        aux acc
  in
  aux acc

let run ~on_stdout ~on_stderr cmd =
  Printf.printf "Running: '%s'\n" cmd;
  let stdout, _, stderr = Unix.open_process_full cmd (Unix.environment ()) in
  let stdout_read = read_lines stdout on_stdout in
  let stderr_read = read_lines stderr on_stderr in
  (stdout_read, stderr_read)

let run_default cmd =
  let _, _ = run ~on_stdout:On_output.nothing ~on_stderr:On_output.print cmd in
  ()
