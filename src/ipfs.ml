type hash = string

let write_tmp name s =
  let fn = Filename.temp_file name ".tmp" in
  let oc = open_out fn in
  output_string oc s;
  close_out oc;
  fn

let to_hash v =
  let fn = write_tmp "irmin-ipfs.to_hash" v in
  let cmd = "ipfs add -n -Q " ^ fn in
  let hash, _ =
    Cmd.run cmd
      ~on_stdout:(None, fun _ s -> Some s)
      ~on_stderr:Cmd.On_output.print
  in
  match hash with None -> assert false | Some h -> h

let read k =
  let cmd = "ipfs cat " ^ k in
  let buf, _ =
    Cmd.run cmd ~on_stderr:Cmd.On_output.print
      ~on_stdout:
        ( Buffer.create 0,
          fun b s ->
            Buffer.add_string b s;
            b )
  in
  let str = Buffer.contents buf in
  (* Printf.printf "%s = \n%s\n" k str; *)
  str

let write v =
  let fn = write_tmp "irmin-ipfs.write" v in
  let cmd = "ipfs add -Q " ^ fn in
  let hash, _ =
    Cmd.run cmd
      ~on_stdout:(None, fun _ s -> Some s)
      ~on_stderr:Cmd.On_output.print
  in
  match hash with None -> assert false | Some h -> h
