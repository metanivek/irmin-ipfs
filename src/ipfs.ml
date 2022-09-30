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
  let ic = Unix.open_process_in cmd in
  let hash = input_line ic in
  hash

let read k =
  let cmd = "ipfs cat " ^ k in
  let ic = Unix.open_process_in cmd in
  let b = Buffer.create 0 in
  let rec loop () =
    match In_channel.input_line ic with
    | Some s ->
        Buffer.add_string b s;
        loop ()
    | None -> Buffer.contents b
  in
  loop ()

let write v =
  let fn = write_tmp "irmin-ipfs.write" v in
  let cmd = "ipfs add -Q " ^ fn in
  let ic = Unix.open_process_in cmd in
  let hash = input_line ic in
  hash
