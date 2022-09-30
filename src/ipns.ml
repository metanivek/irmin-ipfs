module Key = struct
  type t = { hash : string; name : string }

  let v hash name = { hash; name }

  let of_string s =
    let parts = String.split_on_char ' ' s in
    v (List.nth parts 0) (List.nth parts 1)

  let hash t = t.hash
  let name t = t.name
end

let keys () =
  let cmd = "ipfs key list -l" in
  let ic = Unix.open_process_in cmd in
  let rec loop acc =
    match In_channel.input_line ic with
    | None -> acc
    | Some s -> Key.of_string s :: acc |> loop
  in
  loop []

let key name = keys () |> List.find_opt (fun k -> name = Key.name k)

let create name =
  match key name with
  | Some key -> key
  | None -> (
      let cmd = "ipfs key gen " ^ name in
      let ic, _, _ = Unix.open_process_full cmd (Unix.environment ()) in
      match In_channel.input_line ic with
      | None -> assert false (* unexpected error occurred *)
      | Some hash -> Key.v hash name)

let resolve key =
  let hash = Key.hash key in
  let cmd = "ipfs name resolve " ^ hash in
  let ic, _, _ = Unix.open_process_full cmd (Unix.environment ()) in
  match In_channel.input_line ic with
  | None -> None
  | Some s -> (
      let split = String.split_on_char '/' s in
      assert (List.length split = 3);
      (* /ipfs/CID *)
      match List.nth split 1 with
      | "ipfs" -> Some (List.nth split 2)
      | "ipns" -> assert false (* we should never point keys to ipns entries *)
      | _ -> None)

let publish key hash =
  (* painfully slow without --offline.

     thank you kind internet soul:
     https://github.com/ipfs/kubo/issues/6236#issuecomment-743917867 *)
  let cmd =
    "ipfs name publish --offline --allow-offline -k "
    ^ Key.hash key
    ^ " "
    ^ hash
  in
  let _, _, _ = Unix.open_process_full cmd (Unix.environment ()) in
  ()
