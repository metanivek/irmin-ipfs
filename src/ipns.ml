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
  let k, _ =
    Cmd.run cmd
      ~on_stdout:([], fun acc s -> Key.of_string s :: acc)
      ~on_stderr:Cmd.On_output.print
  in
  k

let lookup name = keys () |> List.find_opt (fun k -> name = Key.name k)

let create name =
  match lookup name with
  | Some key -> key
  | None -> (
      let cmd = "ipfs key gen " ^ name in
      let key, _ =
        Cmd.run cmd
          ~on_stdout:(None, fun _ hash -> Some (Key.v hash name))
          ~on_stderr:Cmd.On_output.print
      in
      match key with
      | None -> assert false (* unexpected error occurred *)
      | Some key -> key)

let resolve key =
  let hash = Key.hash key in
  let cmd = "ipfs name resolve " ^ hash in
  let hash, _ =
    Cmd.run cmd
      ~on_stdout:
        ( None,
          fun _ s ->
            let split = String.split_on_char '/' s in
            assert (List.length split = 3);
            (* /ipfs/CID *)
            match List.nth split 1 with
            | "ipfs" -> Some (List.nth split 2)
            | "ipns" ->
                assert false (* we should never point keys to ipns entries *)
            | _ -> None )
      ~on_stderr:Cmd.On_output.print
  in
  hash

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
  Cmd.run_default cmd
