open Irmin_ipfs

(** WIP test of how things are working (or not). It sometimes works and
    sometimes fails. *)

let key = "Hello"
let repo_name = "simple"

let check_head () =
  let open Lwt.Syntax in
  let* repo = Store.Repo.v (Conf.v repo_name) in
  let* main = Store.main repo in
  let* v = Store.get main [ key ] in
  Printf.printf "%s, %s" key v |> Lwt.return

let write_data () =
  let open Lwt.Syntax in
  let* repo = Store.Repo.v (Conf.v repo_name) in
  let* main = Store.main repo in
  let info () = Store.Info.v 0L in
  let v = Printf.sprintf "world! %d" (Random.int 42) in
  let* () = Store.set_exn main [ key ] ~info v in
  Lwt.return_unit

let main () =
  let open Lwt.Syntax in
  let* () = write_data () in
  let* () = check_head () in
  Lwt.return_unit

let () =
  Random.self_init ();
  Lwt_main.run @@ main ()
