open Irmin_ipfs

(* A simple test of writing and reading from IPFS. *)

let key = "Hello"
let repo_name = "simple"

let check_head () =
  Printf.printf "\nREAD data from store.\n";
  let open Lwt.Syntax in
  let* repo = Store.Repo.v (Conf.v ~fresh:false repo_name) in
  let* main = Store.main repo in
  let* v = Store.get main [ key ] in
  Printf.printf "%s, %s\n" key v |> Lwt.return

let write_data () =
  Printf.printf "\nWRITE data to store.\n";
  let open Lwt.Syntax in
  let conf = Conf.v ~fresh:true repo_name in
  let* repo = Store.Repo.v conf in
  let* main = Store.main repo in
  let info () = Store.Info.v 0L in
  let v = Printf.sprintf "world! %d" (Random.int 42) in
  let* () = Store.set_exn main [ key ] ~info v in
  (match Store.Ipns.key conf with
  | None -> assert false
  | Some (k : Ipns.Key.t) -> Printf.printf "IPNS key: %s\n" (Ipns.Key.hash k));
  Lwt.return_unit

let main () =
  Printexc.record_backtrace true;
  let open Lwt.Syntax in
  let* () = write_data () in
  let* () = check_head () in
  Lwt.return_unit

let () =
  Random.self_init ();
  Lwt_main.run @@ main ()
