module Base_storage =
functor
  (Key : Irmin.Type.S)
  (Value : Irmin.Type.S)
  ->
  struct
    module M = Map.Make (struct
      type t = Key.t

      let compare = Irmin.Type.(unstage (compare Key.t))
    end)

    module M_repr = Irmin.Type.Of_map (struct
      include M

      let key_t = Key.t
    end)

    let m_ty = M_repr.t Irmin.Type.string

    (** Types *)

    type t = {
      ipns_key : Ipns.Key.t;
      mutable m : Ipfs.hash M.t;
      mutable hash : Ipfs.hash option;
    }

    type key = Key.t
    type value = Value.t

    (** IPFS saving and loading *)

    let publish t =
      match t.hash with None -> () | Some h -> Ipns.publish t.ipns_key h

    let save t =
      let encode = Irmin.Type.(to_json_string m_ty) in
      let r = encode t.m in
      let hash = Ipfs.write r in
      t.hash <- Some hash

    let load t =
      let hash = Ipns.resolve t.ipns_key in
      t.hash <- hash;
      let decode = Irmin.Type.(of_json_string m_ty) in
      match t.hash with
      | None -> ()
      | Some hash -> (
          let data = Ipfs.read hash in
          match decode data with Ok m -> t.m <- m | _ -> ())

    (** Internal map handling *)

    let update_map t f =
      load t;
      (* FIXME [load t] is a hack to make sure all storages
         are using updated values *)
      t.m <- f t.m;
      save t;
      publish t

    (** Initialisation / Closing *)

    let v config =
      Printf.printf "--- Store.v BEGIN ---\n";
      let name = Irmin.Backend.Conf.get config Conf.Key.name in
      let fresh = Irmin.Backend.Conf.get config Conf.Key.fresh in
      let ipns_key = Ipns.create name in
      let t = { ipns_key; m = M.empty; hash = None } in
      (match fresh with
      | true ->
          save t;
          publish t
      | false -> load t);
      Printf.printf "--- Store.v END ---\n\n";
      t |> Lwt.return

    let close _t = Lwt.return_unit

    (** Operations *)

    let set t key value =
      let to_bin_string_v = Irmin.Type.(unstage (to_bin_string Value.t)) in
      let bin_v = to_bin_string_v value in
      let v = Ipfs.write bin_v in
      update_map t (M.add key v) |> Lwt.return

    let mem { m; _ } key = M.mem key m |> Lwt.return

    let find { m; _ } key =
      let of_bin_string_v = function
        | None -> None
        | Some s -> (
            let v = Ipfs.read s in
            match Irmin.Type.(unstage (of_bin_string Value.t)) v with
            | Ok s -> Some s
            | Error _ -> None)
      in
      M.find_opt key m |> of_bin_string_v |> Lwt.return

    let keys { m; _ } = M.bindings m |> List.map fst |> Lwt.return
    let remove t key = update_map t (M.remove key) |> Lwt.return
    let clear t = update_map t (fun _ -> M.empty) |> Lwt.return

    let batch t f =
      let open Lwt.Syntax in
      let+ x = Lwt.catch (fun () -> f t) (fun exn -> raise exn) in
      x
  end

module Branch_storage =
functor
  (Key : Irmin.Type.S)
  (Value : Irmin.Type.S)
  ->
  struct
    include Base_storage (Key) (Value)

    let base_v = v

    let v config =
      let base_name = Irmin.Backend.Conf.get config Conf.Key.name in
      let name = base_name ^ "/branches" in
      let config = Irmin.Backend.Conf.add config Conf.Key.name name in
      base_v config
  end

module Object_storage =
functor
  (Key : Irmin.Type.S)
  (Value : Irmin.Type.S)
  ->
  struct
    include Base_storage (Key) (Value)

    let base_v = v

    let v config =
      let base_name = Irmin.Backend.Conf.get config Conf.Key.name in
      let name = base_name ^ "/objects" in
      let config = Irmin.Backend.Conf.add config Conf.Key.name name in
      base_v config
  end

module H = Irmin.Hash.SHA256
module C = Irmin.Contents.String
module CA = Irmin.Storage.Content_addressable (Object_storage)
module AW = Irmin.Storage.Atomic_write (Branch_storage)
module Maker = Irmin.Maker (CA) (AW)

include Maker.Make (struct
  module Hash = H
  module Contents = C
  module Info = Irmin.Info.Default
  module Metadata = Irmin.Metadata.None
  module Path = Irmin.Path.String_list
  module Branch = Irmin.Branch.String
  module Node = Irmin.Node.Make (Hash) (Path) (Metadata)
  module Commit = Irmin.Commit.Make (Hash)
end)
