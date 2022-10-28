module Object_storage =
functor
  (Key : Irmin.Type.S)
  (Value : Irmin.Type.S)
  ->
  struct
    (** Types *)

    type t = unit
    (** [t] is [unit] because we read/write directly with IPFS. *)

    type key = Key.t
    type value = Value.t

    (** Initialisation / Closing *)

    let v _config = Lwt.return_unit
    let close _t = Lwt.return_unit

    (** Operations *)

    let set t key value =
      let to_bin_string_v = Irmin.Type.(unstage (to_bin_string Value.t)) in
      let bin_v = to_bin_string_v value in
      let v = Ipfs.write bin_v in
      (* The hash that IPFS returns should be the same as [key]. *)
      let v_key = Irmin.Type.(of_string Key.t) v in
      (match v_key with
      | Ok v_key -> assert (Irmin.Type.(unstage (equal Key.t)) key v_key)
      | Error (`Msg e) -> failwith e);
      Lwt.return t

    let find _t key =
      let key_s = Irmin.Type.to_string Key.t key in
      let of_bin_string_v = function
        | None -> None
        | Some s -> (
            let v = Ipfs.read s in
            match Irmin.Type.(unstage (of_bin_string Value.t)) v with
            | Ok s -> Some s
            | Error _ -> None)
      in
      of_bin_string_v (Some key_s) |> Lwt.return

    let mem t key =
      let open Lwt.Syntax in
      let* r = find t key in
      match r with None -> Lwt.return_false | Some _ -> Lwt.return_true

    let keys _t = failwith "Cannot call [keys] for object store."
    let remove _t _key = failwith "Cannot call [remove] for object store."
    let clear _t = failwith "Cannot call [clear] for object store."

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
    (** [M] stores the mapping of branch name to hash. It is stored to IPFS and
        published with the name of the storage.*)
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
      t.m <- f t.m;
      save t;
      publish t

    (** Initialisation / Closing *)

    let v config =
      let name = Irmin.Backend.Conf.get config Conf.Key.name in
      let fresh = Irmin.Backend.Conf.get config Conf.Key.fresh in
      let ipns_key = Ipns.create name in
      let t = { ipns_key; m = M.empty; hash = None } in
      (match fresh with
      | true ->
          save t;
          publish t
      | false -> load t);
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

module H = Hash
module CA = Irmin.Storage.Content_addressable (Object_storage)
module AW = Irmin.Storage.Atomic_write (Branch_storage)
module Maker = Irmin_ext.Maker (CA) (AW)

include Maker.Make (struct
  module Hash = H

  (* Use [String_v2] to avoid "prehash prefixing". *)
  module Contents = Irmin.Contents.String_v2
  module Info = Irmin.Info.Default
  module Metadata = Irmin.Metadata.None
  module Path = Irmin.Path.String_list
  module Branch = Irmin.Branch.String
end)

module Ipns = struct
  let key config =
    let name = Irmin.Backend.Conf.get config Conf.Key.name in
    Ipns.lookup name
end
