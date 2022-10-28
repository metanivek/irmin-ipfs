module Key : sig
  type t

  val hash : t -> string
  val name : t -> string
end

val create : string -> Key.t
(** [create name] creates a key named [name] *)

val resolve : Key.t -> Ipfs.hash option
(** [resolve key] resolves [key] to its IPFS hash *)

val publish : Key.t -> Ipfs.hash -> unit
(** [publish key hash] updates IPNS to point [key] to [hash] *)

val lookup : string -> Key.t option
(** [lookup name] looks up a local key with the give name. *)
