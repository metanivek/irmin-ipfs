type hash = string

val to_hash : string -> hash
(** [to_hash v] returns the IPFS hash of [v] *)

val read : hash -> string
(** [read h] returns the value with hash of [h]

    Note: this will look across the IPFS network for content associated with
    [h]. If it is not already in the local node (or doesn't exist!), this could
    take a long time or never return. *)

val write : string -> hash
(** [write v] writes [v] to the local IPFS node and returns its hash *)
