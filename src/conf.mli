module Key : sig
  val fresh : bool Irmin.Backend.Conf.key
  val name : string Irmin.Backend.Conf.key
end

val v : ?fresh:bool -> string -> Irmin.Backend.Conf.t
