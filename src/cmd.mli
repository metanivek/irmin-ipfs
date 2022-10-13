module On_output : sig
  val print : unit * (unit -> string -> unit)
  val nothing : unit * (unit -> string -> unit)
end

val run :
  on_stdout:'a * ('a -> string -> 'a) ->
  on_stderr:'b * ('b -> string -> 'b) ->
  string ->
  'a * 'b

val run_default : string -> unit
