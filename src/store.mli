include
  Irmin.S
    with type Schema.Path.step = string
     and type Schema.Path.t = string list
     and type Schema.Branch.t = string
     and type Schema.Contents.t = string
