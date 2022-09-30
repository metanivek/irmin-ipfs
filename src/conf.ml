let spec = Irmin.Backend.Conf.Spec.v "ipfs"
let name = Irmin.Backend.Conf.key ~spec "name" Irmin.Type.string "irmin-ipfs"
let empty = Irmin.Backend.Conf.empty spec

let v repo_name =
  let config = empty in
  Irmin.Backend.Conf.add config name ("irmin-ipfs/" ^ repo_name)
