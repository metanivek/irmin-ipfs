let spec = Irmin.Backend.Conf.Spec.v "ipfs"

module Key = struct
  let fresh = Irmin.Backend.Conf.key ~spec "fresh" Irmin.Type.bool false
  let name = Irmin.Backend.Conf.key ~spec "name" Irmin.Type.string "irmin-ipfs"
end

let empty = Irmin.Backend.Conf.empty spec

let v ?(fresh = false) repo_name =
  let config =
    Irmin.Backend.Conf.add empty Key.name ("irmin-ipfs/" ^ repo_name)
  in
  Irmin.Backend.Conf.add config Key.fresh fresh
