type t = Ipfs.hash

let t = Irmin.Type.string

let hash f =
  let buf = Buffer.create 0 in
  let acc s = Buffer.add_string buf s in
  let () = f acc in
  Ipfs.to_hash (Buffer.contents buf)

let short_hash t =
  let b = String.to_bytes t in
  Bytes.get_int8 b 0

let hash_size = 46
let to_raw_string t = t
let unsafe_of_raw_string t = t

let short_hash_substring t ~off =
  Bigstringaf.substring t ~off ~len:hash_size
  |> unsafe_of_raw_string
  |> short_hash
