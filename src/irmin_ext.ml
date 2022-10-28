(** This is mostly copied from irmin.ml's Make functor so that we can use v2
    Nodess and Commits that do not have "prehash prefixing".

    TODO: figure out how to upstream changes that allow for easy v2 store
    construction (or determine if I missed somethign! :) *)

open Irmin

module type Maker_generic_key_args = sig
  module Contents_store : Indexable.Maker_concrete_key2
  module Node_store : Indexable.Maker_concrete_key1
  module Commit_store : Indexable.Maker_concrete_key1
  module Branch_store : Atomic_write.Maker
end

module Maker_generic_key (Backend : Maker_generic_key_args) = struct
  type endpoint = unit
  type ('h, 'v) contents_key = ('h, 'v) Backend.Contents_store.key
  type 'h node_key = 'h Backend.Node_store.key
  type 'h commit_key = 'h Backend.Commit_store.key

  module Make (S : Schema.S) = struct
    module X = struct
      module Schema = S
      module Hash = S.Hash
      module Contents_key = Backend.Contents_store.Key (S.Hash) (S.Contents)
      module Node_key = Backend.Node_store.Key (S.Hash)
      module Commit_key = Backend.Commit_store.Key (S.Hash)

      module Contents = struct
        module Backend = Backend.Contents_store.Make (S.Hash) (S.Contents)
        include Contents.Store_indexable (Backend) (S.Hash) (S.Contents)
      end

      module Node = struct
        (* Use [Make_v2]. *)
        module Value =
          Node.Generic_key.Make_v2 (S.Hash) (S.Path) (S.Metadata) (Contents_key)
            (Node_key)

        module Backend = Backend.Node_store.Make (S.Hash) (Value)

        include
          Node.Generic_key.Store (Contents) (Backend) (S.Hash) (Value)
            (S.Metadata)
            (S.Path)
      end

      module Node_portable = Node.Value.Portable

      module Commit = struct
        module Commit_maker = Commit.Generic_key.Maker (Schema.Info)

        (* Use [Make_v2]. *)
        module Value = Commit_maker.Make_v2 (S.Hash) (Node_key) (Commit_key)
        module Backend = Backend.Commit_store.Make (S.Hash) (Value)

        include
          Commit.Generic_key.Store (S.Info) (Node) (Backend) (S.Hash) (Value)
      end

      module Commit_portable = Commit.Value.Portable

      module Branch = struct
        module Val = Commit.Key
        include Backend.Branch_store (S.Branch) (Val)
        module Key = S.Branch
      end

      module Slice = Irmin.Backend.Slice.Make (Contents) (Node) (Commit)
      module Remote = Irmin.Backend.Remote.None (Commit_key) (S.Branch)

      module Repo = struct
        type t = {
          config : Irmin.Backend.Conf.t;
          contents : Perms.read Contents.t;
          nodes : Perms.read Node.t;
          commits : Perms.read Commit.t;
          branch : Branch.t;
        }

        let contents_t t = t.contents
        let node_t t = t.nodes
        let commit_t t = t.commits
        let branch_t t = t.branch
        let config t = t.config

        let batch t f =
          Contents.Backend.batch t.contents @@ fun c ->
          Node.Backend.batch (snd t.nodes) @@ fun n ->
          Commit.Backend.batch (snd t.commits) @@ fun ct ->
          let contents_t = c in
          let node_t = (contents_t, n) in
          let commit_t = (node_t, ct) in
          f contents_t node_t commit_t

        let v config =
          let open Lwt.Syntax in
          let* contents = Contents.Backend.v config in
          let* nodes = Node.Backend.v config in
          let* commits = Commit.Backend.v config in
          let nodes = (contents, nodes) in
          let commits = (nodes, commits) in
          let+ branch = Branch.v config in
          { contents; nodes; commits; branch; config }

        let close t =
          let open Lwt.Syntax in
          let* () = Contents.Backend.close t.contents in
          let* () = Node.Backend.close (snd t.nodes) in
          let* () = Commit.Backend.close (snd t.commits) in
          Branch.close t.branch
      end
    end

    include Of_backend (X)
  end
end

module Maker (CA : Content_addressable.Maker) (AW : Atomic_write.Maker) = struct
  module Indexable_store = struct
    type 'h key = 'h

    module Key = Key.Of_hash

    module Make (Hash : Hash.S) (Value : Type.S) = struct
      module CA = Content_addressable.Check_closed (CA) (Hash) (Value)
      include Indexable.Of_content_addressable (Hash) (CA)

      let v = CA.v
    end
  end

  module Maker_args = struct
    module Contents_store = Indexable.Maker_concrete_key2_of_1 (Indexable_store)
    module Node_store = Indexable_store
    module Commit_store = Indexable_store
    module Branch_store = Atomic_write.Check_closed (AW)
  end

  include Maker_generic_key (Maker_args)
end
