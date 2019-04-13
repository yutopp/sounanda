module TextDocumentSyncKind : sig
  type t =
    | None
    | Full
    | Incremental

  val wrap : int -> t
  val unwrap : t -> int
end = struct
  type t =
    | None
    | Full
    | Incremental

  let wrap raw =
    match raw with
    | 0 -> None
    | 1 -> Full
    | 2 -> Incremental
    | _ -> failwith ""

  let unwrap kind =
    match kind with
    | None -> 0
    | Full -> 1
    | Incremental -> 2
end

module CompletionTriggerKind : sig
  type t =
	(**
	 * Completion was triggered by typing an identifier (24x7 code
	 * complete), manual invocation (e.g Ctrl+Space) or via API.
	 *)
    | Invoked (* 1 *)
	(**
	 * Completion was triggered by a trigger character specified by
	 * the `triggerCharacters` properties of the `CompletionRegistrationOptions`.
	 *)
    | TriggerCharacter (* 2 *)
    (**
	 * Completion was re-triggered as the current completion list is incomplete.
	 *)
    | TriggerForIncompleteCompletions (* 3 *)

  val wrap : int -> t
  val unwrap : t -> int
end = struct
  type t =
    | Invoked (* 1 *)
    | TriggerCharacter (* 2 *)
    | TriggerForIncompleteCompletions (* 3 *)

  let wrap raw =
    match raw with
    | 1 -> Invoked
    | 2 -> TriggerCharacter
    | 3 -> TriggerForIncompleteCompletions
    | _ -> failwith ""

  let unwrap kind =
    match kind with
    | Invoked -> 1
    | TriggerCharacter -> 2
    | TriggerForIncompleteCompletions -> 3
end

module InsertTextFormat : sig
  type t =
	(**
	 * The primary text to be inserted is treated as a plain string.
	 *)
    | PlainText (* 1 *)
	(**
	 * The primary text to be inserted is treated as a snippet.
	 *
	 * A snippet can define tab stops and placeholders with `$1`, `$2`
	 * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
	 * the end of the snippet. Placeholders with equal identifiers are linked,
	 * that is typing in one will update others too.
	 *)
    | Snippet (* 2 *)

  val wrap : int -> t
  val unwrap : t -> int
end = struct
  type t =
    | PlainText (* 1 *)
    | Snippet (* 2 *)

  let wrap raw =
    match raw with
    | 1 -> PlainText
    | 2 -> Snippet
    | _ -> failwith ""

  let unwrap kind =
    match kind with
    | PlainText -> 1
    | Snippet -> 2
  end

module Adapter = struct
  module Message =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make (
        struct
          let type_field_name = "method"
          let value_field_name = "params"
          let known_tags =
            Some (
                [
                  "initialize";
                  "textDocument/didOpen";
                  "textDocument/didChange";
                  "textDocument/completion";
                  "completionItem/resolve";
                ],
                "Unknown"
              )
        end)

  module AsAny : Atdgen_runtime.Json_adapter.S =
    struct
      let normalize orig =
        failwith "normalize (not supported)"

      let restore json =
        match json with
        | `List [`String tag; v] ->
           v
        | _ ->
           failwith (Yojson.Safe.to_string json)
    end
end
