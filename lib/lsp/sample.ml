open! Base

type header_kind_t =
  | ContentLength of int
  | ContentType of string

type header_t = {
  content_length: int;
  content_type: string option;
}

let parse_header line =
  match String.split line ~on:':' with
  | [key_r; value_r] ->
     let key = String.strip key_r in
     let value = String.strip value_r in
     begin match key with
     | "Content-Length" ->
        (* TODO: suppress exceptions *)
        let num = Int.of_string value in
        Ok (ContentLength num)

     | "Content-Type" ->
        failwith ""

     | _ ->
        failwith ""
     end

  | _ ->
     failwith ""

let read_header r : (header_t, string) Result.t =
  let rec f header =
    match r |> Stdio.In_channel.input_line ~fix_win_eol:true with
    | Some "" ->
       (* TODO: validate content length *)
       Ok header
    | Some line ->
       begin match parse_header line with
       | Ok (ContentLength num) when header.content_length = -1 ->
          f {header with content_length = num}

       | Ok (ContentLength _) ->
          failwith "Already set: ContentLength"

       | Ok (ContentType t) when Option.is_none header.content_type ->
          f {header with content_type = Some t}

       | Ok (ContentType _) ->
          failwith "Already set: ContentType"

       | _ ->
          failwith "head"
       end

    | None ->
       failwith "none"
  in
  f {content_length = -1; content_type = None;}

let write_header w header =
  Stdio.Out_channel.fprintf w "Content-Length: %d\r\n" header.content_length;
  Stdio.Out_channel.fprintf w "\r\n"

module Overlay = struct
  type t = {
    uri: string;
    source: string;
  }

  let create ~uri ~source =
    {
      uri;
      source;
    }

  let update file source =
    {file with source}
end

module Workspace = struct
  module StringMap = Map.M(String)

  type t = {
    files: Overlay.t StringMap.t;
  }

  let create () =
    {
      files = Map.empty (module String);
    }

  let open_file w ~uri ~source =
    let o = Overlay.create ~uri ~source in
    let files' = Map.add_exn w.files ~key:uri ~data:o in
    {w with files = files'}

  let update_file w ~uri ~source =
    let files' =
      Map.update w.files
                 uri
                 ~f:(fun file_opt ->
                   match file_opt with
                   | Some file -> Overlay.update file source
                   | None -> Overlay.create ~uri ~source
                 )
    in
    {w with files = files'}

  let build w ~uri =
    let file = Map.find_exn w.files uri in
    let (topt, diagnosis) = Sounanda_parser.Parser.parse_from_string file.Overlay.source in
    let (typedopt, s_diagnosis) = match topt with
      | Some t ->
         let (a, _, b) = Sounanda_sema.Analyer.analyze t in
         (Some a, b)
      | None ->
         (None, [])
    in
    (typedopt, diagnosis @ s_diagnosis)
end

type state_t = {
  r_ch: Stdio.In_channel.t;
  w_ch: Stdio.Out_channel.t;
  initialized: bool;
  workspace: Workspace.t;
}

type msg =
  | Response of Message_j.json * (Message_j.response_result_constr, string(* TODO *)) Result.t
  | Notification of string * Message_j.message_params_constr

let create r_ch w_ch =
  {
    r_ch;
    w_ch;
    initialized = false;
    workspace = Workspace.create ();
  }

let make_publish_diagnostics_message uri diagnostics =
  let diagnostics_items =
    List.map diagnostics
             ~f:(fun msg ->
               let range =
                 let open Sounanda_parser in
                 let (b, e) = msg.Parser.Diagnostics.span in
                 let pos_b =
                   Message_j.{
                     position_line = b.Span.lnum - 1; (* A line in span is 1-origin *)
                     position_character = b.Span.cnum + 1;
                   }
                 in
                 let pos_e =
                   Message_j.{
                     position_line = e.Span.lnum - 1; (* A line in span is 1-origin *)
                     position_character = e.Span.cnum + 1;
                   }
                 in
                 Message_j.{
                   range_start = pos_b;
                   range_end_ = pos_e;
                 }
               in
               Message_j.{
                 diagnostic_range = range;
                 diagnostic_severity = Some 1; (* error *)
                 diagnostic_code = None;
                 diagnostic_source = Some "sounanda-ls";
                 diagnostic_message = "bo";
               }
             )
  in
  Message_j.{
    publish_diagnostics_params_uri = uri;
    publish_diagnostics_params_diagnostics = diagnostics_items;
  }

let invoke_after_initialized msg state : (msg list * state_t)=
  match msg.Message_j.message_params with
  | Some (`NotificationTextDocumentDidOpen params) ->
     let open Message_j in
     let uri = params.did_open_text_document_params_text_document.text_document_item_uri in
     let w' =
       Workspace.open_file state.workspace
                           ~uri
                           ~source:params.did_open_text_document_params_text_document.text_document_item_text
     in
     let state' = {state with workspace = w'} in
     let (_, diagnostics) = Workspace.build w' ~uri (* TODO: async *) in
     let notification =
       `NotificationPublishDiagnostics (make_publish_diagnostics_message uri diagnostics)
     in
     ([Notification ("textDocument/publishDiagnostics", notification)], state')

  | Some (`NotificationTextDocumentDidChange params) ->
     let open Message_j in
     let uri = params.did_change_text_document_params_text_document.versioned_text_document_identifier_uri in
     let w' =
       List.fold_left params.did_change_text_document_params_content_changes
                      ~init:state.workspace
                      ~f:(fun w c ->
                        Workspace.update_file
                          w
                          ~uri
                          ~source:c.text_document_content_change_event_text
                      )
     in
     let state' = {state with workspace = w'} in
     let (_, diagnostics) = Workspace.build w' ~uri (* TODO: async *) in
     let notification =
       `NotificationPublishDiagnostics (make_publish_diagnostics_message uri diagnostics)
     in
     ([Notification ("textDocument/publishDiagnostics", notification)], state')

  | Some (`RequestTextDocumentCompletion params) ->
     let id = Option.value_exn msg.Message_j.message_id in

     let td = params.Message_j.completion_params_text_document in
     let uri = td.Message_j.text_document_identifier_uri in

     let pos = params.Message_j.completion_params_position in
     let line = pos.Message_j.position_line in
     let character = pos.Message_j.position_character in
     (* TODO: *)
     let span =
       let module S = Sounanda_parser.Span in
       let b = S.create_pos ~line:(line+1) ~column:(character-1) in
       let e = S.create_pos ~line:(line+1) ~column:(character-1) in
       S.create ~b ~e
     in
     let (typed_opt, _) = Workspace.build state.workspace ~uri (* TODO: async *) in
     let names =
       match typed_opt with
       | Some typed ->
          let module TNode = Sounanda_sema.Analyer.Node in
          let module Ty = Sounanda_sema.Analyer.Ty in
          (* TODO: *)
          let names = match TNode.find_node typed span with
            | Some t ->
               Stdio.eprintf "-> %s\n" (t |> Sounanda_sema.Analyer.Node.sexp_of_t |> Sexp.to_string_hum ~indent:2);
               begin match t with
               | TNode.{kind = Id _; ty = Ty.Compound kvs; _} ->
                  List.map ~f:(fun (k, v) -> k)
                           kvs
               | _ ->
                  []
               end
            | None ->
               Stdio.eprintf "=> NOT FOUND\n";
               []
          in
          names
       | None ->
          []
     in

     (* TODO *)
     let items =
       List.map ~f:(fun name ->
                  Message_j.{
                      completion_item_label = name;
                      completion_item_kind = Some 5; (* Field *)
                      completion_item_detail = None;
                      completion_item_deprecated = None;
                      completion_item_preselect = None;
                      completion_item_sort_text = None;
                      completion_item_filter_text = None;
                      completion_item_insert_text = None;
                      completion_item_commit_characters = None;
                      completion_item_data = None;
                  }
                )
                names
     in

     let ret =
       Ok (`ResponseTextDocumentCompletion Message_j.{
             completion_list_is_incomplete = false;
             completion_list_items = items;
          })
     in
     ([Response (id, ret)], state)

  | Some (`RequestCompletionItemResolve item) ->
     let id = Option.value_exn msg.Message_j.message_id in

     let ret =
       Ok (`ResponseCompletionItemResolve item)
     in
     ([Response (id, ret)], state)

  | _ ->
     let method_ = msg.Message_j.message_method_ in
     let id = msg.Message_j.message_id |> Option.map ~f:Yojson.Safe.to_string in
     let params = msg.Message_j.message_params |> Option.map ~f:Message_j.string_of_message_params_constr in
     (* Debug *)
     Stdio.eprintf "Ignore: method = %s, id = %s, params = %s"
                   method_
                   (id |> Option.value ~default:"<NO ID>")
                   (params |> Option.value ~default:"<NO PARAMS>");
     ([], state)

let invoke msg state =
  let str = Message_j.string_of_message msg in
  (* Debug *)
  Stdio.eprintf "Invoked: msg = %s\n\n\n" str;
  Stdio.Out_channel.flush Stdio.stderr;

  match msg.Message_j.message_params with
  | Some (`RequestInitialize _) ->
     let state' = {state with initialized = true} in

     let id = Option.value_exn msg.Message_j.message_id in

     let doc_sync_opts =
       Message_j.{
         text_document_sync_options_open_close = Some true;
         text_document_sync_options_change = Some (Message_json.TextDocumentSyncKind.Full);
         text_document_sync_options_will_save = None;
         text_document_sync_options_will_save_wait_until = None;
         text_document_sync_options_save = None;
       }
     in
     let completion_opts =
       Message_j.{
         completion_options_resolver_provider = Some true;
         completion_options_trigger_characters = Some ["."];
       }
     in
     let server_caps =
       Message_j.{
         server_capabilities_text_document_sync = Some doc_sync_opts;
         server_capabilities_hover_provider = None;
         server_capabilities_completion_provider = Some completion_opts;
         server_capabilities_signature_help_provider = None;
         server_capabilities_definition_provider = None;
         server_capabilities_reference_provider = None;
         server_capabilities_document_highlight_provider = None;
         server_capabilities_document_symbol_provider = None;
         server_capabilities_workspace_symbol_provider = None;
         server_capabilities_code_lens_provider = None;
         server_capabilities_document_formatting_provider = None;
         server_capabilities_document_range_formatting_provider = None;
         server_capabilities_document_on_type_formatting_provider = None;
         server_capabilities_rename_provider = None;
         server_capabilities_document_link_provider = None;
         server_capabilities_execute_command_provider = None;
         server_capabilities_experimental = None;
       }
     in
     let ret =
       Ok (`ResponseInitialize Message_j.{
             initialize_result_capabilities = server_caps;
          })
     in
     ([Response (id, ret)], state')

  | _ when not state.initialized ->
     (* TODO: return error *)
     failwith ""

  | _ ->
     invoke_after_initialized msg state

let write_to w msg =
  let str =
    match msg with
    | Response (id, ret) ->
       let response =
         match ret with
         | Ok result ->
            Message_j.{
             response_jsonrpc = "2.0";
             response_id = id;
             response_result = Some result;
             response_error = None;
            }

         | Error _ ->
            Message_j.{
             response_jsonrpc = "2.0";
             response_id = id;
             response_result = None;
             response_error = None; (* TODO: fix *)
            }
       in
       let str = Message_j.string_of_response response in

       (* Debug *)
       Stdio.eprintf "Response: id = %s, msg = %s\n\n\n" (Yojson.Safe.to_string id) str;

       str

    | Notification (method_, n) ->
       let notification =
         Message_j.{
           message_jsonrpc = "2.0";
           message_id = None;
           message_method_ = method_;
           message_params = Some n;
         }
       in
       let str = Message_j.string_of_message notification in

       (* Debug *)
       Stdio.eprintf "Notification: msg = %s\n\n\n" str;

       str
  in

  (* Debug *)
  Stdio.Out_channel.flush Stdio.stderr;

  let header = {
    content_length = String.length str;
    content_type = None;
  } in
  write_header w header;
  Stdio.Out_channel.fprintf w "%s" str

let start () =
  let rec loop state =
    let r = state.r_ch in
    let w = state.w_ch in

    let header = read_header r in
    match header with
    | Ok header ->
       let buffer = Buffer.create (header.content_length) in
       begin match Stdio.In_channel.input_buffer r buffer ~len:(header.content_length) with
       | Some () ->
          (* Debug *)
          Stdio.eprintf "-> %s\n" (Buffer.contents buffer);
          Stdio.Out_channel.flush Stdio.stderr;

          let msg = Message_j.message_of_string (Buffer.contents buffer) in

          let (res_msgs, state) = invoke msg state in
          let () = res_msgs |> List.iter ~f:(write_to w) in
          Stdio.Out_channel.flush w;

          loop state
       | None ->
          failwith ""
       end
    | _ ->
       failwith "no header"
  in
  let state = create Stdio.stdin Stdio.stdout in
  loop state
