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

type state_t = {
  r_ch: Stdio.In_channel.t;
  w_ch: Stdio.Out_channel.t;
  initialized: bool;
}

let create r_ch w_ch =
  {
    r_ch;
    w_ch;
    initialized = false;
  }

let invoke_after_initialized msg state =
  match msg.Message_j.message_params with
  | Some (`RequestTextDocumentCompletion params) ->
     let id = Option.value_exn msg.Message_j.message_id in

     (* TODO *)
     let item0 =
       Message_j.{
         completion_item_label = "補完サンプル";
         completion_item_kind = Some 2;
         completion_item_detail = None;
         completion_item_deprecated = None;
         completion_item_preselect = None;
         completion_item_sort_text = None;
         completion_item_filter_text = None;
         completion_item_insert_text = None;
         completion_item_commit_characters = None;
         completion_item_data = None;
       }
     in
     let item1 =
       Message_j.{
         completion_item_label = "進捗どうですか？ｗ";
         completion_item_kind = Some 14;
         completion_item_detail = None;
         completion_item_deprecated = None;
         completion_item_preselect = None;
         completion_item_sort_text = None;
         completion_item_filter_text = None;
         completion_item_insert_text = None;
         completion_item_commit_characters = None;
         completion_item_data = None;
       }
     in
     let items = [item0; item1] in

     let ret =
       Ok (`ResponseTextDocumentCompletion Message_j.{
             completion_list_is_incomplete = false;
             completion_list_items = items;
          })
     in
     (Some (ret, id), state)

  | Some (`RequestCompletionItemResolve item) ->
     let id = Option.value_exn msg.Message_j.message_id in

     let ret =
       Ok (`ResponseCompletionItemResolve item)
     in
     (Some (ret, id), state)

  | _ ->
     let method_ = msg.Message_j.message_method_ in
     let id = msg.Message_j.message_id |> Option.map ~f:Yojson.Safe.to_string in
     let params = msg.Message_j.message_params |> Option.map ~f:Message_j.string_of_message_params_constr in
     (* Debug *)
     Stdio.eprintf "Ignore: method = %s, id = %s, params = %s"
                   method_
                   (id |> Option.value ~default:"<NO ID>")
                   (params |> Option.value ~default:"<NO PARAMS>");
     (None, state)

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
     (Some (ret, id), state')

  | _ when not state.initialized ->
     (* TODO: return error *)
     failwith ""

  | _ ->
     invoke_after_initialized msg state

let write_to w (ret, id) =
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

          let (res, state) = invoke msg state in
          let () = res |> Option.iter ~f:(write_to w) in
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
