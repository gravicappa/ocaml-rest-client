let recv_buffer_size = 16384

let writer_callback a d =
  Buffer.add_string a d;
  String.length d

type setting =
  | Timeout of int

let default_settings = [Timeout 1200]

let init_connection ?(settings = default_settings) url =
  let recv_buf = Buffer.create recv_buffer_size in
  let apply_setting c = function
    | Timeout ms -> Curl.set_timeout c ms
  in
  let c = Curl.init () in
    List.iter (apply_setting c) settings;
    Curl.set_sslverifypeer c false;
    Curl.set_sslverifyhost c Curl.SSLVERIFYHOST_EXISTENCE;
    Curl.set_writefunction c (writer_callback recv_buf);
    Curl.set_tcpnodelay c true;
    Curl.set_verbose c false;
    Curl.set_post c false;
    Curl.set_url c url;
    recv_buf, c

let set_headers c headers =
  match headers with
  | None -> ()
  | Some h -> Curl.set_httpheader c h

let with_curl ?(settings = []) url proc =
  let recv_buf, c = init_connection ~settings url in
  Lwt.finalize (fun () -> proc recv_buf c)
               (fun () -> Curl.cleanup c |> Lwt.return)

let to_result (code, data) =
  if code >= 200 && code < 300 then
    Ok (code, data) |> Lwt.return
  else
    Error (code, data) |> Lwt.return

let request recv_buf c =
  let%lwt _ = Curl_lwt.perform c in
  (Curl.get_responsecode c, Buffer.contents recv_buf) |> to_result

let get ?headers ?(settings = []) url =
  with_curl ~settings url @@ fun recv_buf c ->
    set_headers c headers;
    Curl.set_followlocation c true;
    request recv_buf c

let perform_post ?(content_type = "application/json") ?(headers = []) c
    recv_buf data =
  Curl.set_httpheader c ([ "Content-Type: " ^ content_type ] @ headers);
  Curl.set_postfields c data;
  Curl.set_postfieldsize c (String.length data);
  request recv_buf c

let post ?(content_type = "application/json") ?(headers = []) ?(settings = [])
         url data =
  with_curl ~settings url @@ fun recv_buf c ->
    Curl.set_post c true;
    perform_post ~content_type ~headers c recv_buf data

let patch ?(content_type = "application/json") ?(headers = [])
          ?(settings = []) url data =
  with_curl ~settings url @@ fun recv_buf c ->
    Curl.set_post c true;
    Curl.set_customrequest c "PATCH";
    perform_post ~content_type ~headers c recv_buf data

let put ?(content_type = "application/json") ?(headers = []) ?(settings = [])
        url data =
  let pos = ref 0
  and len = String.length data in
  let rf cnt =
    let can_send = len - !pos in
    let to_send = if can_send > cnt then cnt else can_send in
    let r = String.sub data !pos to_send in
      pos := !pos + to_send; r in
  with_curl ~settings url @@ fun recv_buf c ->
    Curl.set_put c true;
    Curl.set_upload c true;
    Curl.set_readfunction c rf;
    Curl.set_httpheader c (["Content-Type: " ^ content_type] @ headers);
    request recv_buf c

let delete ?headers ?(settings = []) url =
  with_curl ~settings url @@ fun recv_buf c ->
    Curl.set_customrequest c "DELETE";
    Curl.set_followlocation c false;
    set_headers c headers;
    request recv_buf c

let () = Curl.global_init Curl.CURLINIT_GLOBALALL
let () = at_exit Curl.global_cleanup

let from_resp proc = Lwt.map @@ function
  | Ok (200, json) -> begin
      try Yojson.Safe.from_string json |> proc
      with | Yojson.Json_error x -> Error ("Error: " ^ x)
  end
  | Ok _ -> Error "Response error"
  | Error (0, "") -> Error (Printf.sprintf "Cannot connect")
  | Error (code, err) -> Error (Printf.sprintf "Error: %d: %s" code err)
