type setting = {
  recv_buffer_bytes: int;
  timeout_ms: int;
  setup: Curl.t -> unit;
}

type header = string * string

let default_settings = {
  recv_buffer_bytes = 16384;
  timeout_ms = 1200;
  setup = fun _ -> ();
}

let init_connection ?(settings = default_settings) url =
  let recv_buf = Buffer.create settings.recv_buffer_bytes in

  let writer_callback a d =
    Buffer.add_string a d;
    String.length d in

  let c = Curl.init () in
  Curl.set_timeout c settings.timeout_ms;
  Curl.set_sslverifypeer c false;
  Curl.set_sslverifyhost c Curl.SSLVERIFYHOST_EXISTENCE;
  Curl.set_writefunction c (writer_callback recv_buf);
  Curl.set_tcpnodelay c true;
  Curl.set_verbose c false;
  Curl.set_post c false;
  Curl.set_url c url;
  Curl.set_maxredirs c 1;
  settings.setup c;
  recv_buf, c

let string_of_header (k, v) = k ^ ": " ^ v

let set_headers c headers =
  List.map string_of_header headers |> Curl.set_httpheader c

let init () =
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  at_exit Curl.global_cleanup

let with_curl ?(settings = default_settings) url proc =
  let recv_buf, c = init_connection ~settings url in
  Lwt.finalize (fun () -> proc recv_buf c)
               (fun () -> Curl.cleanup c |> Lwt.return)

let request recv_buf c =
  let%lwt _ = Curl_lwt.perform c in
  let data = Buffer.contents recv_buf in
  match Curl.get_responsecode c with
  | code when code >= 200 && code < 300 -> Lwt.return_ok (code, data)
  | code -> Lwt.return_error (code, data)

let get ?(headers = []) ?(settings = default_settings) url =
  with_curl ~settings url @@ fun recv_buf c ->
    set_headers c headers;
    Curl.set_followlocation c true;
    request recv_buf c

let head ?(headers = []) ?(settings = default_settings) url =
  with_curl ~settings url @@ fun recv_buf c ->
    set_headers c headers;
    Curl.set_followlocation c true;
    Curl.set_customrequest c "HEAD";
    let%lwt _ = Curl_lwt.perform c in
    match Curl.get_responsecode c with
    | code when code >= 200 && code < 300 -> Lwt.return_ok code
    | code -> Lwt.return_error code

let set_headers_w_content_type c content_type headers =
  (("Content-Type", content_type) :: headers) |> set_headers c

let perform_post ?(content_type = "application/json") ?(headers = []) c
    recv_buf data =
  set_headers_w_content_type c content_type headers;
  Curl.set_postfields c data;
  Curl.set_postfieldsize c (String.length data);
  request recv_buf c

let post ?(content_type = "application/json")
         ?(headers = [])
         ?(settings = default_settings)
         url
         data =
  with_curl ~settings url @@ fun recv_buf c ->
    Curl.set_post c true;
    perform_post ~content_type ~headers c recv_buf data

let patch ?(content_type = "application/json")
          ?(headers = [])
          ?(settings = default_settings)
          url
          data =
  with_curl ~settings url @@ fun recv_buf c ->
    Curl.set_post c true;
    Curl.set_customrequest c "PATCH";
    perform_post ~content_type ~headers c recv_buf data

let put ?(content_type = "application/json")
        ?(headers = [])
        ?(settings = default_settings)
        url
        data =
  let pos = ref 0 in

  let len = String.length data in

  let rf cnt =
    let can_send = len - !pos in
    let to_send = if can_send > cnt then cnt else can_send in
    let r = String.sub data !pos to_send in
      pos := !pos + to_send; r in

  with_curl ~settings url @@ fun recv_buf c ->
    Curl.set_put c true;
    Curl.set_upload c true;
    Curl.set_readfunction c rf;
    set_headers_w_content_type c content_type headers;
    request recv_buf c

let delete ?(headers = []) ?(settings = default_settings) url =
  with_curl ~settings url @@ fun recv_buf c ->
    Curl.set_customrequest c "DELETE";
    Curl.set_followlocation c false;
    set_headers c headers;
    request recv_buf c

let from_resp proc = Lwt.map @@ function
  | Ok (200, json) -> begin
      try Yojson.Safe.from_string json |> proc
      with | Yojson.Json_error x -> Error ("Error: " ^ x)
  end
  | Ok _ -> Error "Response error"
  | Error (0, "") -> Error (Printf.sprintf "Connection failed")
  | Error (code, err) -> Error (Printf.sprintf "Error: %d: %s" code err)
