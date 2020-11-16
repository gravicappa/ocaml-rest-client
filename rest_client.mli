type setting = {
  recv_buffer_bytes: int;
  timeout_ms: int;
  setup: Curl.t -> unit;
}

type header = string * string

val default_settings : setting

val init : unit -> unit

val with_curl :
  ?settings:setting ->
  string -> (Buffer.t -> Curl.t -> 'a Lwt.t) -> 'a Lwt.t

val request :
  Buffer.t -> Curl.t -> (int * string, int * string) result Lwt.t

val get :
  ?headers:header list ->
  ?settings:setting ->
  string -> (int * string, int * string) result Lwt.t

val head :
  ?headers:header list ->
  ?settings:setting ->
  string -> (int, int) result Lwt.t

val post :
  ?content_type:string ->
  ?headers:header list ->
  ?settings:setting ->
  string -> string -> (int * string, int * string) result Lwt.t

val patch :
  ?content_type:string ->
  ?headers:header list ->
  ?settings:setting ->
  string -> string -> (int * string, int * string) result Lwt.t

val put :
  ?content_type:string ->
  ?headers:header list ->
  ?settings:setting ->
  string -> string -> (int * string, int * string) result Lwt.t

val delete :
  ?headers:header list ->
  ?settings:setting ->
  string -> (int * string, int * string) result Lwt.t

val from_resp :
  (Yojson.Safe.json -> ('a, string) result) ->
  (int * string, int * string) result Lwt.t -> ('a, string) result Lwt.t

