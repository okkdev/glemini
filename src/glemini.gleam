import gleam/bit_array
import gleam/bytes_builder
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/otp/actor
import gleam/otp/supervisor
import gleam/result
import gleam/string
import gleam/uri
import glisten.{type StartError, Packet}

pub type ServerConfig {
  ServerConfig(
    port: Int,
    certfile: String,
    keyfile: String,
    request_handler: fn(Request) -> Response,
  )
}

pub opaque type Request {
  Request(host: String, path: String, query: String)
}

pub opaque type Response {
  Response(status: Int, content: String)
}

pub opaque type GleminiError {
  RequestError
  SchemeError
}

@internal
pub fn main() {
  io.println("Starting server!")

  let config =
    new_config()
    |> add_ssl(certfile: "certs/cert.crt", keyfile: "certs/cert.key")
    |> add_handler(fn(_) { text_response(20, "# Hello, world!") })

  let assert Ok(_) = start(config)

  io.println("Server running!")
  process.sleep_forever()
}

pub fn start(
  config: ServerConfig,
) -> Result(Subject(supervisor.Message), StartError) {
  glisten.handler(fn(_conn) { #(Nil, None) }, fn(req, _state, conn) {
    let assert Packet(req) = req
    let assert Ok(res) = handle_gemini_request(req, config.request_handler)
    let assert Ok(_) = glisten.send(conn, bytes_builder.from_string(res))
    actor.Stop(process.Normal)
  })
  |> glisten.serve_ssl(
    port: config.port,
    certfile: config.certfile,
    keyfile: config.keyfile,
  )
}

pub fn new_config() -> ServerConfig {
  ServerConfig(port: 1965, certfile: "", keyfile: "", request_handler: fn(_) {
    Response(51, "")
  })
}

pub fn add_ssl(
  config: ServerConfig,
  certfile certfile: String,
  keyfile keyfile: String,
) -> ServerConfig {
  ServerConfig(..config, certfile: certfile, keyfile: keyfile)
}

pub fn add_handler(
  config: ServerConfig,
  request_handler: fn(Request) -> Response,
) -> ServerConfig {
  ServerConfig(..config, request_handler: request_handler)
}

fn handle_gemini_request(
  req: BitArray,
  handler: fn(Request) -> Response,
) -> Result(String, GleminiError) {
  use req <- result.try(parse_request(req))

  io.debug(req)

  handler(req)
  |> io.debug
  |> response_to_string
  |> Ok
}

fn response_to_string(res: Response) -> String {
  int.to_string(res.status) <> " " <> res.content
}

fn text_response(code: Int, content: String) -> Response {
  Response(code, "text/gemini\r\n" <> content)
}

fn parse_request(req: BitArray) -> Result(Request, GleminiError) {
  use req <- result.try(
    bit_array.to_string(req)
    |> result.map(fn(x) { string.trim(x) })
    |> result.replace_error(RequestError),
  )

  use uri <- result.try(uri.parse(req) |> result.replace_error(RequestError))

  case uri {
    uri.Uri(
      scheme: Some("gemini"),
      host: Some(host),
      query: Some(query),
      path:,
      ..,
    ) -> Ok(Request(host, path, query))
    uri.Uri(scheme: Some("gemini"), host: Some(host), path:, ..) ->
      Ok(Request(host, path, ""))
    _ -> Error(SchemeError)
  }
}
