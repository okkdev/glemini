import gleam/bit_array
import gleam/bytes_builder
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervisor
import gleam/result
import gleam/string
import gleam/uri
import glisten.{type StartError, Packet}

import glemini/gemtext

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

pub type Response {
  Response(status: Int, content: String, body: Option(String))
}

pub opaque type GleminiError {
  RequestParseError
  RequestSchemeError
}

@internal
pub fn main() {
  io.println("Starting server!")

  let config =
    new_config()
    |> add_ssl(certfile: "certs/cert.crt", keyfile: "certs/cert.key")
    |> add_handler(fn(_) {
      [
        gemtext.heading1("Welcome to Glemini!"),
        gemtext.link_with_caption("/", "Home"),
        gemtext.link("/"),
        gemtext.blank_line(),
        gemtext.blank_line(),
        gemtext.text("just some text"),
        gemtext.blank_line(),
        gemtext.list_item("item 1"),
        gemtext.list_item("item 2"),
        gemtext.list_item("item 3"),
        gemtext.blank_line(),
        gemtext.blank_line(),
        gemtext.preformatted("              this is \n    preformatted \ntext"),
        gemtext.blank_line(),
        gemtext.preformatted_with_caption("fn gleam() {}", "gleam"),
      ]
      |> gemtext_response()
    })

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

// Config

pub fn new_config() -> ServerConfig {
  ServerConfig(port: 1965, certfile: "", keyfile: "", request_handler: fn(_) {
    Response(51, "", None)
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

// Response functions

pub fn gemtext_response(lines: gemtext.Lines) -> Response {
  Response(20, "text/gemini", Some(gemtext.to_string(lines)))
}

pub fn input_response(prompt: String) -> Response {
  Response(10, prompt, None)
}

pub fn sensitive_input_response(prompt: String) -> Response {
  Response(11, prompt, None)
}

pub fn temporary_redirect_response(uri: String) -> Response {
  Response(30, uri, None)
}

pub fn permanent_redirect_response(uri: String) -> Response {
  Response(31, uri, None)
}

// Helpers

fn handle_gemini_request(
  req: BitArray,
  handler: fn(Request) -> Response,
) -> Result(String, GleminiError) {
  use req <- result.try(parse_request(req))

  handler(req)
  |> response_to_string
  |> Ok
}

fn parse_request(req: BitArray) -> Result(Request, GleminiError) {
  use req <- result.try(
    bit_array.to_string(req)
    |> result.map(fn(x) { string.trim(x) })
    |> result.replace_error(RequestParseError),
  )

  use uri <- result.try(
    uri.parse(req) |> result.replace_error(RequestParseError),
  )

  case uri {
    uri.Uri(
      scheme: Some("gemini"),
      host: Some(host),
      query: Some(query),
      path: path,
      ..,
    ) -> Ok(Request(host, path, query))
    uri.Uri(scheme: Some("gemini"), host: Some(host), path: path, ..) ->
      Ok(Request(host, path, ""))
    _ -> Error(RequestSchemeError)
  }
}

fn response_to_string(res: Response) -> String {
  case res.body {
    Some(body) ->
      int.to_string(res.status) <> " " <> res.content <> "\r\n" <> body
    None -> int.to_string(res.status) <> " " <> res.content <> "\r\n"
  }
}
