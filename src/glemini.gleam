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
  SuccessResponse(status: Int, mimetype: String, body: String)
  InputResponse(status: Int, prompt: String)
  RedirectResponse(status: Int, uri: String)
  ErrorResponse(status: Int, message: Option(String))
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
      [gemtext.heading1("Welcome to Glemini!")]
      |> gemtext_response()
    })

  let assert Ok(_) = start(config)

  io.println("Server running!")
  process.sleep_forever()
}

/// Start the gemini server.
/// Expects a `ServerConfig`
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

/// Creates a new server configuration with missing certificate and empty request handler.
pub fn new_config() -> ServerConfig {
  ServerConfig(port: 1965, certfile: "", keyfile: "", request_handler: fn(_) {
    ErrorResponse(51, None)
  })
}

/// Adds ssl configuration to the server configuration.
pub fn add_ssl(
  config: ServerConfig,
  certfile certfile: String,
  keyfile keyfile: String,
) -> ServerConfig {
  ServerConfig(..config, certfile: certfile, keyfile: keyfile)
}

/// Adds a request handler to the server configuration.
/// The request handler is your main function where you'll do routing.
/// # Example
/// ```gleam
/// fn my_request_handler(req: Request) -> Response {
///   case req.path {
///     "/hello" -> gemtext_response([gemtext.heading1("Hello, world!")])
///     _ -> not_found_response()
///   }
/// }
/// ```
pub fn add_handler(
  config: ServerConfig,
  request_handler: fn(Request) -> Response,
) -> ServerConfig {
  ServerConfig(..config, request_handler: request_handler)
}

// Response functions

// Success

pub fn gemtext_response(lines: gemtext.Lines) -> Response {
  SuccessResponse(20, "text/gemini", gemtext.to_string(lines))
}

// Input

pub fn input_response(prompt: String) -> Response {
  InputResponse(10, prompt)
}

pub fn sensitive_input_response(prompt: String) -> Response {
  InputResponse(11, prompt)
}

// Redirects

pub fn temporary_redirect_response(uri: String) -> Response {
  RedirectResponse(30, uri)
}

pub fn permanent_redirect_response(uri: String) -> Response {
  RedirectResponse(31, uri)
}

// Temporary errors

pub fn temporary_failure_response(message: String) -> Response {
  ErrorResponse(40, Some(message))
}

pub fn server_unavailable_response() -> Response {
  ErrorResponse(41, None)
}

pub fn cgi_error_response() -> Response {
  ErrorResponse(42, None)
}

pub fn proxy_error_response() -> Response {
  ErrorResponse(43, None)
}

pub fn slow_down_response() -> Response {
  ErrorResponse(44, None)
}

// Permanent errors

pub fn permanent_failure_response(message: String) -> Response {
  ErrorResponse(50, Some(message))
}

pub fn not_found_response() -> Response {
  ErrorResponse(51, None)
}

pub fn gone_response() -> Response {
  ErrorResponse(52, None)
}

pub fn proxy_request_refused_response() -> Response {
  ErrorResponse(53, None)
}

pub fn bad_request_response() -> Response {
  ErrorResponse(59, None)
}

// Certificate errors

pub fn client_certificate_required_response() -> Response {
  ErrorResponse(60, None)
}

pub fn certificate_not_authorised_response() -> Response {
  ErrorResponse(61, None)
}

pub fn certificate_not_valid_response() -> Response {
  ErrorResponse(62, None)
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
  case res {
    SuccessResponse(status, mimetype, body) ->
      int.to_string(status) <> " " <> mimetype <> "\r\n" <> body
    InputResponse(status, prompt) ->
      int.to_string(status) <> " " <> prompt <> "\r\n"
    RedirectResponse(status, uri) ->
      int.to_string(status) <> " " <> uri <> "\r\n"
    ErrorResponse(status, message) ->
      int.to_string(status)
      <> " "
      <> case message {
        Some(message) -> message
        None -> ""
      }
      <> "\r\n"
  }
}
