//// Main Glemini module. Contains the server and response functions.
//// # Example usage:
//// ```gleam
//// let config =
////   new_config()
////   |> add_ssl(certfile: "certs/cert.crt", keyfile: "certs/cert.key")
////   |> add_handler(fn(req) {
////     case req.path {
////       "/" -> gemtext_response([gemtext.heading1("Welcome to Glemini!")])
////       _ -> not_found_response()
////     }
////   })
//// let assert Ok(_) = start(config)
//// ```

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

/// Glemini server configuration.
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
    |> add_handler(fn(req) {
      case req.path {
        "/" ->
          [gemtext.heading1("Welcome to Glemini!")]
          |> gemtext_response()
        _ -> not_found_response()
      }
    })

  let assert Ok(_) = start(config)

  io.println("Server running!")
  process.sleep_forever()
}

/// Starts the gemini server.
/// Expects a `ServerConfig`
/// # Example
/// ```gleam
/// new_config()
/// |> add_ssl(certfile: "certs/cert.crt", keyfile: "certs/cert.key")
/// |> add_handler(fn(req) {
///   case req.path {
///     "/hello" -> gemtext_response([gemtext.heading1("Welcome to Glemini!")])
///     _ -> not_found_response()
///   }
/// })
/// |> start
/// ```
pub fn start(
  config: ServerConfig,
) -> Result(Subject(supervisor.Message), StartError) {
  glisten.handler(fn(_conn) { #(Nil, None) }, fn(req, _state, conn) {
    let assert Packet(req) = req
    let response =
      handle_gemini_request(req, config.request_handler)
      |> response_to_string
    let assert Ok(_) = glisten.send(conn, bytes_builder.from_string(response))
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
/// # Example handler
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

/// Creates a success response, status 20, with a given mimetype and body.
pub fn success_response(mimetype: String, body: String) -> Response {
  SuccessResponse(20, mimetype, body)
}

/// Creates a success response, status 20, with a gemtext body created from a list of gemtext lines.
pub fn gemtext_response(lines: gemtext.Lines) -> Response {
  SuccessResponse(20, "text/gemini", gemtext.lines_to_string(lines))
}

// Input

/// Creates an input response, status 10, with a given prompt.
pub fn input_response(prompt: String) -> Response {
  InputResponse(10, prompt)
}

/// Creates a sensitive input response, status 11, with a given prompt.
pub fn sensitive_input_response(prompt: String) -> Response {
  InputResponse(11, prompt)
}

// Redirects

/// Creates a temporary redirect response, status 30, with a given uri.
pub fn temporary_redirect_response(uri: String) -> Response {
  RedirectResponse(30, uri)
}

/// Creates a permanent redirect response, status 31, with a given uri.
pub fn permanent_redirect_response(uri: String) -> Response {
  RedirectResponse(31, uri)
}

// Temporary errors

/// Creates a temporary failure response, status 40, with a given message.
pub fn temporary_failure_response(message: String) -> Response {
  ErrorResponse(40, Some(message))
}

/// Creates a server unavailable response, status 41.
pub fn server_unavailable_response() -> Response {
  ErrorResponse(41, None)
}

/// Creates a CGI error response, status 42.
pub fn cgi_error_response() -> Response {
  ErrorResponse(42, None)
}

/// Creates a proxy error response, status 43.
pub fn proxy_error_response() -> Response {
  ErrorResponse(43, None)
}

/// Creates a slow down response, status 44.
pub fn slow_down_response() -> Response {
  ErrorResponse(44, None)
}

// Permanent errors

/// Creates a permanent failure response, status 50, with a given message.
pub fn permanent_failure_response(message: String) -> Response {
  ErrorResponse(50, Some(message))
}

/// Creates a not found response, status 51.
pub fn not_found_response() -> Response {
  ErrorResponse(51, None)
}

/// Creates a gone response, status 52.
pub fn gone_response() -> Response {
  ErrorResponse(52, None)
}

/// Creates a proxy request refused response, status 53.
pub fn proxy_request_refused_response() -> Response {
  ErrorResponse(53, None)
}

/// Creates a bad request response, status 59, with a given message.
pub fn bad_request_response(message: String) -> Response {
  ErrorResponse(59, Some(message))
}

// Certificate errors

/// Creates a client certificate required response, status 60, with a given message.
pub fn client_certificate_required_response(message: String) -> Response {
  ErrorResponse(60, Some(message))
}

/// Creates a certificate not authorized response, status 61, with a given message.
pub fn certificate_not_authorized_response(message: String) -> Response {
  ErrorResponse(61, Some(message))
}

/// Creates a certificate not valid response, status 62, with a given message.
pub fn certificate_not_valid_response(message: String) -> Response {
  ErrorResponse(62, Some(message))
}

// Helpers

/// Parse a request and pass it to the handler function.
fn handle_gemini_request(
  req: BitArray,
  handler: fn(Request) -> Response,
) -> Response {
  case parse_request(req) {
    Ok(request) -> handler(request)
    Error(error) -> error_handler(error)
  }
}

fn error_handler(error: GleminiError) -> Response {
  case error {
    RequestParseError -> bad_request_response("Couldn't parse request.")
    RequestSchemeError -> bad_request_response("Bad request scheme.")
    // _ -> temporary_failure_response("Unknown error")
  }
}

/// Parse a request from a BitArray into a Request type.
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

/// Convert a Response to a string.
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
