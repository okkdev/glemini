# glemini

[![Package Version](https://img.shields.io/hexpm/v/glemini)](https://hex.pm/packages/glemini)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glemini/)

A Gleam library for building [Gemini](https://geminiprotocol.net/) servers.

## Usage

Add `glemini` to your project:
```sh
gleam add glemini
```

Create self-signed server certificates:
```sh
openssl req -x509 -nodes -newkey rsa:4096 -keyout certs/cert.key -out certs/cert.crt -sha256 -days 365
```

Create the gemini server with a request handler:
```gleam
import gleam/erlang/process
import gleam/io
import glemini.{
  add_handler, add_ssl, gemtext_response, new_config, not_found_response, start,
}
import glemini/gemtext.{heading1, text}

pub fn main() {
  io.println("Starting server!")

  let config =
    new_config()
    |> add_ssl(certfile: "certs/cert.crt", keyfile: "certs/cert.key")
    |> add_handler(fn(request) {
      case request.path {
        "/" ->
          [
            heading1("Welcome to Glemini!"),
            text("This is a simple gemini server library"),
          ]
          |> gemtext_response()
        _ -> not_found_response()
      }
    })

  let assert Ok(_) = start(config)

  io.println("Server running!")
  process.sleep_forever()
}
```

Further documentation can be found at <https://hexdocs.pm/glemini>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```
