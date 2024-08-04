//// A helper module for generating gemtext.
//// # Example:
//// ```gleam
//// [
////   heading1("Welcome to Glemini!"),
////   text("This is a simple gemini server library"),
////   link_with_caption("gemini://geminiprotocol.net", "Gemini Website"),
//// ]
//// |> lines_to_string
//// ```
//// ## Results in:
//// ```gemtext
//// # Welcome to Glemini!
//// This is a simple gemini server library
//// => gemini://geminiprotocol.net Gemini Website
//// ```

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// Type alias for a list of gemtext lines.
pub type Lines =
  List(Line)

/// Type representing gemtext lines.
pub opaque type Line {
  Text(text: String)
  Link(uri: String, caption: Option(String))
  Heading1(text: String)
  Heading2(text: String)
  Heading3(text: String)
  ListItem(text: String)
  Qoute(text: String)
  Preformatted(text: String, caption: Option(String))
}

/// Convert a list of gemtext lines into a string.
pub fn lines_to_string(lines: Lines) -> String {
  list.map(lines, line_to_string)
  |> string.join("\n")
}

/// Convert a gemtext line into a string.
pub fn line_to_string(line: Line) -> String {
  case line {
    Text(text:) -> text
    Link(uri:, caption: Some(caption)) -> "=> " <> uri <> " " <> caption
    Link(uri:, caption: None) -> "=> " <> uri
    Heading1(text:) -> "# " <> text
    Heading2(text:) -> "## " <> text
    Heading3(text:) -> "### " <> text
    ListItem(text:) -> "* " <> text
    Qoute(text:) -> "> " <> text
    Preformatted(text:, caption: Some(caption)) ->
      "```" <> caption <> "\n" <> text <> "\n```"
    Preformatted(text:, caption: None) -> "```\n" <> text <> "\n```"
  }
}

pub fn text(text: String) -> Line {
  Text(text:)
}

pub fn link(uri: String) -> Line {
  Link(uri:, caption: None)
}

pub fn link_with_caption(uri: String, text: String) -> Line {
  Link(uri:, caption: Some(text))
}

pub fn heading1(text: String) -> Line {
  Heading1(text:)
}

pub fn heading2(text: String) -> Line {
  Heading2(text:)
}

pub fn heading3(text: String) -> Line {
  Heading3(text:)
}

pub fn list_item(text: String) -> Line {
  ListItem(text:)
}

pub fn qoute(text: String) -> Line {
  Qoute(text:)
}

pub fn preformatted(text: String) -> Line {
  Preformatted(text:, caption: None)
}

pub fn preformatted_with_caption(text: String, caption: String) -> Line {
  Preformatted(text:, caption: Some(caption))
}

pub fn blank_line() -> Line {
  Text(text: "")
}
