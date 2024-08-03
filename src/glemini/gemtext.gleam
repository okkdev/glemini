import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type Lines =
  List(Line)

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

pub fn to_string(lines: Lines) {
  list.map(lines, fn(line) {
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
  })
  |> string.join("\n")
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