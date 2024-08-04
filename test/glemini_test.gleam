import gleeunit
import gleeunit/should

import glemini/gemtext

pub fn main() {
  gleeunit.main()
}

pub fn gemtext_test() {
  [
    gemtext.heading1("Welcome to Glemini!"),
    gemtext.link_with_caption("/", "Home"),
    gemtext.link("/"),
    gemtext.text("just some text"),
    gemtext.list_item("item 1"),
    gemtext.list_item("item 2"),
    gemtext.list_item("item 3"),
    gemtext.blank_line(),
    gemtext.preformatted("  this is\n    preformatted\n      text"),
    gemtext.preformatted_with_caption("fn gleam() {}", "gleam"),
  ]
  |> gemtext.to_string
  |> should.equal(
    "# Welcome to Glemini!
=> / Home
=> /
just some text
* item 1
* item 2
* item 3

```
  this is
    preformatted
      text
```
```gleam
fn gleam() {}
```",
  )
}
