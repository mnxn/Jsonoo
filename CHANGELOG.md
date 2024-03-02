## Unreleased

- Use `json Js.t` type for compatibility with Js_of_ocaml bindings.
- Change internal representation to `Ojs.t` and make `Jsonoo.t` abstract.
- Add a `Make` functor to create modules with a custom type representation.

## v0.2.1

- Reduce the size of .js binaries produced by using this library by not linking
  the ppx toolchain.

## v0.2.0

- Allow specifying spaces for `stringify`

## v0.1.0

- Initial release
