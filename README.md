# ppx_deriving_comparison

Generates a function that compares value constructors.

```reason
[@deriving comparison]
type foo =
  | A(bool)
  | B
  | C;

let () = {
  assert(foo_is_same_constructor(A(true), A(false)));
  assert(foo_is_same_constructor(B, B));
  assert(! foo_is_same_constructor(B, C));
};
```
