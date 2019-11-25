# Rust impl of FiM++

- Ahead-of-time compiled
- [cranelift](https://github.com/bytecodealliance/cranelift) backend

source -> ast -> backend

References:
- https://fimpp.fandom.com
- https://docs.google.com/document/d/1gU-ZROmZu0Xitw_pfC1ktCDvJH5rM85TxxQf5pg_xmg/edit
- https://github.com/avian2/fimpp


# Design decisions

These are decisions made where places where unclear in the references.

## Arithmetic Order of Operations.

It says
> Arithmetic operators can be chained together like so:
  <value><operator><value><operator><value>...
  ORDER OF OPERATIONS IS NOT NECESSARILY GUARANTEED.

As fun as it would be to randomly decide order of operations, I went with left-to-right to match boolean operations.
ex: `1 plus 1 times 2` gives `4`.

## Printing Booleans

No where does it say how booleans should be represented when printed. Decided to go with `yes/no`.

## Variable Assignment

It's unclear what's allowed on the right side of a variable assignment. I went with an expression as there should be no
ambiguity parsing that, much like printing. ex: `Spike's age is now 10 plus 1`.
