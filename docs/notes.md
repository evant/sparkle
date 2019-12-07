# Rust impl of FiM++

- Ahead-of-time compiled
- [cranelift](https://github.com/bytecodealliance/cranelift) backend

source -> ast -> backend

References:
- https://fimpp.fandom.com
- https://docs.google.com/document/d/1gU-ZROmZu0Xitw_pfC1ktCDvJH5rM85TxxQf5pg_xmg/edit
- https://github.com/avian2/fimpp

# Why Sparkle?

It was the codename of official 1.0 version, I like it better than FiM++, it's
easier to say.

# Design decisions

These are decisions made where places where unclear in the references.

## Keywords

Comments and newlines are allowed in keywords.

## Arithmetic Order of Operations.

It says
> Arithmetic operators can be chained together like so:
  <value><operator><value><operator><value>...
  ORDER OF OPERATIONS IS NOT NECESSARILY GUARANTEED.

As fun as it would be to randomly decide order of operations, I went with left-to-right to match boolean operations.
ex: `1 plus 1 times 2` gives `4`.

## Printing Booleans

No where does it say how booleans should be represented when printed. Decided to go with `yes/no`.

## Variable Declaration

Added the ability to omit the type when declaring with a value. The type can be inferred by the value.
ex: `Did you know that Spike's age is 10?`. This often reads better.

## Variable Assignment

It's unclear what's allowed on the right side of a variable assignment. I went with an expression as there should be no
ambiguity parsing that, much like printing. ex: `Spike's age is now 10 plus 1`.

## Comparision

Unlike the reference, values are _not_ converted to strings if their types differ. It is a compiler error instead. You
can do this explicilty with concatination ex: `"1" is 1 ""`.

## Calling

You can use arbitrary expressions when calling another paragraph. However, using the infix `and`
operator is ambiguous in this case so it's disabled in this context. You can either: use the prefix
version, use an alternate name, or assign to a variable and use that instead. ex:
`I remembered to give my friend Appljack's apples plus 1 and add Rarity's dresses and 2.`

## Scoping

Variable scoping isn't described at all in the reference. I went with lexical scoped variables. You may also redeclare a 
variable at any time in the same scope and it'll shadow the previous one.
