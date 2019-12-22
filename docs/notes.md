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

## Printing

No where does it say how booleans should be represented when printed. Decided to go with `yes/no`.

No where does it say how arrays should be represented when printed. Decided to go with `<item1> and <item2> and <item3>`
to match declaration.

## Variable Declaration

Added the ability to omit the type when declaring with a value. The type can be inferred by the value.
ex: `Did you know that Spike's age is 10?`. This often reads better. Unfortunately, this does not work for arrays as 
it's ambiguous if you are using `and` as an operator or an element separator.

`always` denotes deep immutability. That is, you can't re-assign a variable declared as `always` nor modify it. ex: 
arrays will throw a runtime exception if you attempt to do so.

## Variable Assignment

It's unclear what's allowed on the right side of a variable assignment. I went with an expression as there should be no
ambiguity parsing that, much like printing. ex: `Spike's age is now 10 plus 1`.

## Comparision

Unlike the reference, values are _not_ converted to strings if their types differ. It is a compiler error instead. You
can do this explicitly with concatenation ex: `"1" is 1 ""`.

The reference says that for arrays, ex: the less than "the operator will return true if the length of the left is smaller 
than that of the right, or if they are the same length and the sum of the values in the left array is smaller than that
of the right." There's a couple of problems with this. First, this method of comparison is kinda useless, it relies on
the writer to know ahead of time if the arrays will always be the same size or not because the behavior is completely
different based on that. Second, "the sum of all the values" only works for arrays of numbers, arrays of other types is
not defined. Instead, I decided to _only_ compare arrays based on length as this is well-defined in all cases.

## Calling

You can use arbitrary expressions when calling another paragraph. However, using the infix `and`
operator is ambiguous in this case so it's disabled in this context. You can either: use the prefix
version, use an alternate name, or assign to a variable and use that instead. ex:
`I remembered to give my friend using Appljack's apples plus 1 and add Rarity's dresses and 2.`

## Scoping

Variable scoping isn't described at all in the reference. I went with lexical scoped variables. You may also redeclare a 
variable at any time in the same scope and it'll shadow the previous one.

## Default Values

Since it's not well defined in the reference, the default values for various types are as follows:

| Type    | Default Value |
|---------|---------------|
| string  | nothing       |
| number  | 0             |
| boolean | false         |
| array   | nothing       |

## Precedences 

Since it's not well defined in the reference, precedence order is (from tightest to loosest):
- Operators
- Indexing/Calls
- Concatenation

## Literals

Declaring literals is vague in the reference, only giving examples. I took them to mean that you can optionally include
the type before them, ex: `a number 10` or `the logic no`.

## Input

The reference grammar includes the option `the next <type>` but has no examples. I decided to allow this phrase with the
type matching how you declare a variable (without `a`/`the`). ex: `I asked Twilight the next number.` By adding this you 
are only being explicit, it does not change the behavior of the report. The type must match the type the variable is
declared as, or it is a compile error.

For booleans, any value that can be used to declare one is valid input for parsing. ex: `yes`, `wrong`, `correct`, `false`.

A prompt can be any expression, it will be evaluated and then printed before asking for input. ex
`I asked your favorite number "Is you favorite number " guess " "?`. You may also have both `the next <type>` and a 
prompt, if so, the former proceeds the later. ex: `I asked Applejack the next number "How many apples do you have?"`.

## Arrays

The reference shows arrays being indexed directly, ex: `cake 2`. Unfortunately, this is ambiguous as `cake 2` is a valid
identifier. Instead I went with a different syntax:

```
<identifier><whitespace><at><whitespace><expression>
```

You can use `at` operator. ex: `the cake at the cake index plus 1`, `the cake at the cake index plus 1 is now "apple"`.

## For Loops

The reference is unclear if the range for a for loop is inclusive or exclusive. I went with inclusive, ex: 
`For every number n from 1 to 3` will loop 3 times.
