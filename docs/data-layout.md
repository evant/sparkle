# Data Layout

The different data types are layed out in memory as follows:

## Number

This is a 64 bit floating point value. 

## Boolean

This is a 32 bit value, where 0 is false, anything else is true.

## String

Strings are length-prefixed. They also include a reference count so they can be freed when the reference drops to 0. For
constant strings this ref count is -1 to denote they are not allocated on the heap so they do not need to be freed.

```
[ref count, size, contents]
```

## Array

An array is represented with a header and it's contents. The header includes a size and capacity to aid with bounds 
checking and growing the array when necessary.

```
[size, contents pointer, capacity] -- [contents]
```

When an array is declared with `always` it cannot be changed. Therefore, it's representation can be optimized to

```
[size, contents pointer, contents]
```

This saves the overhead of the capacity and more importantly the extra allocation for it's contents. Detecting that an
array is constant can be done at runtime by checking `contents pointer - array pointer = 16`, as it's impossible to
allocate the contents at this location otherwise.
