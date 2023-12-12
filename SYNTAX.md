# Language Syntax

The Apollo language has two base language constructs that compose all of the `AST`: statements and expressions.

All of the kinds of statements and expressions can be seen below.

## Statements

These are the main building blocks. They usually come with more statements or expressions associated with them.

### Variable Declaration

Variables can be declared using the `var` keyword:
```apollo
var foo = 11;
```

You can also specify a specific type when declaring it:
```apollo
var foo: i32 = 43;
```

### Assignment

Variables can have their value reassigned with this statement:

```apollo
foo = 550;
```

### Function Definition

Functions can be defined using the `function` keyword:

```apollo
function add(x: i32, y: i32, z: i32): i32 { ... }
```

You can also specify C-style `va_list` arguments by letting an ellipsis (...) at the end of the argument specification:

```apollo
function format(format: string, ...) { ... }
```

### Function Declaration

The declaration of a function is made with the `extern` keyword, where you can specify the calling convention:

```apollo
extern "C" function printf(format: string, ...): i32;
```

By default, all functions use the "C" calling convention.

To list all of the currently available calling conventions, you can run the following command:

```bash
apolloc print calling-conventions
```

### If-Else Conditionals

This is declared using the keyword `if` and optionally the keyword `else`.

Here there is a couple of examples, one with and one without `else`:

```apollo
if (myNumber == 43)
    doSomething();
```

```apollo
if (client->isAdmin()) {
    clientInterface::sendMessage("Permission granted");
    client::setPermissionGroup(0);
} else
    clientInterface::refuseRequest();
```

### While Loops

A while loop is essentially a block that executes while the specified condition is true. It is specified using the `while` keyword:

```apollo
// runs indefinitely
while (true)
    printf("Hello, world!\n");
```

### Do-While Loops

A do-while loop is a while loop that executes its body at least once. It is specified using the `do` and `while` keywords:

```apollo
do {
    age = age + 1;
} while (age != 80)
```

### Call

A call is essentially a call where the return value is discarded:

```apollo
doSomething();
```

### Return

Returns a value from a function.

```apollo
function main(): i32 {
    return 0;
}
```

### Return Nothing

Returns nothing, `none` from a function.

```apollo
function example(): none {
    // ... some logic here
    return;
}
```

### Block

A statement that is composed of various other statements.

```apollo
{
    // ... other statements here
    doSomething();
    var b = 33;
}
```

### Struct Definition

Defines a struct in the local scope.

```apollo
struct Person {
    age: i32,
}
```

### Struct Field Assignment

Assigns to a struct's field.

```apollo
var person: Person = Person { age: 13 };
person.age = 14;
```

### Struct Field Indirect Assignment

Assign to a pointer-to-struct field.

```apollo
var person: Person = Person { age: 13 };
var person_ptr: *Person = &person;
person_ptr->age = 14;
```

### Struct Method Definition

Define a struct method using the `method` keyword:

```apollo
struct Person {
    age: i32,
}

method Person::get_age(this) {
    return this->age;
}
```

### Struct Method Call

Call a struct's method using the `::` operator:

```apollo
struct Person {
    age: i32,
}

method Person::get_age(this) {
    return this->age;
}

var person = Person { age: 40 };
var age = person::get_age();
```

## Expressions

Expressions are general things that have a value associated with it.

### List

A list of values:

```apollo
[1, 2, 3, 4, 5]
```

### Unchecked Cast

Cast any value to any type. The source type and the destination type's sizes must be the same.

```apollo
unchecked_cast(u64, -5i64)
```

### Struct Field Access

Access a struct field.

```apollo
struct.field
```

### Struct Field Indirect Access

Access a field of a pointer to a struct.

```apollo
struct_ptr->field
```

### Dereference

Dereferences a pointer to the value it points to.

```apollo
*pointer
```

### Variable

Gets the value stored at the variable.

```apollo
variable_name
```

### String

A string expression.

```apollo
"Hello, world!\n"
```

### Call

A call to a function. Captures its return value.

```apollo
add(11, 9)
```

### Integers

An integer expression will automatically default to `i32`, but its type can be specified by passing it after the number itself, like this:

```apollo
43u64
```

This can be done for any of the integer types.

### Binary Operators

An expression composed of a left operand, right operand and operator.

Here are some examples:

Addition:
```apollo
4 + 4
```

Subtraction:
```apollo
16 - 8
```

Multiplication:
```apollo
4 * 4
```

Division
```apollo
5 / 2
```

Equal
```apollo
5 == 5
```

Not Equal
```apollo
5 != 8
```

Less Than
```apollo
9 < 5
```

Less Than Or Equal
```apollo
5 <= 2
```

Greater Than
```apollo
5 > 6
```

Greater Than Or Equal
```apollo
1 >= 5
```

AND
```apollo
4 && 55
```

OR
```apollo
0 || 1
```

XOR
```apollo
1 ^ 1
```

### Type Cast

Perform a safe conversion between primitives.

```apollo
&variable as opaqueptr
```

### Struct Instantiate

Instantiate a struct by specifying all of its fields.

```apollo
Person { age: 4i32 }
```

### Null Pointer

A pointer to the address zero.

```apollo
null
```

### Struct Method Call

Calls a method of the struct and captures its value.

```apollo
person::get_age()
```

## Types

There are some kinds of types: primitives, which are subdivided into integers, floating point numbers, strings, booleans, pointers, and none, structural types (like the Person struct) and slices (dynamically-sized arrays).

There are listed how to spell out each of these types and how many of them are.

### Primitives

#### Integers

The integer types are, for unsigned types:
* `u8`;
* `u16`;
* `u32`; and
* `u64`.

And for signed types:
* `i8`;
* `i16`;
* `i32`; and
* `i64`.

#### Floating Point Numbers

There are two types of floating point numbers:
* `f32`; and
* `f64`.

#### String

A string is a pointer to an array of null-terminated characters, typically constant. But they can also be allocated dynamically. It has only one variant: `string`.

#### Booleans

A boolean is a value that can be either `true` (1) or `false` (0). It has only one variant, being `bool`.

#### Pointers

Pointers are subdivided into two categories:

##### Typed Pointers

Typed pointers are pointers that have an explicit type attached to them, like:
* `*i32`;
* `*i64`;
* `*Person`;
*, etc.

They are spelled by a star followed by the type they point to.
These pointer types can be dereferenced.

##### Opaque Pointers

Opaque pointer types are raw addresses that don't carry with them any metadata about the underlying type.
It is spelled as `opaqueptr`.

- These pointer types cannot be dereferenced because the type it points to is unknown.

- To dereference it, one must cast the opaque pointer to a pointer with an explicit type before dereferencing it. The compiler will always warn when casting from an opaque pointer to a typed one because it is extremely unsafe.

#### Structural Types

Structural types are user-defined structures. They are locals in memory that have fields in a specific order that can be accessed and changed.

They can be spelled out by just the struct's name, such as `Person`, `Client` etc.

#### Slices

Slices are heap-stored arrays with a dynamic size. They can be spelled by `slice` followed by the type they store, like `slice i32`, `slice Person`, and so on.

