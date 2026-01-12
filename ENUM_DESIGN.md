===============================================================================
4. ALGEBRAIC_TYPES_DESIGN.txt — Fat Enums / ADTs
===============================================================================

Purpose
-------
Introduce algebraic data types via Rust-like “fat enums”:
- expressive sum types
- predictable layout
- exhaustiveness-checked match
- Option<T> / Result<T,E>
- clean MIR + LLVM lowering

Syntax
------
enum Option<T> {
  None
  Some(T)
}

enum Result<T,E> {
  Ok(T)
  Err(E)
}

Construction
------------
let x = Some(3)
let y = None
let r = Ok("hi")

Type rules
----------
- Enums are nominal
- Constructors are typed functions
- None requires contextual type if ambiguous

Layout (v0)
-----------
struct {
  tag: u8/u16/u32
  payload: [max variant size]
}

No ABI stability across compilations.

Pattern matching TODO! -- We don't have patter matching yet
----------------
match x {
  None    => 0
  Some(v) => v
}

Rules:
- exhaustive unless `_`
- expression-valued
- all arms same type

MIR lowering
------------
- evaluate scrutinee once
- switch on tag
- extract payload
- phi join

Move semantics 
--------------
- payload moved by default
- matching on &Option<T> borrows payload

Std enums
---------
Option<T>
Result<T,E>

? operator. TODO!
----------
- valid on Result<T,E>
- early-return Err(E)
- lowers to tag switch

Diagnostics
-----------
- non-exhaustive match
- unreachable arms
- use-after-move
- ambiguous None