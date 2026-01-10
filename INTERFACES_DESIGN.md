# Interface Implementation Design

## Overview

Interfaces define method contracts that structs can implement, similar to Rust traits or Java interfaces. They enable compile-time polymorphism without runtime overhead.

## Syntax

### Defining an Interface

```glyph
interface Drawable {
  fn draw(self: &Point) -> i32;
  fn erase(self: &Point);
}
```

Interfaces declare method signatures without implementations. Each method specifies its receiver type (`self`, `&self`, or `&mut self`), parameters, and optional return type.

### Implementing an Interface

Glyph provides two implementation styles:

**1. Inline Implementation (within struct definition):**

```glyph
struct Point {
  x: i32
  y: i32

  impl Drawable {
    fn draw(self: &Point) -> i32 {
      ret self.x + self.y
    }
  }
}
```

**2. Separate Implementation (outside struct definition):**

```glyph
impl Drawable for Point {
  fn draw(self: &Point) -> i32 {
    ret self.x + self.y
  }
}
```

Both styles are equivalent. Use inline for small, tightly coupled implementations, and separate for larger implementations or when organizing code across files.

### Calling Interface Methods

Interface methods are called using the same syntax as inherent methods:

```glyph
fn main() -> i32 {
  let p = Point { x: 10, y: 20 }
  ret p.draw()  // Calls Point::Drawable::draw
}
```

## Method Resolution

When a method is called (e.g., `obj.method()`), the compiler resolves it in this order:

1. **Built-in methods** - checked first (`.len()` for arrays, `.into_raw()` for owned pointers)
2. **Inherent methods** - methods defined directly on the struct
3. **Interface methods** - methods from implemented interfaces
4. **Error** - if no match is found or multiple interfaces provide the same method (ambiguity)

### Auto-Borrowing

When a method expects `&self` or `&mut self`, but the receiver is a by-value expression, the compiler automatically borrows it:

```glyph
let p = Point { x: 1, y: 2 }
p.draw()  // Automatically becomes (&p).draw()
```

This convenience feature matches Rust's auto-borrowing behavior.

## Implementation Details

### Symbol Mangling

To avoid naming conflicts, the compiler mangles method names based on their origin:

- **Inherent methods**: `Struct::method_name`
  - Example: `Point::norm`
- **Interface methods**: `Struct::Interface::method_name`
  - Example: `Point::Drawable::draw`

This allows structs to have methods with the same name from different interfaces or as inherent methods.

### LLVM Codegen

Interface methods compile to regular functions with mangled names. There are **no vtables** or **runtime dispatch** - all method calls are resolved at compile time and emit direct LLVM `call` instructions.

Example LLVM IR (simplified):

```llvm
define i32 @"Point::Drawable::draw"(ptr %self) {
  ; method body
}

define i32 @main() {
  %result = call i32 @"Point::Drawable::draw"(ptr %p_ref)
  ret i32 %result
}
```

Note that LLVM quotes function names containing special characters like `::`.

## Examples

### Example 1: Basic Interface

```glyph
interface Drawable {
  fn draw(self: &Point) -> i32;
}

struct Point {
  x: i32
  y: i32

  impl Drawable {
    fn draw(self: &Point) -> i32 {
      ret self.x + self.y
    }
  }
}

fn main() -> i32 {
  let p = Point { x: 10, y: 20 }
  ret p.draw()  // Returns 30
}
```

### Example 2: Multiple Interfaces

```glyph
interface Drawable {
  fn draw(self: &Shape) -> i32;
}

interface Serializable {
  fn serialize(self: &Shape) -> i32;
}

struct Shape {
  sides: i32

  impl Drawable {
    fn draw(self: &Shape) -> i32 {
      ret self.sides * 10
    }
  }

  impl Serializable {
    fn serialize(self: &Shape) -> i32 {
      ret self.sides * 100
    }
  }
}

fn main() -> i32 {
  let s = Shape { sides: 4 }
  ret s.draw() + s.serialize()  // Returns 40 + 400 = 440
}
```

### Example 3: Mixed Inherent and Interface Methods

```glyph
interface Drawable {
  fn draw(self: &Point) -> i32;
}

struct Point {
  x: i32
  y: i32

  fn norm(self: &Point) -> i32 {
    ret self.x * self.x + self.y * self.y
  }

  impl Drawable {
    fn draw(self: &Point) -> i32 {
      ret self.x + self.y
    }
  }
}

fn main() -> i32 {
  let p = Point { x: 3, y: 4 }
  ret p.norm() + p.draw()  // Returns 25 + 7 = 32
}
```

## Testing

The interface implementation includes comprehensive test coverage:

### Parser Tests
- Interface definition syntax parsing
- Inline and separate `impl` block parsing
- Method call expression parsing

### Resolver Tests
- Interface signature validation
- Missing method detection
- Duplicate method detection
- Signature matching (param count, types, return type)
- Unknown interface detection

### MIR Tests
- Method call lowering to `Rvalue::Call`
- Auto-borrowing transformation
- Type inference for receivers
- Symbol mangling

### Codegen Tests
- LLVM IR verification for interface method calls
- Mangled function name presence in IR
- Correct receiver passing (by-value vs by-reference)
- Multiple interface dispatch
- Mixed inherent + interface methods

## Current Limitations

1. **No generic interfaces** - interfaces cannot be parameterized (e.g., `interface Drawable<T>`)
2. **No default implementations** - all interface methods must be implemented by each struct
3. **No associated types** - interfaces cannot declare associated type members
4. **Manual ambiguity resolution** - if multiple interfaces provide the same method, the compiler errors (no trait disambiguation syntax yet)

## Future Work

- Generic interfaces with type parameters
- Default method implementations
- Associated types and constants
- Trait bounds and where clauses
- Trait objects for dynamic dispatch
- Explicit disambiguation syntax for ambiguous method calls
