# Shared<T> Implementation Design

## Overview

Shared<T> is a reference-counted smart pointer that allows multiple owners of the same data. It automatically tracks ownership count and frees memory when the last owner is dropped. This enables safe memory sharing without garbage collection.

## Syntax

### Creating a Shared Pointer

```glyph
let s = Shared::new(42)
```

The `Shared::new()` function allocates memory on the heap with a reference count initialized to 1.

### Cloning a Shared Pointer

```glyph
let s1 = Shared::new(7)
let s2 = s1.clone()  // Increments refcount, both point to same data
```

Unlike `Own<T>` which has move-only semantics, `Shared<T>` can be cloned. Each clone increments the reference count.

### Automatic Cleanup

```glyph
fn main() -> i32 {
  let s1 = Shared::new(99)
  let s2 = s1.clone()  // refcount = 2
  // s2 drops here, refcount decrements to 1
  ret 0
}
// s1 drops here, refcount decrements to 0, memory freed
```

When a `Shared<T>` goes out of scope, its reference count decrements. When the count reaches zero, the memory is freed.

## Memory Layout

Each `Shared<T>` allocation contains:

```
+-------------------+
| refcount: usize   | <- offset 0 (8 bytes on 64-bit)
+-------------------+
| data: T           | <- offset 8
+-------------------+
```

This keeps the refcount and data together in a single allocation for cache locality.

## Semantics

### Copy Semantics vs Move Semantics

**Shared<T>**: Copy semantics (can be used multiple times)
```glyph
let s = Shared::new(42)
let a = s
let b = s  // OK - Shared is copyable
```

**Own<T>**: Move semantics (consumed on first use)
```glyph
let o = Own::new(42)
let a = o
let b = o  // ERROR - use of moved value
```

### Reference Counting

- **Creation**: `Shared::new(value)` allocates with refcount = 1
- **Clone**: `.clone()` increments refcount
- **Drop**: Decrements refcount, frees when count reaches 0

### Single-Threaded Design

Shared<T> uses non-atomic reference counting for performance. It is **not thread-safe**. For multi-threaded code, a future `Arc<T>` (atomic reference counting) would be needed.

## Implementation Details

### Type System

Shared<T> is represented as `Type::Shared(Box<Type>)` in the compiler's type system, with helper methods:
- `is_shared() -> bool`
- `shared_inner_type() -> Option<&Type>`

### MIR Lowering

Two MIR rvalues support Shared operations:
- `SharedNew { value: MirValue, elem_type: Type }` - allocation + refcount init
- `SharedClone { base: LocalId, elem_type: Type }` - refcount increment

### LLVM Codegen

**SharedNew** generates:
1. Create struct type `[i64, T]`
2. Call `malloc` with struct size
3. Cast to typed pointer
4. Initialize refcount to 1
5. Store data value
6. Return pointer

**SharedClone** generates:
1. Load Shared<T> pointer
2. GEP to refcount field
3. Load current refcount
4. Add 1
5. Store new refcount
6. Return same pointer

**Drop** generates:
1. Load pointer and null-check
2. GEP to refcount field
3. Load and decrement refcount
4. Compare with zero
5. If zero: call `free` and set slot to null
6. If non-zero: continue

## Examples

### Example 1: Basic Shared Pointer

```glyph
fn main() -> i32 {
  let s = Shared::new(42)
  ret 0
}
```

**What happens**:
- Malloc allocates 16 bytes (8 for refcount + 8 for i32)
- Refcount initialized to 1
- Data (42) stored
- At function end, refcount decremented to 0
- Memory freed

### Example 2: Clone and Multiple Owners

```glyph
fn main() -> i32 {
  let s1 = Shared::new(7)
  let s2 = s1.clone()
  let s3 = s2.clone()
  ret 0
}
```

**Refcount timeline**:
- After `Shared::new(7)`: refcount = 1
- After `s1.clone()`: refcount = 2
- After `s2.clone()`: refcount = 3
- End of scope, s3 drops: refcount = 2
- s2 drops: refcount = 1
- s1 drops: refcount = 0, memory freed

### Example 3: Shared Struct

```glyph
struct Point { x: i32, y: i32 }

fn main() -> i32 {
  let s = Shared::new(Point { x: 10, y: 20 })
  let s2 = s.clone()
  ret 0
}
```

**Memory layout**:
```
+-------------------+
| refcount: 1       | (becomes 2 after clone)
+-------------------+
| Point {           |
|   x: 10           |
|   y: 20           |
| }                 |
+-------------------+
```

## Comparison with Other Pointer Types

| Type | Ownership | Semantics | Cleanup |
|------|-----------|-----------|---------|
| `Own<T>` | Exclusive | Move-only | Immediate on drop |
| `Shared<T>` | Shared (refcounted) | Clone-able | When refcount reaches 0 |
| `RawPtr<T>` | None (unsafe) | Copy | Manual |
| `&T` / `&mut T` | Borrowed | Copy | N/A (stack-based) |

## Testing

The implementation includes comprehensive test coverage:

### MIR Tests (4 fixtures)
- `shared_basic.glyph` - Basic allocation and drop
- `shared_clone.glyph` - Single clone operation
- `shared_multiple.glyph` - Multiple clones
- `shared_struct.glyph` - Shared pointer to struct

### Codegen Test
- Verifies LLVM IR contains malloc, refcount init to 1, increment (add), decrement (sub), conditional free

### Test Results
All 110+ workspace tests passing, including 4 new MIR snapshot tests and 1 codegen test.

## Current Limitations

1. **No thread safety** - Uses non-atomic operations (not safe across threads)
2. **No weak references** - Cannot create weak pointers to break cycles
3. **No custom drop** - No support for custom cleanup logic
4. **Immediate decrement** - Refcount decremented even if not last owner (potential optimization)

## Future Work

- `Arc<T>` (atomic reference counted) for thread-safe sharing
- `Weak<T>` weak references to break reference cycles
- Custom `Drop` implementations
- Optimization: defer refcount decrements
- Interior mutability: `RefCell<T>`, `Cell<T>`

## Performance

**Advantages**:
- Single allocation (refcount + data together)
- Non-atomic operations (faster than `Arc<T>`)
- Cache-friendly layout

**Costs**:
- Clone increments refcount (cheap but not free)
- Drop checks and potentially frees (conditional branch + free call)
- Slightly larger than raw pointer (8-byte refcount overhead)

## Safety

Shared<T> is safe because:
- Reference count prevents use-after-free
- Automatic drop prevents memory leaks
- Null checks prevent double-free
- Copy semantics prevent use-after-move errors
- Type system ensures inner type outlives the pointer
