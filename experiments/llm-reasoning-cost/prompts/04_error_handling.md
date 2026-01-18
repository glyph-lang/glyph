# Task: Error Handling with Result Type

Write a function that divides two integers with error handling.

**Requirements:**
- Function name: `safe_divide`
- Parameters: `numerator` and `denominator` (both 32-bit signed integers)
- Return type: Result type containing either the quotient or an error
- If denominator is 0, return an error
- Otherwise, return the quotient (integer division)
- Use the language's idiomatic error handling mechanism

**Example:**
- `safe_divide(10, 2)` → `Ok(5)`
- `safe_divide(7, 3)` → `Ok(2)`
- `safe_divide(5, 0)` → `Err("division by zero")`

**Implementation notes:**
- Use Result/Option type if available
- Use `?` operator or equivalent for error propagation if applicable
