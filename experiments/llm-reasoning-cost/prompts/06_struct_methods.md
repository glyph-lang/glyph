# Task: Struct Methods and Collections

Write code that defines a point type and operates on a collection of points.

**Requirements:**

1. Define a `Point` type/struct with two fields: `x` (32-bit signed integer) and `y` (32-bit signed integer)

2. Define a method/function `squared_mag` on Point that returns `x * x + y * y` (32-bit signed integer)

3. Define a function `filter_and_sum`:
   - Parameter: a vector/list of Points, and a threshold `max_mag` (32-bit signed integer)
   - Return type: 32-bit signed integer
   - Iterate through the points
   - For each point where `squared_mag() <= max_mag`, add its `squared_mag()` to a running total
   - Return the total

**Example:**
- Points: `[(1, 2), (3, 4), (0, 1)]`
- `squared_mag` values: `[5, 25, 1]`
- With `max_mag = 10`: filtered points are `(1,2)` and `(0,1)` with mags `5` and `1`
- Return: `6`

**Implementation notes:**
- Use the language's struct/class/record type
- Define `squared_mag` as a method on the type (not a standalone function)
- Use vector/list iteration and conditional filtering
