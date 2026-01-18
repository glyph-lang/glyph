# Task: File I/O

Write a function that writes text to a file with error handling.

**Requirements:**
- Function name: `write_greeting`
- Parameter: `filename` (string/path)
- Return type: Result type indicating success or error
- Create/open a file at the given path
- Write the text: `Hello from the program!`
- Close the file properly
- Return success or propagate errors

**Example:**
- `write_greeting("output.txt")` → creates file with "Hello from the program!"
- `write_greeting("/invalid/path")` → returns error

**Implementation notes:**
- Use standard library file I/O
- Handle errors properly (use Result type and `?` operator if available)
- Ensure file is closed even on error
