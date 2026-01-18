```python
def safe_divide(numerator: int, denominator: int) -> 'Result':
    if denominator == 0:
        return Err("division by zero")
    return Ok(numerator // denominator)

class Ok:
    def __init__(self, value):
        self.value = value

class Err:
    def __init__(self, error):
        self.error = error

# Example usage:
# result1 = safe_divide(10, 2)  # Ok(5)
# result2 = safe_divide(7, 3)   # Ok(2)
# result3 = safe_divide(5, 0)   # Err("division by zero")
```