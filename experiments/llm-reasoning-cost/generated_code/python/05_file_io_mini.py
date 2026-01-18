```python
def write_greeting(filename: str) -> str:
    try:
        with open(filename, 'w') as file:
            file.write("Hello from the program!")
        return "Success"
    except Exception as e:
        return f"Error: {str(e)}"
```