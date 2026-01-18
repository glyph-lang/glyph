def write_greeting(filename):
    try:
        with open(filename, "w", encoding="utf-8") as f:
            f.write("Hello from the program!")
        return True, None
    except Exception as e:
        return False, e