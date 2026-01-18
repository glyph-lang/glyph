```python
def sum_positive(numbers):
    positive_numbers = []
    for number in numbers:
        if number > 0:
            positive_numbers.append(number)
    return sum(positive_numbers)
```