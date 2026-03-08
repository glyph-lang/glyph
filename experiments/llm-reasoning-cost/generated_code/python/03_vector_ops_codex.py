def sum_positive(numbers: list[int]) -> int:
    positives: list[int] = []
    for n in numbers:
        if n > 0:
            positives.append(n)
    return sum(positives)