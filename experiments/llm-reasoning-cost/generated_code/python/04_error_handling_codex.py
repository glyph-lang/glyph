from typing import Union


def safe_divide(numerator: int, denominator: int) -> Union[int, str]:
    if denominator == 0:
        return "division by zero"
    return numerator // denominator