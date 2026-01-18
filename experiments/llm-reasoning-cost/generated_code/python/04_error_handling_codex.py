from dataclasses import dataclass
from typing import Union

@dataclass(frozen=True)
class Ok:
    value: int

@dataclass(frozen=True)
class Err:
    error: str

Result = Union[Ok, Err]

def safe_divide(numerator: int, denominator: int) -> Result:
    if denominator == 0:
        return Err("division by zero")
    return Ok(numerator // denominator)