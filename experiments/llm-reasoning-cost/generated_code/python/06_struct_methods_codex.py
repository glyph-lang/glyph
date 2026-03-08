from dataclasses import dataclass
from typing import List


@dataclass
class Point:
    x: int
    y: int

    def squared_mag(self) -> int:
        return self.x * self.x + self.y * self.y


def filter_and_sum(points: List[Point], max_mag: int) -> int:
    total = 0
    for p in points:
        mag = p.squared_mag()
        if mag <= max_mag:
            total += mag
    return total