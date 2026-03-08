class Point:
    def __init__(self, x: int, y: int):
        self.x = x
        self.y = y

    def squared_mag(self) -> int:
        return self.x * self.x + self.y * self.y

def filter_and_sum(points: list[Point], max_mag: int) -> int:
    total = 0
    for point in points:
        if point.squared_mag() <= max_mag:
            total += point.squared_mag()
    return total
