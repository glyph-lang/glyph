#[derive(Debug)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn squared_mag(&self) -> i32 {
        self.x * self.x + self.y * self.y
    }
}

fn filter_and_sum(points: Vec<Point>, max_mag: i32) -> i32 {
    let mut total = 0;
    for point in points {
        let mag = point.squared_mag();
        if mag <= max_mag {
            total += mag;
        }
    }
    total
}

fn main() {
    let points = vec![
        Point { x: 1, y: 2 },
        Point { x: 3, y: 4 },
        Point { x: 0, y: 1 },
    ];
    let max_mag = 10;
    let result = filter_and_sum(points, max_mag);
    println!("Total: {}", result); // Output: Total: 6
}
