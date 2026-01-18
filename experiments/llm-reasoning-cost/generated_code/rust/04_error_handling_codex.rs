pub fn safe_divide(numerator: i32, denominator: i32) -> Result<i32, String> {
    if denominator == 0 {
        Err("division by zero".to_string())
    } else {
        Ok(numerator / denominator)
    }
}