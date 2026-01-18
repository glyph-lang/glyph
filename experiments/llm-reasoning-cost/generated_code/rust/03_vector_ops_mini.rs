```rust
fn sum_positive(nums: Vec<i32>) -> i32 {
    let mut positive_numbers = Vec::new();
    
    for &num in &nums {
        if num > 0 {
            positive_numbers.push(num);
        }
    }
    
    positive_numbers.iter().sum()
}
```