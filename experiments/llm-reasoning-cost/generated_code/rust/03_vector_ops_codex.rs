pub fn sum_positive(nums: Vec<i32>) -> i32 {
    let mut positives: Vec<i32> = Vec::new();
    for n in nums {
        if n > 0 {
            positives.push(n);
        }
    }
    positives.iter().sum()
}