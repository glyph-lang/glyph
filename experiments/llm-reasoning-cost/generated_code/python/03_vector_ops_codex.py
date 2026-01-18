def sum_positive(nums):
    positives = []
    for n in nums:
        if n > 0:
            positives.append(n)
    return sum(positives)