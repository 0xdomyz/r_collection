# Sample data frame
df <- data.frame(A = c(0, 1, 2, 0, 3), B = c(4, 0, 0, 5, 6), C = c(0, 0, 7, 8, 9))

# Count zeros and non-zeros in each column
zero_counts <- sapply(df, function(x) sum(x == 0))
non_zero_counts <- sapply(df, function(x) sum(x != 0))

# Combine results into a new data frame
result <- data.frame(Column = names(df),
                     Zero_Count = zero_counts,
                     Non_Zero_Count = non_zero_counts)

# Print result
print(result)