# Set up
set.seed(122)
histdata <- rnorm(500)

# input
num_observations <- 50

# process
data <- histdata[seq_len(num_observations)]

# results
hist(data)
