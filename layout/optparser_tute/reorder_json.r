# Rscript reorder_json.R --filename settings.json

library(jsonlite)
library(tidyverse)
library(stringr)
library(optparse)

# Define command-line options
option_list <- list(
  make_option(
    c("-f", "--filename"),
    type = "character",
    help = "Name of the JSON file to sort"
  )
)

# Define the reorder_json function
reorder_json <- function(filename) {
  # Load the JSON file into a data frame
  data <- fromJSON(file = filename)

  # Sort the data frame by column name
  sorted_data <- data %>% 
    as_tibble() %>% 
    arrange(name)

  # Write the sorted data frame to a new file
  new_filename <- str_replace(filename, ".json", "_sorted.json")
  write_json(sorted_data, new_filename)

  cat(sprintf("Successfully sorted %s and saved as %s.\n", filename, new_filename))
}

# Parse command-line options
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Call the reorder_json function with the filename argument
reorder_json(opt$filename)