# Examples
#
# rscript layout\argparser_tute.r
# rscript layout\argparser_tute.r -h
# rscript layout\argparser_tute.r -d 3 3.345234

library(argparser, quietly=TRUE)

# Create a parser
p <- arg_parser("Round a floating point number")

# Add command line arguments
p <- add_argument(p, "number", help="number to round", type="numeric")
p <- add_argument(p, "--digits", help="number of decimal places", default=0)

# Parse the command line arguments
argv <- parse_args(p)

# Do work based on the passed arguments
cat( round(argv$number, argv$digits), "\n")