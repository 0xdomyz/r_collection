# make a formula
fml <- "z~ 1+ 3.22*x^2 + 1.89*y^2 * z"
fml <- as.formula(fml)
fml

# make into a char
fml_char <- as.character(fml)
fml_char

# save fml into a plain txt file
writeLines(fml_char, "data/text/text.txt")


# save output of lm into a plain txt file
fit <- lm(
    "mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb",
    mtcars
)

fit

# save output of lm into a plain txt file
sink("data/text/output.txt")
fit
sink()

fit

# display a number in more decimal places
long <- 0.23542563463462362356253623346236235344
print(long, digits = 22)
