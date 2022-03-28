import::here('std.r', std, .directory = here::here('model'))
import::here('weights.r', parameters_to_weights, .directory = here::here('model'))

make_r2 <- function (x, y) cor(x, y) ^ 2

# Examples
# ------------
# fit = lm(mpg ~ wt + disp + hp + qsec, mtcars)
# summary(fit)
# make_adjr2(predict(fit, mtcars), mtcars$mpg, 4)
make_adjr2 <- function (x, y, k) {
    n = length(x)
    r2 = cor(x, y) ^ 2
    return(1 - (1 - r2) * (n - 1)/(n - k - 1))
}

if (identical(environment(), globalenv())){
    data = mtcars |> dplyr::mutate_all(std)
    fit = lm(mpg ~ ., data)
    summary(fit)
    ce = fit$coefficients
    parameters_to_weights(ce[1], ce[2:length(ce)])
    preds = predict(fit, data)
    actual = data$mpg

    rss <- sum((preds - actual) ^ 2); rss
    tss <- sum((actual - mean(actual)) ^ 2); tss
    r2 <- 1 - rss/tss; r2

    r2 = make_r2(actual, preds); r2

    r2 = cor(actual, preds) ^ 2; r2

    n = nrow(mtcars)
    k = ncol(mtcars) - 1
    r2_adj = 1 - (1 - r2) * (n - 1)/(n - k - 1); r2_adj

    make_adjr2(actual, preds, 10)
}

