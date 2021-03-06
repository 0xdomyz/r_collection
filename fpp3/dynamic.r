import::here(dplyr, .all=TRUE)
import::here(ggplot2, .all=TRUE)
import::here(tsibble, .all=TRUE)
import::here(distributional, guide_level)
import::here(fabletools, model, report)
import::here(fable, ARIMA)

plot_arima_error = function(fit, index_col) {
    data = bind_rows(
        `Regression residuals` =
            tibble::as_tibble(residuals(fit, type = "regression")),
        `ARIMA residuals` =
            tibble::as_tibble(residuals(fit, type = "innovation")),
        .id = "type"
    )

    data['index'] = data[index_col]

    plt = data %>%
        mutate(
            type = factor(type, levels=c("Regression residuals", "ARIMA residuals"))
        ) %>%
        ggplot(aes(x = index, y = .resid)) +
        geom_line() +
        facet_grid(vars(type))
    return(plt)
}


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


if (identical(environment(), globalenv())) {
    data = fpp3::us_change

    fit = data %>%
        model(ARIMA(
            Consumption ~ Income + Production + Savings + Unemployment +
            pdq(d=0)
        ))

    fit %>% report()

    plot_arima_error(fit, 'Quarter')

    fit %>% feasts::gg_tsresiduals()

    generics::augment(fit) %>%
        fabletools::features(.innov, feasts::ljung_box, dof = 8, lag = 7)

    calc = fabletools::forecast(fit, data)

    make_adjr2(calc$.mean, data$Consumption, 8)

    calc %>% autoplot(data)


}

