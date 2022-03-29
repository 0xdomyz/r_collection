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
        ggplot(aes(x = Quarter, y = .resid)) +
        geom_line() +
        facet_grid(vars(type))
    return(plt)
}


if (identical(environment(), globalenv())) {
    fit = fpp3::us_change %>%
        model(ARIMA(
            Consumption ~ Income + Production + Savings + Unemployment +
            pdq(d=0)
        ))

    fit %>% report()

    plot_arima_error(fit, 'Quarter')

    fit %>% feasts::gg_tsresiduals()

    generics::augment(fit) %>%
        fabletools::features(.innov, feasts::ljung_box, dof = 8, lag = 7)

    fabletools::forecast(fit, fpp3::us_change) %>%
        autoplot(fpp3::us_change)


}

