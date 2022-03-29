import::here(dplyr, .all=TRUE)
import::here(ggplot2, .all=TRUE)
import::here(tsibble, .all=TRUE)
import::here(fabletools, model, report)
import::here(fable, ARIMA)


# Examples
# -----------
# > plot_2_ts(fpp3::us_change, 'Consumption', 'Income', 'Quarter')
plot_2_ts = function(data, col1, col2, index_col) {
    data = data[, c(index_col, col1, col2)]
    colnames(data) = c('index', col1, col2)
    plt = data %>%
        tidyr::pivot_longer(
            c({{col1}}, {{col2}}),
            names_to = "var",
            values_to = "value"
        ) %>%
        ggplot(aes(x=index, y=value)) +
        geom_line() +
        facet_grid(vars(var), scales='free_y')
    return(plt)
}


# Examples
# -----------
# > plot_scatter(fpp3::us_change, 'Consumption', 'Income')
plot_scatter = function(data, x, y, z = NA) {
    if (is.na(z)) {
        plt = data %>% ggplot(aes_string(x = x, y = y)) + geom_point()
    } else {
        plt = data %>% ggplot(aes_string(x = x, y = y, colour = z)) + geom_point()
    }
    return(plt)
}


if (identical(environment(), globalenv())) {
    data = fpp3::us_change
    cols = colnames(data)[c(-1, -2)]

    for (col in cols) {
        p = plot_2_ts(data, 'Consumption', col, 'Quarter')
        ggsave(here::here('fpp3', 'sfa',
            paste0('Consumption', '_', col, '.png')
        ), p)
    }

    plot_scatter(data, 'Income', 'Consumption')

}


