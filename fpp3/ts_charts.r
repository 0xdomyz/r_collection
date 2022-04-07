import::here(dplyr, .all=TRUE)
import::here(ggplot2, .all=TRUE)
import::here(tsibble, .all=TRUE)


# Examples
# -----------
# > plot_2_ts(fpp3::us_change, 'Consumption', 'Income', 'Quarter')
plot_2_ts = function(data, x, y, index_col) {
    data = data[, c(index_col, x, y)]
    colnames(data) = c('index', x, y)
    plt = data %>%
        tidyr::pivot_longer(
            c({{x}}, {{y}}),
            names_to = "var",
            values_to = "value"
        ) %>%
        ggplot(aes(x=index, y=value)) +
        geom_line() +
        facet_grid(vars(var), scales='free_y') +
        ggtitle(paste0(x, ' vs ', y))
    return(plt)
}


# Make scatter plot
#
# Examples
# -----------
# > plot_scatter(mtcars, 'mpg', 'disp')
# > data = mtcars
# > data['cyl_gt_5'] = ifelse(data['cyl'] > 5, 1, 0)
# > plot_scatter(data, 'mpg', 'disp', 'cyl_gt_5')
plot_scatter = function(data, x, y, z = NA) {
    if (is.na(z)) {
        plt = ggplot(data, aes_string(x = x, y = y)) +
            geom_point() +
            ggtitle(paste0(x, ' vs ', y))
    } else {
        plt = ggplot(data, aes_string(x = x, y = y, colour = z)) +
            geom_point() +
            ggtitle(paste0(x, ' vs ', y, ' coloured by ', z))
    }
    return(plt)
}


if (identical(environment(), globalenv())) {
    1
}


