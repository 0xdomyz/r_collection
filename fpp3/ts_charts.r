import::here(dplyr, .all = TRUE)
import::here(ggplot2, .all = TRUE)
import::here(tsibble, .all = TRUE)
# library(fpp3)
library(zoo)


# plot 2 times series one on top another
###########################################################################################
plot_2_ts_top_down <- function(data, x, y, index_col) {
    data <- data[, c(index_col, x, y)]
    plt <- data %>%
        tidyr::pivot_longer(
            c({{ x }}, {{ y }}),
            names_to = "var",
            values_to = "value"
        ) %>%
        ggplot(aes_string(x = index_col, y = "value")) +
        geom_line() +
        facet_grid(vars(var), scales = "free_y") +
        ggtitle(paste0(x, " vs ", y))
    return(plt)
}

df <- fpp3::us_change %>% filter(Quarter >= yearquarter("2000 Q1"))

# plot
p <- plot_2_ts_top_down(df, "Consumption", "Income", "Quarter")
p
p + scale_x_yearquarter(date_breaks = "1 year", date_labels = "%Y")


# plot 2 time series on same chart, scales on left and right sides, use ggplot2, tsibble
###########################################################################################
plot_2_ts <- function(df, x, y, index_col) {
    df <- df[, c(index_col, x, y)]
    p <- df %>%
        ggplot(aes_string(x = index_col)) +
        geom_line(aes_string(y = x), colour = "black") +
        geom_line(aes_string(y = y), colour = "red") +
        scale_y_continuous(
            sec.axis = sec_axis(~ . * 1, name = y)
        ) +
        ggtitle(paste0(
            x,
            "(black) vs ",
            y,
            "(red)"
        ))

    return(p)
}

df <- fpp3::us_change %>% filter(Quarter >= yearquarter("2000 Q1"))

p <- plot_2_ts(df, x = "Consumption", y = "Income", index_col = "Quarter")
p

p + scale_x_yearquarter(date_breaks = "1 year", date_labels = "%Y")

# add vertical lines, wip
p +
    geom_vline(xintercept = as.numeric(as.yearqtr("2008 Q1")), colour = "red") +
    geom_vline(xintercept = as.numeric(as.yearqtr("2009 Q3")), colour = "red")

# plot scatter
###########################################################################################
plot_scatter <- function(data, x, y, z = NA) {
    if (is.na(z)) {
        plt <- ggplot(data, aes_string(x = x, y = y)) +
            geom_point() +
            ggtitle(paste0(x, " vs ", y))
    } else {
        plt <- ggplot(data, aes_string(x = x, y = y, colour = z)) +
            geom_point() +
            ggtitle(paste0(x, " vs ", y, " coloured by ", z))
    }
    return(plt)
}

plot_scatter(mtcars, "mpg", "disp")
data <- mtcars
data["cyl_gt_5"] <- ifelse(data["cyl"] > 5, 1, 0)
plot_scatter(data, "mpg", "disp", "cyl_gt_5")