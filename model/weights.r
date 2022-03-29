# Examples
# -----------
# > std <- function(v) (v-mean(v, na.rm = TRUE))/sqrt(var(v, na.rm = TRUE))
# > data = purrr::map(mtcars, std) |> dplyr::bind_cols()
# > ce = lm(mpg ~ cyl+disp, data)$coefficients; ce
#   (Intercept)           cyl          disp
# -3.194421e-17 -4.703462e-01 -4.232837e-01 
# > w = parameters_to_weights(ce); w
# $weights
#       cyl      disp
# 0.5263322 0.4736678
# 
# $signs
#  cyl disp
#   -1   -1
# 
# $calibration
#             a             b
#  8.936298e-01 -3.194421e-17
# 
# > weights_to_parameters(w$weights, w$signs, w$calibration[1])
#        cyl       disp
# -0.4703462 -0.4232837
parameters_to_weights <- function(parameters){
    inter = parameters[1]
    names(inter) = NULL
    non_inter = parameters[2:length(parameters)]
    list(
        'weights' = abs(non_inter)/sum(abs(non_inter)),
        'signs' = ifelse(non_inter > 0, 1, -1),
        'calibration' = c('a' = sum(abs(non_inter)), 'b' = inter)
    )
}

weights_to_parameters <- function(weights, signs, parameters){
    weights*signs*sum(abs(parameters))
}


# Examples
# --------------
# > std <- function(v) (v-mean(v, na.rm = TRUE))/sqrt(var(v, na.rm = TRUE))
# > means = (purrr::map(mtcars, mean) |> unlist())[c('cyl','disp')]
# > stds = (purrr::map(mtcars, ~sqrt(var(., na.rm = TRUE))) |> unlist())[c('cyl'$
# > ce = lm(mpg ~ cyl+disp, mtcars)$coefficients; ce
# (Intercept)         cyl        disp 
# 34.66099474 -1.58727681 -0.02058363
# > parameters_to_norm_parameters(ce[1], ce[2:length(ce)], means, stds)
# $norm_intercept
# (Intercept)
#    20.09062
# 
# $norm_parameters
#       cyl      disp
# -2.834752 -2.551109
# 
# > norm_data = purrr::map(mtcars, std) |> dplyr::bind_cols()
# > norm_data['mpg'] = mtcars['mpg']
# > lm(mpg ~ cyl+disp, norm_data)$coefficients
# (Intercept)         cyl        disp
#   20.090625   -2.834752   -2.551109
parameters_to_norm_parameters <- function(intercept, parameters, means, stds) {
    list(
        'norm_intercept' = sum(parameters * means) + intercept,
        'norm_parameters' = parameters * stds
    )
}

if (identical(environment(), globalenv())){
    1
}
