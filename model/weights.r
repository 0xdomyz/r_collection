std <- function(v){
    (v-mean(v, na.rm = TRUE))/sqrt(var(v, na.rm = TRUE))
}

# Examples
# -----------
# data = purrr::map(mtcars, std) |> dplyr::bind_cols()
# ce = lm(mpg ~ cyl+disp, data)$coefficients; ce
#   (Intercept)           cyl          disp 
# -3.194421e-17 -4.703462e-01 -4.232837e-01
# w = parameters_to_weights(ce[1], ce[2:length(ce)]); w
# $weights
#       cyl      disp
# 0.5263322 0.4736678
# 
# $calibration
#             a             b 
#  8.936298e-01 -3.194421e-17
# weights_to_parameters(w$weights, w$calibration[1])
#       cyl      disp
# 0.4703462 0.4232837
parameters_to_weights <- function(intercept, parameters){
    intercept = intercept
    names(intercept) = NULL
    list(
        'weights' = abs(parameters)/sum(abs(parameters)),
        'calibration' = c('a' = sum(abs(parameters)), 'b' = intercept)
    )
}

weights_to_parameters <- function(weights, parameters){
    weights*sum(abs(parameters))
}

# Examples
# --------------
# means = (purrr::map(mtcars, mean) |> unlist())[c('cyl','disp')]
# stds = (purrr::map(mtcars, ~sqrt(var(., na.rm = TRUE))) |> unlist())[c('cyl','disp')]
# ce = lm(mpg ~ cyl+disp, mtcars)$coefficients; ce
# parameters_to_norm_parameters(ce[1], ce[2:length(ce)], means, stds)
# norm_data = purrr::map(mtcars, std) |> dplyr::bind_cols()
# norm_data['mpg'] = mtcars['mpg']
# lm(mpg ~ cyl+disp, norm_data)$coefficients
parameters_to_norm_parameters <- function(intercept, parameters, means, stds) {
    list(
        'norm_intercept' = parameters * means + intercept,
        'norm_parameters' = parameters * stds
    )
}

if (identical(environment(), globalenv())){
    1
}
