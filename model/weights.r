import::here(here, here)
import::here('rsq.r', make_adjr2, .directory = here('model'))
import::here(stringr, str_c)

std <- function(v) (v-mean(v, na.rm = TRUE))/sqrt(var(v, na.rm = TRUE))

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

#does not give intercept back
weights_to_parameters <- function(weights, signs, calibration_a){
    weights*signs*sum(abs(calibration_a))
}

# Examples
# -----------
# > fit = lm(mpg ~ cyl+disp, mtcars)
# > ce = fit$coefficients; ce
# (Intercept)         cyl        disp
# 34.66099474 -1.58727681 -0.02058363
# > fml = parameters_to_formula(ce, 'mpg'); fml
# mpg ~ 34.6609947413328 + -1.58727680900718 * cyl + -0.0205836333707016 *
#     disp
# <environment: 0x000000000b9ac710>
# > pred_from_fml = eval(parse(text = fml), mtcars)
# > pred_from_fit = predict(fit, mtcars)
# > names(pred_from_fit) = NULL
# > sum(abs(pred_from_fml - pred_from_fit) > 1e10)
# [1] 0
parameters_to_formula = function(parameters, target){
    inter = parameters[1]
    non_inter = parameters[2:length(parameters)]
    fml_no_inter = str_c(non_inter, names(non_inter), sep = ' * ', collapse = ' + ')
    fml = str_c(inter, fml_no_inter, sep=' + ')
    fml_target = str_c(target, fml, sep=' ~ ')
    as.formula(fml_target)
}

# Examples
# -------------
# > fml = make_formula(c('cyl', 'disp', 'hp', 'drat'), 'mpg')
# > lm(fml, mtcars)$coefficients
# (Intercept)         cyl        disp          hp        drat
# 23.98524441 -0.81402201 -0.01389625 -0.02317068  2.15404553
# > fml = make_formula(c('cyl', 'disp', 'hp', 'drat'), 'mpg', 0)
# > lm(fml, mtcars)$coefficients
#          cyl         disp           hp         drat
#  0.711008285 -0.009176678 -0.043640211  6.701808900
make_formula = function(parameters, target, intercept = 1){
    fml = str_c(c(intercept, parameters), collapse = ' + ')
    fml_target = str_c(target, fml, sep=' ~ ')
    as.formula(fml_target)
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
    data = purrr::map(mtcars, std) |> dplyr::bind_cols()
    fit = lm(mpg ~ cyl + disp + hp + drat, data)
    ce = fit$coefficients
    inter = ce[1]
    w = parameters_to_weights(ce); w$weights
    make_adjr2(predict(fit, data), data$mpg, 4)

    man_wts = c(0.25, 0.3, 0.25, 0.2); names(man_wts) = names(w$weights); man_wts
    man_para = c(inter, weights_to_parameters(man_wts, w$signs, w$calibration[1]))
    fml = parameters_to_formula(man_para, 'mpg')
    make_adjr2(eval(parse(text = fml), data), data$mpg, 4)

    man_wts = c(0.25, 0.25, 0.25, 0.25); names(man_wts) = names(w$weights); man_wts
    man_para = c(inter, weights_to_parameters(man_wts, w$signs, w$calibration[1]))
    fml = parameters_to_formula(man_para, 'mpg')
    make_adjr2(eval(parse(text = fml), data), data$mpg, 4)

    man_wts = c(0.3, 0.3, 0.3, 0.1); names(man_wts) = names(w$weights); man_wts
    man_para = c(inter, weights_to_parameters(man_wts, w$signs, w$calibration[1]))
    fml = parameters_to_formula(man_para, 'mpg')
    make_adjr2(eval(parse(text = fml), data), data$mpg, 4)

    man_wts = c(0.33, 0.33, 0.34, 0); names(man_wts) = names(w$weights); man_wts
    man_para = c(inter, weights_to_parameters(man_wts, w$signs, w$calibration[1]))
    fml = parameters_to_formula(man_para, 'mpg')
    make_adjr2(eval(parse(text = fml), data), data$mpg, 3)
}
