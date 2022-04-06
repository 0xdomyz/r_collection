import::here(here, here)
import::here('rsq.r', make_r2, make_adjr2, .directory = here('model'))
import::here(stringr, str_c)
import::here(Hmisc, rcorr.cens)

# Examples
# -----------
# > std <- function(v) (v-mean(v, na.rm = TRUE))/sqrt(var(v, na.rm = TRUE))
std <- function(v) (v-mean(v, na.rm = TRUE))/sqrt(var(v, na.rm = TRUE))

# Examples
# -----------
# > data = purrr::map(mtcars, std) |> dplyr::bind_cols()
# > data$mpg = mtcars$mpg
# > ce = lm(mpg ~ cyl+disp, data)$coefficients; ce
# (Intercept)         cyl        disp 
#   20.090625   -2.834752   -2.551109 
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
#         a         b
#  5.385861 20.090625
# 
# > weights_to_parameters(w$weights, w$signs, w$calibration[1])
#       cyl      disp
# -2.834752 -2.551109
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
# > sum(abs(pred_from_fml - pred_from_fit) > 1e-10)
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
make_formula = function(variables, target, intercept = 1){
    fml = str_c(c(intercept, variables), collapse = ' + ')
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

# > std <- function(v) (v-mean(v, na.rm = TRUE))/sqrt(var(v, na.rm = TRUE))
# > data = purrr::map(mtcars, std) |> dplyr::bind_cols()
# > data$mpg = mtcars$mpg
# > fit_lm(c('cyl', 'disp'), 'mpg', data, 2)
# > fit_lm(c('cyl', 'disp'), 'mpg', data, 2, concise=FALSE)
# > vars = setdiff(colnames(mtcars),'mpg')
# > fit_lm(vars, 'mpg', data, 10)
# > fit_lm(c('disp', 'hp', 'wt', 'qsec', 'am'), 'mpg', data, 5)
fit_lm = function(
        variables,
        target,
        data,
        k,
        model_has_intercept=TRUE,
        concise=TRUE
    ){
    fml = make_formula(variables, target, model_has_intercept*1)
    fit = lm(fml, data)
    ce = fit$coefficients
    if (model_has_intercept){
        wts = parameters_to_weights(ce)
    } else {
        wts = parameters_to_weights(c(0, ce))
    }
    preds = predict(fit, data)
    actual = data[[target]]
    r2 = make_r2(preds, actual, model_has_intercept)
    adjr2 = make_adjr2(preds, actual, k, model_has_intercept)
    dxy = rcorr.cens(preds, actual)['Dxy']
    
    if (concise){
        metrics = c(r2, adjr2, dxy)
        names(metrics) = c('r2', 'adjr2', 'dxy')
        return(list(
            'fml'=fml,
            'fit'=fit$coefficients,
            'weights'=round(wts$weights*100,2),
            'signs'=wts$signs,
            'metrics'=metrics
        ))
    } else{
        print(summary(fit))
        return(list(
            'fml'=fml,
            'fit'=fit,
            'weights'=wts,
            'r2'=r2,
            'adjr2'=adjr2,
            'dxy'=dxy
        ))
    }
}

# Examples
# -------------
# > std <- function(v) (v-mean(v, na.rm = TRUE))/sqrt(var(v, na.rm = TRUE))
# > data = purrr::map(mtcars, std) |> dplyr::bind_cols()
# > data$mpg = mtcars$mpg
# > fit = fit_lm(c('disp', 'hp', 'wt', 'qsec', 'am'), 'mpg', data, 5)
# > fit
# > test_wts(c('disp', 'hp', 'wt', 'qsec', 'am'), c(0.1, 0.15, 0.4, 0.2, 0.15),
# >     fit$signs, 'mpg', data, 5)
# > test_wts(c('disp', 'hp', 'wt', 'qsec', 'am'), c(0, 0.2, 0.4, 0.2, 0.2),
# >     fit$signs, 'mpg', data, 4)
test_wts = function(
        variables,
        weights,
        signs,
        target,
        data,
        k,
        concise=TRUE
    ){
    man_wts = weights
    names(man_wts) = variables
    fml = parameters_to_formula(c(1, man_wts * signs), target)
    x = eval(parse(text = fml), data)
    y = data[[target]]
    ce = lm(y ~ 1 + x)$coefficients
    calibration_a = ce[2]
    calibration_b = ce[1]

    preds = eval(parse(text = fml), data) * calibration_a + calibration_b
    actual = data[[target]]
    r2 = make_r2(preds, actual)
    adjr2 = make_adjr2(preds, actual, k)
    dxy = rcorr.cens(preds, actual)['Dxy']

    if (concise){
        metrics = c(r2, adjr2, dxy)
        names(metrics) = c('r2', 'adjr2', 'dxy')
        return(list(
            'fml'=fml,
            'calibration'=ce,
            'weights'=round(weights*100,2),
            'metrics'=metrics
        ))
    } else{
        print(summary(fit))
        return(list(
            'fml'=fml,
            'calibration_a'=calibration_a,
            'calibration_b'=calibration_b,
            'weights'=round(weights*100,2),
            'r2'=r2,
            'adjr2'=adjr2,
            'dxy'=dxy
        ))
    }
}

if (identical(environment(), globalenv())){
    1
}
