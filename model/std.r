# Examples
# ----------
# > x <- c(1,2,3,4,5,NA,7)
# > std(x)
# [1] -1.2344268 -0.7715167 -0.3086067  0.1543033  0.6172134         NA  1.5430335
std <- function(v) (v-mean(v, na.rm = TRUE))/sqrt(var(v, na.rm = TRUE))


if (FALSE) {
  #apply std on data
  std <- function(v) (v-mean(v, na.rm = TRUE))/sqrt(var(v, na.rm = TRUE))
  data = purrr::map(mtcars, std) |> dplyr::bind_cols()
  data
  
  #calc mean/std
  means = purrr::map_df(mtcars, function(v) mean(v, na.rm = TRUE))
  as.numeric(means)
  
  stds = purrr::map_df(mtcars, function(v) sqrt(var(v, na.rm = TRUE)))
  as.numeric(stds)
  
  #apply mean/std on diff population
  purrr::pmap(list(mtcars*2-1, as.numeric(means), as.numeric(stds)),
    function(v, m, std) (v-m)/std) |> dplyr::bind_cols()
}


if (identical(environment(), globalenv())){
    x <- c(1,2,3,4,5,NA,7)
    mean(x, na.rm = TRUE)
    var(x, na.rm = TRUE)
    std(x)
}
