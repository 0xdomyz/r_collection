# Examples
# ----------
# > x <- c(1,2,3,4,5,NA,7)
# > std(x)
# [1] -1.2344268 -0.7715167 -0.3086067  0.1543033  0.6172134         NA  1.5430335
std <- function(v) (v-mean(v, na.rm = TRUE))/sqrt(var(v, na.rm = TRUE))


if (identical(environment(), globalenv())){
    x <- c(1,2,3,4,5,NA,7)
    mean(x, na.rm = TRUE)
    var(x, na.rm = TRUE)
    std(x)
}
