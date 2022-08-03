log_stop = function(x) {
    if (x <= 0) {
        stop(paste0("x has to be large than or equal to zero, you have: ", x))
    } else {
        log(x)
    }
}

log_stop(-2)
log_stop(2)


log_trycatch <- function(x){
    tryCatch(
        expr = {
            return(log(x))
        },
        error = function(e){
            return(paste0('Caught an error!', e))
        },
        warning = function(w){
            return(paste0('Caught an warning!', w))
        }
    )
}

log_trycatch(-2)
log_trycatch(2)


    
