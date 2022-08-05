inner_var = 1.6

func = function(x) {
    res = x + inner_var
    print(paste0("inner_var is: ", inner_var))
    return(res)
}

