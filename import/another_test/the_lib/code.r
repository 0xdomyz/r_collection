import::here("another.r",b_func,.directory=here::here("."))

a_func = function(x) {
  res = b_func(x) + 1
  return(res)
}