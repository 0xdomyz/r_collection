#method 1
#setwd("C:/Users/User/Desktop/New folder")
#import::here("code.r" ,a_func, .directory = "the_lib")

#method 2
#import::here("code.r" ,a_func, .directory = "C:/Users/User/Desktop/New folder/the_lib")

#method 3
import::here("code.r" ,a_func, .directory = "../../the_lib")
a_func(0)
