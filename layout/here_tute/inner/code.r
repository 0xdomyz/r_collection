dir = here::here("inner", "inner2")
import::here("inner2_code.r", a_func, .directory = dir)
print_dir = function() {print(dir)}
print(a_func(1))

file = here::here("inner", "inner2", "inner2.txt")
print(readLines(file))
print_file = function() {print(file)}

print("inner wd:")
print(getwd())

#at project outer root
#r
#import::here("code.r", print_file, .directory = "layout/here_tute/inner")
#print_file

