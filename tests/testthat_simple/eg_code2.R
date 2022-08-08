#via txt
a = mtcars
b = head(mtcars)

hashs = function(items) {
  Map(digest::digest, items)
}

list_to_save = hashs(list(a, b))
file = here::here("tests", "testthat", "saved.txt")
write(toString(Sys.time()), file)
lapply(list_to_save, write, file, append=TRUE)


#via rlist
a = mtcars
b = head(mtcars)

list_to_save = list(
  "time" = toString(Sys.time()),
  "a" = digest::digest(a),
  "b" = digest::digest(b)
)
file = here::here("tests", "testthat", "saved.yaml")
rlist::list.save(list_to_save, file, "yaml")

