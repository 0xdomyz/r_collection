#files
folder = here::here("layout","files_tute")

list.files(folder)

new_file = here::here("layout","files_tute","test.csv")
file.create(new_file)
unlink(new_file)
file.remove(new_file)

#dir
new_folder = here::here("layout","files_tute","new_folder")
dir.create(new_folder)

file.exists(new_folder)
file.remove(new_folder)

if (!file.exists(new_folder)) {
    dir.create(new_folder)
}

unlink(new_folder, recursive = TRUE)
