files <- list.files('.', pattern = 'copy.*\\.r')

for (file in files) {
    print(file)
}
