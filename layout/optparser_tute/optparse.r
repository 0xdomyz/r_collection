suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
    make_option(c("-s", "--silent"), action="store_true", 
        help="Silent mode to reduce logs"),
    make_option(c("-n", "--number"), type="integer", default=5, 
        help="Number of number to generate [default %default]",
        metavar="number"),
    make_option(c("-w", "--word"), default="word", 
        help = "Word to use [default \"%default\"]")
    )

opt <- parse_args(OptionParser(option_list=option_list))

if ( opt$verbose ) { 
    write("writing some verbose output to standard error...\n", stderr()) 
}

if( opt$generator == "rnorm") {
    cat(paste(rnorm(opt$count, mean=opt$mean, sd=opt$sd), collapse="\n"))
} else {
    cat(paste(do.call(opt$generator, list(opt$count)), collapse="\n"))
}
cat("\n")
