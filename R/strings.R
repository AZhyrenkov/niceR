# nice strings glue ----
`%+%` <- function(a, b) paste(a,b, sep="")

# read sql query from file -----
read_query <- function(path) suppressWarnings(paste(readLines(path),collapse = '\n'))
