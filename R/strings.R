# nice strings glue ----
`%+%` <- function(a, b) paste(a,b, sep="")

# read sql query from file -----
read_query <- function(path) suppressWarnings(paste(readLines(path),collapse = '\n'))

# fix cyrrilic encoding
fix_Enc_Cyr <- function(vector) {
  if(is.character(vector)){iconv(x = vector,"UTF-8","CP1251")}
  else{vector}
}
