

# Function for calculating biatlon by inputted columns
make_biatlon <- function(dataset,columns,base_column){
  # @dataset: Interested DataFrame
  # @columns: Vector with column's names for biathlon calculating
  # @base_column: Column in dataset to compare within biathlon
  require(dplyr)
  require(foreach)
  require(rlang)

  res_ds <- dataset

  new_columns <- paste('tmp',columns,sep = '_')

  base <- enquo(base_column)
  chars <- length(columns)

  foreach(col = columns) %do% {
    col <- as.name(col)
    new_col = paste('tmp',col,sep = '_')

    res_ds <- res_ds %>%
      mutate(!!new_col := ifelse(!!col >= !!base, '.', '_'))
  }

  res_ds <- res_ds %>%
    mutate_(biathlon = paste0('paste0(`',paste(new_columns,collapse = '`,`'),'`)')) %>%
    mutate(currentBiathlon = str_sub(biathlon,2,chars)) %>%
    select(-one_of(new_columns))
}


# Function for calculating biathlon by factor value without spreading
getBiathlon <- function(x, length){
  # @x: the month or time period factor variable
  # @length: the number of periods to build the biathlon for
  baseString <- rep('_', length)
  subsets <- as.integer(strsplit(toString(unique(as.integer(x))), ", ")[[1]])

  baseString[subsets] <- '.'

  paste0(baseString, collapse = '')
}


# Functions for calculating retentions
get_retentions <- function(biathlon){
  require(stringr)
  require(dplyr)
  require(rlang)
  # get biathlon size
  chars <- nchar(biathlon[1])
  # prepare basic working dataset with long biathlon and current biathlon
  workingSet <- tibble(long  = biathlon,
                       currentBiathlon = str_sub(long,2,chars))
  # calc number of users in segments
  finalSet <- workingSet %>%
    group_by(currentBiathlon) %>%
    summarise(users = n()) %>%
    ungroup() %>%
    mutate(rightNice = paste0(currentBiathlon,'.'))
  # calc number of success users
  workingSet <- workingSet %>%
    group_by(long) %>%
    summarise(usersNice = n()) %>%
    ungroup()
  # final result
  finalSet <- finalSet %>%
    left_join(workingSet,by = c('rightNice'='long')) %>%
    mutate(retention = usersNice/users) %>%
    select(-rightNice)
  return(finalSet)
}
