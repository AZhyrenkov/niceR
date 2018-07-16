library(dplyr)
library(tidyr)
library(stringr)
library(RPostgreSQL)
library(ggplot2)
library(magrittr)
library(lubridate)
library(openxlsx)
Sys.setlocale(locale = "Ukrainian_Ukraine")
Sys.setenv("R_ZIPCMD" = "C:/Users/aleksey.zhirenkov/Documents/R/Rtools/bin/zip.exe") ## path to zip.exe

## Define fuctions
`%+%` <- function(a, b) paste(a,b, sep="")
days_in_month <- function(date){
  require(lubridate) <- 
  day(floor_date(as.Date(date),'month') + months(1) - days(1))
}


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

# Function to redistribute NAs within managers honestly
distribute_honestly <- function(pool){
  # @pool: two-column dataframe with user_id and mnager columns. To redistribution columns must be NA
  # basic inputs
  total_capacity <- nrow(pool)
  capacity_per_manager <- ceiling(total_capacity/(n_distinct(pool$manager)-1))
  to_redist <- nrow(pool[is.na(pool$manager),])
  
  # capacity dataframe
  capacity <- pool %>% 
    filter(!is.na(manager)) %>% 
    group_by(manager) %>% 
    summarise(users = n()) %>% 
    ungroup() %>% 
    arrange(desc(users)) %>% 
    rowwise() %>% 
    mutate(to_redist = to_redist,
           capacity = max(capacity_per_manager-users,0)) %>% 
    ungroup() %>% 
    mutate(cumsum = cumsum(capacity)) %>% 
    rowwise() %>% 
    mutate(capacity = capacity+min(to_redist-cumsum,0))
  
  # initialize empty dataframe for results storing
  distribution_result <- data.frame(user_id = numeric(),manager=character())
  
  # filter out zero-capacity managers
  capacity_work <- filter(capacity, capacity > 0)
  
  # Rearrange pool randomly (uniform distribution)
  randomized_pool <- pool %>% 
    filter(is.na(manager)) %>% 
    rowwise() %>% 
    mutate(rand = runif(1)) %>% 
    arrange(rand) %>% 
    select(-rand)
  
  for(i in 1:nrow(capacity_work)){
    # get a name of manager with free capacity
    managerTo = capacity_work[i, ]$manager
    
    # get a number of users to reassign to the aforementioned manager
    users = capacity_work[i, ]$capacity
    
    # slice the required number of rows
    sampled <- slice(randomized_pool, 1:users)
    
    # add manager's name into the resulting set
    sampled <- mutate(sampled, manager=rep(managerTo, users))
    
    # filter the resulting set out of the distribution dataset
    randomized_pool <- filter(randomized_pool, 
                              !(user_id %in% sampled$user_id))
    
    # unite result and sliced data
    distribution_result <- bind_rows(distribution_result, sampled)
  }
  distribution_result <- pool %>% 
    filter(!is.na(manager)) %>% 
    bind_rows(.,distribution_result)
  
  return(list(
    total_capacity = total_capacity,
    capacity_per_manager = capacity_per_manager,
    to_redist = to_redist,
    distribution_result = distribution_result,
    capacity = capacity))
}

# Distribution with capacity
distribute <- function(vector_pool, capacity){
  # @vector_pool: vector with users ti distribute
  # @capacity: two-column dataset with manager and capacity columns
  
  # initialize empty dataframe for results storing
  distribution_result <- data.frame(user_id = numeric(),manager=character())
  
  # filter out zero-capacity managers
  capacity_work <- filter(capacity, capacity > 0)
  
  # Rearrange pool randomly (uniform distribution)
  randomized_pool <- tibble(user_id = unique(vector_pool)) %>% 
    rowwise() %>% 
    mutate(rand = runif(1)) %>% 
    arrange(rand) %>% 
    select(-rand)
  
  for(i in 1:nrow(capacity_work)){
    # get a name of manager with free capacity
    managerTo = capacity_work[i, ]$manager
    
    # get a number of users to reassign to the aforementioned manager
    users = capacity_work[i, ]$capacity
    
    # slice the required number of rows
    sampled <- slice(randomized_pool, 1:users)
    
    # add manager's name into the resulting set
    sampled <- mutate(sampled, manager=rep(managerTo, users))
    
    # filter the resulting set out of the distribution dataset
    randomized_pool <- filter(randomized_pool, 
                              !(user_id %in% sampled$user_id))
    
    # unite result and sliced data
    distribution_result <- bind_rows(distribution_result, sampled)
  }
  
  return(distribution_result)
}

## credentials dataset
credentials <-  readRDS('input/credentials.RDS')

## aws connection
# amz <- dbConnect(MySQL(),user=credentials$aws$login,
#                  host=credentials$aws$server,
#                  dbname=credentials$aws$db,
#                  password=credentials$aws$password)

## yamato connection
yamato <- dbConnect(dbDriver("PostgreSQL"), 
                    host=credentials$yamato$server,
                    port=credentials$yamato$port,
                    user=credentials$yamato$login,
                    dbname=credentials$yamato$db,
                    password=credentials$yamato$password)

# set month-range for queries and further calculation
mindate <- floor_date(Sys.Date() - days(1), "month") - months(4)
lastdate <- floor_date(Sys.Date() - days(1), "month")
month_range <- tibble(
  YM01 = as.Date(seq(mindate, lastdate,by = 'month')),
  YM = format(YM01, format='%Y-%m'),
  M = month(YM01),
  MYM = 'M_' %+% YM
)