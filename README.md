# niceR
***
Few nice functions for every-day use

## Download and Install

* Install [devtools](https://github.com/hadley/devtools) package

```R
install.packages("devtools")```

* Load the `devtools` package

```R
library(devtools)```

* Install package

```R
install_github("AZhyrenkov/niceR")```

* Load package

```R
library(niceR)``` 

## Strings:
***

### Concatenate string `x` and `y`.

`x %+% y` 
 \
 \ 

### Read sql query from file

Only one query per file. \

`read_query(path)` - read single query. \
 \
 
Read all queries from `sql` directory and execute them: 
```R
library(magrittr)
library(purrr)
library(dplyr)
library(RPostgreSQL) #your_database_library
library(stringr)

queries <- list.files('sql/',full.names = T)
all_data <- tibble(queries) %>% 
    rowwise() %>% 
    mutate(query = map_chr(queries, read_query),  # read all queries
           name = str_sub(str_extract(string = queries, '/.+\\.'),2,-2), # extract file name as query name
           result = map(query,~dbGetQuery(db, query)) # execute query
    )
```

## Databases:
***
### Kill all MySQL Connections.

`killMySQLConnections()` \
 \ 
 
### Write data to redshift db by chunks

`write_data_redshift(con,df,name, chunk = 100000)`

* _con_ - RPostgreSQL formal class database connection
* _df_ - dataframe with data to write
* _name_ - Name of targeted table (will be overwritten)
* _chunk_ -  chunk-size, default 100000 \
 \ 

### Read data from database by chunks

`read_data()`

`read_data(con,chunk = 100000, table, order_column = 'Id')`

* _con_ - DBI formal class database connection (RMySQL, RPostgreSQL)
* _table_ - string with table to read name
* _order_column_ - Name of Primary key to sort during splitting by chunks, must be unique.
* _chunk_ -  chunk-size, default 100000 \
 \ 
 
### Copy data from one DB to anoother by chunks

`copy_data()`

Copy data from DBI-formal-class connection database (tested with MySQL) to redshift by chunks
`copy_data(redshift, mysql, from_table, to_table, chunk = 100000, order_column = 'Id', rewrite = T, test = F)`

* _redshift_ - RPostgreSQL formal class database connection? Targeted database
* _mysql_ - DBI formal class database connection (RMySQL, RPostgreSQL), source database
* _to_table_ - string with final table name
* _order_column_ - Name of Primary key to sort during splitting by chunks, must be unique.
* _chunk_ -  chunk-size, default 100000
* _rewrite_ - If `False` will ap[end data to existing table
* _test_ - If `TRUE` will be limited to 3 iterations \
 \ 

## Biathlon(financial pattern retentions):
***
### groupby-summarise function

Thanks to [lxii](https://github.com/lxii) for contribution!

`getBiathlon(x, length)` 

* _x_ - the month or time period factor variable
* _length_ - the number of periods to build the biathlon for \
 \

### Add financial pattern column to dataset 

Note: monthly data must be in columns \

`make_biatlon <- (dataset,columns,base_column)` 

* _dataset_ - input dataframe
* _columns_ - vector with strings names of montly data. Without NAs
* _base_column_ - column to compare with.

Example:
`make_biatlon(ds,month_range$MYM[1:4],threshold)` \
 \ 
 
### Calculate retentioans by financial pattern segments
`get_retentions(biathlon)`
* _biathlon_ - vector with all finpattern data \
 \ 

## Distribution of rows by groups:
***
### Distribution by all available items

`pool_ds` consist of two columns: 

* _user_id_ (key for distributed row)
* _distribution_value_ - partly missing identificator (branch, region, sales rep etc) 

Example: 
```R
# Distribution
distribution <- distribute_honestly(pool_ds)
rm(pool_ds)
``` 

Gathering results:

```R
# How many rows per one distribution_value
capacity <- distribution$capacity
# final dataset
distribution_result <- distribution$distribution_result
```

### Distribution with precalculated volumes 

```R
distribution <- distribute(pool_ds,capacity = capacity)
```
 
## Error logging
***
Log errors and warnings to slack channel.

Thanks to [lxii](https://github.com/lxii) for contribution!

Using: `errorHandler(code, prefix)`

Example:
```R
library(slackr)
slackr_setup(config_file = slackr'), echo = T) # configure slackR bot
errorHandler({
  # some code, connection to database e.g.
}, prefix = 'Connection to database is broken')
```
