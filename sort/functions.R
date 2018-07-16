# kill all db connections ----
killDbConnections <- function () {
  require(RMySQL)
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}

# read sql query from file -----
read_query <- function(path) suppressWarnings(paste(readLines(path),collapse = '\n'))

# nice strings glue ----
`%+%` <- function(a, b) paste(a,b, sep="")

# basic error handler and slack logger ----
errorHandler <- function(code, prefix) {
  tryCatch(
    code, 
    error = function(c){
      errorMessage = paste0(now(),', ', 'Error ', prefix, c$message)
      slackr_bot(errorMessage)
      stop()
    },
    warning = function(c){
      warningMessage = paste0(now(),', ', 'Warning ', prefix, '\n', c$message)
      slackr_bot(warningMessage)
    })
}

# write data to db chunks ----
write_data_redshift <- function(con,df,name, chunk = 100000){
  require(dplyr)
  name <- as.character(name)
  df <- as.data.frame(df)
  iterations <- ceiling(nrow(df)/chunk)
  print(paste0('Iterations: ',iterations))
  chunk_work <- min(chunk,nrow(df))
  df_initial <- slice(df,chunk_work) 
  name_temp <- paste0(name,'__temp')
  update_query <- paste0('INSERT INTO ',name,' SELECT * FROM ',name_temp)
  drop_query <- paste0('DROP TABLE ',name_temp)
  
  copy_to(con, df_initial,name,overwrite = T)
  print('1 OK')
  if (iterations > 1) {
  for (i in 2:iterations) {
    df_work <- slice(df,(chunk_work+1):(chunk_work+chunk))
    print(paste0('STR, iteration ',iterations))
    str(df_work)
    copy_to(con,df_work,name_temp,overwrite = T)
    print('temp table copied')
    print(update_query)
    dbSendQuery(con,update_query)
    print('update ok')
    dbSendQuery(con,drop_query)
    print('drop ok')
    chunk_work <- chunk_work+chunk
    print(paste0(i,' OK'))
  }
  print('Process finished!')
  } else {
    print('Finished with 1 iteration')
  }
}


# write data by chunks mysql
# dbWriteTable(connection, name=tableName, value=rows , append=TRUE, row.names=FALSE, overwrite=FALSE);

# read data from db by chunks ----
read_data <- function(con,chunk = 100000, table, order_column = 'Id'){
  q_chunk <- chunk
  q_offset <- 0
  rows <- dbGetQuery(con,paste0('select count(*) FROM ',table))[1,1]
  iterations <- floor(rows/q_chunk)+1
  data_result <- data.frame()
  print(paste0('Iterations: ',iterations))
  for (i in 1:iterations) {
    query <- paste0('SELECT * FROM ',table,' ORDER BY ',order_column, ' OFFSET ',q_offset, ' Limit ', q_chunk)
    print(paste0('Iteration ',i,': ',query))
    q_offset <- q_offset + q_chunk
    data_chunk <- dbGetQuery(con,query)
    data_result <- rbind(data_result, data_chunk)
    
  }
  return(as.data.frame(data_result))
}

copy_data <- function(redshift, mysql, from_table, to_table, chunk = 100000, order_column = 'Id', rewrite = T, test = F){
  require(dplyr)
  require(RPostgreSQL)
  require(RMySQL)
  # source table
  from_table <- as.character(from_table)
  
  #destination table
  to_table <- as.character(to_table)
  
  # offset for read statement
  q_offset <- 0
  
  # number of rows in source table
  query_rows <- paste0('SELECT count(*) FROM ',from_table)
  rows <- dbGetQuery(redshift, query_rows)[1,1]
  
  # iterations stuff
  iterations <- ifelse(test == T,3,floor(rows/chunk + 1))
  chunk_work <- chunk
  
  # feedback about process scale
  print(paste0(rows,' rows by ',chunk_work,'-rows chunks within ',iterations, ' iterations'))
  
  # initial load
  query <- paste0('SELECT * FROM ',from_table,' ORDER BY ',order_column, ' OFFSET ',q_offset, ' Limit ', chunk_work)
  ## feedback to console
  print(paste0('Iteration 1 :: Downloaded: ',query))
  ## get data
  data_chunk <- RPostgreSQL::dbGetQuery(redshift,query)
  ## new offset
  q_offset <- q_offset + chunk_work
  
  # check for rewrite
  # and write data to table
  RMySQL::dbWriteTable(mysql,name = to_table, value = data_chunk,append = !rewrite, overwrite = rewrite, row.names = F)  
  
  print(paste0('Iteration 1 :: Uploaded'))
  if (iterations > 1) {
    # rest of iterations
    for (i in 2:iterations) {
      # read first chunk from db
      ## create sql-statement
      query <- paste0('SELECT * FROM ',from_table,' ORDER BY ',order_column, ' OFFSET ',q_offset, ' Limit ', chunk_work)
      ## feedback to console
      print(paste0('Iteration ',i,' :: Downloaded: ',query))
      ## new offset
      q_offset <- q_offset + chunk_work
      ## get data
      data_chunk <- RPostgreSQL::dbGetQuery(redshift,query)
      
      # write data to table
      RMySQL::dbWriteTable(mysql,name = to_table, value = data_chunk, append = T, overwrite = F, row.names = F)
      print(paste0('Iteration ',i,' :: Uploaded'))
    }
    print("Process finished!")} else{
    print("Process finished with 1 iteration!")  
    }
}

update_data <- function(redshift, mysql, from_table, to_table, chunk = 100000, where = NA, test = F){
  require(dplyr)
  require(RPostgreSQL)
  require(RMySQL)
  # source table
  from_table <- as.character(from_table)
  
  #destination table
  to_table <- as.character(to_table)
  
  # offset for read statement
  q_offset <- 0
  
  # number of rows in source table
  query_rows <- paste0('SELECT count(*) FROM ',from_table)
  rows <- dbGetQuery(redshift, query_rows)[1,1]
  
  # iterations stuff
  iterations <- ifelse(test == T,3,floor(rows/chunk + 1))
  chunk_work <- chunk
  
  # feedback about process scale
  print(paste0(rows,' rows by ',chunk_work,'-rows chunks within ',iterations, ' iterations'))
  
  # initial load
  query <- paste0('SELECT * FROM ',from_table, if(!is.na(where)) {paste0(' ', where)}, ' OFFSET ',q_offset, ' Limit ', chunk_work)
  ## feedback to console
  print(paste0('Iteration 1 :: Downloaded: ',query))
  ## get data
  data_chunk <- RPostgreSQL::dbGetQuery(redshift,query)
  ## new offset
  q_offset <- q_offset + chunk_work
  
  # write data to table
  RMySQL::dbWriteTable(mysql,name = to_table, value = data_chunk, append = T, overwrite = F, row.names = F)
  print(paste0('Iteration 1 :: Uploaded'))
  
  # rest of iterations
  for (i in 2:iterations) {
    # read first chunk from db
    ## create sql-statement
    query <- paste0('SELECT * FROM ', from_table, if(!is.na(where)) {paste0(' ', where)},' OFFSET ', q_offset, ' Limit ', chunk_work)
    ## feedback to console
    print(paste0('Iteration ',i,' :: Downloaded: ',query))
    ## new offset
    q_offset <- q_offset + chunk_work
    ## get data
    data_chunk <- RPostgreSQL::dbGetQuery(redshift,query)
    
    # write data to table
    RMySQL::dbWriteTable(mysql,name = to_table, value = data_chunk, append = T, overwrite = F, row.names = F)
    print(paste0('Iteration ',i,' :: Uploaded'))
  }
  print("Process finished!")
}