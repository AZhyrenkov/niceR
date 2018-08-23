write.spreadsheet <- function(data, token, ss_key, sheet, cell, last_rows_path, col_names = F, verbose = T){

  require(googlesheets)
  require(dplyr)
  require(stringr)
  require(foreach)

  sheet <-  as.character(sheet)

  # auth by token
  gs_auth(token = token)

  # initialize spreadsheet object
  ss <- gs_key(ss_key)

  # improove writing function for splitting things by columns
  gs_write_table <- function(ss, ws, df, cell, col_names = T, verbose = T){

    upper_letters <- toupper(letters)
    start_letter_symbol <- str_extract(cell,'\\D')
    start_letter_number <- match(start_letter_symbol, upper_letters)

    start_number <- str_extract(cell,'\\d')

    end_letter_number <- length(colnames(data)) + start_letter_number - 1
    end_letter_symbol <- upper_letters[end_letter_number]

    col_area_numbers <- start_letter_number:end_letter_number
    col_area_letters <- upper_letters[col_area_numbers]

    system.time({
      for(i in 1:length(colnames(df))){
        col <- colnames(df)[i]
        anchor <- paste0(col_area_letters[i],start_number)
        gs_edit_cells(ss = ss, ws = sheet, input = as.vector(df[, col]), anchor = anchor, col_names = col_names, verbose = verbose, byrow = F)
      }
    })
  }

  # catching last rows for cleaning area before
  if (file.exists(last_rows_path)){
    last_rows <- suppressWarnings(as.numeric(readLines(last_rows_path)))
  } else{
    last_rows <- nrow(data)
  }
  last_rows <- max(last_rows,nrow(data))

  # generate empty dataset for cleaning
  empty_df <- as.data.frame(matrix('', nrow = last_rows, ncol = length(colnames(data))))


  # clean table
  print(paste0('Cleaning started, size: ', last_rows , '*', length(colnames(data))))
  suppressMessages(suppressWarnings(gs_write_table(ss = ss, ws = sheet, df = empty_df, cell = cell)))
  print('Cleaning ended!')

  # write data
  print('Writing started...')
  gs_write_table(ss = ss, ws = sheet, df = data, cell = cell)
  print('Writing ended!')

  # write lastrows
  cat(nrow(data), file = last_rows_path)
}
