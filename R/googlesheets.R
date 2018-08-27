write.spreadsheet <- function(data, token, ss_key, sheet, cell, last_rows_path, col_names = T, verbose = T){

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

    require(dplyr)
    upper_letters <- toupper(letters)
    start_letter_symbol <- str_extract(cell,'\\D')
    start_letter_number <- match(start_letter_symbol, upper_letters)

    start_number <- as.numeric(str_extract(cell,'\\d'))

    end_letter_number <- length(colnames(data)) + start_letter_number - 1
    end_letter_symbol <- upper_letters[end_letter_number]

    col_area_numbers <- start_letter_number:end_letter_number
    col_area_letters <- upper_letters[col_area_numbers]

    system.time({
      gs_edit_cells(ss = ss, ws = sheet, input = colnames(df), anchor = cell, col_names = T, byrow = T)
      for(i in 1:nrow(df)){
        row <- slice(df, i)
        anchor <- paste0(start_letter_symbol, i + start_number)
        gs_edit_cells(ss = ss, ws = sheet, input = row, anchor = anchor, col_names = F, byrow = F)
      }
    })
  }

  # cleaning buffer
  cleaning_df <- as.data.frame(matrix('', nrow = 5, ncol = length(colnames(data))))

  print('Cleaning buffer started...')
  suppressMessages(suppressWarnings(gs_write_table(ss = ss, ws = sheet, df = cleaning_df, cell = cell)))
  print('Cleaning buffer ended!')

  # write data
  print('Writing started...')
  gs_write_table(ss = ss, ws = sheet, df = data, cell = cell)
  print('Writing ended!')

  # Cleaning
  # catching last rows for cleaning area before
  if (file.exists(last_rows_path)){
    last_rows <- suppressWarnings(as.numeric(readLines(last_rows_path)))
  } else{
    last_rows <- nrow(data)
  }
  last_rows <- max(last_rows, nrow(data))

  if (last_rows > nrow(data)){

    # generate empty dataset for cleaning

    clean_rows <- last_rows - nrow(data)

    empty_df <- as.data.frame(matrix('', nrow = clean_rows, ncol = length(colnames(data))))

    anchor_number <- as.numeric(str_extract(cell,'\\d')) + nrow(data)
    anchor_letter <- str_extract(cell,'\\D')
    anchor_cleaning <- paste0(anchor_letter, anchor_number)

    # clean table
    print(paste0('Cleaning started, size: ', last_rows , '*', length(colnames(data))))
    suppressMessages(suppressWarnings(gs_write_table(ss = ss, ws = sheet, df = empty_df, cell = anchor_cleaning)))
    print('Cleaning ended!')
  }

  # write lastrows
  cat(nrow(data), file = last_rows_path)
}



gs_clear_column <- function(token, ss_key, sheet, start_cell, rows){

  require(googlesheets)
  require(dplyr)
  require(stringr)
  require(foreach)

  sheet <-  as.character(sheet)

  # auth by token
  gs_auth(token = token)

  # initialize spreadsheet object
  ss <- gs_key(ss_key)

  # generate empty dataset for cleaning
  empty_df <- as.data.frame(matrix('', nrow = rows, ncol = 1))


  # clear column
  gs_edit_cells(ss = ss, ws = sheet, input = empty_df, anchor = start_cell, col_names = F, byrow = F)
}



# testing
