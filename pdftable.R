library(pdftools)

trim <- function(x) gsub("^\\s+|\\s+$", "", x)

# from 1 line to a vector of strings (texts of different columns in that line)
line_to_vector <- function(line, col_delims) {
  vect <- vector(length = length(col_delims))
  for (delim_i in 1:(length(col_delims)-1))
    vect[delim_i] <- substring(line, col_delims[delim_i], col_delims[delim_i+1]-1)
  vect[delim_i+1] <- substring(line, col_delims[delim_i+1])
  return(trim(vect))
}

# from lines to a matrix, each matrix row contains strings in 1 line and each matrix column corresponds to a table column
lines_to_matrix <- function(table_lines, col_delims) {
  mtx <- matrix(nrow = length(table_lines), ncol = length(col_delims))
  for (i in 1:length(table_lines))
    mtx[i,] <- line_to_vector(table_lines[i], col_delims)
  return(mtx)
}

# check if there is any empty string in a row of a matrix
is_there_any_empty_string <- function(mtx, row_i) {
  for (j in 1:ncol(mtx)) {
    if (mtx[row_i, j] == "")
      return(TRUE)
  }
  return(FALSE)
}

# from matrix rows (lines) to a row of the table, for the case when multiple lines correspond to a table row
matrix_rows_to_row <- function(mtx, idx_in_mtx, row_height, isAlignedBottom) {
  max_idx_in_mtx <- idx_in_mtx + row_height - 1
  the_row <- mtx[idx_in_mtx,]
  idx_in_mtx <- idx_in_mtx + 1
  # if the table is aligned bottom, concatenate non-empty upper strings to the bottom one
  if (isAlignedBottom) {
    while (idx_in_mtx <= max_idx_in_mtx) {
      for (j in 1:ncol(mtx))
        if (the_row[j] != "")
          the_row[j] <- paste(the_row[j], mtx[idx_in_mtx,j])
        else
          the_row[j] <- paste0(the_row[j], mtx[idx_in_mtx,j])
      idx_in_mtx <- idx_in_mtx + 1
    }
  } else { # if the table is aligned top, concatenate non-empty lower strings to the top one
    while (idx_in_mtx <= max_idx_in_mtx) {
      for (j in 1:ncol(mtx))
        if (mtx[idx_in_mtx,j] != "")
          the_row[j] <- paste(the_row[j], mtx[idx_in_mtx,j])
        else
          the_row[j] <- paste0(the_row[j], mtx[idx_in_mtx,j])
      idx_in_mtx <- idx_in_mtx + 1
    }
  }
  return(the_row)
}

# from lines to a table (of type matrix)
lines_to_table <- function(table_lines, num_lines_header, col_delims, row_heights, isAlignedBottom) {
  mtx <- lines_to_matrix(table_lines, col_delims)
  # In case row_heights is not provided, assume that the first cell of each table row has only 1 line of text,
  #which means the number of non-empty elements in the first column of the matrix is equal to the number of rows,
  #and that each table row ends (if aligned bottom) or starts (if aligned top) with a line that has text in all columns.
  # The term +1 is for the header row.
  # row_heights is needed when there might be errors.
  # Errors: when in the first column of the table, some cell is empty or has more than 1 lines of text,
  #      or when a table row has an empty cell or has more than 1 lines with text in all columns.
  if (length(row_heights) != 0)
    the_table <- matrix(nrow = length(row_heights) + 1, ncol = length(col_delims))
  else
    the_table <- matrix(nrow = nrow(mtx[mtx[,1] != "",]) + 1, ncol = length(col_delims))
  
  # header row
  the_table[1,] <- matrix_rows_to_row(mtx, 1, num_lines_header, isAlignedBottom)
  mtx_i <- 1 + num_lines_header
  
  # body rows
  table_i <- 2
  if (length(row_heights) != 0) {
    while (table_i - 1 <= length(row_heights)) {
      the_table[table_i,] <- matrix_rows_to_row(mtx, mtx_i, row_heights[table_i - 1], isAlignedBottom)
      mtx_i <- mtx_i + row_heights[table_i - 1]
      table_i <- table_i + 1
    }
  } else {
    if (isAlignedBottom) {
      wait_for_more <- FALSE
      while (mtx_i <= length(table_lines)) {
        if (!is_there_any_empty_string(mtx, mtx_i)) {
          if (!wait_for_more) {
            the_table[table_i,] <- mtx[mtx_i,]
          } else {
            the_table[table_i,] <- trim(paste(the_table[table_i,], mtx[mtx_i,]))
            wait_for_more <- FALSE
          }
          table_i <- table_i + 1
          # stop waiting for more and go to the next row when there is a line with no empty string
          # it may be wrong here
        } else {
          if (!wait_for_more) {
            the_table[table_i,] <- mtx[mtx_i,]
            wait_for_more <- TRUE
          } else {
            the_table[table_i,] <- trim(paste(the_table[table_i,], mtx[mtx_i,]))
          }
        }
        mtx_i <- mtx_i + 1
      }
    } else {
      the_table[table_i,] <- mtx[mtx_i,]
      mtx_i <- mtx_i + 1
      while (mtx_i <= length(table_lines)) {
        if (!is_there_any_empty_string(mtx, mtx_i)) {
          table_i <- table_i + 1
          the_table[table_i,] <- mtx[mtx_i,]
          # stop waiting for more and go to the next row when there is a line with no empty string
          # it may be wrong here
        } else {
          the_table[table_i,] <- trim(paste(the_table[table_i,], mtx[mtx_i,]))
        }
        mtx_i <- mtx_i + 1
      }
    }
  }
  
  if (table_i < nrow(the_table))
    the_table <- the_table[1:table_i,]
  
  return(the_table)
}

get_table <- function(file_name, page_number, table_lines_indexes, num_lines_header, col_delims, row_heights, isAlignedBottom) {
  # ? which file
  text <- pdf_text(file_name)
  # ? which page
  page <- text[page_number]
  
  lines <- strsplit(page, "\n")[[1]]
  
  # ? where the table is
  table_lines <- lines[table_lines_indexes]

  return(lines_to_table(table_lines, num_lines_header, col_delims, row_heights, isAlignedBottom))
}

####
# Usage
####
# Give me the file name and the page number where the table is on
file_name <- "report.pdf"
page_number <- 5

# I get you all lines of the page
lines <- strsplit(pdf_text(file_name)[page_number], "\n")[[1]]
lines

# Give me indexes of lines of the table
table_lines_indexes <- 3:17

# I get you lines of the table
table_lines <- lines[table_lines_indexes]
table_lines

# Give me
num_lines_header <- 2 # number of lines of the table header
# col_delims can be obtained by copying table_lines into, for example, Sublime Text, then get positions of first letters of columns
col_delims <- c(1,13,25,37,50,67) # positions of the beginning of each column
row_heights <- c() # heights of all table rows. An empty vector if no confusion can occur
isAlignedBottom <- TRUE # whether table is aligned TRUE: bottom, FALSE: top

# I get you the table
the_table <- lines_to_table(table_lines, num_lines_header, col_delims, row_heights, isAlignedBottom)
the_table

# Or you can use this function
the_table_2 <- get_table(file_name, page_number, table_lines_indexes, num_lines_header, col_delims, row_heights, isAlignedBottom)
the_table_2
