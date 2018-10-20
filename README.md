# pdftable
Convert a table in a pdf file to a matrix in R

### Convention:
- Row, column: a row and a column in a table or a matrix.
- Line: a line of the pdf page. It is not necessarily a table row. A table row can contain 1 or more lines.

### Usage: (see sample in code file)
1. Provide the file name and the page number where the table is on.
2. Run 
```R
lines <- strsplit(pdf_text(file_name)[page_number], "\n")[[1]]
lines
```
3. Provide indexes of lines of the table to the variable `table_lines_indexes`, get lines of the table by running
```R
table_lines <- lines[table_lines_indexes]
table_lines
```
4. Based on `table_lines`, provide:
  - `num_lines_header`: number of lines of the table header,
  - `col_delims`: a vector of positions of the beginning of each column. Can be obtained by copying table_lines into, for example, Sublime Text, then get positions of first letters of columns,
  - `row_heights`: a vector of heights of all table rows. An empty vector if no confusion can occur,
  - `isAlignedBottom`: whether table is aligned TRUE: bottom, FALSE: top.
5. Run `lines_to_table` or `get_table`.

### Note
Function `lines_to_table` has a parameter `row_heights` that can be an empty vector. In case it is an empty vector, we assume that the first cell of each table row has only 1 line of text, which means the number of non-empty elements in the first column of the matrix is equal to the number of rows, and that each table row ends (if aligned bottom) or starts (if aligned top) with a line that has text in all columns.
So `row_heights` is needed when there might be confusion. Confusion can occur when:
  
* In the first column of the table, some cell is empty or has more than 1 lines of text
* A table row has an empty cell or has more than 1 lines with text in all columns.
