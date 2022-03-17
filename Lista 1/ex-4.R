## Exercício 4

solve_sudoku <- function(board) {
  find_missing_position <- function(board) {
    for (row in 1:nrow(board)) {
      for (col in 1:ncol(board)) {
        element <- board[row,col]
        if (element == -1)
          return(c(row, col))
      }
    }
    return(NULL)
  }
  
  find_missing_number <- function(board, row, col) {
    # find in row
    frequency <- rep(0, times = 9)
    for (j in 1:9) {
      frequency[board[row,j]] <- frequency[board[row,j]] + 1
    }
    missing_in_row <- -1
    for (i in 1:9) {
      if (frequency[i] == 0) {
        missing_in_row <- i
        break
      }
    }
    
    # find in col
    frequency <- rep(0, times = 9)
    for (i in 1:9) {
      frequency[board[i,col]] <- frequency[board[i,col]] + 1
    }
    missing_in_col <- -1
    for (i in 1:9) {
      if (frequency[i] == 0) {
        missing_in_col <- i
        break
      }
    }
    
    if (missing_in_row != missing_in_col)
      return(-1)
    return(missing_in_col)
  }
  
  pos <- find_missing_position(board)
  if (is.null(pos)) {
    missing_number <- find_missing_number(board, pos[1], pos[2])
    board[pos] <- missing_number 
  }
  return(board)
}
