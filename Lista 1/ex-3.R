## Exercício 3

validate_sudoku <- function(board) {
  validate_dimensions <- function(board) {
    return(
      nrow(board) == 9 &&
      ncol(board) == 9
    )
  }
  
  validate_data_type <- function(board) {
    for (row in 1:nrow(board)) {
      for (col in 1:ncol(board)) {
        element <- board[row,col]
        if ((!is.numeric(element)) || element < 1 || element > 9)
          return(FALSE)
      }
    }
    return(TRUE)
  }
  
  validate_rows <- function(board) {
    for (i in 1:9) {
      frequency <- rep(0, times = 9)
      for (j in 1:9) {
        frequency[board[i,j]] <- frequency[board[i,j]] + 1
        if (frequency[ board[i,j] ] > 1) {
          return(FALSE)
        }
      }
    }
    return(TRUE)
  }
  
  validate_cols <- function(board) {
    for (j in 1:9) {
      frequency <- rep(0, times = 9)
      for (i in 1:9) {
        frequency[board[i,j]] <- frequency[board[i,j]] + 1
        if (frequency[ board[i,j] ] > 1) {
          return(FALSE)
        }
      }
    }
    return(TRUE)
  }
  
  validate_sections <- function(board) {
    for (offset in 0:2) {
      for (i in (1+3*offset):(3+3*offset)) {
        frequency <- rep(0, times = 9)
        for (offset2 in 0:2) {
          for (j in (1+3*offset2):(3+3*offset2)) {
            frequency[ board[i,j] ] <- frequency[ board[i,j] ] + 1
            if (frequency[ board[i,j] ] > 1) {
              return(FALSE)
            }
          }
        }
      }
    }
    return(TRUE)
  }
  
  return(
    validate_dimensions(board) &&
    validate_data_type(board) &&
    validate_rows(board) &&
    validate_cols(board) &&
    validate_sections(board)
  )
}
