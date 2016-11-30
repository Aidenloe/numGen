#' @export
#' @param items Number of items to generate.
#' @details This function creates Fibonacci sequences. The maximum number to be generated is 15 items.
#' @description  This uses item model 10 to create number series items.
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 10
#' @examples \dontrun{
#'
#' nmTen(items=3)
#'
#' }


# generates first 20 numbers of the sequence

nmTen <- function(items){
  if(missing(items)){
    stop("Please include x number of items to generate")
  }
  if(items > 15){
    stop("Please select less than 16 items.")
  }

fib <- matrix(c(1,1), ncol=1)
x <- NULL

for (i in 1:18) {
  x[i] <- c(fib[i,1] + fib[(i+1),1])
  fib <- rbind(fib, x[i])
}

vector.fib <- fib[ ,1]
bank_fib <- matrix(ncol=6)
colnames(bank_fib) <- colnames(bank_fib, do.NULL = FALSE, prefix = "Q")
colnames(bank_fib)[6] <- "A"

for (i in 1:items) {
  item <- c(vector.fib[i:(i+5)])
  bank_fib <- rbind(bank_fib, item)
  bank_fib <- na.omit(bank_fib)
}

return(bank_fib)
}

