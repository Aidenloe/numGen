#' @export
#' @import "stats"
#' @param x To get the length value
#' @param items The number of items you have to generate.
#' @description This uses item model 1 to create number series items.
#' @details counting the alphabets representation.
#' @author Aiden Loe and Filip Simonfy, \email{bsl28@@cam.ac.uk}
#' @title Item Model 1
#' @examples \dontrun{
#'
#' nmOne(3,26)
#'
#' }

# count (number representation)
# 1.1 single object: x x x x ?

# create vector containing all alphabet characters
# transform into a matrix column

# create bank of items
# each item contains x characters

nmOne <- function(x,items) {
  if(items>26){
    stop("Number of items must less than 27.")
  }
  alphabet <- LETTERS[seq(1:items)]
  column <- matrix(alphabet, nrow = items, ncol = 1)
  bank_11 <- matrix(nrow = items, ncol = (x+1))
  bank_11[ ,c(1:x)] <- column
  colnames(bank_11) <- colnames(bank_11, do.NULL = FALSE, prefix = "Q")
  rownames(bank_11) <- rownames(bank_11, do.NULL = FALSE, prefix = "item")

  bank_11[ ,(x+1)] <- x
  colnames(bank_11)[x+1] <- "A"

  return(bank_11)
}


# bank of items of 4, 5, 6 characters

# bank_11_4 <- model_11(4)
# bank_11_5 <- model_11(5)
# bank_11_6 <- model_11(6)

# multiple choice answer
# NOT YET DONE

