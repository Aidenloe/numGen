#' @export
#' @param x To get the length value
#' @param items The number of items you have to generate.
#' @param random The alphabets are not in order.
#' @details counting the alphabets representation.
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 2
#' @description
#' This function will only return two letter groups of different length.
#'
#' @examples \dontrun{
#'
#' nmTwo(3,6, random=FALSE)
#' }

# 1.2 two objects: y y x x x ? (2,3)
# creates 100 items with 2 objects of length x

nmTwo <- function(x,items, random=FALSE) {
 # alphabet <- LETTERS[seq(1:26)]
  bank_11b <- matrix(ncol = (x+2))
  colnames(bank_11b) <- colnames(bank_11b, do.NULL = FALSE, prefix = "Q")

  for (i in 1:items) {
    a <- sample(LETTERS, 2, replace = FALSE)

    # might sample all the same letters. We want to have at least sample 1 each letter,     if not reject the sampling.
    b <- sample(a, x, replace=TRUE)

    # always draw at least one
    if(sort(unique(b %in% b[duplicated(b)]))[1] == TRUE){
      b <- sample(a, x, replace=TRUE);
    }

    if(random==FALSE){
    b <- b[order(b)]
    }

    table <- as.data.frame(table(b))
    answer <- c(table[1:2,2])

colnames(bank_11b)[c(x+1, x+2)] <- "A"
    item <- c(b, answer)
    item

    bank_11b <- rbind(bank_11b, item)
    bank_11b <- na.omit(bank_11b)
    bank_11b
  }

  return(bank_11b)
}



# bank of items of 6, 7, 8 characters

# bank_11b_6 <- model_11b(6)
# bank_11b_7 <- model_11b(7)
# bank_11b_8 <- model_11b(8)




