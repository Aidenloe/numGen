#' @export
#' @param numLetter Number of Letters you want.
#' @param value Value + numLetter gives you back the total number of letters.
#' @param items Number of items to generate.
#' @param random If random=FALSE, the items will follow in sequential order.
#' @details Returns the number representation which is the count of the specific letters.
#' @description This function tells you the count of a specific letter.
#' @author Aiden Loe
#' @title Letter Representation
#' @examples \dontrun{
#'
#' nmRepresent(numLetter= 2 ,value=3, items=10, random=FALSE)
#'
#' }

# number representation + relationship between objects
# with order a 1 b b b 3 c c
nmRepresent <- function(numLetter,value, items, random=FALSE){
  bank_rep <- matrix(ncol = value+numLetter*2)
  bank_rep
  colnames(bank_rep) <- colnames(bank_rep, do.NULL = FALSE, prefix = "Q")
  colnames(bank_rep)[value+numLetter*2] <- "A"
for(i in 1:items) {
  a <- sample(LETTERS, numLetter, replace = FALSE)
  b <- c(a, unlist(sample(a, value, replace = TRUE)))
  table <- as.data.frame(table(b))
  table[ ,1] <- as.character(table[ ,1])
  element <- NULL
  for(j in 1:numLetter){
  element <- c(element, c((rep(table[j,1], c(table[j,2]))), table[j,2]))
  }
  if(random==TRUE){
    element <- sample(element, length(element), replace = FALSE)
  }
  bank_rep <- rbind(bank_rep, element)
  bank_rep <- na.omit(bank_rep)
}
  return(bank_rep)
}








