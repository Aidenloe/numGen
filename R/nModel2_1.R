#' @export
#' @param length This is the length of each item
#' @param items The number of items you want to generate
#' @details This is based on linearity rule / pattern recognition rule.
#' @description This uses item model 3 to create number series items.
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 3 (Addition)
#' @examples \dontrun{
#'
#' nmAdd(4,5)
#'
#' }


nmAdd <- function(length, items){
  bank_lin <- matrix(ncol=length)
  colnames(bank_lin) <- colnames(bank_lin, do.NULL = FALSE, prefix = "Q")
  colnames(bank_lin)[length] <- "A"

# number of items
  firstItem <- NULL
  items <- items-1
for (i in 1:items) {
    #length of items
    for(j in 1:length){
        firstItem[j] =0 + j
     }

  item <- firstItem + i  # +1 to every value in the vector
  bank_lin <- rbind(bank_lin,item)
  bank_lin <- na.omit(bank_lin)
}

  bank_lin <- rbind(firstItem, bank_lin) #add the first vector to the matrix
  rownames(bank_lin) <- NULL #remove the row names
  bank_lin <- as.data.frame(bank_lin)
  return(bank_lin)
}




#' @export
#' @param items The number of items you want to generate
#' @param reverse Changing reverse to TRUE will flip to direction of the progressive alphabets
#' @details This is based on linearity rule / pattern recognition rule
#' @description This uses item model 3 to create letter series items.
#' This is for alphabets and is restricted to 21 items.
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 3 (Alpha)
#' @examples \dontrun{
#'
#' nmAlpha(3,reverse=TRUE)
#'
#' }


# linearity using alphabet
nmAlpha <- function(items, reverse=FALSE){
  #   if(length > 26){
  #     print("please choose a smaller length value")
  #   }
  if(items > 21){
    stop("please choose an item value of < 21")
  }

  bank_alpha <- matrix(ncol=6)
  colnames(bank_alpha) <- colnames(bank_alpha, do.NULL = FALSE, prefix = "Q")
  colnames(bank_alpha)[6] <- "A"

  alphabet <- LETTERS[seq(1:26)]
  if(reverse==TRUE){
    value = c(5,4,3,2,1)
    for (i in 1:items) {
      item <- c(alphabet[i+value[1]], alphabet[i+value[2]], alphabet[i+value[3]], alphabet[i+value[4]], alphabet[i+value[5]], alphabet[i])
      bank_alpha <- rbind(bank_alpha, item)
      bank_alpha <- na.omit(bank_alpha)
    }
  }else{
    for (i in 1:items) {
      item <- c(alphabet[i], alphabet[i+1], alphabet[i+2], alphabet[i+3], alphabet[i+4], alphabet[i+5])
      bank_alpha <- rbind(bank_alpha, item)
      bank_alpha <- na.omit(bank_alpha)
    }
  }
  return(bank_alpha)
}




#' @export
#' @param items The number of items you want to generate
#' @param n Value you want use the arithmetic operator on
#' @param arithmetic Use either 'add', 'substr', 'multi', 'div'.
#' @details This is based on linearity rule / pattern recognition rule and also arithmetic operators. Currently it only displays up to a series of 9.
#' @description This uses item model 3 to create number series items.
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 3 (Arithmetic)
#' @examples \dontrun{
#'
#' nmThree(items=4,n=2,arithmetic="add")
#'
#' }


nmThree <- function(items,n, arithmetic){
  stopifnot(arithmetic =="add" || arithmetic =="multi" || arithmetic =="substr"  || arithmetic =="div")


  bank <- matrix(ncol=9)
  colnames(bank) <- colnames(bank, do.NULL = FALSE, prefix = "Q")
  colnames(bank)[9] <- "A"
  if(arithmetic== "add") {
    for(i in 1:items) {
      item <- c(i, i+n, i+2*n, i+3*n, i+4*n, i+5*n, i+6*n, i+7*n, i+8*n)
      bank <- rbind(bank, item)
      bank <- na.omit(bank)
    }

  }else if(arithmetic== "multi") {
    for(i in 1:items) {
      item <- c(i, i*n, i*n^2, i*n^3, i*n^4, i*n^5, i*n^6, i*n^7, i*n^8)
      bank <- rbind(bank, item)
      bank <- na.omit(bank)
      }
    }else
      if(arithmetic== "substr") {
    for(i in 1:items) {
      item <- c(i, i+n, i+2*n, i+3*n, i+4*n, i+5*n, i+6*n, i+7*n, i+8*n)
      bank <- rbind(bank, item)
      bank <- na.omit(bank)
    }
    bank <- bank[,ncol(bank):1]
    bank <- na.omit(bank)
    colnames(bank) <- c("Q1", "Q2", "Q3", "Q4", "Q5","6","7","8", "A")
  }else{
    for(i in 1:items) {
      item <- c(i, i*n, i*n^2, i*n^3, i*n^4, i*n^5, i*n^6, i*n^7, i*n^8)
      bank <- rbind(bank, item)
      bank <- na.omit(bank)
    }
    bank <- bank[,ncol(bank):1]
    bank <- na.omit(bank)
    colnames(bank) <- c("Q1", "Q2", "Q3", "Q4", "Q5","6","7","8", "A")
  }

  return(bank)
}



