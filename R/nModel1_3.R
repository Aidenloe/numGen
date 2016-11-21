
#' @export
#' @param cat Length of categorical groups per question.
#' @param items The number of items you want to generate.
#' @param random To randomise the position of the numeric values.
#' @details Identification of the number series follows a group pattern recognition rule.
#' @description This is based on the categorical / pattern recognition rule.
#' @author Aiden Loe and Filip Simonfy
#' @title Group Categorisation
#' @examples \dontrun{
#'
#' nmCat(cat=2,items=4,random=FALSE)
#'
#' }

# categorization (1	1	1	5	5	?)
nmCat <- function(cat, items ,random=FALSE ){
  if(cat==1)
  stop("Please choose a 'cat' value greater than 1")

  #create columns
  bank_cat_6 <- matrix(ncol=cat^2)
  colnames(bank_cat_6) <- colnames(bank_cat_6, do.NULL = FALSE, prefix = "Q")
  colnames(bank_cat_6)[cat^2] <- "A"

    #create items
    for (i in 1:items) {
      item <- NULL
      objects <- sample(seq(1:99), cat, replace = FALSE) #random sample
      #generate number of category
        for(i in 1:length(objects)){
          item <-   c(item,rep(objects[i],cat))
         }
      bank_cat_6 <- rbind(bank_cat_6, item)
      bank_cat_6 <- na.omit(bank_cat_6)
    }

    if(random==TRUE){
      bank_noorder_6 <- matrix(ncol=cat^2)
        for (i in 1:nrow(bank_cat_6)) {
          item <- bank_cat_6[i, sample(1:cat^2, cat^2, replace = FALSE)]
          bank_noorder_6 <- rbind(bank_noorder_6, item)
          bank_noorder_6 <- na.omit(bank_noorder_6)
        }
      colnames(bank_noorder_6) <- colnames(bank_cat_6, do.NULL = FALSE, prefix = "Q")
      colnames(bank_noorder_6)[cat^2] <- "A"
      return(bank_noorder_6)
    }else{
      return(bank_cat_6)
      }

}






