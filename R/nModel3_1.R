#' @export
#' @param arith Select the arithmetric operator of choice ("add","multi", "sub", "div").
#' @param n Value you want use the arithmetic operator on.
#' @param items Generate a random mix of items.
#' @details 2 linear sequences (With use of the same arithmetic operator)
#' @description  i.e combination of: sequences x y x y x y x y.
#' The selected arithmetic and the operator will remind the same between the combined sequences.
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 6
#'
#' @examples \dontrun{
#'
#' set.seed(1)
#' nmParaSeq(arithmetic="add",n=2,5)
#'
#' }

## 2+ linear sequences (with use of arithmetic)
# + - x / 2
# same/different operator
# combining sequences x y x y x y x y

# random selection >> creating 1000 items
nmParaSeq<- function(arith="add",n=1,items=2){
  if(arith == "add"){
    bank_list <- nmThree(items,n,arithmetic="add")
  }else if(arith == "substr"){
    bank_list <- nmThree(items,n,arithmetic="substr")
  }else if(arith == "multi"){
    bank_list <- nmThree(items,n,arithmetic="multi")
  }else {
    bank_list <- nmThree(items,n,arithmetic="div")
  }

  #bank_list <- rbind(bank_21add, bank_21multi, bank_21sub, bank_21div)

  bank_31 <- matrix(ncol = 10)
  colnames(bank_31) <- colnames(bank_31, do.NULL = FALSE, prefix = "Q")
  colnames(bank_31)[9:10] <- "A"

  for (i in 1:items) {
    a <- sample(nrow(bank_list), 2, replace = FALSE)
    f <- bank_list[a[1], ]
    g <- bank_list[a[2], ]
    item <- c(f[1], g[1], f[2], g[2], f[3], g[3], f[4], g[4], f[5], g[5])
    bank_31 <- rbind(bank_31, item)
    bank_31 <- na.omit(bank_31)
  }
  return(bank_31)
}



