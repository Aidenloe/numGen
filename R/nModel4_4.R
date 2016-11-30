#' @export
#' @param items Generate a random mix of items.
#' @param seed Generate a fixed ratio set but not final item set.
#' @details Combination of sequences and ratios. This function creates number series creates a combination of sequences and ratios. The seed generates a fixed ratio set to be combined with sequence but does not fix the final item set. Only the addition and substraction arimethic operators are used to generate the number series items.
#' @description   This uses item model 14 to create number series items.
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 14
#' @examples \dontrun{
#'
#' #Draws 10 items randomly.
#' nmFourteen(10,5)
#'
#' }

# a : b = c : d = e : f
# a + b = x, c + d = x ?

# RUN MODEL 2_1 FIRST
# RUN MODEL 3_4 a, b, & d FIRST
# + linear (below)

nmFourteen<- function(items, seed=1){
  if(missing(items)){
    stop("Please include x number of items to generate")
  }
#Model 2_1
model2_1 <- function(value){
bank_lin <- nmAdd(9,95)
bank_lin <- as.matrix(bank_lin)

#add
add<- NULL
for(i in 1:50){
  add[[i]] <- nmThree(items=19,n=i,arithmetic="add")
}
add <- do.call("rbind", add)

#substruct
sub<- NULL
for(i in 1:25){
  sub[[i]] <- nmThree(items=19,n=i,arithmetic="substr")
}
sub <- do.call("rbind", sub)

#multi
# multi<- NULL
# for(i in 2:50){
#   multi[[i]] <- nmThree(items=19,n=i,arithmetic="multi")
# }
# multi <- do.call("rbind", multi)
# multi <- subset(multi, multi[,6] < 500)

#division
# div<- NULL
# for(i in 2:50){
#   div[[i]] <- nmThree(items=100,n=i,arithmetic="div")
# }
# div <- do.call("rbind", div)
# div <- subset(div, div[,1] < 500)

#combind all together
#bank_list <- rbind(bank_lin, add, multi, sub, div)
bank_list <- rbind(bank_lin, add, sub)
bank_seq <- na.omit(bank_list)
return(bank_seq)
}

bank_seq <- model2_1(100) # always 100

#Model 3_4
bank_34 <- nmRatios(10, combo="one",seed)
bank_34b <- nmRatios(10, combo="two",seed)
#bank_34d<- nmRatios(10, combo="four",seed)

bank_ratio <- rbind(bank_34, bank_34b)

#combind together
bank_44 <- matrix(ncol=10)
colnames(bank_44) <- colnames(bank_44, do.NULL = FALSE, prefix = "Q")
colnames(bank_44)[9:10] <- "A"

for (i in 1:items) {
  random_seq <- bank_seq[sample(1:nrow(bank_seq), 1), ]
  random_rat <- bank_ratio[sample(1:nrow(bank_ratio), 1), ]
  item <- c(random_seq[1], random_rat[1], random_rat[2], random_seq[2], random_rat[3], random_rat[4], random_seq[3], random_rat[5], random_rat[6], random_seq[4])

  bank_44 <- rbind(bank_44, item)
  bank_44 <- na.omit(bank_44)
}
return(bank_44)
}

