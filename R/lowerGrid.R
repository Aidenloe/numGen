#' @export
#' @param rank This is the Rank of the maze.
#' @description This tells you all the nodes position in the lower grid.
#' @details returns the nodes positioned at the lower grid
#' @author Aiden Loe
#' @title lowerGrid

#returns the lower grid
lowerGrid <-function(rank){
  M<-c()
  for (j in 1:rank){
    k<-rank-j
    M<-c(M,((j-1)*rank+1):((j*rank)-(j-1)))
  }
return(M)
}



