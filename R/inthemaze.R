
#' @export
#' @param R This is the Rank of the maze.
#' @details returns the nodes positioned at the lower grid
#' @author Aiden Loe
#' @title lowerGrid

#returns the lower grid
lowerGrid <-function(R){
  M<-c()
  for (j in 1:R){
    k<-R-j
    M<-c(M,((j-1)*R+1):((j*R)-(j-1)))
  }
return(M)
}



