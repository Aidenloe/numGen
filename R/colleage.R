#' @export
#' @param n This is the node number.
#' @param R This is the Rank of the maze.
#' @details This function tells us which nodes are connected to the node you are interested in based on a given Grid.
#' @author Aiden Loe
#' @title colleage

colleage <- function(n,R){
  a<-n+1
  b<-n+R
  c<-R*(R-1)
  d<-c(a,b)
  if(n>c)
    d<-a
  if (n%%R==0)
    d<-b
  if (n==R^2)
    d<-c()
  return(d)
}

