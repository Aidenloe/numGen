#' @export
#' @param R This is the Rank of the maze.
#' @details The Genmaze function generates the list of edges
#' @author Aiden Loe
#' @title genMaze


genMaze<- function(R){
  lista<-c()
  RR<-as.numeric(R)^2
  for (i in 1:RR){
    col<-colleage(i,R)
    k<-length(colleage(i,R))
    for (j in 1:k){
      t<-c(i,colleage(i,R)[j])
      lista<-c(lista,t)
      }
  }
  A<-length(lista)
  AA<-A-2
  return(lista[1:AA])
}



