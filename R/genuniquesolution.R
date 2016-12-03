#' @export
#' @import igraph
#' @param R This is the Rank of the maze.
#' @details The generate unique solution function searches for the SEED that returns only one unique solution for a given rank and saturation.
#' @author Aiden Loe and Maria Sanchez
#' @title genUniqueSolution
#' @examples \dontrun{
#'
#' rank <- 5
#' satPercent <- 0.5
#'
#' #Number of unique solutions
#' lookUniqueSolution(rank,satPercent,11)
#'
#' #Searches for just one unique solution
#' justOne <- genUniqueSolution(rank,satPercent,11)
#'
#' }



genUniqueSolution<-function(rank,satPercent,seed){
  num<-lookUniqueSolution(rank,satPercent,seed)
  while (num>1){
    seed<-seed+1
    num<-lookUniqueSolution(rank,satPercent,seed)
  }
  return(seed)
}


