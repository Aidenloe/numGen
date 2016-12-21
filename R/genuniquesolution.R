#' @export
#' @import igraph
#' @param rank This is the Rank of the maze.
#' @param satPercent Percentage of saturation.
#' @param seed Returns the seed that only gives one unique solution for a given rank and saturation.
#' @description The generate unique solution function searches for the SEED that returns only one unique solution for a given rank and saturation.
#' @details The generate unique solution function searches for the SEED that returns only one unique solution for a given rank and saturation. This might be computationally intensive as the maze size increases. The seed is necessary so that the algorithm does not always begin from the smallest value. Based on the seed value, it will search for the next biggest that returns 1 unique solution.
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
  count <- 1
  while (num>1){
    seed<-seed+1
    num<-lookUniqueSolution(rank,satPercent,seed)
    count <- count + 1
    if(count > 300){
      num <- 1
      stop("No unique solution can be found. Please reduce saturation percentage.")
    }
  }
  return(seed)
}

