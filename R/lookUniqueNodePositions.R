#' @export
#' @import igraph
#' @param rank This is the Rank of the maze.
#' @param satPercent Percentage of saturation.
#' @param seed Returns a unique node distribution specific to the local computer.
#' @description The function returns a black nodes distribution with a single unique solution and seed number
#' @details The function returns a black nodes distribution with a single unique solution and seed number
#' @author Aiden Loe and Maria Sanchez
#' @title lookUniqueNodePositions
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
#' #Black nodes distribution for single unique solution + seed number
#' lookUniqueNodePositions(justOne)
#'


lookUniqueNodePositions <- function(rank,satPercent, seed){
  num<-lookUniqueSolution(rank,satPercent,seed)
  while (num>1){
    seed<-seed+1
    num<-lookUniqueSolution(rank,satPercent,seed)
  }

  nodePosition <-  colourNodePosition(rank, satPercent,seed)
  listObject <- list(nodePosition,seed)
  names(listObject) <- c("nodePositions","seed")
  return(listObject)
}

satPercent <- 0.5

lookUniqueNodePositions(rank,satPercent=satPercent)

 genUniqueSolution(rank,satPercent,11)
