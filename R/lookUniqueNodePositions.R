#' @export
#' @import igraph
#' @param Seed Returns a unique node distribution specific to the local computer.
#' @details The function returns a black nodes distribution with a single unique solution + seed number
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
#' #Black nodes distribution
#' nodePosition <- colourNodePosition(rank,satPercent,justOne)
#'
#' #Compare to the Solution function
#' solution(rank,satPercent, nodePosition)
#' }

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




