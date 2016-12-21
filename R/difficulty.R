#' @export
#' @param rank This is the Rank of the maze.
#' @param satPercent This is the saturation
#' @param nodePosition This is the distribution of the colour node positions
#' @description This function tells us the difficulty level of the rank given a saturation and black node distribution
#' @details This function tells us the difficulty level of the rank given a saturation and black node distribution. The calculation of the difficulty level follows the paper at XXXX. There are however, more than one way to calculate difficulty. We followed this because it was incorporated all the possible parameters of the task features that may potentially influence item difficulty.
#' @author Aiden Loe and Maria Sanchez
#' @title Maze Diffculty
#' @examples \dontrun{
#'
#' rank <- 5
#' satPercent <- 0.5
#'
#' #Black nodes distribution
#' nodePosition <- colourNodePosition(rank,satPercent,1)
#'
#' #calculate difficulty
#' diff(rank,satPercent ,nodePosition)
#' }


mazeDiff <- function(rank, satPercent, nodePosition){#
  u_Mhat <-nrow(maxScore(rank,nodePosition)) # number of optimised routes
  legs <- minStep(rank,nodePosition)
  diff<- log((2^rank * satPercent^4 * legs^4)/u_Mhat)
  return(diff)
}



