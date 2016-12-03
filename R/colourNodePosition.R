#' @export
#' @import igraph
#' @param Rank This is the Rank of the maze.
#' @param satPercent Percentage of saturation.
#' @param setseed. To always get the same position.
#' @details Returns the colour node position. You need to use the node position function first.
#' @description This function will not sample from the first node position. If you consider sampling from the first node, then in javascript, the summing of the black dotes need to begin from 1 rather than 0. To keep it simple, always ensure that the first node is not sampled as a black dot.
#' @author Aiden Loe
#' @title colourNodePosition
#' @examples \dontrun{
#'
#' colourNodePosition(rank=3,satPercent=0.5,seed=1)
#'
#' }

#### Colour Node Position #####
colourNodePosition<- function(rank, satPercent, seed){
saturation<- ceiling(length(lowerGrid(rank))*satPercent)
set.seed(seed)
nodePosition <- sample(lowerGrid(rank)[-1], saturation, replace=FALSE)
return(nodePosition)
}



