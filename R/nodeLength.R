#' @export
#' @param R This is the Rank of the maze.
#' @details This function tells us the nodes on the longest row given a rank.
#' This needs to have a rank value of greater than 1.
#' @author Aiden Loe
#' @title Node Length

# Longest row
#lengthOfLongestRow <- rank
nodeLength <- function(rank){
nodeLength <- NULL
nodeLength[[1]] <- rank
for(i in 1:(rank-1))
{
  nodeLength[[i+1]]<- nodeLength[[i]] + rank-1
}
return(nodeLength)
}

