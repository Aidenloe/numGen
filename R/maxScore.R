#' @export
#' @import igraph
#' @param rank This is the Rank of the maze.
#' @details The maxScore function returns the maximum score for a given rank and a given node Position. You need to use the colour node position function first.
#' @author Aiden Loe
#' @title Maximum Score

maxScore <- function(rank,nodePosition){
#### Lower Grid Maze Nodes ####
G <- graph(genMaze(rank), directed = TRUE )
lowerGridCombind<- lowerGrid(rank) # lower grid nodes

#### Calculate all Path ####
allPaths <- all_simple_paths(G, 1,lowerGrid(rank))

#### max colour gives you the points for every route on a black dot ####
maxColour <- NULL
for(i in 1:length(allPaths)){
  maxColour[[i]] <- ifelse(as.numeric(allPaths[[i]]) %in% nodePosition,1,0)
}

#### summing up the total score ####
totalScore <- NULL
for(i in 1:length(allPaths)){
  totalScore[i]<- sum(maxColour[[i]])
}

totalScore.df <- as.data.frame(totalScore)
index <- 1:nrow(totalScore.df)
totalScore.df.1<- cbind.data.frame(index,totalScore.df)
optimisedScore <- totalScore.df.1[which(totalScore.df.1$totalScore == max(totalScore.df.1$totalScore, na.rm = TRUE)), ]
return(optimisedScore)
}




