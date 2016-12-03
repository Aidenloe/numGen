#' @export
#' @import igraph
#' @param R This is the Rank of the maze.
#' @details The unique solution function returns the number of unique series of black dots for a given rank, saturation and seed.
#' @author Aiden Loe and Maria Sanchez
#' @title lookUniqueSolution
#' @examples \dontrun{
#'
#' rank <- 5
#' satPercent <- 0.5
#'
#' #Number of unique solutions
#' lookUniqueSolution(rank,satPercent,1)
#'

#' }


lookUniqueSolution <- function(rank,satPercent,seed){
  ##RETURNS: Black points position, optimal routes, number of optimal routes and number of steps for optimal solution

  set.seed(seed)
  nodePosition <-  colourNodePosition(rank, satPercent,seed)
  #nodePosition <- nodePosition

  ##COMPUTE THE PATHS

  #all of them
  G<-graph(genMaze(rank))
  allPaths<-c()
  allPaths <- all_simple_paths(G,1,lowerGrid(rank))

  #### max colour gives you the points for every route on a black dot ####
  maxColour <- NULL
  for(i in 1:length(allPaths)){
    maxColour[[i]] <- ifelse(as.numeric(allPaths[[i]]) %in% nodePosition,1,0)
  }

  ##allPaths
  #### summing up the total score ####
  totalScore <- NULL
  for(i in 1:length(allPaths)){
    totalScore[i]<- sum(maxColour[[i]])
  }

  totalScore.df <- as.data.frame(totalScore)
  index <- 1:nrow(totalScore.df)
  totalScore.df.1<- cbind.data.frame(index,totalScore.df)
  optimisedScore <- totalScore.df.1[which(totalScore.df.1$totalScore == max(totalScore.df.1$totalScore, na.rm = TRUE)), ]
  n<-nrow(optimisedScore)
  M<-matrix(unlist(optimisedScore),ncol=n,byrow=TRUE)

  maxnu<-M[2,1]

  #number of steps & optimal paths
  LL<-c()

  for (j in 1:n){
    M<-matrix(unlist(optimisedScore),ncol=n,byrow=TRUE)
    N<-unlist(maxColour[M[1,j]])
    LL<-c(LL,length(N))
  }

  W<-which( LL == rank)

allPaths
  P<-matrix(unlist(allPaths[M[1,W]]), nrow = rank) # Different series of connected path
nodePosition
# collapsing the black node positions together
  YY<- NULL
   for (k in 1:length(W)){
     YY<-union(YY, paste(intersect(nodePosition,P[,k]),collapse= ""))}

#  for (k in 1:length(W)){
#    print(intersect(nodePosition,P[,k]))}

 noptimalsolutions<-length(YY)
#  noptimalsolutions<-length(W)
  return(noptimalsolutions)
}





