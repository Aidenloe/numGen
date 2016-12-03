#' @export
#' @param R This is the Rank of the maze.
#' @param nodePosition tells you all the position of the black dots
#' @details This function returns the frequently of paths going through a sequence of black dots
#' @author Aiden Loe and Maria
#' @title blackNodeRoutes

blackNodeRoutes <- function(rank,nodePosition){

  ##COMPUTE THE PATHS
  #all of them
  #allPaths
  G <- graph(genMaze(rank), directed = TRUE )
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

  maxColour
totalScore.df <- as.data.frame(totalScore)
totalScore.df
index <- 1:nrow(totalScore.df)
totalScore.df.1<- cbind.data.frame(index,totalScore.df)
n<-nrow(totalScore.df.1)
M<-matrix(unlist(totalScore.df.1),ncol=n,byrow=TRUE)


#number of steps & optimal paths
LL<-c()

for (j in 1:length(totalScore)){
  M<-matrix(unlist(totalScore.df.1),ncol=n,byrow=TRUE)
  N<-unlist(maxColour[M[1,j]])
  LL<-c(LL,length(N)) #length of every route
}


# We only want routes that is the same as the rank
W<-which( LL == rank)

endScore <- totalScore.df.1[which(totalScore.df.1$index %in% W),]

table(endScore$totalScore)
return(table(endScore$totalScore))
}



