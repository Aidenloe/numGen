

estimate <- function(rank, nodePosition){


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

### POSSBLE BLACK NODE ROUTES ####
totalScore.df <- as.data.frame(totalScore)
index <- 1:nrow(totalScore.df)
totalScore.df.1<- cbind.data.frame(index,totalScore.df)
n<-nrow(totalScore.df.1)
M<-matrix(unlist(totalScore.df.1),ncol=n,byrow=TRUE)

LL<-c()
for (j in 1:length(totalScore)){
  M<-matrix(unlist(totalScore.df.1),ncol=n,byrow=TRUE)
  N<-unlist(maxColour[M[1,j]])
  LL<-c(LL,length(N)) #length of every route
}

# We only want routes that is the same as the rank
W<-which( LL == rank)
endScore <- totalScore.df.1[which(totalScore.df.1$index %in% W),]
possibleBlackNodeRoutes<- table(endScore$totalScore)


#number of steps & optimal paths
# totalScore.df <- as.data.frame(totalScore)
# index <- 1:nrow(totalScore.df)
# totalScore.df.1<- cbind.data.frame(index,totalScore.df)
maxScore <- totalScore.df.1[which(totalScore.df.1$totalScore == max(totalScore.df.1$totalScore, na.rm = TRUE)), ]

n<-nrow(maxScore)
LL<-c()
for (j in 1:n){
  M<-matrix(unlist(maxScore),ncol=n,byrow=TRUE)
  N<-unlist(maxColour[M[1,j]])
  LL<-c(LL,length(N))
}

#print("the minimum number of steps for the optimal solution is: ")
minStep <- (min(LL)-1)

# MINIMUM LEG ROUTES ####
#print("The optimium path(s) with minimum legs is: ")
W<-which( LL == min(LL))

allminPath <- allPaths[M[1,W]]

#print("the minimum number of steps for the optimal solution is: ")
#print(min(LL)-1)
#print(("the number of solutions is: "))
minLegRoutes <- length(W)


#### ALL POSSIBLE PATH ####
#print("The optimal paths are: ")
allPossiblePaths <-which( LL == rank) #only select those that reaches to the top
allPath <- allPaths[M[1,allPossiblePaths]]
#print(("the number of solutions is: "))
legRoutes <- length(allPath)






est <- list(maxScore=maxScore,
            possibleBlackNodeRoutes=possibleBlackNodeRoutes,
            minStep=minStep,
            minPath = list(allminPath = allminPath,
                           minLegRoutes=minLegRoutes),
            allPP = list(allPath = allPath,
                           legRoutes=legRoutes)
     )
return(est)
}


c <- estimate(rank,a)

