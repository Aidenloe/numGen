#' @export
#' @param rank This is the Rank of the maze.
#' @param nodePosition Tells you all the position of the black dots.
#' @description This returns several results.
#' @details This function calculates the count of all the possible black node routes, the maximum score one can achieve for a given rank of a colour node position, all the minimum routes possible, and all the possible routes.
#' @author Aiden Loe
#' @title Calculate
#' @examples
#' rank <- 10
#' nodePosition <- colourNodePosition(rank=10,satPercent=0.5,seed=16)
#' c <- cal(rank,nodePosition)

cal <- function(rank, nodePosition){

  #rank <- 5
  #nodePosition <- colourNodePosition(rank=5,satPercent=0.5,seed=16)

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
# pbnr<- t(as.matrix(possibleBlackNodeRoutes))
# pbnr <- as.data.frame(pbnr)
# require(stringr)
# names(pbnr)
#
# pbnr$`1`
#
# for(i in names(pbnr)){
#   pbnr[[paste(i, 'length', sep="_")]] <- str_length(pbnr[[i]])
# }
# pbnr[,paste(i, 'length', sep="_")]
# str_length(pbnr[[2]])
#
# colnames(pbnr) <- c("")
# t(as.data.frame(pbnr[2,]))

#number of steps & optimal paths
# totalScore.df <- as.data.frame(totalScore)
# index <- 1:nrow(totalScore.df)
# totalScore.df.1<- cbind.data.frame(index,totalScore.df)

#### MAXIMUM SCORE ####
maxScore <- totalScore.df.1[which(totalScore.df.1$totalScore == max(totalScore.df.1$totalScore, na.rm = TRUE)), ]
n<-nrow(maxScore)
M<-matrix(unlist(maxScore),ncol=n,byrow=TRUE)
maxnu<-M[2,1]

#### MIN STEP ####
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
allminPath<- do.call("rbind",allminPath)
m2 <- 1:nrow(allminPath)
rownames(allminPath) <- rownames(m2, do.NULL = FALSE, prefix = "min.Route.")

#print("the minimum number of steps for the optimal solution is: ")
#print(min(LL)-1)
#print(("the number of solutions is: "))
minLegRoutes <- length(W)


#### ALL POSSIBLE PATH ####
#print("The optimal paths are: ")
allPossiblePaths <-which( LL == rank) #only select those that reaches to the top
allPath <- allPaths[M[1,allPossiblePaths]]
allPath<- do.call("rbind",allPath)
m2 <- 1:nrow(allPath)
rownames(allPath) <- rownames(m2, do.NULL = FALSE, prefix = "pos.Route.")
#print(("the number of solutions is: "))
maxScoreRoutes <- nrow(allPath)

is.null(maxScore)


maxnu<- paste0("The Maximum Score is ",maxnu)
#maxnu <- print.data.frame(maxnu,quote=TRUE,row.names = FALSE)
(maxnu <- as.data.frame(maxnu,quote=TRUE))
row.names(maxnu) <- NULL

cl = match.call()
  est <- list(maxScore=maxnu,
              possibleBlackNodeRoutes=possibleBlackNodeRoutes,
              minStep=minStep,
              minPath = list(allminPath = allminPath,
                             minRoutes=minLegRoutes),
              allPP = list(allPath = allPath,
                           maxScoreRoutes=maxScoreRoutes))

  class(est) <- c("aig", "est")

return(est)

}

#

rank <- 10
satPercent <- 0.5
genUniqueSolution(rank,satPercent,15)
nodePosition <- colourNodePosition(rank=10,satPercent=0.5,seed=16)
c <- cal(rank,nodePosition)
c
