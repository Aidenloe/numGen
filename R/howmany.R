#' @export
#' @param rank This is the Rank of the maze.
#' @param satPercent Percentage of saturation.
#' @description Calculates how many possible variation of black dotes for a given saturation.
#' @details Calculates how many possible variation of black dotes for a given saturation.
#' Does not account for the first node being a black dot. The first node will not be a black dot.
#' @author Aiden Loe
#' @title howMany

howMany <- function(rank,satPercent){
  t<-0
  for (i in 1:rank){
    t<-t + i
  }
  saturation<- ceiling(t*satPercent)
  #print(saturation)
  return(choose(t, saturation))
}

