#' @export
#'
print.est <-function(x,...){
  est <- x
  cat("Summary:")
  cat(paste0("\n The Maximum Score(dots) that can be achieved in this maze is ",est$maxScore, ".\n"))
  cat("\n The number of separate paths across the different number of dots is: \n")
  cat('\n')
  print(est$possibleBlackNodeRoutes)
  #print(allPath)
  cat(paste0("\n The minimum number of steps to achieve maximum score is ", est$minStep, ".\n"))
  cat(paste0("\n The maximum number of solution for this maze is ", est$allPP$maxScoreRoutes, ".\n"))
}



