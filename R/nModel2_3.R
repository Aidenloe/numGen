#' @export
#' @param items Number of items you want to generate.
#' @param seed This gives you the same result again.
#' @details 2 linear sequences (without use of arithmetic)
#' @description  i.e combination of:
# same number / same letter
# simple linear (without arithmetic) 1 2 3 / a b c / 10 20 30
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 4 and 5
#' @examples \dontrun{
#'
#' nmCombo(items=5, seed=5)
#'
#' }

# same letter sequence bank = bank_1
nmCombo<- function(items,seed){
  column <- matrix(LETTERS, nrow = 26, ncol = 1)
  bank_1 <- matrix(nrow = 26, ncol = 5)
  bank_1[ ,c(1:5)] <- column

  # same number sequence bank = bank_2
  bank_2 <- matrix(rep(1:99, 5), byrow = FALSE, ncol = 5)

  # simple linear 'by 1' = bank_3
  bank_3 <- matrix(ncol=5)
  for (i in 1:96) {
    item <- c(i:(i+4))
    bank_3 <- rbind(bank_3, item)
  }
  bank_3
  # simple linear 'by 10' = bank_4
  bank_4 <- matrix(ncol=5)
  sequence_10 <- seq(50)*10
  for (i in 1:(length(sequence_10) - 4)) {
    item <- c(sequence_10[i], sequence_10[i+1], sequence_10[i+2], sequence_10[i+3], sequence_10[i+4])
    bank_4 <- rbind(bank_4, item)
  }

  # simple linear 'alphabet' = bank_5
  bank_5 <- matrix(ncol=5)
  alphabet <- LETTERS[seq(1:26)]
  for (i in 1:(length(alphabet) - 4)) {
    item <- c(alphabet[i], alphabet[i+1], alphabet[i+2], alphabet[i+3], alphabet[i+4])
    bank_5 <- rbind(bank_5, item)
  }

  # combining sequences x y x y x y x y
  bank_list <- list(bank_1, bank_2, bank_3, bank_4, bank_5)

  a <- c(rep(1:5, 5))
  b <- c(a[order(a)])

  combinations <- matrix(ncol=2, nrow=length(a))
  combinations[ ,1] <- b
  combinations[ ,2] <- a

  sub_bank <- matrix(ncol = 10)
  bank_23 <- matrix(ncol = 10)
  colnames(bank_23) <- colnames(bank_23, do.NULL = FALSE, prefix = "Q")
  colnames(bank_23)[9:10] <- "A"


  generate <- function(x, y) {
    bank_x <- bank_list[[x]]
    bank_y <- bank_list[[y]]

    for (i in 1:nrow(bank_x)) {
      for (j in 1:nrow(bank_y)) {
        f <- bank_x[i, ]
        g <- bank_y[j, ]
        item <- c(f[1], g[1], f[2], g[2], f[3], g[3], f[4], g[4], f[5], g[5])
        sub_bank <- rbind(sub_bank, item)
      }
    }
    return(sub_bank)
  }

  for (l in 1:nrow(combinations)) {
    h <- generate(combinations[l,1], combinations[l,2])
    bank_23 <- rbind(bank_23, h)
    bank_23 <- na.omit(bank_23)
  }

  set.seed(seed)
  sample_bank_23 <- bank_23[sample(nrow(bank_23), items, replace=FALSE), ]
  return(sample_bank_23)
}






