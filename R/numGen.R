#' numGen: A package for generating number series items.
#'
#' The numGen package provides 14 item models for generating number series items
#'
#'
#' @section Item model 1:
#' This number series counts the alphabets representation.\cr
#' \code{\link{nmOne}}
#'
#' @section Item model 2:
#' This number series counts the alphabets representation, but contains 2 letter groups of different length.\cr
#' \code{\link{nmTwo}}
#'
#' @section Item model 3:
#' There are three functions to this item model. \cr
#' \code{\link{nmAdd}} follows a sequence succession rule (numbers). \cr
#' \code{\link{nmAlpha}} follows a sequence succession rule (alphabets) \cr
#' \code{\link{nmThree}}  allows you to select one of the four arithmetic operators following a sequence succession rule.
#'
#' @section Item model 4 and 5:
#' This create items that relates to comprehension of abstract object representation (Item model 5) and Identification of co-occurring relationships between elements (Item model 4).\cr
#' \code{\link{nmCombo}}
#'
#' @section Item model 6:
#' Generate items with two sequences combined into one number series.\cr
#' \code{\link{nmParaSeq}}
#'
#' @section Item model 7:
#' This model uses the addition and substraction (Arithmetic) operator, Linear pattern and Progressive coefficient to create the number series.\cr
#' \code{\link{nmSeven}}
#'
#' @section Item model 8:
#' This function creates number series that is a combination of Arithmetic, Linear and Complex coefficient. \cr
#' First logic of complex coefficient = i*x+y.\cr
#' Second logic of complex coefficient = (i+x)*y.\cr
#'  \code{\link{nmProgress}}
#'
#'  @section Item model 9:
#' This is based on the categorical / pattern recognition rule. Neighbouring pairs or triads of objects are related, includes arithmetic operations. \cr
#' \code{\link{nmNeighbour}}
#'
#'  @section Item model 10:
#' This function creates Fibonacci sequences. The maximum number to be generated is 15 items.\cr
#' \code{\link{nmFib}}
#'
#'@section Item model 11:
#' The number series is a combination of Arithmetic, linear sequence and progressive coefficient. \cr
#' First logic is combining sequences x y x y x y x y = one simple, one progressive. \cr
#' Second logic is combining sequences x y x y x y x y = two progressive. \cr
#'  \code{\link{nmEleven}}
#'
#'@section Item model 12:
#' Neighbouring objects + 2-sequence coefficient. \cr
#' This function creates number series that is a combination of Neighbouring objects + 2-sequence coefficient. \cr
#' Multiplication and Division is removed since the calculated value is too big. \cr
#'  \code{\link{nmTwelve}}
#'
#'  @section Item model 13:
#' This function creates number series that is a irregular combination of sequences a b b a b b a ... \cr
#' Only the addition and substraction arithmetic operators are used to create the number series items. \cr
#'  \code{\link{nmIrregular}}
#'
#' @section Item model 14:
#' Combination of sequences and ratios. \cr
#' \code{\link{nmSeqRatio}}
#' @docType package
#' @name numGen
NULL
