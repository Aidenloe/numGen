% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/item_model_3.R
\name{imThree}
\alias{imThree}
\title{Item Model 3}
\usage{
imThree(items, n, arith = "add")
}
\arguments{
\item{items}{The number of items to generate}

\item{n}{Value to use the arithmetic operator on}

\item{arith}{Use either 'add', 'substr', 'multi', 'div'.}
}
\description{
This uses item model 3 to create number series items - Use of basic algebraic skills.
}
\details{
Each element in the sequence is derived from the preceding by applying one of four basic arithmetic operations - addition,  subtraction, multiplication, or division. Coefficient of change is invariant across the sequence. 20 18 16 14 (12). Currently it only displays up to a series of 9.
}
\examples{

imThree(items=4,n=2,arith="add")


}
\author{
Aiden Loe and Filip Simonfy
}
