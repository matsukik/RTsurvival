#' survival curves
#'
#' Cacluate survival proportion of Reaction Time (RT) data in each bin
#'
#' This funciton is used internally to calculate the survival proportion.
#'
#' @param x A numeric vector of RT data.
#' @param .bin A numeric vector specifying upper bounds of each timing bin
#' @param sample logical. If \code{TRUE}, random sample of \code{x} is used to
#' calculate survival proportion.
#' @param sample.n A size of the sample when \code{sample=TURE}. Default to
#' \code{length(x)}
#' @param replace logical. If \code{TRUE}, sampling (in case of
#' \code{sample=TURE}) will be with replacement.
#' @return A vector of length \code{length(.bin)} corresponding to survival
#' proportion at each of timing bin in \code{.bin}.
#' @seealso \code{\link{surv.mean}}
#'
#' @examples
#'
#' #generate a random RT data
#' set.seed(1)
#' sbj.x <- rnorm(n = 50, mean=300, sd=50)
#'
#' #obtain a survival curve for 600 bins form 0 ms to 600 ms.
#' curv.x <- surv.curv(sbj.x, .bin=seq(0,600,by=1))
#'
#' #plot
#' plot(curv.x)
#'
#' @export
#'

surv.curv <- function(x, .bin, sample=FALSE, sample.n=length(x), replace=TRUE) {
  if(sample)  x <- sample(x, size = sample.n, replace=replace)
  res <- rep(0, length(.bin))
  res <- .C("surv_curv",
            as.double(x),
            as.integer(length(x)),
            as.double(.bin),
            as.integer(length(.bin)),
            res)
  res[[5]]
}

# .surv.curv.old <- function(x, .bin, sample=FALSE, sample.n=length(x), replace=TRUE)
# {
#    if(sample)  x <- sample(x, size = sample.n, replace=replace)
#    sapply(.bin, function(val, dat){sum(dat>val)/length(dat)}, dat=x)
# }

# .surv.curv.oldest <- function(x, .bin, sample=FALSE, sample.n=length(x), replace=TRUE)
# {
#   if(sample)  x <- sample(x, size = sample.n, replace=replace)
#   n.row <- length(x)
#   n.col <- length(.bin)
#   latency_mat <- matrix(rep(x,n.col), nrow=n.row, ncol=n.col, byrow=FALSE)
#   bin_mat <- matrix(rep(.bin,n.row), nrow=n.row, ncol=n.col, byrow=TRUE)
#   res <- colMeans(matrix(latency_mat>bin_mat, nrow=n.row, ncol=n.col, byrow=FALSE))
#   attr(res, "median") <- median(res)
#   res
# }
