#' Mean survival curves
#'
#' Obtain survival proportion per condition for each participant then calculate
#' the means of each condition and difference btween the conditions.
#'
#'
#' @param subject a vector speficying participant number or ID
#' @param latency a numeric vector containing the latency measures such as
#' reaction time or fixation in millisecond.
#' @param condition a vector or factor specifying the experimental conditions.
#' The conditions should be ordered such that the one that is expected to have
#' shorter latencies (i.e., faster condition) comes first.
#' @param binsize numeric. The size of each timing bins in millisecond. Default
#' to 1.
#' @param window numeric. The maximum window of the timing bin in millisecond.
#' Default to 600.
#' @return A list of class \code{RTsurvival} containing \item{curve}{Mean
#' survival proportion at each timing bin for each condition in a data frame}
#' \item{differnece}{differnce in mean survival proportion between slower vs
#' faster condition at each timing bins} \item{binsize}{The same value supplied
#' in the argument \code{binsize}} \item{window}{The same value supplied in the
#' argument \code{window}}
#' @seealso \code{\link{surv.curv}}, \code{\link{DPA.orig}}
#' @references Reingold, E. M. & Sheridan, H. (2014). Estimating the divergence
#' point: A novel distributional analysis procedure for determining the onset
#' of the influence of experimental variables. Frontiers in Psychology. doi:
#' 10.3389/fpsyg.2014.01432.
#'
#' Reingold, E. M., Reichle, E. D., Glaholt, M. G., & Sheridan, H. (2012).
#' Direct lexical control of eye movements in reading: Evidence from a survival
#' analysis of fixation durations. Cognitive Psychology, 65, 177-206.
#' @keywords survival
#' @examples
#'
#' data(DPAsample)
#' msc1 <- surv.mean(DPAsample$subject, DPAsample$duration, DPAsample$condition)
#' plot(msc1)
#'
#' @export
#'
surv.mean <- function(subject, latency, condition,
                      binsize = 1, window = 600)
{
  data <- data.frame(subject, latency, condition)
  n <- length(unique(subject))
  bins <- seq(from=0, to=window, by=binsize)

  bycond <- split(data, data$condition)
  cond.names <- levels(factor(condition))
  c1 <- tapply(bycond[[1]]$latency, bycond[[1]]$subject, FUN=surv.curv, .bin=bins)
  c2 <- tapply(bycond[[2]]$latency, bycond[[2]]$subject, FUN=surv.curv, .bin=bins)

  c1 <- do.call("rbind", c1)
  c2 <- do.call("rbind", c2)
  c3 <- c2 - c1

  res <- list(curve=data.frame(colMeans(c1), colMeans(c2)),
              difference=colMeans(c3),
              binsize = binsize,
              window = window)

  names(res$curve) <- c(cond.names[1:2])
  class(res) <- c("RTsurvival", class(res))
  res
}



