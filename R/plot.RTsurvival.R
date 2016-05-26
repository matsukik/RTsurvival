#' plot Method for the object of class \code{RTsurvival}
#'
#' This function plot the survival curves and divergence point much in line
#' with the ones used in Reingold & Sheridan (2014).
#'
#'
#' @param x An object of class \code{RTsurvival}
#' @param dp.point (optional.) An object of class \code{DPA} or a numeric
#' vector correspoinding to the divergence point.
#' @param dif.plot logical. If \code{TRUE}, difference between two conditions
#' rather than curves for each condition will be plotted. Default to
#' \code{FALSE}.
#' @param add.arrows logical. Only used when \code{dp.point} is supplied. If
#' \code{TRUE}, an allow pointing to the divergence point and the corresponding
#' value of divergence point will be printed. Defaul to \code{FALSE}.
#' @param xlab A title used for the x axis.
#' @param ylab A title used for the y axis.
#' @param legend logical. If \code{TRUE} (default), legend will be printed.
#' @param legend.txt character vector. If supplied, its element will be used
#' for the legend.
#' @param legend.inset distance of legend (placed in topright) from the
#' margins.
#' @param \dots Any other arguments to be passed to plotting methods. Currently
#' ignored.
#' @seealso \code{\link{surv.mean}}, \code{\link{surv.curv}},
#' \code{\link{DPA.orig}}
#' @references Reingold, E. M. & Sheridan, H. (2014). Estimating the divergence
#' point: A novel distributional analysis procedure for determining the onset
#' of the influence of experimental variables. Frontiers in Psychology. doi:
#' 10.3389/fpsyg.2014.01432.
#' @keywords survival
#' @examples
#'
#' data(DPAsample)
#' msc1 <- surv.mean(DPAsample$subject, DPAsample$duration, DPAsample$condition)
#' dp1 <- DPA.ci(DPAsample$subject, DPAsample$duration, DPAsample$condition)
#' plot(msc1, dp1, add.arrows = TRUE, legend.txt = c("Fast Condition", "Slow Condition"))
#'
#' @export
#'
plot.RTsurvival <- function(x, dp.point, dif.plot=FALSE, add.arrows = FALSE,
                            xlab = "Duration", ylab, legend=TRUE, legend.txt, legend.inset=.05, ...)
{
  bins <- seq(0,x$window,by=x$binsize)
  if(dif.plot)
  {
    if(missing(ylab)) ylab = "Proportion"
    plot(x=bins, y = x$difference, type='l', xlab=xlab, ylab=ylab)
  } else {
    lty <- c("dashed", "solid")
    if(missing(ylab)) ylab = "Survival (%)"
    plot(x=bins, y=x$curve[,1]*100, type='l', xlab=xlab, ylab=ylab, lty=lty[1])
    lines(x=bins, y=x$curve[,2]*100, lty=lty[2])

    if(!missing(dp.point))
    {
      if(inherits(dp.point, "DPA"))
      {
        if(dp.point$type == "Original")
        {
          lty <- c(lty, "dotted")[1:3]
          abline(v=dp.point$dp, lty=lty[3])
          ast.range <- seq(from=dp.point$dp, to=dp.point$dp.max)
          ast.vect <- rep("*", time=length(ast.range))
          text(ast.vect, x=ast.range, y=100)
        }
        else if(dp.point$type == "CI")
        {
          abline(v=dp.point$dp)
          abline(v=dp.point$ci[1], lty='dotted')
          abline(v=dp.point$ci[2], lty='dotted')
        }
        if(add.arrows)
        {
          arrows(0,35,dp.point$dp-5, 35)
          text(x=(dp.point$dp-5)/2, y=45, label=dp.point$dp, cex=1.5)
        }
      } else if(numeric(dp.point)) {
        abline(v=dp.point)
      }
    }

    if(legend){
      if(missing(legend.txt)) legend.txt <- names(x$curve)[1:2]
      if(!missing(dp.point)){
        if(dp.point$type == "Original") legend.txt <- c(legend.txt, "Divergence Point")[1:3]
      }
      legend("topright", legend=legend.txt, lty=lty, inset=legend.inset)
    }
  }
}
