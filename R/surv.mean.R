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
