#' Divergence Point Analysis
#'
#' Perform Divergence Point Anlysis of Reaction Time Survival Curves as
#' described by Reingold, Reichle, Glaholt, and Sheridan (2012) and by Reingold
#' and Sheridan (2014)
#'
#'
#' @aliases DPA.orig DPA.ci DPA.ip print.DPA
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
#' @param n.boot The number of bootstrapping to perform. Default to 10000 for
#' \code{DPA.orig} and 1000 for \code{DPA.ip} and \code{DPA.ci}.
#' @param num_vbin The number of survival bins (or number of data points) to be
#' sampled with replacement. Used only for \code{DPA.ip}. Default to 1200.
#' @param r.crit The number of bins in a row that needs be significant when
#' determinging the divergence point.
#' @param e.crit numeric. Divergence point empirical criterion for detecting
#' the signigicant difference between condition on survival proportion. Used
#' only for \code{DPA.ci}. Default to 0.015.
#' @param ci.probs A numeric vector numeric vector of probabilities with values
#' in \code{[0,1]}, with \code{length(ci.probs) = 2}. Default values differ
#' depending on DPA methods.
#' @param quiet logical. If \code{FALSE} (default), text progress bar will be
#' displyed.
#' @param reorder logical. If \code{TRUE} (default), values of \code{condition}
#' is checked to make sure that the order of conditions are coded appropriately
#' (the condition with expected shorter duration comes first) and reorders the
#' condition when needed.
#' @return A list of class \code{DPA} containing: \item{type}{ Type of DPA
#' performed (one of 'Original', 'CI', and 'IP') } \item{dp}{The divergence
#' point estimate.}
#'
#' \item{ci}{The confidence interval of the divergence point.}
#'
#' \item{binsize}{The same value supplied in the argument \code{binsize}}
#' \item{window}{The same value supplied in the argument \code{window}}
#' \item{n.boot}{The same value supplied in the argument \code{n.boot}}
#'
#' Items specific to \code{DPA.orig}: \item{dp.max}{The maximum of the
#' significant divergence points. } \item{dp.vec}{A vector containing all the
#' significant divergence points. }
#'
#' The fucntion \code{DPA.ip} additionally returns a data.frame
#' \code{dp_matrix} that contins individual participant specific estimates. The
#' dara.frame has following columns: \item{subject}{ Participant IDs
#' corresponding to the values specified in the argument \code{subjects}}
#' \item{dpcount}{ The number of bootstrap samples from which divergence points
#' were observed. } \item{median_dp_point}{ the median of the divergence point
#' estimates in the unit of tthe number of survival bins }
#' \item{median_duration}{ the median of the divergence point estimates in
#' duration. } \item{ci.lower}{ lower confidece interval of the divergence
#' point estimate (in duration) } \item{ci.upper}{ upper confidece interval of
#' the divergence point estimate (in duration)}
#'
#' The function \code{DPA.ci} additionally returns a data.frame
#' \code{DV_strap_dat} with following columns: \item{divergence_point_strap}{ A
#' value for each bootstrap samples corresponding to the posiiton of the bin at
#' which the divergence was found. } \item{percentage_below_strap}{ corresponds
#' to the percentage of the data points that 'died off' before the observed
#' divergence point. } \item{area_measure_strap}{corresponds to the area (in
#' percentage) formed by the difference in two survival curve proportional to
#' the area below the curve for 'slower' condition. }
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
#' ## n.boot is set to 100 here just so it runs and
#' ## finishes within a reasonal amount of time
#' o.dpa <- DPA.orig(DPAsample$subject, DPAsample$duration, DPAsample$condition, n.boot = 100)
#'
#' msc1 <- surv.mean(DPAsample$subject, DPAsample$duration, DPAsample$condition)
#' plot(msc1, dp.point = o.dpa, add.arrows = TRUE)
#'
#' ci.dpa <- DPA.ci(DPAsample$subject, DPAsample$duration, DPAsample$condition)
#' plot(msc1, dp.point = ci.dpa, add.arrows = TRUE)
#'
#' @export
DPA.orig <- function(subject, latency, condition,
                     binsize = 1, window = 600, n.boot = 10000,
                     r.crit = 5, ci.probs = c(0.001, 0.999), quiet=FALSE, reorder = TRUE)
{
  n <- length(unique(subject))
  nbins <- window/binsize
  bins <- seq(from=0, to=binsize*nbins, by=binsize)
  ## checking if condition is coded correctly
  if(reorder)
  {
    cond.mean <- tapply(latency, condition, FUN = mean)
    if(sign(diff(cond.mean)) < 0)
    {
      warning("The conditions should be ordered so that",
              "the one with shorter mean latencies comes first.",
              "Reordering...")
      condition <- factor(condition)
      condition <- relevel(condition, ref=2)
    }
  }
  ###### bootstrapping
  diffs <- list()
  if(!quiet) pb <- txtProgressBar(min = 1, max = n.boot, char = "=", style = 3)
  for(bs in 1:n.boot){
    if (!quiet) setTxtProgressBar(pb, bs)
    boot.mat <- tapply(latency, list(subject, condition), FUN=surv.curv, .bin=bins, sample=T);
    boot.diff.mat <- mapply(`-`, boot.mat[,2], boot.mat[,1])
    diffs[[bs]] <- rowMeans(boot.diff.mat)
  }

  diff_mat <- do.call("rbind", diffs)
  diff_ci <- apply(do.call("rbind", diffs), 2, quantile, prob=ci.probs)

  splits <- c(diff_ci[1,] > 0)
  dp <- which(apply(embed(splits, r.crit), 1, FUN=sum)==r.crit)

  res <- list(type = "Original",
              dp = min(dp)*binsize,
              dp.max = max(dp)*binsize,
              dp.vec = dp * binsize,
              binsize = binsize,
              window = window,
              n.boot = n.boot)
  class(res) <- c("DPA", "list")
  res
}

#' @rdname DPA.orig
#' @export
DPA.ip <- function(subject, latency, condition,
                   binsize = 1, window = 600, n.boot = 1000,
                   num_vbin = 1200,
                   r.crit = 100,
                   ci.probs = c(.025, .975), quiet=FALSE, reorder = TRUE)
{
  n <- length(unique(subject))
  nbins <- window/binsize
  bins <- seq(from=0, to=binsize*nbins, by=binsize)
  uniq.sbj <- unique(subject)
  median_srvl <- list()
  dp_stats <- list()
  ## checking if condition is coded correctly
  if(reorder)
  {
    cond.mean <- tapply(latency, condition, FUN = mean)
    if(sign(diff(cond.mean)) < 0)
    {
      warning("The conditions should be ordered so that",
              "the one with shorter mean latencies comes first.",
              "Reordering...")
      condition <- factor(condition)
      condition <- relevel(condition, ref=2)
    }
  }
  #####
  if(!quiet) pb <- txtProgressBar(min = 1, max = length(uniq.sbj) * n.boot, char = "=", style = 3)
  for(i in 1:length(uniq.sbj))
  {
    sbj <- uniq.sbj[i]
    c1.dat <- latency[subject==sbj & condition == unique(condition)[1]]
    c2.dat <- latency[subject==sbj & condition == unique(condition)[2]]
    ### prepare output vectors
    dp.strap <- rep(NA, time=n.boot)
    duration_at_divergence <- rep(NA, times=n.boot)
    median_c1 <- median_c2 <- rep(NA, times=n.boot)

    for(bs in 1:n.boot){
      if (!quiet) setTxtProgressBar(pb, (i-1)*n.boot+bs)
      boot.c1.dat <- sample(c1.dat, size = num_vbin, replace=T)
      boot.c2.dat <- sample(c2.dat, size = num_vbin, replace=T)
      boot.c1.sort <- sort(boot.c1.dat)
      boot.c2.sort <- sort(boot.c2.dat)
      boot.c3 <- boot.c2.sort - boot.c1.sort
      dp.strap[bs] <- which(apply(embed(c(boot.c3 > 0),r.crit), 1, FUN=sum)==r.crit)[1]
      if(!is.na(dp.strap[bs])){
        duration_at_divergence[bs] <- as.numeric((boot.c1.sort[dp.strap[bs]] + boot.c2.sort[dp.strap[bs]])/2)
      }
      #duration_at_divergence[bs] <- .average_dp_duration(dp.strap[bs], boot.c1.dat, boot.c2.dat)
      #median_c1[bs] <- median(boot.c1.dat)
      #median_c2[bs] <- median(boot.c2.dat)
    }
    #median_srvl[[sbj]] <- cbind('subject'=sbj, 'c1'=median_c1, 'c2'=median_c2)
    dp.ci <- as.numeric(quantile(duration_at_divergence, probs = ci.probs, na.rm=TRUE))
    dp_stats[[sbj]] <- c('dpcount'= length(na.omit(dp.strap)),
                         'median_dp_point' = median(dp.strap, na.rm=TRUE),
                         'median_duration'= median(duration_at_divergence, na.rm = TRUE),
                         'ci.lower' = dp.ci[1],
                         'ci.upper' = dp.ci[2]
                         )
  }

  dp_stat_mat <- cbind(subject=uniq.sbj, as.data.frame(do.call('rbind', dp_stats)))
  res <- list(type='IP',
              dp=mean(dp_stat_mat$median_duration),
              ci=c(mean(dp_stat_mat$ci.lower), mean(dp_stat_mat$ci.upper)),
              dp_matrix = dp_stat_mat,
              #survial_meidan = median_srvl,
              binsize = binsize,
              window = window,
              n.boot = n.boot)
  class(res) <- c("DPA", "list")
  res
}

#' @rdname DPA.orig
#' @export

DPA.ci <- function(subject, latency, condition,
                   binsize = 1, window = 600, n.boot = 1000,
                   e.crit = .015, r.crit = 5,
                   ci.probs = c(.025, .975), quiet=FALSE, reorder=TRUE)
{
  n <- length(unique(subject))
  nbins <- window/binsize
  bins <- seq(from=0, to=binsize*nbins, by=binsize)
  ## checking if condition is coded correctly
  if(reorder)
  {
    cond.mean <- tapply(latency, condition, FUN = mean)
    if(sign(diff(cond.mean)) < 0)
    {
      warning("The conditions should be ordered so that",
              "the one with shorter mean latencies comes first.",
              "Reordering...")
      condition <- factor(condition)
      condition <- relevel(condition, ref=2)
    }
  }
  ###### split the data
  bycond <- split(data.frame(subject, latency, condition), condition)
  ###### bootstrapping
  DV_strap <- list()
  if(!quiet) pb <- txtProgressBar(min = 1, max = n.boot, char = "=", style = 3)
  for(bs in 1:n.boot){
    if (!quiet) setTxtProgressBar(pb, bs)

    boot.c1 <- tapply(bycond[[1]]$latency, bycond[[1]]$subject, FUN=surv.curv, .bin=bins, sample=T)
    boot.c2 <- tapply(bycond[[2]]$latency, bycond[[2]]$subject, FUN=surv.curv, .bin=bins, sample=T)

    boot.c1.mat <- do.call("rbind", boot.c1)
    boot.c2.mat <- do.call("rbind", boot.c2)
    boot.c3.mat <- boot.c2.mat - boot.c1.mat
    srvl_straps <- list(colMeans(boot.c1.mat), colMeans(boot.c2.mat), colMeans(boot.c3.mat))

    splits <- c(srvl_straps[[3]] >= e.crit)
    dp.strap <- which(apply(embed(splits, r.crit), 1, FUN=sum)==r.crit)[1]

    if(!is.na(dp.strap)){
      percentage_below_strap <- (1 - mean(c(srvl_straps[[1]][dp.strap], srvl_straps[[2]][dp.strap]))) *100;
      area_under_low_strap <- sum(srvl_straps[[2]][dp.strap:length(bins)])
      area_under_high_strap <- sum(srvl_straps[[1]][dp.strap:length(bins)])
      area_between_strap <-  area_under_low_strap - area_under_high_strap
      area_measure_strap <- area_between_strap/area_under_low_strap;
    }else{
      percentage_below_strap <- area_measure_strap <- area_under_low_strap <- area_under_high_strap <- NA;
    }
    DV_strap[[bs]] <- c('divergence_point_strap'= dp.strap,
                        'percentage_below_strap'= percentage_below_strap,
                        'area_under_low_strap' = area_under_low_strap,
                        'area_under_high_strap' = area_under_high_strap,
                        'area_measure_strap' = area_measure_strap)
  }

  DV_strap_dat <- as.data.frame(do.call("rbind", DV_strap))
  dv_strap_vector <- na.omit(DV_strap_dat$divergence_point_strap)
  res <- list(type = "CI",
              'dp' = median(dv_strap_vector),
              'ci' = quantile(dv_strap_vector, probs=ci.probs),
              'DV_strap_dat'=DV_strap_dat,
              binsize = binsize,
              window = window,
              n.boot = n.boot)
  class(res) <- c("DPA", "list")
  res
}


#' @export
print.DPA <- function(x,...)
{
  cat('\nMethod of Estimation:\n')
  print(x$type)
  cat('Divergence Point Estimate:\n')
  print(x$dp)
  if(x$type == "Original")
  {
    cat('Range of Significant Difference:\n')
    print(c(x$dp, x$dp.max))
  } else {
    cat('Confidence Interval:\n');
    print(x$ci)
  }
}
