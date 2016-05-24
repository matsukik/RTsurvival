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
                         'ci.upper' = dp.ci[2],
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
