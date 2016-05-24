\name{DPA.orig}
\alias{DPA.orig}
\alias{DPA.ci}
\alias{DPA.ip}
\alias{print.DPA}
\title{
Divergence Point Analysis
}
\description{
Perform Divergence Point Anlysis of Reaction Time Survival Curves as described by Reingold, Reichle, Glaholt, and Sheridan (2012) and by Reingold and Sheridan (2014)
}
\usage{
DPA.orig(subject, latency, condition, binsize = 1, window = 600, n.boot = 10000,
  r.crit = 5, ci.probs = c(0.001, 0.999),
  quiet = FALSE, reorder = TRUE)

DPA.ci(subject, latency, condition, binsize = 1, window = 600, n.boot = 1000,
  e.crit = 0.015, r.crit = 5, ci.probs = c(0.025, 0.975),
  quiet = FALSE, reorder = TRUE)

DPA.ip(subject, latency, condition, binsize = 1, window = 600, n.boot = 1000,
  num_vbin = 1200, r.crit = 100, ci.probs = c(0.025, 0.975),
  quiet = FALSE, reorder = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{subject}{
a vector speficying participant number or ID
}
  \item{latency}{
a numeric vector containing the latency measures such as reaction time or fixation in millisecond.
}
  \item{condition}{
a vector or factor specifying the experimental conditions. The conditions should be ordered such that the one that is expected to have shorter latencies (i.e., faster condition) comes first.
}
  \item{binsize}{
numeric. The size of each timing bins in millisecond. Default to 1.
}
  \item{window}{
numeric. The maximum window of the timing bin in millisecond. Default to 600.
}
  \item{n.boot}{
The number of bootstrapping to perform. Default to 10000 for \code{DPA.orig} and 1000 for \code{DPA.ip} and \code{DPA.ci}.
}
  \item{num_vbin}{
The number of survival bins (or number of data points) to be sampled with replacement. Used only for \code{DPA.ip}. Default to 1200.
}
  \item{r.crit}{
The number of bins in a row that needs be significant when determinging the divergence point.
}
  \item{e.crit}{
numeric. Divergence point empirical criterion for detecting the signigicant difference between condition on survival proportion. Used only for \code{DPA.ci}. Default to 0.015.
}
  \item{ci.probs}{
A numeric vector numeric vector of probabilities with values in \code{[0,1]}, with \code{length(ci.probs) = 2}. Default values differ depending on DPA methods.
}
  \item{quiet}{
logical. If \code{FALSE} (default), text progress bar will be displyed.
}
  \item{reorder}{
  logical. If \code{TRUE} (default), values of \code{condition} is checked to make sure that the order of conditions are coded appropriately (the condition with expected shorter duration comes first) and reorders the condition when needed.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list of class \code{DPA} containing:
\item{type}{
Type of DPA performed (one of 'Original', 'CI', and 'IP')
}
  \item{dp}{The divergence point estimate.}

  \item{ci}{The confidence interval of the divergence point.}


  \item{binsize}{The same value supplied in the argument \code{binsize}}
  \item{window}{The same value supplied in the argument \code{window}}
  \item{n.boot}{The same value supplied in the argument \code{n.boot}}

Items specific to \code{DPA.orig}:
  \item{dp.max}{The maximum of the significant divergence points.
}
  \item{dp.vec}{A vector containing all the significant divergence points.
}

The fucntion \code{DPA.ip} additionally returns a data.frame \code{dp_matrix} that contins individual participant specific estimates. The dara.frame has following columns:
\item{subject}{ Participant IDs corresponding to the values specified in the argument \code{subjects}}
\item{dpcount}{ The number of bootstrap samples from which divergence points were observed. }
\item{median_dp_point}{ the median of the divergence point estimates in the unit of  tthe number of survival bins }
\item{median_duration}{ the median of the divergence point estimates in duration. }
\item{ci.lower}{ lower confidece interval of the divergence point estimate (in duration) }
\item{ci.upper}{ upper confidece interval of the divergence point estimate (in duration)}


The function \code{DPA.ci} additionally returns a data.frame \code{DV_strap_dat} with following columns:
\item{divergence_point_strap}{ A value for each bootstrap samples corresponding to the posiiton of the bin at which the divergence was found.
}
\item{percentage_below_strap}{ corresponds to the percentage of the data points that 'died off' before the observed divergence point.
}
\item{area_measure_strap}{corresponds to the area (in percentage) formed by the difference in two survival curve proportional to the area below the curve for 'slower' condition.
}


}


\references{
Reingold, E. M. & Sheridan, H. (2014). Estimating the divergence point: A novel distributional analysis procedure for determining the onset of the influence of experimental variables. Frontiers in Psychology. doi: 10.3389/fpsyg.2014.01432.

Reingold, E. M., Reichle, E. D., Glaholt, M. G., & Sheridan, H. (2012). Direct lexical control of eye movements in reading: Evidence from a survival analysis of fixation durations. Cognitive Psychology, 65, 177-206.
}
\examples{
data(DPAsample)
## n.boot is set to 100 here just so it runs and
## finishes within a reasonal amount of time
o.dpa <- DPA.orig(DPAsample$subject, DPAsample$duration, DPAsample$condition, n.boot = 100)

msc1 <- surv.mean(DPAsample$subject, DPAsample$duration, DPAsample$condition)
plot(msc1, dp.point = o.dpa, add.arrows = TRUE)

ci.dpa <- DPA.ci(DPAsample$subject, DPAsample$duration, DPAsample$condition)
plot(msc1, dp.point = ci.dpa, add.arrows = TRUE)
}

\keyword{survival}
