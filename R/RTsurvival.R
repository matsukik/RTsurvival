#' Response Time (RT) Survival Analysis.
#'
#' Functions to generate survival curves and perform divergence point
#' analysis procedure that determines the onset of the effect of experimental
#' manipulation on reaction time data such as eye-fixation duration and decision latency.
#'
#' The package is a modification and R port of the MATLAB routines developed by Eyal Reingold and Heather Sheridan in
#' Reingold & Sheridan (2014).
#' The original MATLAB routines are part of Supplementary Material in Reingold & Sheridan (2014),
#' and are distributed under Creative Commons Attribution License (CC BY)
#' @import stats
#' @import utils
#' @import graphics
#' @useDynLib RTsurvival
#' @method print DPA
"_PACKAGE"
