#' Extract formatted dataframes from simex object.
#'
#' @param simex A simex object returned by \code{run_model}.
#'
#' @param what What to plot: one of "prevalence" (number in each category per
#'   day), "deltas" (change in prevalence from one day to the next) and
#'   "incidence" (new additions to that compartment per day).
#'
#' @param stratify_by Variables to stratify output but, must be one or more of
#'   "time", "age", "compartment", "vax".
#'
#' @export
#'
extract.simex <- function(simex,
                           what = c("prevalence", "incidence"),
                           cri = FALSE,
                           cri_alpha = 0.95,
                           stratify_by = c("time", "vax", "compartment", "age")) {

  # check arguments
  variables <- c("time", "age", "compartment", "vax")
  if (!all(stratify_by %in% variables))
    stop(paste("stratify_by must be one or more of",
               paste(variables, collapse = ", ")))
  what <- match.arg(what)

  # select incidence or prevalence
  out <- simex[[what]]

  # don't sum if stratified by all variables
  if (!all(variables %in% stratify_by))
    out <- out[
    , .(value = sum(value)),
      by = intersect(c(stratify_by, "particle", "sample"), names(out))
    ]

  # generate credible intervals if needed
  if (any(c("particle", "sample") %in% names(out))) {
    if (cri)
      out <- out[, as.list(get_cri(value, cri_alpha)), by = stratify_by]
    else
      out <- out[, .(value = median(value)), by = stratify_by]
  } else {
    if (cri) {
      warning("Generating credible interval for single particle")
      out[, c("lower", "upper") := value]
    }
  }

  return(out)

}

#' Define generic extract method.
#'
#' @export
#'
extract <- function(x, ...) UseMethod("extract", x)
