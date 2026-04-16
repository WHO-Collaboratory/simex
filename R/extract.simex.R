## Sum daily incidence into non-overlapping periods (first day of period on x-axis)
aggregate_incidence_period_dt <- function(dt, period_days) {
  if (period_days <= 1L) {
    return(dt)
  }
  dt <- data.table::copy(data.table::as.data.table(dt))
  if (!"time" %in% names(dt)) {
    stop("Incidence data must have a \"time\" column for period aggregation.")
  }
  tmin <- suppressWarnings(min(as.numeric(dt$time), na.rm = TRUE))
  if (!is.finite(tmin)) {
    return(dt)
  }
  grp <- setdiff(names(dt), c("time", "value"))
  dt[, tbin := tmin + (as.numeric(time) - tmin) %/% period_days * period_days]
  if (length(grp) > 0L) {
    out <- dt[, list(value = sum(value)), by = c(grp, "tbin")]
  } else {
    out <- dt[, list(value = sum(value)), by = tbin]
  }
  out[, time := as.numeric(tbin)]
  out[, tbin := NULL]
  out[]
}

#' Extract formatted dataframes from simex object.
#'
#' @param simex A simex object returned by [run_simex()] (odin/dust2 backend).
#'
#' @param what What to plot: one of "prevalence" (number in each category per
#'   day), "deltas" (change in prevalence from one day to the next) and
#'   "incidence" (new additions to that compartment per day).
#'
#' @param filter Named list of column filters, e.g.
#'   `list(compartment = c("E", "C"))`, applied before aggregation.
#' @param stratify_by Variables to stratify output but, must be one or more of
#'   "time", "age", "compartment", "vax".
#'
#' @param period_days If greater than 1 and `what = "incidence"`, daily counts
#'   are summed into non-overlapping periods of this length (in days). The
#'   reported `time` is the first day of each period (aligned to the minimum
#'   observed time). Ignored when `what` is not `"incidence"`.
#'
#' @param cri If `TRUE`, summarise particles with median and interval bounds.
#' @param cri_alpha Width of the central credible interval (e.g. `0.75` for
#'   the 12.5% to 87.5% quantile band).
#'
#' @details
#' For `what = "incidence"`, each compartment letter is the **daily flow into**
#' that stage: `E` new infections, `C` new symptomatic community cases,
#' `H` new hospital admissions from `E`, `D` new deaths (from `C` or `H`).
#' With default [get_parameters()], `prop_hosp` is derived from IFR and
#' `hosp_mortality`, so most people leaving `E` go to `C` rather than `H`;
#' **`H` and `D` counts are therefore usually far smaller than `E` or `C`**
#' (often single digits per day per age), not missing. Your filter omits `D`;
#' add `compartment = "D"` to extract deaths. Use a larger `period_days` or
#' sum over time yourself if you need weekly or cumulative totals.
#'
#' @export
#'
extract.simex <- function(simex,
                          what = c("prevalence", "incidence"),
                          cri = FALSE,
                          cri_alpha = 0.75,
                          filter = NULL,
                          stratify_by = c("time", "vax", "compartment", "age"),
                          period_days = 1L) {

  # check arguments
  variables <- c("time", "age", "compartment", "vax")
  if (!all(stratify_by %in% variables))
    stop(paste("stratify_by must be one or more of",
               paste(variables, collapse = ", ")))
  what <- match.arg(what)
  period_days <- as.integer(period_days)
  if (length(period_days) != 1L || is.na(period_days) || period_days < 1L) {
    stop("period_days must be a single positive integer.", call. = FALSE)
  }

  # select incidence or prevalence
  out <- simex[[what]]

  # apply filter if provided
  if (!is.null(filter))
    out <- out[
      Reduce(`&`, Map(function(col, val) get(col) %in% val, names(filter), filter))
    ]

  # sum daily incidence into multi-day periods (before stratify / CRI collapse)
  if (period_days > 1L && what == "incidence") {
    out <- aggregate_incidence_period_dt(out, period_days)
  }

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

#' Generic extract method for simex objects
#'
#' Dispatches to [extract.simex()] when `x` inherits from `"simex"`.
#'
#' @param x Object to extract from.
#' @param ... Further arguments passed to methods.
#'
#' @export
extract <- function(x, ...) UseMethod("extract", x)
