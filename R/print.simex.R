#' Printing method for simex object.
#'
#' @param simex A simex object returned by \code{run_model}.
#'
#' @export
#'
print.simex <- function(simex) {

  times <- as.numeric(str_remove(dimnames(simex$prevalence)$time, "day_"))
  ages <- dimnames(simex$prevalence)$age

  cat("\n [Simex Object]")
  cat("\n - Days:", paste(min(times), "to", max(times)))
  cat("\n - Age Categories:", paste(ages[1], "to", ages[length(ages)]))
  cat("\n - Compartments:", paste0(dimnames(simex$prevalence)$state, collapse = " | "))

  cat("\n")
  cat("\n")

}
