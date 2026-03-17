#' Printing method for simex object.
#'
#' @param simex A simex object returned by \code{run_model}.
#'
#' @importFrom stringr str_remove
#'
#' @export
#'
print.simex <- function(simex) {

  times <- unique(simex$incidence$time)
  ages <- levels(simex$incidence$age)
  particles <- length(unique(simex$incidence$particle))
  samples <- length(unique(simex$incidence$sample))

  cat("\n [Simex Object]")
  cat("\n - Time:", paste(min(times), "to", max(times)))
  cat("\n - Age Categories:", paste(ages[1], "to", ages[length(ages)]))
  cat("\n - Compartments:", paste0(levels(simex$incidence$compartment), collapse = " | "))
  cat("\n - Particles:", particles)
  cat("\n - Samples:", samples)

  cat("\n")
  cat("\n")

}
