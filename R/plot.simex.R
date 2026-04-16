#' Plot a simex object
#'
#' Dispatches to [plot_simex()] so you can call the generic `plot()` on results
#' from [run_simex()].
#'
#' @param x A `simex` object from [run_simex()].
#' @param y Ignored; present for compatibility with `plot()`.
#' @param ... Arguments passed to [plot_simex()].
#'
#' @return Value from [plot_simex()].
#'
#' @method plot simex
#' @export
plot.simex <- function(x, y = NULL, ...) {
  plot_simex(x, ...)
}
