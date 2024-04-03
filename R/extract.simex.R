#' Extract formatted dataframes from simex object.
#'
#' @param simex A simex object returned by \code{run_model}.
#' @param what What to plot: one of "prevalence" (number in each category per
#'   day), "deltas" (change in prevalence from one day to the next) and
#'   "incidence" (new additions to that compartment per day).
#'
#' @importFrom stringr str_remove
#' @importFrom forcats fct_inorder
#'
#' @export
#'
extract.simex <- function(simex,
                          what = c("prevalence", "deltas", "incidence")) {

  what <- match.arg(what)
  apply(simex[[what]], c(1, 3), sum) %>%
    {tibble(day = as.numeric(str_remove(rownames(.), "day_")), as_tibble(.))} %>%
    pivot_longer(-day, names_to = "compartment") %>%
    separate(compartment, c("compartment", "vax")) %>%
    mutate(
      vax = grepl("v", vax),
      compartment = fct_inorder(compartment)
    )

}

#' Define generic extract method.
#'
#' @export
#'
extract <- function(x, ...) UseMethod("extract", x)
