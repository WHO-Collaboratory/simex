#' Extract formatted dataframes from simex object.
#'
#' @param simex A simex object returned by \code{run_model}.
#' @param what What to plot: one of "prevalence" (number in each category per
#'   day), "deltas" (change in prevalence from one day to the next) and
#'   "incidence" (new additions to that compartment per day).
#' @param stratify_by Variables to stratify output but, must be one or more of
#'   "day", "age", "compartment", "vax".
#'
#' @importFrom stringr str_remove
#' @importFrom forcats fct_inorder
#'
#' @export
#'
extract.simex <- function(simex,
                          what = c("prevalence", "deltas", "incidence"),
                          stratify_by = c("day", "vax", "compartment")) {

  ## check arguments
  variables <- c("day", "age", "compartment", "vax")
  if (!all(stratify_by %in% variables))
    stop(paste("stratify_by must be one or more of",
               paste(variables, collapse = ", ")))
  what <- match.arg(what)

  as_tibble(simex[[what]]) %>%
    mutate(day = seq_len(n())) %>%
    pivot_longer(-day) %>%
    separate(name, c("age", "compartment"), sep = "\\.") %>%
    separate(compartment, c("compartment", "vax")) %>%
    mutate(
      vax = grepl("v", vax),
      age = fct_inorder(age),
      compartment = fct_inorder(compartment)
    ) %>%
    group_by(across(all_of(stratify_by))) %>%
    summarise(value = sum(value)) %>%
    ungroup()

}

#' Define generic extract method.
#'
#' @export
#'
extract <- function(x, ...) UseMethod("extract", x)
