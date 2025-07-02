#' Extract formatted dataframes from simex object.
#'
#' @param simex A simex object returned by \code{run_model}.
#'
#' @param what What to plot: one of "prevalence" (number in each category per
#'   day), "deltas" (change in prevalence from one day to the next) and
#'   "incidence" (new additions to that compartment per day).
#'
#' @param stratify_by Variables to stratify output but, must be one or more of
#'   "day", "age", "compartment", "vax".
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

  ## add vaccination as new array dimension
  arr <- simex[[what]]
  arr <- abind(
    u = arr[,,grep("_u", dimnames(arr)[[3]], value = TRUE)],
    v = arr[,,grep("_v", dimnames(arr)[[3]], value = TRUE)],
    along = 4
  )
  names(dimnames(arr)) <- variables

  ## collapse across stratifiers
  arr <- apply(arr, stratify_by, sum)

  ## functions for converting array dimnames to labels
  fn <- c(
    day = \(x) as.integer(sub(".*_", "", x)),
    age = \(x) factor(x, unique(x)),
    vax = \(x) x == "v",
    compartment = \(x) toupper(gsub("\\_.*", "", x))
  )

  ## base R implemention of reshape2::melt using label names
  tibble(
    expand.grid(imap(dimnames(arr), \(value, name) fn[[name]](value))),
    value = as.vector(arr)
  )

  ## as_tibble(simex[[what]]) %>%
  ##   mutate(day = seq_len(n())) %>%
  ##   pivot_longer(-day) %>%
  ##   separate(name, c("age", "compartment"), sep = "\\.") %>%
  ##   separate(compartment, c("compartment", "vax")) %>%
  ##   mutate(
  ##     vax = grepl("v", vax),
  ##     age = factor(age, unique(age)),
  ##     compartment = factor(compartment, unique(compartment))
  ##   ) %>%
  ##   group_by(across(all_of(stratify_by))) %>%
  ##   summarise(value = sum(value)) %>%
  ##   ungroup()

}

#' Define generic extract method.
#'
#' @export
#'
extract <- function(x, ...) UseMethod("extract", x)
