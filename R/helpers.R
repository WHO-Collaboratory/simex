#' Get age categories used in analysis.
#'
get_age_cat <- function() {
  x <- map_chr(cdat[[1]]$pop$age_floor, ~ paste0(.x, "-", .x+4))
  x[length(x)] <- "75+"
  x <- setNames(x, paste0("age_", seq_along(x)))
  return(x)
}


#' Get median of age categories used in analysis.
#'
get_age_median <- function() {
  x <- cdat[[1]]$pop$age_floor + 2.5
  x[length(x)] <- 80
  return(x)
}

