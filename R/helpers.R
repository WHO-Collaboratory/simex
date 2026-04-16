#' @importFrom RColorBrewer brewer.pal
#' @keywords internal
NULL

#' Get age categories used in analysis.
#'
get_age_cat <- function() {
  x <- map_chr(cdat[[1]]$pop$age_floor, ~ paste0(.x, "-", .x+4))
  x[length(x)] <- "75+"
  x <- setNames(x, paste0("age_", seq_along(x)))
  return(x)
}

## Sum observed daily incidence into the same period bins as extract.simex
aggregate_obs_incidence_by_period <- function(df, period_days, time_col = "time") {
  if (period_days <= 1L) {
    return(df)
  }
  df <- as.data.frame(df)
  tm <- as.numeric(df[[time_col]])
  tmin <- min(tm, na.rm = TRUE)
  df$tbin <- tmin + (tm - tmin) %/% period_days * period_days
  gcols <- setdiff(names(df), c(time_col, "value", "tbin"))
  if (length(gcols) == 0L) {
    out <- df |>
      dplyr::group_by(.data$tbin) |>
      dplyr::summarise(value = sum(.data$value), .groups = "drop")
  } else {
    out <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(gcols, "tbin")))) |>
      dplyr::summarise(value = sum(.data$value), .groups = "drop")
  }
  out[[time_col]] <- out$tbin
  out$tbin <- NULL
  out
}

## Named vector of RColorBrewer Dark2 colours, one per scenario (list order)
scenario_colors_dark2 <- function(names_in_order) {
  nms <- as.character(names_in_order)
  n <- length(nms)
  if (n < 1L) {
    return(stats::setNames(character(0), character(0)))
  }
  pal_n <- max(3L, n, 1L)
  cols <- brewer.pal(pal_n, "Dark2")[seq_len(n)]
  stats::setNames(cols, nms)
}

## Human-readable label for timeline titles (maps age_1 -> "0-4", etc.)
age_stratum_title <- function(ag) {
  ag <- as.character(ag)
  labs <- get_age_cat()
  if (ag %in% names(labs)) {
    return(unname(labs[ag]))
  }
  ag
}

#' Get median of age categories used in analysis.
#'
get_age_median <- function() {
  x <- cdat[[1]]$pop$age_floor + 2.5
  x[length(x)] <- 80
  return(x)
}

#' Format character vector into HTML bulleted list
#'
#' @param char a character vector. Each element will be a bullet
#' @param ordered logical (T/F). If `TRUE`, return numbered list.
#'
format_html_list <- function(char, ordered = FALSE){

  seps <- c("<li>", "</li>")
  html_wrapper <-  if(ordered) c("<ol>", "</ol>") else c("<ul>", "</ul>")

  bullets <- paste0(seps[1], char, seps[2], collapse = "")

  html_list <- paste0(html_wrapper[1], bullets, html_wrapper[2])

  return(html_list)
}

# Set dimension names
setDimnames <- function(x, nm) {
  dimnames(x) <- nm
  x
}

# Central interval width: e.g. alpha = 0.75 -> 12.5% and 87.5% quantiles.
get_cri <- function(x, alpha = 0.75) {
  setNames(
    quantile(x, c(0.5, 0.5 - alpha / 2, 0.5 + alpha / 2)),
    c("value", "lower", "upper")
  )
}
