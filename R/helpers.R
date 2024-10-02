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
