#' Calculate IFR from ages using meta-analysis from Levin et al (2020).
#'
#' @param age A vector of ages.
#'
#' @source Levin et al. (2020). https://link.springer.com/article/10.1007/s10654-020-00698-1#Sec7
#'
#' @export
#'
age_to_ifr <- function(age) 10^(-3.27 + 0.0524*age)/100
