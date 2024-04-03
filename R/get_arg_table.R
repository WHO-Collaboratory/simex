#' Generate table describing function arguments and their defauts.
#'
#' @param fn_name A string describing the function name. This must be loaded
#'   into the namespace.
#'
#' @importFrom gt gt
#'
get_arg_table <- function(fn_name) {

  arg <- formals(get(fn_name))
  tibble(
    "Argument" = names(arg),
    "Description" = map_chr(names(arg), ~ get_description(fn_name, .x)),
    "Default value" = map_chr(arg, get_default)
  ) %>%
    gt::gt()

}


#' Extract description of function argument from documentation.
#'
#' @param fn_name A string describing the function name. This must be loaded
#'   into the namespace.
#'
#' @param arg_name A string describing the argument of the function.
#'
#' @importFrom gbRd Rdo_args2txt
#'
get_description <- function(fn_name, arg_name) {
  txt <- strsplit(gbRd::Rdo_args2txt(fn_name, arg_name), ":")[[1]][2]
  txt <- gsub("^ *|(?<= ) | *$", "", txt, perl = TRUE)
  txt <- gsub("[\r\n]", "", txt)
  return(txt)
}


#' Extract default value call as string from formal.
#'
#' @param arg An argument returned by \code{formals}.
#'
get_default <- function(arg) {
  txt <- paste0(deparse(arg), collapse = " ")
  txt <- gsub("^ *|(?<= ) | *$", "", txt, perl = TRUE)
  return(txt)
}
