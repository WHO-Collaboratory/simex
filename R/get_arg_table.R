#' Generate table describing function arguments and their defauts.
#'
#' @param fn_name A string describing the function name. This must be loaded
#'   into the namespace.
#'
#' @importFrom gt gt
#' @importFrom knitr kable
#'
get_arg_table <- function(fn_name, type = c("gt", "kable", "tibble")) {

  type <- match.arg(type)

  arg <- formals(get(fn_name))
  df <- tibble(
    "Argument" = names(arg),
    "Description" = map_chr(names(arg), ~ get_description(fn_name, .x)),
    "Default value" = map_chr(arg, get_default)
  )

  if(type == "gt") gt(df) else if(type == "kable") kable(df) else df

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
  txt <- strsplit(Rdo_args2txt(fn_name, arg_name), ":")[[1]][2]
  txt <- gsub("^ *|(?<= ) | *$", "", txt, perl = TRUE)
  txt <- gsub("[\r\n]", "", txt)
  return(txt)
}


#' Extract default value call as string from formal.
#'
#' @param arg An argument returned by \code{formals}.
#'
get_default <- function(arg, what = c("text", "value")) {
  what <- match.arg(what)
  if(what == "text") {
    out <- paste0(deparse(arg), collapse = " ")
    out <- gsub("^ *|(?<= ) | *$", "", out, perl = TRUE)
  } else {
    out <- eval(arg)
  }
  return(out)
}
