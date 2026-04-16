#' Generate table describing function arguments and their defauts.
#'
#' @param fn_name A string describing the function name. This must be loaded
#'   into the namespace.
#' @param type Output format: \code{"gt"}, \code{"kable"}, or \code{"tibble"}.
#'
get_arg_table <- function(fn_name, type = c("gt", "kable", "tibble")) {

  type <- match.arg(type)

  arg <- formals(get(fn_name))
  df <- tibble(
    "Argument" = names(arg),
    "Description" = map_chr(names(arg), ~ get_description(fn_name, .x)),
    "Default value" = map_chr(arg, get_default)
  )

  if(type == "gt") gt::gt(df) else if(type == "kable") knitr::kable(df) else df

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
  desc <- get_description_from_package_rd(fn_name, arg_name)
  if (nzchar(desc)) {
    return(desc)
  }
  out <- tryCatch(
    {
      txt <- Rdo_args2txt(fn_name, arg_name)
      parts <- strsplit(txt, ":")[[1]]
      if (length(parts) < 2L) {
        trimws(txt)
      } else {
        trimws(parts[[2]])
      }
    },
    error = function(e) ""
  )
  out <- gsub("^ *|(?<= ) | *$", "", out, perl = TRUE)
  out <- gsub("[\r\n]", "", out)
  return(out)
}


## Read \\arguments \\item text from man/*.Rd (works with pkgload::load_all;
## gbRd::Rdo_args2txt needs a registered help page, which load_all omits).
get_description_from_package_rd <- function(fn_name, arg_name) {
  fn <- tryCatch(
    match.fun(fn_name),
    error = function(e) NULL
  )
  if (is.null(fn)) {
    return("")
  }
  pkg_name <- packageName(environment(fn))
  if (is.null(pkg_name) || !nzchar(pkg_name)) {
    return("")
  }
  pkg_path <- tryCatch(
    getNamespaceInfo(asNamespace(pkg_name), "path"),
    error = function(e) NULL
  )
  if (is.null(pkg_path) || !nzchar(pkg_path)) {
    return("")
  }
  rd_path <- file.path(pkg_path, "man", paste0(fn_name, ".Rd"))
  if (!file.exists(rd_path)) {
    return("")
  }
  db <- tools::Rd_db(dir = pkg_path)
  key <- paste0(fn_name, ".Rd")
  if (!key %in% names(db)) {
    return("")
  }
  rd <- db[[key]]
  arg_sec <- rd_find_tagged_section(rd, "\\arguments")
  if (is.null(arg_sec)) {
    return("")
  }
  for (elem in arg_sec) {
    if (!is.list(elem)) {
      next
    }
    if (!identical(attr(elem, "Rd_tag"), "\\item")) {
      next
    }
    if (length(elem) < 2L) {
      next
    }
    aname <- trimws(paste(unlist(elem[[1]]), collapse = ""))
    if (aname != arg_name) {
      next
    }
    desc <- paste(gsub("\\s+", " ", unlist(elem[-1])), collapse = " ")
    desc <- trimws(gsub("\\s+", " ", desc))
    ## Rd flattening often leaves a space before . ) , ;
    desc <- gsub("\\s+([.,;:\\)])", "\\1", desc)
    desc <- gsub("\\(\\s+", "(", desc)
    return(desc)
  }
  ""
}


rd_find_tagged_section <- function(x, tag) {
  if (!is.list(x)) {
    return(NULL)
  }
  if (identical(attr(x, "Rd_tag"), tag)) {
    return(x)
  }
  for (el in x) {
    found <- rd_find_tagged_section(el, tag)
    if (!is.null(found)) {
      return(found)
    }
  }
  NULL
}


#' Extract default value call as string from formal.
#'
#' @param arg An argument returned by \code{formals}.
#' @param what Return deparsed text (\code{"text"}) or evaluated value
#'   (\code{"value"}).
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
