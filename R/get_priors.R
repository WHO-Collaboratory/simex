#' Create a Monty Prior Model from List Specifications
#'
#' A wrapper around \code{monty::monty_dsl} that dynamically constructs
#' prior distributions using list inputs, validated against \code{monty_dsl_distributions()}.
#'
#' @param ... Named lists. The name corresponds to the parameter name.
#'   Each list must contain a \code{dist} element (case-insensitive)
#'   and parameters matching a signature in \code{monty_dsl_distributions()}.
#'
#' @return A \code{monty_model} object.
#'
#' @importFrom purrr imap discard_at
#' @export
get_priors <- function(...) {

  # Collect inputs
  args <- list(...)

  # Get the reference table from the package
  ref <- monty::monty_dsl_distributions()

  # prior constructor in monty DSL format
  build_prior <- function(vals, name) {
    if (is.null(vals$dist) || !vals$dist %in% ref$name)
      stop(sprintf(
        "Distribution '%s' not found in monty. See monty::monty_dsl_distributions().",
        vals$dist
      ))
    mtch <- ref$args[[match(vals$dist, ref$name)]] %in% names(vals)
    if (!all(mtch))
      stop(sprintf(
        "Distribution %s is missing the following parameters: %s",
        vals$dist,
        toString(ref$args[[match(vals$dist, ref$name)]][!mtch])
      ))
    txt <- paste0(
      imap(discard_at(vals, "dist"), ~ glue::glue("{.y} = {.x}")),
      collapse = ", "
    )
    glue::glue("{name} ~ {vals$dist}({txt})")
  }

  # iterate
  dsl <- paste("{", paste(imap(args, build_prior), collapse = "\n"), "}")

  # parse
  eval(substitute(
    monty::monty_dsl(EXPR),
    list(EXPR = parse(text = dsl)[[1]])
  ))

}
