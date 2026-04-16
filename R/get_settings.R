#' Settings for [fit_simex()] and related helpers
#'
#' Returns a list consumed by [fit_simex()] (particle filter, sampler, and
#' parallel worker counts). Also sets `thinning_factor` from `n_steps`,
#' `n_samples`, and `n_chains`.
#'
#' @param n_particles,n_steps,n_chains,n_samples,burnin Sampler and filter size
#'   controls (see field names in the returned list).
#' @param rerun_every,proposal_sd Tuning for adaptive / random-walk proposals.
#' @param save_state,save_trajectories,snapshots Passed to
#'   `dust2::dust_likelihood_monty()`.
#' @param groups Optional grouping metadata (must have length > 1 if set).
#' @param deterministic If `TRUE`, use an unfiltered (deterministic) likelihood
#'   path and an adaptive sampler configuration.
#'
#' @return A named `list` of settings.
#'
#' @export
get_settings <- function(n_particles = 200,
                         n_steps = 10000,
                         n_chains = 4,
                         n_samples = 1000,
                         burnin = 1000,
                         rerun_every = 100,
                         proposal_sd = 0.02,
                         save_state = TRUE,
                         save_trajectories = TRUE,
                         snapshots = NULL,
                         groups = NULL,
                         deterministic = FALSE) {
  out <- mget(names(formals()), environment())
  out$thinning_factor <- floor(n_steps / (n_samples / n_chains))
  n_cores_available <- as.integer(Sys.getenv("CONTEXT_CORES", 1))
  out$n_workers <- min(n_chains, n_cores_available)
  if (!is.null(out$groups) && length(out$groups) == 1)
    stop("There must be at least two groups")
  return(out)
}

# get variance-covariance matrix either from settings or from
# posterior samples if provided
#' @noRd
get_vcv <- function(parameters, settings, samples = NULL) {
  if (!is.null(samples)) {
    vcv <- samples |>
      posterior::as_draws_array() |>
      posterior::as_draws_matrix() |>
      stats::cov()
    stopifnot(all.equal(colnames(vcv), parameters))
    return(vcv)
  } else {
    if (length(settings$proposal_sd) == 1)
      diag(settings$proposal_sd^2, length(parameters))
    else if (length(settings$proposal_sd) == length(parameters))
      diag(settings$proposal_sd)
    else stop("propsal_sd must be length 1 or length of fitted parameters")
  }
}

# get initial values either from settings or from posterior samples if
# provided
#' @noRd
get_initial <- function(fitted_pars, settings, packer,
                        samples = NULL,
                        shared_pars = NULL) {
  if (!is.null(samples)) {
    initial <- matrix(
      samples$pars[, dim(samples$pars)[2], ],
      ncol = dim(samples$pars)[3],
      dimnames = dimnames(samples$pars)[1:2]
    )
    stopifnot(all.equal(rownames(initial), packer$names()))
    stopifnot(ncol(initial) == settings$n_chains)
    return(initial)
  } else {
    # extract initial values
    initial <- setNames(
      c(fitted_pars, shared_pars)[gsub("<.*?>", "", packer$names())],
      packer$names()
    )
    replicate(settings$n_chains, initial, simplify = FALSE)
  }
}

#' @importFrom posterior as_draws_array as_draws_matrix
NULL
