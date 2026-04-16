#' Bayesian fit of the simex model to incidence data
#'
#' Builds a dust2 filter from long-format observations, combines it with a
#' Monty prior model from [get_priors()], and runs [monty::monty_sample()].
#'
#' @param data A `data.frame` with columns `time`, `age`, `compartment`, and
#'   `value` (incidence counts).
#' @param parameters Named list of fixed model parameters; defaults to
#'   [get_parameters()]. Entries named in `priors$parameters` are treated as
#'   fitted scalars instead.
#' @param priors A Monty model from [get_priors()] listing fitted parameters.
#' @param settings A list from [get_settings()] controlling chains, particles,
#'   and sampler behaviour.
#' @param samples Optional previous `monty` samples used to initialise proposal
#'   covariance when supported by the sampler branch.
#'
#' @return A `monty` samples object with attribute `packer` (Monty packer
#'   mapping fitted names to the dust likelihood).
#'
#' @importFrom data.table dcast
#' @export
fit_simex <- function(data,
                      parameters = get_parameters(),
                      priors = get_priors(
                        p_trans = list(dist = "Beta", a = 1, b = 1)
                      ),
                      settings = get_settings(),
                      samples = NULL) {

  # shape fitting data
  fitting_data <- dcast(data, time + age ~ compartment, value.var = "value")
  fitting_data <- fitting_data[, .(E_reported = list(E)), by = time]
  data.table::setkey(fitting_data, NULL)

  # define start date in context of model
  time_start <- 0

  # define fixed parameters
  fixed_pars <- parameters
  fixed_pars[priors$parameters] <- NULL

  # build filter
  if (settings$deterministic)
    filter <- dust2::dust_unfilter_create(simex, time_start, fitting_data)
  else
    filter <- dust2::dust_filter_create(simex, time_start, fitting_data, settings$n_particles)

  # Define packer from fitted and fixed parameters
  packer <- monty::monty_packer(
    scalar = priors$parameters,
    fixed = fixed_pars
  )

  # define likelihood
  likelihood <- dust2::dust_likelihood_monty(
    filter, packer,
    save_state = settings$save_state,
    save_trajectories = settings$save_trajectories,
    save_snapshots = settings$snapshots
  )

  # define posterior
  posterior <- priors + likelihood

  # define sampler
  sampler <- if (settings$deterministic) {
    monty::monty_sampler_adaptive(
      get_vcv(packer$names(), settings, samples),
      initial_vcv_weight = 100, min_scaling = 0.9
    )
  } else {
    monty::monty_sampler_random_walk(get_vcv(priors$parameters, settings, samples))
  }

  # get samples
  samples <- monty::monty_sample(
    model = posterior,
    sampler = sampler,
    # initial = get_initial(priors$parameters, settings, packer, samples),
    n_steps = settings$n_steps + settings$burnin,
    n_chains = settings$n_chains,
    thinning_factor = settings$thinning_factor,
    runner = monty::monty_runner_callr(settings$n_workers, "fancy"),
    burnin = settings$burnin
  )

  # attack packer
  attr(samples, "packer") <- packer

  return(samples)

}
