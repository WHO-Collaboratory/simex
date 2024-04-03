#' Solve the ODE given a starting state and specific set of parameters.
#'
#' @param pars A single parameter set as returned by \code{get_parameters}.
#'
#' @param days A vector of days to solve the ODE across.
#'
#' @param state A matrix indicating the state of number of individuals in every
#'   compartment. Rows represent age cateogorie, columns the disease
#'   compartment.
#'
#' @importFrom deSolve ode
#'
#' @author Finlay Campbell, Prabasaj Paul
#'
solve_ode <- function(pars, days, state) {

  solved <- ode(ode_model, y = c(state), times = days, parms = pars)

  ## Format output to age-stratified 3D array
  prevalence <- array(solved[,-1], dim=c(nrow(solved), dim(state)))
  dimnames(prevalence) = list(
    time = paste0("day_", days),
    age = dimnames(state)[[1]],
    state = dimnames(state)[[2]]
  )

  ## calculate change in categories
  deltas <- vapply(
    seq_len(dim(prevalence)[1]),
    function(i) {
      if(i == 1) prevalence[i,,] - prevalence[i,,]
      else prevalence[i,,] - prevalence[i-1,,]
    },
    prevalence[1,,]
  ) %>%
    aperm(c(3, 1, 2)) %>%
    "dimnames<-"(dimnames(prevalence))

  ## incidence is the number of new entries into that category
  incidence <- array(
    c(
      ## S: we never have an entries into S
      rep(0, prod(dim(prevalence)[1:2])),
      ## E_u: infections of Susceptible from Exposed, and Susceptible from Infected
      t(apply(prevalence, 1, \(state) {
        state[, "S_u"] * (
          state[, "E_u"] %*% pars$beta_E  +
          state[, "C_u"] %*% pars$beta_I_c +
          state[, "H_u"] %*% pars$beta_I_h +
          (1-pars$vax_infectiousness) * state[, "E_v"] %*% pars$beta_E +
          (1-pars$vax_infectiousness) * state[, "C_v"] %*% pars$beta_I_c +
          (1-pars$vax_infectiousness) * state[, "H_v"] %*% pars$beta_I_h
        )
      })),
      ## C_u: E_u multiplied by the rate of leaving E_u into cu
      t(apply(prevalence, 1, \(state) state[,"E_u"]*pars$lambda_cu)),
      ## H_u: E_u multiplied by the rate of leaving E_u into hu
      t(apply(prevalence, 1, \(state) state[,"E_u"]*pars$lambda_hu)),
      ## R_u: I_c * community recovery rate + I_h * hosp recovery rate (unvax)
      t(apply(prevalence, 1, \(state) {
        state[,"C_u"]*pars$sigma_cu + state[,"H_u"]*pars$sigma_hu
      })),
      ## D_u: I_c * community mortality rate + I_h * hosp mortality rate (unvax)
      t(apply(prevalence, 1, \(state) {
        state[,"C_u"]*pars$mu_cu + state[,"H_u"]*pars$mu_hu
      })),
      ## S_v: entry from S
      t(apply(prevalence, 1, \(state) state[,"S_u"]*pars$vax_rate)),
      ## E_v: V being infected by E_u, C_u and H_u, E_v, C_v, H_v
      ## these are protected from infection by vax_infection
      ## infectiousness reduction of vaccinated incorporated here
      t(apply(prevalence, 1, \(state) {
        state[, "S_v"] * (1-pars$vax_infection) * (
          state[, "E_u"] %*% pars$beta_E  +
          state[, "C_u"] %*% pars$beta_I_c +
          state[, "H_u"] %*% pars$beta_I_h +
          (1-pars$vax_infectiousness) * state[, "E_v"] %*% pars$beta_E +
          (1-pars$vax_infectiousness) * state[, "C_v"] %*% pars$beta_I_c +
          (1-pars$vax_infectiousness) * state[, "H_v"] %*% pars$beta_I_h
        )
      })),
      ## C_v: E_v multiplied by the rate of leaving E_v into cv
      t(apply(prevalence, 1, \(state) state[,"E_v"]*pars$lambda_cv)),
      ## H_v: E_v multiplied by the rate of leaving E_v into hv
      t(apply(prevalence, 1, \(state) state[,"E_v"]*pars$lambda_hv)),
      ## R_v: I_c * community recovery rate + I_h * hosp recovery rate (for vax)
      t(apply(prevalence, 1, \(state) {
        state[,"C_v"]*pars$sigma_cv + state[,"H_v"]*pars$sigma_hv
      })),
      ## D_v: I_c * community mortality rate + I_h * hosp mortality rate (for vax)
      t(apply(prevalence, 1, \(state) {
        state[,"C_v"]*pars$mu_cv + state[,"H_v"]*pars$mu_hv
      }))
    ),
    dim = dim(prevalence),
    dimnames = dimnames(prevalence)
  )

  list(prevalence = prevalence, deltas = deltas, incidence = incidence)

}
