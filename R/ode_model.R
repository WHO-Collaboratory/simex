#' The series of ODEs passed to deSolve.
#'
#' @param time A vector of days.
#'
#' @param state A matrix indicating the state of number of individuals in every
#'   compartment. Rows represent age cateogorie, columns the disease
#'   compartment.
#'
#' @param pars A parameter set as returned by \code{get_parameters}.
#'
#' @importFrom deSolve ode
#'
#' @author Finlay Campbell, Prabasaj Paul
#'
## define ODEs for SEIR
ode_model <- function(time, state, pars) {

  ## Each element is number of persons/total population (sum(state)=1)
  nr = nrow(pars$beta_I_c) ## Gettin the number of age groups, N
  state = matrix(
    state, nrow=nr,
    dimnames=list(
      paste0("AGE", 1:nr),
      c("S_u", "E_u", "C_u", "H_u", "R_u", "D_u", "S_v", "E_v", "C_v", "H_v", "R_v", "D_v")
    )
  )
  dstate = state * 0

  ## update rates in response to changes in hospitalisation
  pars %<>% add_rates(state)

  ## New E_u come from S being infected by E_u, C_u and H_u, E_v, C_v, H_v
  ## infectiousness reduction of vaccinated incorporated here
  new_E_u <- state[, "S_u"] * (
    state[, "E_u"] %*% pars$beta_E  +
    state[, "C_u"] %*% pars$beta_I_c +
    state[, "H_u"] %*% pars$beta_I_h +
    (1-pars$vax_infectiousness) * state[, "E_v"] %*% pars$beta_E +
    (1-pars$vax_infectiousness) * state[, "C_v"] %*% pars$beta_I_c +
    (1-pars$vax_infectiousness) * state[, "H_v"] %*% pars$beta_I_h
  )

  ## New E_v come from V being infected by E_u, C_u and H_u, E_v, C_v, H_v
  ## these are protected from infection by vax_infection
  ## infectiousness reduction of vaccinated incorporated here
  new_E_v <- state[, "S_v"] * (1-pars$vax_infection) * (
    state[, "E_u"] %*% pars$beta_E +
    state[, "C_u"] %*% pars$beta_I_c +
    state[, "H_u"] %*% pars$beta_I_h +
    (1-pars$vax_infectiousness) * state[, "E_v"] %*% pars$beta_E +
    (1-pars$vax_infectiousness) * state[, "C_v"] %*% pars$beta_I_c +
    (1-pars$vax_infectiousness) * state[, "H_v"] %*% pars$beta_I_h
  )

  ## Define absolute number of vaccinations not proportions (behaviour is not
  ## geometric) (i.e. not a proportion of S are vaccinated every day)
  vax <- state[, "S_u"]
  vax[] <- 0
  vaxleft <- pars$vax_rate

  ## Prioritised vaccination
  if(pars$vax_prioritised) {
    for(i in rev(seq_along(vax))) {
      if(vaxleft > state[i, "S_u"]) {
        vax[i] <- state[i, "S_u"]
        vaxleft <- vaxleft - state[i, "S_u"]
      } else {
        vax[i] <- vaxleft
        vaxleft <- 0
      }
    }
  } else {
    ## vaccinate a given proportion of all age groups equally
    if(pars$vax_rate > sum(state[, "S_u"])) vax <- state[, "S_u"]
    else vax <- pars$vax_rate*state[, "S_u"]/sum(state[, "S_u"])
  }

  ## if(time > 100 & time < 170)
  ##   df <<- rbind(df, tibble(
  ##     time = time,
  ##     ## S_u = sum(state[, "S_u"]),
  ##     ## E_u = sum(state[, "E_u"]),
  ##     ## C_u = sum(state[, "C_u"]),
  ##     beta_E = sum(pars$beta_E),
  ##     from_E_u = sum(state[, "E_u"] %*% pars$beta_E),
  ##     from_C_u = sum(state[, "C_u"] %*% pars$beta_I_c),
  ##     from_H_u = sum(state[, "H_u"] %*% pars$beta_I_h),
  ##     entering = sum(new_E_u),
  ##     leaving_rate = (pars$lambda_hu + pars$lambda_cu),
  ##     leaving = sum(state[, "E_u"]*(pars$lambda_hu + pars$lambda_cu)),
  ##     diff = entering - leaving
  ##   ))

  ## S are lost to E_u by infection and to V by vaccination
  dstate[, "S_u"] = -new_E_u - vax

  ## E_u enter from S compartment, E_u leave as they enter sympomatic state in
  ## community or in hospital
  dstate[, "E_u"] = new_E_u - state[, "E_u"]*(pars$lambda_hu + pars$lambda_cu)

  ## C_u enter from E_u comparment, C_u leave as they die or recover in community
  dstate[, "C_u"] =
    state[, "E_u"]*pars$lambda_cu -
    state[, "C_u"]*(pars$sigma_cu + pars$mu_cu)

  ## H_u enter from E_u comparment, H_u leave as they die or recover in hospital
  dstate[, "H_u"] =
    state[, "E_u"]*pars$lambda_hu -
    state[, "H_u"]*(pars$sigma_hu + pars$mu_hu)

  ## R enter from community or hospital at respective recovery rates
  dstate[, "R_u"] =
    state[, "C_u"]*pars$sigma_cu +
    state[, "H_u"]*pars$sigma_hu

  ## D enter from community or hospital at respective mortality rates
  dstate[, "D_u"] =
    state[, "C_u"]*pars$mu_cu +
    state[, "H_u"]*pars$mu_hu

  ## S_v come from S_u by vaccination and are lost to E_v by infection
  dstate[, "S_v"] = vax - new_E_v

  ## E_v enter from V compartment, E_v leave as they enter sympomatic state in
  ## community or in hospital
  dstate[, "E_v"] = new_E_v - state[, "E_v"]*(pars$lambda_hv + pars$lambda_cv)

  ## I_v enter from E_v comparment, I_v leave as they die or recover in community
  dstate[, "C_v"] =
    state[, "E_v"]*pars$lambda_cv -
    state[, "C_v"]*(pars$sigma_cv + pars$mu_cv)

  ## I_v enter from E_v comparment, I_v leave as they die or recover in hospital
  dstate[, "H_v"] =
    state[, "E_v"]*pars$lambda_hv -
    state[, "H_v"]*(pars$sigma_hv + pars$mu_hv)

  ## R enter from community or hospital at respective recovery rates
  dstate[, "R_v"] =
    state[, "C_v"]*pars$sigma_cv +
    state[, "H_v"]*pars$sigma_hv

  ## D enter from community or hospital at respective mortality rates
  dstate[, "D_v"] =
    state[, "C_v"]*pars$mu_cv +
    state[, "H_v"]*pars$mu_hv

  list(dstate)

}
