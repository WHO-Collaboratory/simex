# simex: age-stratified SEIR with vaccination and hospital capacity
# compartments per age: Su, Eu, Cu, Hu, Ru, Du, Sv, Ev, Cv, Hv, Rv, Dv
# discrete-time stochastic
# compare_data: cases ~ Poisson(incidence)

# -------------------------------------------------------------------------
# Parameters
# -------------------------------------------------------------------------

# number of age compartments
n_age <- parameter()

# scalar parameters
N <- parameter()
hosp_capacity <- parameter()
vax_rate <- parameter()
vax_prioritised <- parameter()
hosp_prioritised <- parameter()
vax_infectiousness <- parameter()
vax_infection <- parameter()
vax_hosp <- parameter()
vax_death <- parameter()

# age-stratified parameters
prop_hosp <- parameter()
hosp_mortality <- parameter()
unhosp_mortality <- parameter()
hosp_duration <- parameter()
comm_mortality <- parameter()
symptomatic_period <- parameter()
incubation_period <- parameter()

# transmission probability per contact
p_trans <- parameter()

# contact matrices (n_age x n_age)
kappa_E <- parameter()
kappa_C <- parameter()
kappa_H <- parameter()

# initial states
init_state <- parameter()

# -------------------------------------------------------------------------
# Force of infection
# -------------------------------------------------------------------------

# force of infection *exerted* by E, C, H, stratified by vaccination
# status to acccount for reduction in infectiousness
lambda_Eu[, ] <- kappa_E[i, j] * Eu[j] / N
lambda_Ev[, ] <- (1 - vax_infectiousness) * kappa_E[i, j] * Ev[j] / N
lambda_Cu[, ] <- kappa_C[i, j] * Cu[j] / N
lambda_Cv[, ] <- (1 - vax_infectiousness) * kappa_C[i, j] * Cv[j] / N
lambda_Hu[, ] <- kappa_H[i, j] * Hu[j] / N
lambda_Hv[, ] <- (1 - vax_infectiousness) * kappa_H[i, j] * Hv[j] / N

# total force of infection *exerted* on i by by j in E, C, H
lambda_E[] <- sum(lambda_Eu[i, ]) + sum(lambda_Ev[i, ])
lambda_C[] <- sum(lambda_Cu[i, ]) + sum(lambda_Cv[i, ])
lambda_H[] <- sum(lambda_Hu[i, ]) + sum(lambda_Hv[i, ])

# force of infection *experienced* by S, adjusted for protection
# against infection by vaccination
lambda_u[] <- p_trans * (lambda_E[i] + lambda_C[i] + lambda_H[i])
lambda_v[] <- p_trans * (1 - vax_infection) * (lambda_E[i] + lambda_C[i] + lambda_H[i])


# -------------------------------------------------------------------------
# Hospital occupancy and mortality rates
# -------------------------------------------------------------------------

# total number of individuals that require hospitalisation
hosp_required[] <- Hu[i] + Hv[i]
hosp_required_total <- sum(hosp_required)

# remaining hospital capacity (reversed order)
hosp_rem[1] <- hosp_capacity
hosp_rem[2:n_age] <- max(0, hosp_rem[i - 1] - hosp_required[n_age - i + 2])

# assign remaining hospital capacity to each age group if prioritised,
# otherwise assign equally
hospitalised[] <- (
  if (hosp_prioritised) (
    min(hosp_required[i], hosp_rem[n_age - i + 1])
  )
  else (
    if (hosp_required_total > 0) (
      hosp_capacity * hosp_required[i] / hosp_required_total
    )
    else 0
  )
)

# calculate mortality of the H compartment as the weighted mean of
# those requiring hospitalisation that are in fact hospitalised vs not
# hospitalised
total_hosp_mortality[] <- (
  if (hosp_required[i] > 0) (
    (
      hosp_mortality[i] * hospitalised[i] + unhosp_mortality[i] * (hosp_required[i] - hospitalised[i])
    ) / hosp_required[i]
  )
  else hosp_mortality[i]
)


# -------------------------------------------------------------------------
# Transition rates and probabilities
# -------------------------------------------------------------------------

# sigma: E -> C
# eta: E -> H
# gamma: C/H -> R
# mu: C/H -> D

# Eu -> Cu
sigma_Eu[] <- (1 - prop_hosp[i]) / incubation_period
# Ev -> Hv
eta_Eu[] <- prop_hosp[i] / incubation_period
# Ev -> Cv
sigma_Ev[] <- (1 - (1 - vax_hosp) * prop_hosp[i]) / incubation_period
# Ev -> Hv
eta_Ev[] <- (1 - vax_hosp) * prop_hosp[i] / incubation_period
# Cu -> R
gamma_Cu[] <- (1 - comm_mortality[i]) / symptomatic_period
# Cu -> D
mu_Cu[] <- comm_mortality[i] / symptomatic_period
# Cv -> R
gamma_Cv[] <- (1 - (1 - vax_death) * comm_mortality[i]) / symptomatic_period
# Cv -> D
mu_Cv[] <- (1 - vax_death) * comm_mortality[i] / symptomatic_period
# Hu -> R
gamma_Hu[] <- (1 - total_hosp_mortality[i]) / hosp_duration[i]
# Hu -> D
mu_Hu[] <- total_hosp_mortality[i] / hosp_duration[i]
# Hv -> R
gamma_Hv[] <- (1 - (1 - vax_death) * total_hosp_mortality[i]) / hosp_duration[i]
# Hv -> D
mu_Hv[] <- (1 - vax_death) * total_hosp_mortality[i] / hosp_duration[i]

# S -> E (lambda)
p_Su_to_Eu[] <- max(0, 1 - exp(-lambda_u[i] * dt))
p_Sv_to_Ev[] <- max(0, 1 - exp(-lambda_v[i] * dt))

# E -> C (sigma) or E -> H (eta)
p_Eu_exit[] <- 1 - exp(-(sigma_Eu[i] + eta_Eu[i]) * dt)
p_Ev_exit[] <- 1 - exp(-(sigma_Ev[i] + eta_Ev[i]) * dt)
frac_Eu_to_Cu[] <- (if (sigma_Eu[i] + eta_Eu[i] > 0) sigma_Eu[i] / (sigma_Eu[i] + eta_Eu[i]) else 0)
frac_Ev_to_Cv[] <- (if (sigma_Ev[i] + eta_Ev[i] > 0) sigma_Ev[i] / (sigma_Ev[i] + eta_Ev[i]) else 0)

# C -> R (gamma) or C -> D (mu)
p_Cu_exit[] <- 1 - exp(-(gamma_Cu[i] + mu_Cu[i]) * dt)
p_Cv_exit[] <- 1 - exp(-(gamma_Cv[i] + mu_Cv[i]) * dt)
frac_Cu_to_R[] <- (if (gamma_Cu[i] + mu_Cu[i] > 0) gamma_Cu[i] / (gamma_Cu[i] + mu_Cu[i]) else 0)
frac_Cv_to_R[] <- (if (gamma_Cv[i] + mu_Cv[i] > 0) gamma_Cv[i] / (gamma_Cv[i] + mu_Cv[i]) else 0)

# H -> R (gamma) or H -> D (mu)
p_Hu_exit[] <- 1 - exp(-(gamma_Hu[i] + mu_Hu[i]) * dt)
p_Hv_exit[] <- 1 - exp(-(gamma_Hv[i] + mu_Hv[i]) * dt)
frac_Hu_to_R[] <- (if (gamma_Hu[i] + mu_Hu[i] > 0) gamma_Hu[i] / (gamma_Hu[i] + mu_Hu[i]) else 0)
frac_Hv_to_R[] <- (if (gamma_Hv[i] + mu_Hv[i] > 0) gamma_Hv[i] / (gamma_Hv[i] + mu_Hv[i]) else 0)


# -------------------------------------------------------------------------
# Stochastic flows
# -------------------------------------------------------------------------

# draw total number leaving compartment first, then split

n_Su_to_Eu[] <- Binomial(Su[i], p_Su_to_Eu[i])
n_Sv_to_Ev[] <- Binomial(Sv[i], p_Sv_to_Ev[i])

n_Eu_exit[] <- Binomial(Eu[i], p_Eu_exit[i])
n_Eu_to_Cu[] <- Binomial(n_Eu_exit[i], frac_Eu_to_Cu[i])
n_Eu_to_Hu[] <- n_Eu_exit[i] - n_Eu_to_Cu[i]

n_Ev_exit[] <- Binomial(Ev[i], p_Ev_exit[i])
n_Ev_to_Cv[] <- Binomial(n_Ev_exit[i], frac_Ev_to_Cv[i])
n_Ev_to_Hv[] <- n_Ev_exit[i] - n_Ev_to_Cv[i]

n_Cu_exit[] <- Binomial(Cu[i], p_Cu_exit[i])
n_Cu_to_Ru[] <- Binomial(n_Cu_exit[i], frac_Cu_to_R[i])
n_Cu_to_Du[] <- n_Cu_exit[i] - n_Cu_to_Ru[i]

n_Hu_exit[] <- Binomial(Hu[i], p_Hu_exit[i])
n_Hu_to_Ru[] <- Binomial(n_Hu_exit[i], frac_Hu_to_R[i])
n_Hu_to_Du[] <- n_Hu_exit[i] - n_Hu_to_Ru[i]

n_Cv_exit[] <- Binomial(Cv[i], p_Cv_exit[i])
n_Cv_to_Rv[] <- Binomial(n_Cv_exit[i], frac_Cv_to_R[i])
n_Cv_to_Dv[] <- n_Cv_exit[i] - n_Cv_to_Rv[i]

n_Hv_exit[] <- Binomial(Hv[i], p_Hv_exit[i])
n_Hv_to_Rv[] <- Binomial(n_Hv_exit[i], frac_Hv_to_R[i])
n_Hv_to_Dv[] <- n_Hv_exit[i] - n_Hv_to_Rv[i]


# -------------------------------------------------------------------------
# Vaccination (prioritised by age if needed)
# -------------------------------------------------------------------------

# available susceptibles for vaccination after incidence on that day
Su_after_inf[] <- max(0, Su[i] - n_Su_to_Eu[i])
Su_after_inf_total <- sum(Su_after_inf)

# vaccines remaining when prioritising by age
vax_rem[1] <- min(Su_after_inf[n_age], vax_rate)
vax_rem[2:n_age] <- vax_rem[i - 1] + min(Su_after_inf[n_age - i + 1], vax_rate - vax_rem[i - 1])

# assign vaccination target by age
vax_target[] <- (
  if (vax_prioritised) (
    if (i == n_age) min(Su_after_inf[n_age], vax_rate)
    else min(Su_after_inf[i], vax_rate - vax_rem[n_age - i])
  )
  else (
    if (Su_after_inf_total > 0) vax_rate * Su_after_inf[i] / Su_after_inf_total
    else 0
  )
)

# Probability to vaccinate: cap at 1, avoid div by zero
p_Su_to_Sv[] <- min(1, vax_target[i] / max(1e-10, Su_after_inf[i]))
n_Su_to_Sv[] <- Binomial(Su_after_inf[i], p_Su_to_Sv[i])


# -------------------------------------------------------------------------
# Initial conditions
# -------------------------------------------------------------------------

# prevalence
initial(Su[]) <- init_state[i, 1]
initial(Eu[]) <- init_state[i, 2]
initial(Cu[]) <- init_state[i, 3]
initial(Hu[]) <- init_state[i, 4]
initial(Ru[]) <- init_state[i, 5]
initial(Du[]) <- init_state[i, 6]
initial(Sv[]) <- init_state[i, 7]
initial(Ev[]) <- init_state[i, 8]
initial(Cv[]) <- init_state[i, 9]
initial(Hv[]) <- init_state[i, 10]
initial(Rv[]) <- init_state[i, 11]
initial(Dv[]) <- init_state[i, 12]

# incidence
initial(Su_i[], zero_every = 1) <- 0
initial(Eu_i[], zero_every = 1) <- 0
initial(Cu_i[], zero_every = 1) <- 0
initial(Hu_i[], zero_every = 1) <- 0
initial(Ru_i[], zero_every = 1) <- 0
initial(Du_i[], zero_every = 1) <- 0
initial(Sv_i[], zero_every = 1) <- 0
initial(Ev_i[], zero_every = 1) <- 0
initial(Cv_i[], zero_every = 1) <- 0
initial(Hv_i[], zero_every = 1) <- 0
initial(Rv_i[], zero_every = 1) <- 0
initial(Dv_i[], zero_every = 1) <- 0

# -------------------------------------------------------------------------
# Updates
# -------------------------------------------------------------------------

# prevalence
update(Su[]) <- Su[i] - n_Su_to_Eu[i] - n_Su_to_Sv[i]
update(Eu[]) <- Eu[i] + n_Su_to_Eu[i] - n_Eu_exit[i]
update(Cu[]) <- Cu[i] + n_Eu_to_Cu[i] - n_Cu_exit[i]
update(Hu[]) <- Hu[i] + n_Eu_to_Hu[i] - n_Hu_exit[i]
update(Ru[]) <- Ru[i] + n_Cu_to_Ru[i] + n_Hu_to_Ru[i]
update(Du[]) <- Du[i] + n_Cu_to_Du[i] + n_Hu_to_Du[i]
update(Sv[]) <- Sv[i] + n_Su_to_Sv[i] - n_Sv_to_Ev[i]
update(Ev[]) <- Ev[i] + n_Sv_to_Ev[i] - n_Ev_exit[i]
update(Cv[]) <- Cv[i] + n_Ev_to_Cv[i] - n_Cv_exit[i]
update(Hv[]) <- Hv[i] + n_Ev_to_Hv[i] - n_Hv_exit[i]
update(Rv[]) <- Rv[i] + n_Cv_to_Rv[i] + n_Hv_to_Rv[i]
update(Dv[]) <- Dv[i] + n_Cv_to_Dv[i] + n_Hv_to_Dv[i]

# incidence
update(Su_i[]) <- Su_i[i]
update(Eu_i[]) <- Eu_i[i] + n_Su_to_Eu[i]
update(Cu_i[]) <- Cu_i[i] + n_Eu_to_Cu[i]
update(Hu_i[]) <- Hu_i[i] + n_Eu_to_Hu[i]
update(Ru_i[]) <- Ru_i[i] + n_Cu_to_Ru[i] + n_Hu_to_Ru[i]
update(Du_i[]) <- Du_i[i] + n_Cu_to_Du[i] + n_Hu_to_Du[i]
update(Sv_i[]) <- Sv_i[i] + n_Su_to_Sv[i]
update(Ev_i[]) <- Ev_i[i] + n_Sv_to_Ev[i]
update(Cv_i[]) <- Cv_i[i] + n_Ev_to_Cv[i]
update(Hv_i[]) <- Hv_i[i] + n_Ev_to_Hv[i]
update(Rv_i[]) <- Rv_i[i] + n_Cv_to_Rv[i] + n_Hv_to_Rv[i]
update(Dv_i[]) <- Dv_i[i] + n_Cv_to_Dv[i] + n_Hv_to_Dv[i]

# -------------------------------------------------------------------------
# Data comparison
# -------------------------------------------------------------------------

# incidence treated as all new symptomatic cases in hospital or community
cases <- data()
cases[] ~ Poisson(Cu_i[i] + Hu_i[i] + Cv_i[i] + Hv_i[i] + 0.1)


# -------------------------------------------------------------------------
# Define dimensions
# -------------------------------------------------------------------------

# vectors of length n_age
dim(
  # prevalence
  Su, Eu, Cu, Hu, Ru, Du,
  Sv, Ev, Cv, Hv, Rv, Dv,
  # prevalence
  Su_i, Eu_i, Cu_i, Hu_i, Ru_i, Du_i,
  Sv_i, Ev_i, Cv_i, Hv_i, Rv_i, Dv_i,
  # mortality parameters
  comm_mortality, hosp_mortality, unhosp_mortality,
  # hospitalisation parameters
  hosp_required, hosp_rem, hospitalised, total_hosp_mortality, prop_hosp, hosp_duration,
  # vaccination parameters
  Su_after_inf, vax_rem, vax_target,
  # rates
  lambda_E, lambda_C, lambda_H, lambda_u, lambda_v,
  sigma_Eu, eta_Eu, sigma_Ev, eta_Ev,
  gamma_Cu, mu_Cu, gamma_Cv, mu_Cv,
  gamma_Hu, mu_Hu, gamma_Hv, mu_Hv,
  # transition probabilities
  p_Su_to_Eu, p_Sv_to_Ev, p_Eu_exit, p_Ev_exit,
  p_Cu_exit, p_Hu_exit, p_Cv_exit, p_Hv_exit,
  # transition fractions
  frac_Eu_to_Cu, frac_Ev_to_Cv, frac_Cu_to_R,
  frac_Hu_to_R, frac_Cv_to_R, frac_Hv_to_R,
  # transitions counts
  n_Su_to_Eu,  n_Sv_to_Ev, n_Eu_exit, n_Ev_exit, n_Eu_to_Cu, n_Eu_to_Hu,
  n_Ev_to_Cv, n_Ev_to_Hv, n_Cu_exit, n_Hu_exit, n_Cv_exit, n_Hv_exit, n_Cu_to_Ru,
  n_Cu_to_Du, n_Hu_to_Ru, n_Hu_to_Du, n_Cv_to_Rv, n_Cv_to_Dv, n_Hv_to_Rv,
  n_Hv_to_Dv, p_Su_to_Sv, n_Su_to_Sv,
  # data
  cases
) <- n_age

# array of dimension n_age x n_age
dim(
  # kappas (n_contacts * p_transmision per contact)
  kappa_E, kappa_C, kappa_H,
  # force of infection
  lambda_Eu, lambda_Cu, lambda_Hu,
  lambda_Ev, lambda_Cv, lambda_Hv
) <- c(n_age, n_age)

# initial state
dim(init_state) <- c(n_age, 12)
