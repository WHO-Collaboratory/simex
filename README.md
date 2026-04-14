
## *simex*: a disease modelling tool for decision makers

[![MIT
license](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE.md)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)

*simex* is an R package for simulating the spread of infectious
diseases, as well as interventions such as social distancing measures,
isolation and vaccination. It uses an age-structured SEIR compartmental
model with country-specific age demographics and contact rates. The
simplest way to interact with the tool is to use the [Shiny
App](https://portal.who.int/eios-colab/rconnect/simex).

The transmission model is written in
[**odin**](https://mrc-ide.github.io/odin/) (odin2 DSL) in
[`inst/odin/simex.R`](inst/odin/simex.R). That specification is compiled
with [**dust2**](https://mrc-ide.github.io/dust2/) to C++ for fast
discrete-time simulation (see the generated sources under `src/`). The
main R entry point that uses this backend is `run_simex()`.

## Installation

To install the development version from github:

``` r
remotes::install_github("WHO-Collaboratory/simex")
```

Load the package using:

``` r
library(simex)
```

## Running *simex*

### Shiny App

You can use *simex* interactively by launching the Shiny app locally
with `simex::run_shiny()`. The app runs the same odin/dust2 model via
`run_simex()`. If you want to use it programmatically, follow the steps
below.

### Parameters and settings

Most settings are specified via `get_parameters()`. The arguments and
their default values are summarised below (from the roxygen
documentation):

| Argument | Description | Default value |
|:---|:---|:---|
| iso3 | The ISO3 code of the country used to draw age-distributions and contact rates from. | “CMR” |
| population | Total population size (scalar) used to construct the initial state matrix passed to the model. | 1e+05 |
| R0 | The basic reproduction number. | 3 |
| generation_time | The mean generation time in days. | 8 |
| incubation_period | The mean incubation period in days. | 3 |
| infectiousness_presymp | Relative infectiousness of presymptomatic cases to symptomatic cases. | 0.25 |
| frac_symp | The proportion of cases that eventually develop symptoms. | 0.8 |
| ifr | Infection fatality rate provided either as a single value or as a vector of the same length as the number of age categories. Use the function age_to_ifr to calculate a COVID-like IFR from a vector of ages. | age_to_ifr(get_age_median()) |
| hosp_mortality | The probability of death given a case is admitted to hospital, either as a single value or as a vector of the same length as the number of age categories. The inverse of the number of cases admitted to hospital per death. | 1/seq(20, 5, length = 16) |
| hosp_protection_death | Given a case requires hospitalisation, the proportion of deaths admission to hospital averts. | 0.75 |
| hosp_duration | The mean duration of stay in the hospital in days, either as a single value or as a vector of the same length as the number of age categories. | seq(7, 21, length = 16) |
| hosp_capacity | Total hospital bed capacity given as a proportion of the population. | 0.0025 |
| comm_mortality | The probability of death of cases that remain in the community, either as a single value or as a vector of the same length as the number of age categories. | rep(0, 16) |
| vax_rate | Number of vaccine doses administered per day (absolute count, not a proportion of the population). | 0 |
| vax_infectiousness | The reduction (as a proportion) in infectioussness of an individual due to vaccination. | 0.3 |
| vax_infection | The protection (as a proportion) against infection provided by vaccination. | 0.5 |
| vax_hosp | The protection (as a proportion) against hospitalisation provided by vaccination, given infection. | 0.5 |
| vax_death | The protection (as a proportion) against death provided by vaccination, given hospitalisation. | 0.8 |
| isolation_adherence | The proportion of symptomatic individuals that adhere to isolation measures. | 0 |
| isolation_effectiveness | The reduction in daily transmission potential of a given individual due to adherence to isolation measures. | 0.8 |
| isolation_delay | The mean delay from symptom onset to isolation in days. | 3 |
| social_distancing | A named vector of length 4 containing the proportion reduction in contacts due to social distancing of ‘home’, ‘school’, ‘work’ and ‘other’. | c(home = 0, school = 0, work = 0, other = 0) |
| init_infections | Total number of individuals infected at time zero, allocated across age groups by one multinomial draw with probabilities equal to the country age fractions. | 5 |
| init_compartment | State column for the initial infection count (same meaning as init_infections). Typical value “Eu” (exposed, unvaccinated). Must match one of the twelve odin state names (Su/Eu/Cu/Hu/Ru/Du and Sv/Ev/Cv/Hv/Rv/Dv). Susceptibles in Su are reduced accordingly. | “Eu” |
| vax_prioritised | A logical indicating whether older age groups are vaccinated first. | TRUE |
| hosp_prioritised | A logical indicating whether older age groups are hospitalised first when hospital capacity is exceeded. | TRUE |

### Running default settings

To run the model using default settings, pass the list from
`get_parameters()` to `run_simex()` and set `time` to the integer days
you want (inclusive).

``` r
# set parameters using defaults
pars <- get_parameters()

# run odin/dust2 model (example: days 1–200)
output <- run_simex(pars, time = 1:200)

# look at output
print(output)
```


     [Simex Object]
     - Time: 1 to 200
     - Age Categories: age_1 to age_16
     - Compartments: S | E | C | H | R | D
     - Particles: 0
     - Samples: 0

### Visualising outputs

To visualise the results, use the generic `plot` function defined for
the `simex` class. Below, we first visualise prevalence and then
incidence, specified using the `what` argument.

``` r
# visualise prevalence
plot(output, what = "prevalence")
```

<img src="man/figures/unnamed-chunk-6-1.png" alt="" width="75%" style="display: block; margin: auto;" />

``` r
# visualise incidence
plot(output, what = "incidence")
```

<img src="man/figures/unnamed-chunk-6-2.png" alt="" width="75%" style="display: block; margin: auto;" />

Hospital capacity can be displayed by toggling the `show_hosp_capacity`
argument.

``` r
# visualise prevalence with hospital capaciy
plot(output, what = "prevalence", show_hosp_capacity = TRUE)
```

<img src="man/figures/unnamed-chunk-7-1.png" alt="" width="75%" style="display: block; margin: auto;" />

### Accessing outputs

Objects returned by `run_simex()` have class `simex` and are a list with
at least:

- `prevalence` — long-format `data.table` with columns including `time`,
  `age`, `compartment` (`S`, `E`, `C`, `H`, `R`, `D`), `vax`, and
  `value`. Stochastic runs add `particle` and/or `sample` when relevant.
- `incidence` — same layout for daily flows into each stage (see
  `?extract`).
- `pars` — parameter object(s) passed into `run_simex()`.

Example: prevalence on day 150 for unvaccinated susceptibles:

``` r
subset(output$prevalence, time == 150 & compartment == "S" & vax == FALSE)
```

           age compartment    vax  time value
        <fctr>      <fctr> <lgcl> <int> <num>
     1:  age_1           S  FALSE   150  2493
     2:  age_2           S  FALSE   150   421
     3:  age_3           S  FALSE   150   326
     4:  age_4           S  FALSE   150   455
     5:  age_5           S  FALSE   150  1092
     6:  age_6           S  FALSE   150  1029
     7:  age_7           S  FALSE   150   931
     8:  age_8           S  FALSE   150   877
     9:  age_9           S  FALSE   150   619
    10: age_10           S  FALSE   150   574
    11: age_11           S  FALSE   150   444
    12: age_12           S  FALSE   150   475
    13: age_13           S  FALSE   150   503
    14: age_14           S  FALSE   150   554
    15: age_15           S  FALSE   150   424
    16: age_16           S  FALSE   150   610

Use `extract()` to aggregate over dimensions:

``` r
# long-form prevalence (default stratification)
extract(output, what = "prevalence")
```

              age compartment    vax  time value
           <fctr>      <fctr> <lgcl> <int> <num>
        1:  age_1           S  FALSE     1 15455
        2:  age_2           S  FALSE     1 14015
        3:  age_3           S  FALSE     1 12346
        4:  age_4           S  FALSE     1 10778
        5:  age_5           S  FALSE     1  9069
       ---                                      
    38396: age_12           D   TRUE   200     0
    38397: age_13           D   TRUE   200     0
    38398: age_14           D   TRUE   200     0
    38399: age_15           D   TRUE   200     0
    38400: age_16           D   TRUE   200     0

``` r
# restrict to a band of days
days_from <- 10
days_to <- 20
df <- extract(output, what = "prevalence")
df <- df[df$time %in% seq(days_from, days_to), ]
df
```

             age compartment    vax  time value
          <fctr>      <fctr> <lgcl> <int> <num>
       1:  age_1           S  FALSE    10 15454
       2:  age_2           S  FALSE    10 14015
       3:  age_3           S  FALSE    10 12346
       4:  age_4           S  FALSE    10 10778
       5:  age_5           S  FALSE    10  9069
      ---                                      
    2108: age_12           D   TRUE    20     0
    2109: age_13           D   TRUE    20     0
    2110: age_14           D   TRUE    20     0
    2111: age_15           D   TRUE    20     0
    2112: age_16           D   TRUE    20     0

### Modelling a single intervention

We use vaccination as an example intervention. The `vax_rate` argument
is the number of doses per day (not a proportion of the population);
here we use 100 doses per day.

``` r
# define vaccination rate (doses per day)
pars <- get_parameters(vax_rate = 100)

# run model
output <- run_simex(pars, time = 1:200)

# visualise prevalence
plot(output)
```

<img src="man/figures/unnamed-chunk-11-1.png" alt="" width="75%" style="display: block; margin: auto;" />

We can see that the daily increase in number of vaccinated individuals,
as well as the impact on infection and disease severity.

### Timed interventions, multiple interventions

To change parameters part-way through a run, pass a one-row **matrix**
of parameter lists whose **column names** are the first model day each
block applies to (one column must start on day 1, matching the Shiny
app). Below, isolation adherence becomes 0.5 from day 75 onward.

``` r
# column names = first day each parameter set applies
parlist <- matrix(
  c(list(get_parameters()), list(get_parameters(isolation_adherence = 0.5))),
  nrow = 1,
  dimnames = list(NULL, c("1", "75"))
)

output <- run_simex(parlist, time = 1:200)

# visualise prevalence
plot(output)
```

<img src="man/figures/unnamed-chunk-12-1.png" alt="" width="75%" style="display: block; margin: auto;" />

Comparing this figure with the first model run with no interventions
shows a clear reduction in severe outcomes when isolation is introduced
mid-outbreak.

### Comparing scenarios

It is useful to directly compare different scenarios visually. The
`vis_comparison()` function accepts a named list of `simex` objects. In
the example below, the default scenario is compared with isolation from
day 75.

``` r
# define two scenarios, one without intervention and one with isolation
parlists <- list(
  "No intervention" = get_parameters(),
  "Isolation on day 75" = matrix(
    c(list(get_parameters()), list(get_parameters(isolation_adherence = 0.5))),
    nrow = 1,
    dimnames = list(NULL, c("1", "75"))
  )
)

outputs <- lapply(parlists, run_simex, time = 1:200)

# compare scenarios
vis_comparison(outputs)
```

<img src="man/figures/unnamed-chunk-13-1.png" alt="" width="75%" style="display: block; margin: auto;" />

## \### Contributors

- [Finlay Campbell](https://github.com/finlaycampbell)
  (<campbellf@who.int>)
- Prabasaj Paul (<ppaul@who.int>)

**Maintainer:** Finlay Campbell

### Licensing

The simex software is made available by Collaboratory (2024) under an
[MIT license](LICENSE.md) ([citation file](CITATION.cff)).

The demographic data used in this software is made available by United
Nations (2024) under a [Creative Commons license CC BY 3.0
IGO](http://creativecommons.org/licenses/by/3.0/igo/). Source: \> United
Nations, Department of Economic and Social Affairs, Population Division
(2024). [World Population Prospects 2024, Online
Edition](https://population.un.org/wpp/).

The contact data used in this software is made available by Prem et
al. (2017) under a [Creative Commons license CC BY
4.0](https://creativecommons.org/licenses/by/4.0/). Source: \> Kiesha
Prem, Alex R. Cook, Mark Jit, *Projecting social contact matrices in 152
countries using contact surveys and demographic data*, PLoS Comp. Biol.
(2017), <https://doi.org/10.1371/journal.pcbi.1005697>.
