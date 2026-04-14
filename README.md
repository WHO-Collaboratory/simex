
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
library("simex")
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

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
| population | NA | 1e+05 |
| R0 | The basic reproduction number. | 3 |
| generation_time | The mean generation time in days. | 8 |
| incubation_period | The mean incubation period in days. | 3 |
| infectiousness_presymp | Relative infectiousness of presymptomatic cases to symptomatic cases. | 0.25 |
| frac_symp | The proportion of cases that eventually develop symptoms. | 0.8 |
| ifr | Infection fatality rate provided either as a single value or as a vector of the same length as the number of age categories. Use the function ‘age_to_ifr’ to calculate a COVID-like IFR from a vector of ages. | age_to_ifr(get_age_median()) |
| hosp_mortality | The probability of death given a case is admitted to hospital, either as a single value or as a vector of the same length as the number of age categories. The inverse of the number of cases admitted to hospital per death. | 1/seq(20, 5, length = 16) |
| hosp_protection_death | Given a case requires hospitalisation, the proportion of deaths admission to hospital averts. | 0.75 |
| hosp_duration | The mean duration of stay in the hospital in days, either as a single value or as a vector of the same length as the number of age categories. | seq(7, 21, length = 16) |
| hosp_capacity | Total hospital bed capacity given as a proportion of the population. | 0.0025 |
| comm_mortality | The probability of death of cases that remain in the community, either as a single value or as a vector of the same length as the number of age categories. | rep(0, 16) |
| vax_rate | The daily rate of vaccination as a proportion of the population. | 0 |
| vax_infectiousness | The reduction (as a proportion) in infectioussness of an individual due to vaccination. | 0.3 |
| vax_infection | The protection (as a proportion) against infection provided by vaccination. | 0.5 |
| vax_hosp | The protection (as a proportion) against hospitalisation provided by vaccination, given infection. | 0.5 |
| vax_death | The protection (as a proportion) against death provided by vaccination, given hospitalisation. | 0.8 |
| isolation_adherence | The proportion of symptomatic individuals that adhere to isolation measures. | 0 |
| isolation_effectiveness | The reduction in daily transmission potential of a given individual due to adherence to isolation measures. | 0.8 |
| isolation_delay | The mean delay from symptom onset to isolation in days. | 3 |
| social_distancing | A named vector of length 4 containing the proportion reduction in contacts due to social distancing of ‘home’, ‘school’, ‘work’ and ‘other’. | c(home = 0, school = 0, work = 0, other = 0) |
| init_infections | NA | 5 |
| init_compartment | NA | “Eu” |
| vax_prioritised | A logical indicating whether older age groups are vaccinated first. | TRUE |
| hosp_prioritised | A logical indicating whether older age groups are hospitalised first when hospital capacity is exceeded. | TRUE |

### Running default settings

To run the model using default settings, pass the list from
`get_parameters()` to `run_simex()` and set `time` to the integer days
you want (inclusive).

``` r
## set parameters using defaults
pars <- get_parameters()

## run odin/dust2 model (example: days 1–365)
output <- run_simex(pars, time = 1:365)

## look at output
print(output)
```

    ## 
    ##  [Simex Object]
    ##  - Time: 1 to 365
    ##  - Age Categories: age_1 to age_16
    ##  - Compartments: S | E | C | H | R | D
    ##  - Particles: 0
    ##  - Samples: 0

### Visualising outputs

To visualise the results, use the generic `plot` function defined for
the `simex` class. Below, we first visualise prevalence and then
incidence, specified using the `what` argument.

``` r
## visualise prevalence
plot(output, what = "prevalence")
```

<img src="man/figures/unnamed-chunk-6-1.png" alt="" width="75%" style="display: block; margin: auto;" />

``` r
## visualise incidence
plot(output, what = "incidence")
```

<img src="man/figures/unnamed-chunk-6-2.png" alt="" width="75%" style="display: block; margin: auto;" />

Hospital capacity can be displayed by toggling the `show_hosp_capacity`
argument.

``` r
## visualise prevalence with hospital capaciy
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

Example: prevalence on day 250 for unvaccinated susceptibles:

``` r
subset(output$prevalence, time == 250 & compartment == "S" & vax == FALSE)
```

    ##        age compartment    vax  time value
    ##     <fctr>      <fctr> <lgcl> <int> <num>
    ##  1:  age_1           S  FALSE   250  2219
    ##  2:  age_2           S  FALSE   250   489
    ##  3:  age_3           S  FALSE   250   281
    ##  4:  age_4           S  FALSE   250   463
    ##  5:  age_5           S  FALSE   250   998
    ##  6:  age_6           S  FALSE   250   924
    ##  7:  age_7           S  FALSE   250   890
    ##  8:  age_8           S  FALSE   250   774
    ##  9:  age_9           S  FALSE   250   604
    ## 10: age_10           S  FALSE   250   577
    ## 11: age_11           S  FALSE   250   453
    ## 12: age_12           S  FALSE   250   439
    ## 13: age_13           S  FALSE   250   444
    ## 14: age_14           S  FALSE   250   543
    ## 15: age_15           S  FALSE   250   417
    ## 16: age_16           S  FALSE   250   606

Use `extract()` to aggregate over dimensions:

``` r
## long-form prevalence (default stratification)
extract(output, what = "prevalence")
```

    ##           age compartment    vax  time value
    ##        <fctr>      <fctr> <lgcl> <int> <num>
    ##     1:  age_1           S  FALSE     1 15454
    ##     2:  age_2           S  FALSE     1 14014
    ##     3:  age_3           S  FALSE     1 12345
    ##     4:  age_4           S  FALSE     1 10778
    ##     5:  age_5           S  FALSE     1  9069
    ##    ---                                      
    ## 70076: age_12           D   TRUE   365     0
    ## 70077: age_13           D   TRUE   365     0
    ## 70078: age_14           D   TRUE   365     0
    ## 70079: age_15           D   TRUE   365     0
    ## 70080: age_16           D   TRUE   365     0

``` r
## restrict to a band of days
days_from <- 10
days_to <- 20
df <- extract(output, what = "prevalence")
df <- df[df$time %in% seq(days_from, days_to), ]
df
```

    ##          age compartment    vax  time value
    ##       <fctr>      <fctr> <lgcl> <int> <num>
    ##    1:  age_1           S  FALSE    10 15453
    ##    2:  age_2           S  FALSE    10 14012
    ##    3:  age_3           S  FALSE    10 12341
    ##    4:  age_4           S  FALSE    10 10778
    ##    5:  age_5           S  FALSE    10  9069
    ##   ---                                      
    ## 2108: age_12           D   TRUE    20     0
    ## 2109: age_13           D   TRUE    20     0
    ## 2110: age_14           D   TRUE    20     0
    ## 2111: age_15           D   TRUE    20     0
    ## 2112: age_16           D   TRUE    20     0

### Modelling a single intervention

We use vaccination as an example intervention. Referencing the table
above, we can see that vaccination rate is specified using the
`vax_rate` argument and set it to 0.5% of the population per day.

``` r
## define vaccination rate
pars <- get_parameters(vax_rate = 0.005)

## run model
output <- run_simex(pars, time = 1:365)

## visualise prevalence
plot(output)
```

<img src="man/figures/unnamed-chunk-11-1.png" alt="" width="75%" style="display: block; margin: auto;" />

We can see that the daily increase in number of vaccinated individuals,
as well as the impact on infection and disease severity.

### Timed interventions, multiple interventions

To change parameters part-way through a run, pass a one-row **matrix**
of parameter lists whose **column names** are the first model day each
block applies to (one column must start on day 1, matching the Shiny
app). Below, isolation adherence becomes 0.5 from day 125 onward.

``` r
## column names = first day each parameter set applies
parlist <- matrix(
  c(list(get_parameters()), list(get_parameters(isolation_adherence = 0.5))),
  nrow = 1,
  dimnames = list(NULL, c("1", "125"))
)

output <- run_simex(parlist, time = 1:365)

## visualise prevalence
plot(output)
```

<img src="man/figures/unnamed-chunk-12-1.png" alt="" width="75%" style="display: block; margin: auto;" />

Comparing this figure with the first model run with no interventions, we
can see the proportion of deaths drops from about 0.7% to 0.4%; a
reduction in deaths of more than 40%!

### Comparing scenarios

It is useful to directly compare different scenarios visually. The
`vis_comparison()` function accepts a named list of `simex` objects. In
the example below, the default scenario is compared with isolation from
day 125.

``` r
## define two scenarios, one without intervention and one with isolation
parlists <- list(
  "No intervention" = get_parameters(),
  "Isolation on day 125" = matrix(
    c(list(get_parameters()), list(get_parameters(isolation_adherence = 0.5))),
    nrow = 1,
    dimnames = list(NULL, c("1", "125"))
  )
)

outputs <- lapply(parlists, run_simex, time = 1:365)

## compare scenarios
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
