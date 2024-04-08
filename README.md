*simex*: a disease modelling tool for decision makers
---------------------------------------------------------------

*simex* is an R package for simulating the spread of infectious diseases, as
well as interventions such as social distancing measures, isolation and
vaccination. It uses an age-structured SEIR compartmental model with
country-specific age demographics and contact rates.



Installation
-------------

To install the development version from github:

<p align='center'>

```r
remotes::install_github("finlaycampbell/simex", dependencies = TRUE, force = TRUE)
```

</p>

Load the package using:

<p align='center'>

```r
library("simex")
```

</p>

Running *simex*
-------------

### Parameters and settings

Most settings are specified via the `get_parameters` function. The arguments and
their default values are given below:

<p align='center'>

|Argument                |Description                                                                                                                                                                                                                   |Default value                                |
|:-----------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:--------------------------------------------|
|iso3                    |The ISO3 code of the country used to draw age-distributions and contact rates from.                                                                                                                                           |"USA"                                        |
|R0                      |The basic reproduction number.                                                                                                                                                                                                |2.5                                          |
|generation_time         |The mean generation time in days.                                                                                                                                                                                             |7                                            |
|incubation_period       |The mean incubation period in days.                                                                                                                                                                                           |3                                            |
|infectiousness_presymp  |Relative infectiousness of presymptomatic cases to symptomatic cases.                                                                                                                                                         |0.25                                         |
|frac_symp               |The proportion of cases that eventually develop symptoms.                                                                                                                                                                     |0.8                                          |
|ifr                     |Infection fatality rate provided either as a single value or as a vector of the same length as the number of age categories. Use the function 'age_to_ifr' to calculate a COVID-like IFR from a vector of ages.               |age_to_ifr(get_age_median())                 |
|hosp_mortality          |The probability of death given a case is admitted to hospital, either as a single value or as a vector of the same length as the number of age categories. The inverse of the number of cases admitted to hospital per death. |1/seq(20, 5, length = 16)                    |
|hosp_protection_death   |Given a case requires hospitalisation, the proportion of deaths admission to hospital averts.                                                                                                                                 |0.75                                         |
|hosp_duration           |The mean duration of stay in the hospital in days, either as a single value or as a vector of the same length as the number of age categories.                                                                                |seq(7, 21, length = 16)                      |
|hosp_capacity           |Total hospital bed capacity given as a proportion of the population.                                                                                                                                                          |0.0025                                       |
|comm_mortality          |The probability of death of cases that remain in the community, either as a single value or as a vector of the same length as the number of age categories.                                                                   |0                                            |
|vax_rate                |The daily rate of vaccination as a proportion of the population.                                                                                                                                                              |0                                            |
|vax_infectiousness      |The reduction (as a proportion) in infectioussness of an individual due to vaccination.                                                                                                                                       |0.3                                          |
|vax_infection           |The protection (as a proportion) against infection provided by vaccination.                                                                                                                                                   |0.5                                          |
|vax_hosp                |The protection (as a proportion) against hospitalisation provided by vaccination, given infection.                                                                                                                            |0.5                                          |
|vax_death               |The protection (as a proportion) against death provided by vaccination, given hospitalisation.                                                                                                                                |0.8                                          |
|isolation_adherence     |The proportion of symptomatic individuals that adhere to isolation measures.                                                                                                                                                  |0                                            |
|isolation_effectiveness |The reduction in daily transmission potential of a given individual due to adherence to isolation measures.                                                                                                                   |0.8                                          |
|isolation_delay         |The mean delay from symptom onset to isolation in days.                                                                                                                                                                       |3                                            |
|social_distancing       |A named vector of length 4 containing the proportion reduction in contacts due to social distancing of 'home', 'school', 'work' and 'other'.                                                                                  |c(home = 0, school = 0, work = 0, other = 0) |
|vax_prioritised         |A logical indicating whether older age groups are vaccinated first.                                                                                                                                                           |TRUE                                         |
|hosp_prioritised        |A logical indicating whether older age groups are hospitalised first when hospital capacity is exceeded.                                                                                                                      |TRUE                                         |

</p>

### Running default settings

To run the model using default settings, specify a parameter object `par` and
feed this into the `run_model` function.

<p align='center'>

```r
## set parameters using defaults
pars <- get_parameters()

## run model using default parameters
output <- run_model(pars)

## look at output
print(output)
```

```
## 
##  [Simex Object]
##  - Days: 1 to 365
##  - Age Categories: age_1 to age_16
##  - Compartments: S_u | E_u | C_u | H_u | R_u | D_u | S_v | E_v | C_v | H_v | R_v | D_v
```

</p>

### Visualising outputs

To visualise the results, use the generic `plot` function defined for the
`simex` class. Below, we first visualise prevalence and then incidence,
specified using the `what` argument.

<p align='center'>

```r
## visualise prevalence
plot(output, what = "prevalence")
```

<img src="figure/unnamed-chunk-6-1.png" width="75%" style="display: block; margin: auto;" />

```r
## visualise incidence
plot(output, what = "incidence")
```

<img src="figure/unnamed-chunk-6-2.png" width="75%" style="display: block; margin: auto;" /></p>

Hospital capacity can be displayed by toggling the `show_hosp_capacity`
argument.

<p align='center'>

```r
## visualise prevalence with hospital capaciy
plot(output, what = "prevalence", show_hosp_capacity = TRUE)
```

<img src="figure/unnamed-chunk-7-1.png" width="75%" style="display: block; margin: auto;" /></p>

### Accessing outputs

The `output` object is of class `simex` and is a list containing four items:

* `prevalance` is an array with 3 dimensions: time (365 days) x age (16
  categories) x state (12 compartments). The compartments are S (susceptible), E
  (exposed, pre-symptomatic), C (symptomatic in community), H (sympomatic in
  hospital), R (recovered), D (dead). Each comparment is split into unvaccinated
  (with subscript _u) and vaccinated (with subscript _v). Each value in the
  array represents the proportion of the population in that given age-disease
  compartment on a given day.
* `deltas` is an array with the same dimensions as `prevalence`. Each value
  represents the change in a given age-disease compartment from the previous
  day.
* `incidence` is an array with the same dimensions as `prevalence`. Each value
  represents the new additions to a given age-disease comparment on a given
  day. For the exposed category E, this represents incidence of new infections,
  for the hospitalised category H this represents new hospital admissions, and
  for the dead category D this represents new deaths.
* `pars` contains the parameter set initially fed into `run_model`.

Accessing the data is easiest using array indexing. Remember, the dimensions are
time, age and compartment. For example, accessing the prevalence on the 250th
day is done with `output$prevalence[250,,]`:

<p align='center'>

```
##         state
## age      S_u E_u C_u H_u R_u D_u S_v E_v C_v H_v R_v D_v
##   age_1  1.7   0   0   0 3.8 0.0   0   0   0   0   0   0
##   age_2  0.9   0   0   0 5.0 0.0   0   0   0   0   0   0
##   age_3  0.5   0   0   0 5.6 0.0   0   0   0   0   0   0
##   age_4  0.3   0   0   0 6.3 0.0   0   0   0   0   0   0
##   age_5  0.9   0   0   0 5.7 0.0   0   0   0   0   0   0
##   age_6  0.9   0   0   0 5.7 0.0   0   0   0   0   0   0
##   age_7  1.1   0   0   0 5.9 0.0   0   0   0   0   0   0
##   age_8  0.9   0   0   0 5.8 0.0   0   0   0   0   0   0
##   age_9  0.9   0   0   0 5.6 0.0   0   0   0   0   0   0
##   age_10 1.0   0   0   0 5.0 0.0   0   0   0   0   0   0
##   age_11 1.1   0   0   0 5.0 0.1   0   0   0   0   0   0
##   age_12 1.7   0   0   0 4.3 0.1   0   0   0   0   0   0
##   age_13 3.0   0   0   0 3.1 0.1   0   0   0   0   0   0
##   age_14 3.5   0   0   0 2.1 0.1   0   0   0   0   0   0
##   age_15 3.0   0   0   0 1.4 0.1   0   0   0   0   0   0
##   age_16 5.6   0   0   0 1.7 0.2   0   0   0   0   0   0
```

</p>

Accessing the prevalence of the 1st age compartment (0-4) and 1st infectious
compartment (unvaccinated susceptible) for days 30 to 35 is done with
`output$prevalence[130:135,1,1]`:

<p align='center'>

```
## day_130 day_131 day_132 day_133 day_134 day_135 
##     5.1     5.1     5.1     5.0     5.0     4.9
```

</p>

The outputs can also be extracted in `tibble` form using the `extract` function,
once again using the `what` argument to specify whether prevalence or incidence
is extracted.

<p align='center'>

```r
## extract prevalence
extract(output, what = "prevalence")
```

```
## # A tibble: 4,380 × 4
##      day compartment vax          value
##    <dbl> <fct>       <lgl>        <dbl>
##  1     1 S           FALSE 1.00        
##  2     1 E           FALSE 0           
##  3     1 C           FALSE 0.0000000469
##  4     1 H           FALSE 0           
##  5     1 R           FALSE 0           
##  6     1 D           FALSE 0           
##  7     1 S           TRUE  0           
##  8     1 E           TRUE  0           
##  9     1 C           TRUE  0           
## 10     1 H           TRUE  0           
## # ℹ 4,370 more rows
```

</p>
If we want to filter this list for a sequence of days, we can then do basic
data.frame manipulation:

<p align='center'>

```r
## define start and end days
days_from <- 10
days_to <- 20

## extract prevalence
df <- extract(output, what = "prevalence")
df <- df[df$day %in% seq(days_from, days_to),]
df
```

```
## # A tibble: 132 × 4
##      day compartment vax      value
##    <dbl> <fct>       <lgl>    <dbl>
##  1    10 S           FALSE 1.00e+ 0
##  2    10 E           FALSE 4.30e- 8
##  3    10 C           FALSE 6.82e- 8
##  4    10 H           FALSE 1.33e- 9
##  5    10 R           FALSE 5.26e- 8
##  6    10 D           FALSE 4.17e-11
##  7    10 S           TRUE  0       
##  8    10 E           TRUE  0       
##  9    10 C           TRUE  0       
## 10    10 H           TRUE  0       
## # ℹ 122 more rows
```

</p>

### Modelling a single intervention

We use vaccination as an example intervention. Referencing the table above, we
can see that vaccination rate is specified using the `vax_rate` argument and
set it to 0.5% of the population per day.

<p align='center'>

```r
## define vaccination rate
pars <- get_parameters(vax_rate = 0.005)

## run model
output <- run_model(pars)

## visualise prevalence
plot(output)
```

<img src="figure/unnamed-chunk-12-1.png" width="75%" style="display: block; margin: auto;" /></p>

We can see that the daily increase in number of vaccinated individuals, as
well as the impact on infection and disease severity.


### Specifying an initial state

The default initial state begins with an entirely susceptible, unvaccinated
population and a single infection. We can also specify a custom initial state by
passing a matrix specifying the number of individuals in each age-infection
compartment. In the example below, we extract the initial state from the
previous run, and modify the compartments so half the susceptible population is
assigned to the vaccinated compartment.

<p align='center'>

```r
## extract starting point from previous run
state <- output$prevalence[1,,]

## assign half of susceptibles to vaccinated
state[,"S_u"] <- state[,"S_v"] <- state[,"S_u"]/2

## run model
output <- run_model(pars, init_state = state)

## visualise prevalence
plot(output)
```

<img src="figure/unnamed-chunk-13-1.png" width="75%" style="display: block; margin: auto;" /></p>

We can see that the simulation now begins with a 50% vaccinated population,
significantly reducing the size and impact of the pandemic.

### Timed interventions, multiple interventions

Sometime we only want to introduce interventions at a certain time, or we want
to model multiple, successive interventions. We can do this easily by generating
a named list of parameters, where the name gives the time that parameter set
should be used from. In the example below, we introduce isolation measures with
an adherence of 50% on the 125th day.

<p align='center'>

```r
## introduce isolation on the 125th day
parlist <- list(
  "1" = get_parameters(),
  "125" = get_parameters(isolation_adherence = 0.5)
)

## run model
output <- run_model(parlist)

## visualise prevalence
plot(output)
```

<img src="figure/unnamed-chunk-14-1.png" width="75%" style="display: block; margin: auto;" /></p>

Comparing this figure with the first model run with no interventions, we can see
the proportion of deaths drops from about 0.7% to 0.4%; a reduction in deaths of
more than 40%!

### Comparing scenarios

It is useful to directly compare different scenarios visually. The
`vis_comarison` function does exactly this, and accepts a named list of `simex`
objects. In the example below, we generate a named list of lists that compares
the default scenario (no interventions) with the a scenario where isolation is
introduced on the 125th day.
	
<p align='center'>

```r
## define two scenarios, one without intervention and one with isolation
parlists <- list(
  "No intervention" = get_parameters(),
  "Isolation on day 125" = list(
    "1" = get_parameters(),
    "125" = get_parameters(isolation_adherence = 0.5)
  )
)

## run model across set of parameter lists
outputs <- lapply(parlists, run_model)

## compare scenarios
vis_comparison(outputs)
```

<img src="figure/unnamed-chunk-15-1.png" width="75%" style="display: block; margin: auto;" /></p>

Contributors
------------
- [Finlay Campbell](https://github.com/finlaycampbell) (campbellf@who.int)
- Prabasaj Paul (ppaul@who.int)

**Maintainer:** Finlay Campbell
