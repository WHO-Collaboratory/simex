*simex*: a disease modelling tool for decision makers
---------------------------------------------------------------

[![MIT license](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE.md) [![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)

*simex* is an R package for simulating the spread of infectious diseases, as well as interventions such as social distancing measures, isolation and vaccination. It uses an age-structured SEIR compartmental model with country-specific age demographics and contact rates.

The transmission model is written in the [**odin DSL**](https://mrc-ide.github.io/odin2/) in [`inst/odin/simex.R`](inst/odin/simex.R). That specification is compiled with [**dust2**](https://mrc-ide.github.io/dust2/) to C++ for fast discrete-time simulation (see the generated sources under `src/`). The main R entry point that uses this backend is `run_simex()`.





Installation
-------------

To install the development version from GitHub:


``` r
remotes::install_github("WHO-Collaboratory/simex")
```

Load the package using:

``` r
library(simex)
```

Running *simex*
-------------

### Shiny App

You can use *simex* interactively by launching the Shiny app locally with `simex::run_shiny()`. The app runs the same odin/dust2 model via `run_simex()`. If you want to use it programmatically, follow the steps below.

### Parameters and settings

Parameters are specified via `get_parameters()`. The arguments and their
default values are summarised below:


```
Error in parse(text = input): <text>:5:6: unexpected symbol
4: 
5: Pass the
        ^
```

``` r
# set parameters using defaults
pars <- get_parameters()

# run odin/dust2 model
output <- run_simex(pars, time = 1:200)

# look at output
print(output)
```

```

 [Simex Object]
 - Time: 1 to 200
 - Age Categories: age_1 to age_16
 - Compartments: S | E | C | H | R | D
 - Particles: 1
 - Samples: 1
```

### Visualising outputs

To visualise the results, use the generic `plot()` method for the `simex` class.
Below we visualise prevalence and then incidence via the `what` argument.


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

### Accessing outputs

Objects of class `simex` contain **`prevalence`** and **`incidence`**: long-format
`data.table`s with these columns:

* **`time`** — model day.
* **`age`** — age stratum.
* **`compartment`** — model compartment: `S`, `E`, `C`, `H`, `R`, or `D`.
* **`vax`** — logical indicating vaccination status.
* **`value`** — number of individuals.
* **`sample`** — indexes the row of `pars`
* **`particle`** — indexes the repeated stochastic simulations

The **`pars`** object passed into `run_simex()` is stored as **`output$pars`**.

Subset and aggregate with **`data.table`** syntax:


``` r
# one day, one compartment, unvaccinated susceptible only
output$prevalence[time == 150L & compartment == "S" & !vax]
```

```
       age compartment    vax  time value
    <fctr>      <fctr> <lgcl> <int> <num>
 1:  age_1           S  FALSE   150  2494
 2:  age_2           S  FALSE   150   494
 3:  age_3           S  FALSE   150   340
 4:  age_4           S  FALSE   150   449
 5:  age_5           S  FALSE   150  1129
 6:  age_6           S  FALSE   150  1023
 7:  age_7           S  FALSE   150  1044
 8:  age_8           S  FALSE   150   828
 9:  age_9           S  FALSE   150   681
10: age_10           S  FALSE   150   629
11: age_11           S  FALSE   150   479
12: age_12           S  FALSE   150   423
13: age_13           S  FALSE   150   490
14: age_14           S  FALSE   150   591
15: age_15           S  FALSE   150   440
16: age_16           S  FALSE   150   611
```


``` r
# all strata between days 10 and 20 (inclusive)
output$prevalence[time %between% c(10L, 20L)]
```

```
         age compartment    vax  time value
      <fctr>      <fctr> <lgcl> <int> <num>
   1:  age_1           S  FALSE    10 15453
   2:  age_2           S  FALSE    10 14014
   3:  age_3           S  FALSE    10 12343
   4:  age_4           S  FALSE    10 10777
   5:  age_5           S  FALSE    10  9068
  ---                                      
2108: age_12           D   TRUE    20     0
2109: age_13           D   TRUE    20     0
2110: age_14           D   TRUE    20     0
2111: age_15           D   TRUE    20     0
2112: age_16           D   TRUE    20     0
```

The **`extract()`** helper remains available (e.g. credible intervals when
`sample` or `particle` are present); for simple shaping, prefer **`data.table`**
as above.

```

### Passing parameters to `simex`

`run_simex()` accepts `pars` in three formats.

1. **Single list from `get_parameters()`** — the same parameter values apply for
   the whole simulation.

2. **Named list of parameters** — names must be **numeric strings** giving
   the **first model day** each parameter set applies from.

3. **Matrix of parameters** — **columns** represent the time breaks as in
   (2). **Rows** are **parallel runs** of the system with different
   parameter values, for example when simulating across a posterior
   distribution of parameters values.


### Modelling a single intervention

Vaccination is controlled with **`vax_rate`** (doses per day, not a proportion of
the population).


``` r
# vaccination rate: doses per day
pars <- get_parameters(vax_rate = 100)

output <- run_simex(pars, time = 1:200)

plot(output, what = "prevalence")
```

<img src="man/figures/unnamed-chunk-9-1.png" alt="" width="75%" style="display: block; margin: auto;" />

### Timed interventions (single system, parameters change over time)

One epidemic with isolation adherence stepping to `0.5` from day 75, using a
**named list** of parameter lists (equivalent to a **one-row matrix** with
column names `c("1", "75")`).


``` r
pars_timed <- list(
  "1" = get_parameters(),
  "75" = get_parameters(isolation_adherence = 0.5)
)

output_timed <- run_simex(pars_timed, time = 1:200)

plot(output_timed, what = "prevalence")
```

<img src="man/figures/unnamed-chunk-10-1.png" alt="" width="75%" style="display: block; margin: auto;" />

### Comparing different scenarios (separate runs)

Two **different** scenarios require two **`run_simex()`** calls, not a
multi-row `pars` matrix. Pass a **named list** of `simex` objects to
**`vis_comparison()`**.


``` r
scenario_pars <- list(
  "No intervention" = get_parameters(),
  "Isolation from day 75" = list(
    "1" = get_parameters(),
    "75" = get_parameters(isolation_adherence = 0.5)
  )
)

outputs <- lapply(scenario_pars, run_simex, time = 1:200)

vis_comparison(outputs)
```

<img src="man/figures/unnamed-chunk-11-1.png" alt="" width="75%" style="display: block; margin: auto;" />

## Contributors

- [Finlay Campbell](https://github.com/finlaycampbell) (campbellf@who.int)
- Prabasaj Paul (ppaul@who.int)

**Maintainer:** Finlay Campbell

## Licensing

The simex software is made available by Collaboratory (2024) under an [MIT license](LICENSE.md) ([citation file](CITATION.cff)).

The demographic data used in this software is made available by United Nations (2024) under a [Creative Commons license CC BY 3.0 IGO](http://creativecommons.org/licenses/by/3.0/igo/). Source:
> United Nations, Department of Economic and Social Affairs, Population Division (2024). [World Population Prospects 2024, Online Edition](https://population.un.org/wpp/).

The contact data used in this software is made available by Prem et al. (2017) under a [Creative Commons license CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). Source:
> Kiesha Prem, Alex R. Cook, Mark Jit, *Projecting social contact matrices in 152 countries using contact surveys and demographic data*, PLoS Comp. Biol. (2017), https://doi.org/10.1371/journal.pcbi.1005697.
