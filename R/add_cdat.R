#' Add country data to package from raw data
#'
#' @importFrom countrycode countrycode
#' @importFrom readxl excel_sheets read_excel
#' @importFrom usethis use_data
#'
add_cdat <- function() {

  ## read population data
  pop <- read.csv(system.file("extdata", "wpp_population.csv", package = "simex")) %>%
    group_by(
      age_floor = ifelse(AgeGrpStart>=75, 75, AgeGrpStart),
      iso3 = ISO3_code
    ) %>%
    summarise(count = 1000*sum(PopTotal), .groups = "drop") %>%
    group_by(iso3) %>%
    mutate(prop = count/sum(count)) %>%
    group_nest(.key = "pop", keep = FALSE) %>%
    filter(iso3 != "")

  ## read in polymod data and combine with population data
  cdat <- tibble(
    ## get list of countries and places for which polymod data exists
    file = list.files(
      system.file("extdata", "polymod", package = "simex"),
      full.names = TRUE
    ),
    place = sub("_.*", "", sub('.*MUestimates_', '', file)),
    country = map(file, excel_sheets)
  ) %>%
    unnest(country) %>%
    transmute(
      country, place,
      ## read in matrix of contacts for every country and place-type
      matrix = map2(
        file, country,
        function(file, country) {
          dat <- suppressMessages(read_excel(file, country, col_names = T))
          ## catch missing column names
          if(nrow(dat) == 15)
            dat <- suppressMessages(read_excel(file, country, col_names = F))
          as.matrix(dat)
        }
      )
    ) %>%
    nest(contacts = -country) %>%
    mutate(iso3 = countrycode(country, "country.name", "iso3c")) %>%
    ## join population data
    left_join(pop, by = "iso3") %>%
    transmute(
      iso3, country, pop,
      polymod = map2(
        contacts, pop,
        function(contacts, pop) {
          tibble(
            ## [from Prabasaj] Regularize the darned polymod matrix for the
            ## national population distribution Arithmetic mean, to ensure
            ## additivity and consistency with flu-models approach
            mod = map(
              setNames(contacts$matrix, contacts$place),
              ~ ((pop$count * .x) + t(pop$count * .x))/(2 * pop$count)
            ),
            ## Scale the polymod by population proportions
            scale = map(mod, ~ t(t(.x)/pop$prop))
          )}
      )
    ) %>%
    ## next steps are all reformatting into list for easier indexing
    unnest_wider(polymod) %>%
    split(setNames(.$iso3, .$iso3)) %>%
    map(~ map(.x, pluck, 1))

  usethis::use_data(cdat)

}
