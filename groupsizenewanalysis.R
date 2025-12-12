library(logolink)
library(dplyr)
library(checkmate)
library(cli)
library(fs)
library(glue)
library(janitor)
library(magrittr)
library(purrr)
library(readr)
library(stringr)
library(xml2)
library(bslib)
library(covr)
library(knitr)
library(spelling)
library(testthat)
library(tibble)


Sys.setenv("NETLOGO_HOME" = file.path("/Users/me591666/Documents/NetLogo 7.0.2"))
netlogo_path <- file.path("/Users/me591666/Documents/NetLogo 7.0.2")

setup_file <- create_experiment(
  name = "Wolf Sheep Simple Model Analysis",
  repetitions = 10,
  sequential_run_order = TRUE,
  run_metrics_every_step = TRUE,
  setup = "setup",
  go = "go",
  time_limit = 1000,
  metrics = c(
    'count wolves',
    'count sheep'
  ),
  run_metrics_condition = NULL,
  constants = list(
    "number-of-sheep" = 500,
    "number-of-wolves" = list(
      first = 5,
      step = 1,
      last = 15
    ),
    "movement-cost" = 0.5,
    "grass-regrowth-rate" = 0.3,
    "energy-gain-from-grass" = 2,
    "energy-gain-from-sheep" = 5
  )
)

setup_file |> inspect_experiment_file()

setup_file = gsub("//", "/", setup_file)


model_path <- file.path(
  "/Users/me591666/Documents/Netlogo 7.0.2/models/IABM Textbook/chapter 4/Wolf Sheep Simple 5.nlogox"
)

results <- run_experiment(
  netlogo_path = netlogo_path,
  model_path = model_path,
  setup_file = setup_file
)

results |> glimpse()

library(dplyr)

results |> glimpse()
#> Rows: 110,110
#> Columns: 10
#> $ run_number             <dbl> 6, 7, 4, 3, 5, 2, 8, 1, 9, 2, 1, 6, 4, 7, 8,…
#> $ number_of_sheep        <dbl> 500, 500, 500, 500, 500, 500, 500, 500, 500,…
#> $ number_of_wolves       <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,…
#> $ movement_cost          <dbl> 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,…
#> $ grass_regrowth_rate    <dbl> 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3,…
#> $ energy_gain_from_grass <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
#> $ energy_gain_from_sheep <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,…
#> $ step                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,…
#> $ count_wolves           <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,…
#> $ count_sheep            <dbl> 500, 500, 500, 500, 500, 500, 500, 500, 500,…
