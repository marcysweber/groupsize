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


Sys.setenv("NETLOGO_HOME" = file.path("/Applications/NetLogo 7.0.2"))
netlogo_path <- file.path("/Applications/NetLogo 7.0.2")


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


model_path <- file.path("/Users/me591666/Documents/Github/groupsize/groupsizeforagingmodel.nlogox")

results <- run_experiment(
  netlogo_path = netlogo_path,
  model_path = model_path,
  setup_file = file.path("/Users/me591666/Documents/Github/groupsize/groupsizeforagingmodel-variation-tgt-neighbors-experiment.xml")
)


results |> glimpse()

