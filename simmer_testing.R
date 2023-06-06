# simmer learning

install.packages("simmer")
library(simmer)
library(simmer.plot)
library(readr)
library(tidyverse)
library(here)

#####

# install.packages("simmer.plot")

# resource = server + queue
# server

# queue (size)

# source (creates new arrivals)

#arrival (the process that interacts with resources - may have prioritisation or other characteristics attached)
# every arrival is attached to a trajectory

# trajectory (a linkage of arrivals, the recipe they must follow)

# activity
# demand = number added to the list
# capacity = number removed from list by completed elective cycle
# rott = number removed from list (reneged?)

#####
parameters <- read_rds(here("all_spec.rds"))
# list <- parameters %>%
#   distinct(list) %>%
#   pull(list)
# spec <- parameters %>%
#   distinct(speciality) %>%
#   pull(speciality)

# Do it for T&O first
    # for(i in wl_type) {
    #   wl_prepared <- wl_comp %>%
    #     filter(., wl == i,
    #            date < train_halt)
    #   # Filter to speciality
    #   for (j in speciality) {
    #     wl_prep <- wl_prepared %>%
    #       mutate(date = ymd(date)) %>%
    #       filter(., speciality == j)
trauma <- parameters %>%
  filter(., speciality == "Trauma & Orthopaedics") %>%
  filter(., list == "Active list") %>%
  pivot_wider(id_cols = -c(speciality, time_period, list), 
              names_from = "metric", values_from = "month_mean")
set.seed(10)
lambda = trauma$demand / 30
patient <- trajectory() %>%
  log_("start") %>%
  leave(function() runif(1) < ((trauma$rott /30) / ((trauma$capacity / 30) + (trauma$rott / 30))), keep_seized = F) %>%
  seize("nurse") %>%
  timeout(1) %>%
  release("nurse") %>%
  log_("complete")
env <- simmer() %>%
  add_resource("nurse", capacity = (trauma$capacity/30)) %>%
  # add_generator("starter", patient, function() c(rep(0, sample((x_test - 1):x_test, 1)), -1)) %>%
  add_generator("patient", patient, function() rexp(1,lambda)) %>%
  run(until = 365)
resources <- get_mon_resources(env)
plot(resources, metric = "usage", "nurse", items = "queue")

trauma$rott / (trauma$capacity + trauma$rott)

#####

# rott = 0.005
# resources <- get_mon_resources(env)
# get_n_generated(env, "patient")
# arrivals <- get_mon_arrivals(env)
# ?get_n_generated
# ?rexp
x_test = 100
plot(resources, metric = "usage", "nurse", items = "queue")
# ?plot.mon
?add_dataframe
?sample
# Next I want to loop it through multiple times and then plot that to see all the simulations
# Also how do I start the queue at a certain size?
rep(sample(98:100, 1),4)


# Arrival rate
lambda <- 20
# Service rate (cars, motorcycles)
mu <- c(2)
# Probability of car
p <- 1

# Theoretical resolution
A <- matrix(c(1,   mu[1],            0,
              1, -lambda, (1-p)*lambda,
              1,   mu[2],       -mu[2]), byrow=T, ncol=3)
B <- c(1, 0, 0)
P <- solve(t(A), B)
N_average_theor <- sum(P * c(1, 0, 1)) ; N_average_theor
#> [1] 0.5031056