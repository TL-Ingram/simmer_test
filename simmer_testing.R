# simmer learning

install.packages("simmer")
library(simmer)

set.seed(1)

env <- simmer("Inpatient")
env

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
# rott = number removed from list (reneged?

# example: lets assume initial queue size = 100; demand = 20; capacity = 15; rott = 2
# DEMAND = 20
# CAPACITY = 15
# ROTT = 2
# set.seed(10)
# CHECKED = function() rexp(1,1)
# NEW_TASK<-function() rpois(20,1)
# ?rpois
# rpois(1,2)
# test <- rexp(100, 1)
rexp(100,20)
h <- plot(test, x = x)
lambda = 20
patient <- trajectory() %>%
  log_("start") %>%
  seize("nurse") %>%
  timeout(1) %>%
  release("nurse") %>%
  log_("complete")
env <- simmer() %>%
  add_resource("nurse", capacity = 20) %>%
  add_generator("patient", patient, function() rexp(1,lambda)) %>%
  run(until = 100)
resources <- get_mon_resources(env)
# get_n_generated(env, "patient")
# arrivals <- get_mon_arrivals(env)
# ?get_n_generated
# ?rexp
rexp(10,20)
# install.packages("simmer.plot")
# library(simmer.plot)
plot(resources, metric = "usage", "nurse", items = "queue")
# ?plot.mon
     

# Next I want to loop it through multiple times and then plot that to see all the simulations
# Also how do I start the queue at a certain size?


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