# simmer learning

install.packages("simmer")
library(simmer)
library(simmer.plot)
library(readr)
library(tidyverse)
library(here)
library(parallel)

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
set.seed(10)
t = "Trauma & Orthopaedics"
u = "Active list"


for(i in list) {
  for (j in spec) {
    trauma <- parameters %>%
      filter(., speciality == "Urology") %>%
      filter(., list == u) %>%
             pivot_wider(id_cols = -c(speciality, time_period, list), 
                         names_from = "metric", values_from = "month_mean")
    
    if(trauma$demand & trauma$capacity & trauma$rott > 0) {

lambda = trauma$demand / 30
patient <- trajectory() %>%
  log_("start") %>%
  leave(function() runif(1) < ((trauma$rott /30) / ((trauma$capacity / 30) + (trauma$rott / 30))), keep_seized = F) %>%
  seize("nurse") %>%
  timeout(1) %>%
  release("nurse") %>%
  log_("complete")
env <-mclapply(1:100, function(x) {
  simmer() %>%
  add_resource("nurse", capacity = (trauma$capacity/30)) %>%
  add_generator("starter", patient, function() c(rep(0, sample((x_test - 1):x_test, 1)), -1)) %>%
  add_generator("patient", patient, function() rexp(1,lambda)) %>%
  run(until = 365) %>%
  wrap()
})
resources <- get_mon_resources(env)
plot(resources, metric = "usage", "nurse", items = "queue")
    }
  }
}


get_palette <- scales::brewer_pal(type = "qual", palette = 1)
plot(patient, fill = get_palette)


resources2<-dplyr::filter(resources,resource=="nurse")
resources2$date<-as.Date(resources2$time,origin="1970-01-01")
resources2$rwdate<-round_date(resources2$date,unit="week")
print(ggplot(resources2,aes(x=date,y=system,color=replication)) +
        geom_point(alpha=0.1,shape=14) +
        scale_color_gradient(low="blue", high="red") +
        stat_summary(aes(x=rwdate,y=system),fun.data="mean_sdl",geom="smooth",se=TRUE) +
        labs(x="Date",y="List size") +
        theme_bw(base_size=12))


sim_start_date = as.Date("2020-01-01")
sim_end_date = as.Date("2021-01-01")
patient_gen<-function(type) {
  date<-sim_start_date
  patients_today<-0
  last_patient<-0
  function() {
    if (patients_today>0){
      patients_today<<-patients_today-1
      return(0)
    } else {
      repeat {
        patients_today<<-rpois(1,max(totals$count[(totals$weekday==weekdays(date))&(totals$Admission.Type==type)]*7/as.numeric(daterange),0))
        if (patients_today>0){
          patients_today<<-patients_today-1
          gap<-date-last_patient
          last_patient<<-date
          date<<-date+1
          return(gap)
        } else {
          date<<-date+1
          if(date>=sim_end_date){return(-1)}
        }
      }
    }
  }
}


# trauma$rott / (trauma$capacity + trauma$rott)

#####

# rott = 0.005
# resources <- get_mon_resources(env)
# get_n_generated(env, "patient")
# arrivals <- get_mon_arrivals(env)
# ?get_n_generated
# ?rexp
# x_test = 100
# plot(resources, metric = "usage", "nurse", items = "queue")
# ?plot.mon
# ?add_dataframe
# ?sample
# Next I want to loop it through multiple times and then plot that to see all the simulations
# Also how do I start the queue at a certain size?
# rep(sample(98:100, 1),4)


# Arrival rate
# lambda <- 20
# Service rate (cars, motorcycles)
# mu <- c(2)
# Probability of car
# p <- 1

# Theoretical resolution
# A <- matrix(c(1,   mu[1],            0,
#               1, -lambda, (1-p)*lambda,
#               1,   mu[2],       -mu[2]), byrow=T, ncol=3)
# B <- c(1, 0, 0)
# P <- solve(t(A), B)
# N_average_theor <- sum(P * c(1, 0, 1)) ; N_average_theor
#> [1] 0.5031056