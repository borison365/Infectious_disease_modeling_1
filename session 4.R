
# PROBABILITY DISTRIBUTION


#1 - Gamma distribution

# Think of the gamma distribution as a flexible tool to describe "waiting times" or "time-to-event" situations, 
# where there’s a mix of fast and slow cases. helps capture this variation and gives us a better understanding of what’s likely to happen.

# the gamma distribution is a continuous probability distribution often used to model variables
# that are always positive and skewed, such as waiting times or generation intervals in epidemiology

# the time delay in the onset of symptoms bwn primary and secondary infections is called generation time
# Generation time is the period when the first person gets infected to the time he is able to infect another person.
# Generation time is not observable,but can be estimated from the serial interval.
# we can estimate the serial interval from the date of onset
# contact tracing investigation!

# classic def: Generation Time: Refers to the interval between the infection of a primary case and a secondary case.
# Serial Interval: Refers to the interval between the onset of symptoms in a primary case and a secondary case.

# AIM: access to covid 19 parameters to estimate generation time

library(tidyverse)

epiparameter::epiparameter_db(epi_name = "generation")  # for the generation time

covid_serialint <- epiparameter::epiparameter_db(
  disease = "covid",
  epi_name = "serial",
  # subset = sample_size == 131
  single_epiparameter = TRUE  # TO get the dataset with the highest sample size
) #%>% 
  # epiparameter::parameter_tbl()  # to get a summary table of the covid 19 data 

covid_serialint

# Get the summary statistics and parameters

covid_serialint$summary_stats$sd

# plot

covid_serialint %>% plot()

# get access to distribution parameters, like "sdlog"

covid_serialint_params <- covid_serialint %>% 
  epiparameter::get_parameters()
covid_serialint_params["meanlog"]

# 2  - Fixed distribution



# Delay distribution and case data

# AIM: create a gamma distribution, mean =4, sd = 2

EpiNow2::Gamma(
  sd = covid_serialint$summary_stats$sd,
  mean = covid_serialint$summary_stats$mean,
  max = 15
)
# it depends on the Shape (k),and scale. 
# if k =1, it’s just a steadily decreasing curve (like an exponential distribution).
# if k >1, it has a peak where events are most likely.

# Scale (θ): Controls how stretched out the distribution is. Bigger values mean events take longer on average.

covid_serialint_dist <-  EpiNow2::LogNormal(
  sd = covid_serialint$summary_stats$sd,
  mean = covid_serialint$summary_stats$mean,
  max = 15
)

plot(covid_serialint_dist)

# uncertain distribution, reporting delay

covid_reporting <- EpiNow2::Gamma(
  mean = EpiNow2::Normal(
    mean = 4, sd = 0.5),
    sd = EpiNow2::Normal(
      mean = 2, sd = 0.5),
  max = 20
)
plot(covid_reporting)
#
library(EpiNow2)
library(incidence2)
library(tidyverse)
dplyr::as_tibble(incidence2::covidregionaldataUK)

###   reporting delays

cases <- incidence2::covidregionaldataUK %>%
  as_tibble() %>% 
  # use {tidyr} to preprocess missing values
  tidyr::replace_na(base::list(cases_new = 0)) %>%
  # use {incidence2} to compute the daily incidence
  incidence2::incidence(
    date_index = "date",
    counts = "cases_new",
    count_values_to = "confirm",
    date_names_to = "date",
    complete_dates = TRUE
  ) %>%
  dplyr::select(-count_variable) %>% 
  dplyr::slice_head(n=90) # retain only the last 90 days

cases

# increase speed
withr::local_options(list(mc.cores= 4))

# estimate the Rt

estimates <- EpiNow2::epinow(
  # with slice_head we keep the first 90 rows of the data frame
  data = cases %>% dplyr::slice_head(n=90),
  generation_time = 
    EpiNow2::generation_time_opts(covid_serialint_dist),
  delays = EpiNow2::delay_opts(covid_reporting + covid_incubation_dist), # NEW object
  stan = EpiNow2::stan_opts(
    samples = 1000, chains = 3)
)

# AIM: get the incubation period for covid 19

covid_incubation <- epiparameter::epiparameter_db(
  disease = "covid",
  epi_name = "incubation",
  single_epiparameter = TRUE
)



covid_incubation_params<- epiparameter::get_parameters(
  covid_incubation
)

covid_serialint_params

covid_incubation_distr<- EpiNow2::LogNormal(
  meanlog = covid_incubation_params["meanlog"],
  sdlog = covid_incubation_params["sdlog"],
  max = 20
)
# from above 
reportx_delay_covid <- EpiNow2::epinow(
  # with slice_head we keep the first 90 rows of the data frame
  generation_time = EpiNow2::generation_time_opts(covid_serialint_dist),
  delays = EpiNow2::delay_opts(covid_reporting + covid_incubation_distr),
  stan = EpiNow2::stan_opts(samples = 1000, chains = 3)
)

# To calculate the Rt we need incidence data, generation time, serial interval, incubation time, reporting delays
######


####  corrected version

estimates <- EpiNow2::epinow(
  # with slice_head we keep the first 90 rows of the data frame
  data = cases %>% dplyr::slice_head(n=90),
  generation_time = 
    EpiNow2::generation_time_opts(covid_serialint_dist),
  delays = EpiNow2::delay_opts(covid_reporting + covid_incubation_distr), # NEW object
  stan = EpiNow2::stan_opts(
    samples = 1500, chains = 3)
)
# then run estimates to get the plots
estimates
