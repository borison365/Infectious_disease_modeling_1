
# https://epiverse-trace.github.io/tutorials-late/modelling-interventions.html

library(epidemics)
library(socialmixr)
library(tidyverse)

bf_pop <- 23.5e6

bf_pop_obj <- epidemics::population(
  name = "Burkina Faso",
  demography_vector = bf_pop,
  contact_matrix = matrix(1.0), # looking  at the pop as a whole without breaking it down into subgroups
  initial_conditions = matrix(
    c(1-1/bf_pop, 0, 1/bf_pop, 0, 0), # the SEIRV model initial values respectively..
    nrow = 1, ncol = 5
  )
)


simple_model <- epidemics::model_default(
  population = bf_pop_obj, 
  recovery_rate = 1/5,
  transmission_rate = 9/5, # beta
  infectiousness_rate = 1/8, # alpha, 
  time_end = 120,         # in days
  increment = 1           # count the number of people after 1 days
  
)
view(simple_model) # first scenario, assuming homogeneity !, everyone interacts with everyone else..

simple_model %>% 
  ggplot(aes(x = time, 
             y = value, # the number of people moving through various compartments at t
             col = compartment)
         )+ 
  geom_line()   

# SECOND SCENARIO - ASSUMING HETEROGENEITY ! - Age structured population

bf_pop_group <- bf_pop*c(0.44, 0.195, 0.29, 0.05, 0.025)

view(bf_pop_group)

bf_survey <-  socialmixr::get_survey("https://doi.org/10.5281/zenodo.13101862") # we used data from the Gambia instead

bf_contact_data <- socialmixr::contact_matrix(survey = bf_survey,
                                              countries = "Gambia",
                                              age.limits = c(0, 15, 25, 55, 65))


bf_contact_data$participants # to view the details on %, age group and number of participants
bf_contact_data$matrix   # counts of contacts interactions between age groups. On average, an individual
# in age group 0-15, have 8.48 interactions.

bf_contact_matrix <- t(bf_contact_data$matrix) # we transpose the contact data
bf_contact_matrix
names(bf_pop_group) <- row.names(bf_contact_matrix) # we set the age interval as rownames in the bf_pop_group data table

init_conditions <- c(S = 1-1/1e6, E = 0, I = 1/1e6, R = 0, V = 0)
view(bf_pop_group)
init_conditions_matrix <- rbind(
  init_conditions,   # initial conditions for the first group
  init_conditions,
  init_conditions,
  init_conditions,
  init_conditions
)

bf_pop_group_obj <- epidemics::population(
  name = "B Faso",
  demography_vector = bf_pop_group, # the total population afore defined
  initial_conditions = init_conditions_matrix, # appends at each iteration
  contact_matrix = bf_contact_matrix # adds the average interactions
)

# create age-structured model

baseline_model <-  epidemics::model_default(
  population = bf_pop_group_obj,
  infectiousness_rate = 1/8,
  recovery_rate = 1/5,
  transmission_rate = 9/5,
  time_end = 120,
  increment=1
)
view(baseline_model)

# create epidemic curve 
baseline_model %>% 
  ggplot(
    aes(x= time, y = value, col= compartment,
        linetype = demography_group)
  )+
  geom_line(linewidth =1.2)+
  theme_bw()+
  labs(x = "Time (in days)",
       y = "cases", title = "Age structured SEIRV model"

  )

# create a compartment for vaccine interventions

vaccine_rollout <- epidemics::vaccination(
  name = "vaccine rollout",
  time_begin = matrix(c(25, 25, 25, 25, 25), nrow = 5, ncol = 1), # vaccinating all the age groups
  time_end = matrix(25 + 50, nrow(bf_contact_matrix)),
  nu = matrix(c(0.5, 0.1, 0.1, 0, 0))  # proportion vaccinated per age group.
)

# create the vaccine model

vaccine_model <- epidemics::model_default(
  population = bf_pop_group_obj,
  time_end = 120,
  increment = 1,
  recovery_rate = 1/5,
  infectiousness_rate = 1/8,
  transmission_rate = 9/5,
  vaccination = vaccine_rollout
)
view(vaccine_model)

vaccine_model %>% 
  ggplot(aes(x = time,
             y = value,
            col = compartment,
            linetype = demography_group,
            ))+
  geom_line(linewidth = 1.5)+
  labs(x = "Time", y= "Number of cases", title = "Vaccination model")+
  theme_bw()

# calculate new infections from baseline

baseline_data <- epidemics::new_infections(
  baseline_model, by_group = FALSE  # takes the new infections over time and display
)
view(baseline_data)

baseline_data$scenario <- "baseline"  # add the value baseline to a new column called scenario

# now view the data table again 
view(baseline_data)

# calculate new infections from vaccine model

vaccine_data <- epidemics::new_infections(
 vaccine_model, by_group = FALSE 
)
vaccine_data$scenario <- "Vaccine"

# combine baseline and vaccine data

baseline_vaccine_combined <- rbind(baseline_data, vaccine_data)
view(baseline_vaccine_combined)   # it keeps only the new infections at baseline + vaccine over time

# visualize the data 

baseline_vaccine_combined %>% 
  ggplot(aes(
    x = time, y = new_infections, col= scenario
  ))+
  geom_line(linewidth = 1)+
  labs(x = "Time in days",
       y = "Number of new infections", title = "Baseline versus Vaccine")


# vaccination by default should not be part of the infectious

vaccine_data <- epidemics::new_infections(
  vaccine_model, by_group = FALSE,
  compartments_from_susceptible = "vaccinated"
)
vaccine_data$scenario <- "Vaccine"

baseline_vaccine_combined <- rbind(baseline_data, vaccine_data)

# Now lets vizualise our data with the corrections

baseline_vaccine_combined %>%         # this plot now make sense
  ggplot(aes(
    x = time, y = new_infections, col= scenario
  ))+
  geom_line(linewidth = 1)+
  labs(x = "Time in days",
       y = "Number of new infections", title = "Baseline versus Vaccine")


              # PRACTICAL EXERCISE

gmb_pop <- 2.5e6
gmb_pop_group <- gmb_pop*c(0.4, 0.35, 0.25)

bf_survey <-  socialmixr::get_survey("https://doi.org/10.5281/zenodo.13101862")

contact_data1 <-  socialmixr::contact_matrix(survey = bf_survey,
                                             countries = "Gambia",
                                             age.limits = c(0, 20, 40))
contact_matrix_data1 <- contact_data1$matrix

# transpose the matrix
contact_matrix_data1 <- t(contact_matrix_data1)

names(contact_matrix_data1) = row.names(gmb_pop_group)

View(contact_matrix_data1)

init_conditions <- c(S = 1, E = 0, I = 0, R = 0, V = 0)
init_conditions_2nd <- c(S = 1-1/2.5e6, E = 1/2.5e6, I = 0, R= 0, V = 0)
init_conditions_3rd <- c(S = 1, E = 0, I = 0, R = 0, V = 0)

init_conditions_matrix1 <- rbind(init_conditions,
                                 init_conditions_2nd, 
                                 init_conditions_3rd
                                 )

gmb_pop_grp <-  epidemics::population(name = "Gambia",
                           demography_vector = gmb_pop_group,
                           contact_matrix = contact_matrix_data1,
                           initial_conditions = init_conditions_matrix1)

View(gmb_pop_grp)

vaccine_rollout1 <- epidemics::vaccination(
  name = "vaccine rollout",
  time_begin = matrix(c(50, 50, 50), nrow = 3, ncol = 1), # vaccinating all the age groups
  time_end = matrix(50 + 60, nrow(contact_matrix_data1)),
  nu = matrix(c(0.01, 0, 0))  # proportion vaccinated per age group.
)

# create the basleine model
baseline_1 <- epidemics::model_default(
  population = gmb_pop_grp,
  time_end = 110,
  increment = 1,
  recovery_rate = 1/5,
  infectiousness_rate = 1/8,
  transmission_rate = 9/5)

vaccine_model1 <- epidemics::model_default(
  population = gmb_pop_grp,
  time_end = 110,
  increment = 1,
  recovery_rate = 1/5,
  infectiousness_rate = 1/8,
  transmission_rate = 9/5,
  vaccination = vaccine_rollout1
)
baseline_data2 <- epidemics::new_infections(
  baseline_1, by_group = FALSE        # takes the new infections and display
)

baseline_data2$scenario <- "baseline"  # add the value baseline to a new column called scenario

# calculate new infections from vaccine model

vaccine_data <- epidemics::new_infections(
  vaccine_model1, by_group = FALSE 
)
vaccine_data$scenario <- "Vaccine"

# combine baseline and vaccine data

baseline_vaccine_combined <- rbind(baseline_data2, vaccine_data)
view(baseline_vaccine_combined)


baseline_vaccine_combined %>% 
  ggplot(aes(
    x = time, y = new_infections, col= scenario
  ))+
  geom_line(linewidth = 1)+
  labs(x = "Time in days",
       y = "Number of new infections", title = "Baseline versus Vaccine")


# NPI - Non Pharmaceutical Interventions day 3

# create mask intervention

mask <- epidemics::intervention(
  name = "mask", 
  time_begin = 30,     # After an outbreak, theres a period when an intervention begins (after 30 days)
  time_end = 30 + 60,   # intervention last for 60 days
  reduction = 0.3,  # estimate the reduction rate due to intervention from surveys, should be bwn 0 and 1
  type = "rate"     # since we want to reduce the rate, type = rate, if we want to reduce the contact, the type = contact
)

mask_model <- epidemics::model_default(
  population = bf_pop_group_obj,
  recovery_rate = 1/5,
  transmission_rate = 9/5,
  infectiousness_rate = 1/8,
  intervention = list(transmission_rate = mask),
  time_end = 120,
  increment = 1
)

mask_model %>% 
  ggplot(aes(x = time, y = value, col= compartment, linetype = demography_group))+
  geom_line(linewidth = 1.3)+
  labs(x = "Time in days", y = "Number of cases", title = "Number of reduction from NPI")+
  theme_bw()

# calculate new infections from mask model
mask_data <- epidemics::new_infections(
  mask_model, by_group = FALSE
)
mask_data
# add  a new column to mask data
mask_data$scenario <-  "Mask"

baseline_mask_combinedx <- rbind(
  baseline_data, mask_data
)
view(baseline_mask_combinedx)

baseline_mask_combinedx %>% 
  ggplot(aes(x= time, y = new_infections, col = scenario, linetype = scenario))+
  geom_line(linewidth = 1.2)+
  labs(x = "Time in days", y = "New infections", title = " Proportion reduction from NPI")+
  theme_bw()

# from the plot, we can observe a 30 % reduction i.e, max number of new infections = 2079356, so
# after the interventions, we have a reduction of 550071 which corresponds to the difference in the peaks bwn
# the baseline and the mask

