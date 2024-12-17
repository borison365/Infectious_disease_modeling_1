
library(tidyverse)

cases <- read_csv(here::here("data", "linelist.csv"))
view(cases)
cases %>% 
  dplyr::select(outcome) %>% 
  dplyr::count(outcome) %>% 
  tidyr::pivot_wider(names_from = outcome, values_from = n) %>% 
  cleanepi::standardize_column_names() %>%     # converts all col names to lower case
  dplyr::mutate(cfr_naive = death/(death + recover))   # cfr the case fatality risk

cases %>% 
  dplyr::select(case_id, 
         date_of_onset, 
         date_of_hospitalisation) %>% 
  dplyr::mutate(
    reporting_delay = date_of_hospitalisation - date_of_onset) %>% 
  ggplot(aes(x = reporting_delay)) +
  geom_histogram(binwidth = 1)+
  labs(x = "Reporting delay", y = "Count", 
       title = "Delays Between Onset of Symptoms and Hospitalization",
       caption = "(C) -linelist_data",
       plot.title = element_text(size = 16, face = "bold", hjust = 0.5))+
 
  theme()

# incidence curve
cases %>%
  ggplot(aes(x = date_of_onset)) +
  geom_histogram(binwidth = 7)
  
cases %>%
  ggplot(aes(x = date_of_onset)) +
  geom_histogram(binwidth = 7, fill = "gray", color = "black") +
  geom_density(aes(y = ..count..), color = "blue", size = 1)


# Calculate the counts for each bin
library(dplyr)
library(ggplot2)

binned_data <- cases %>%
  mutate(week = cut(date_of_onset, breaks = "week")) %>%
  group_by(week) %>%
  summarise(count = n())

# Plot the histogram and overlay a line
cases %>%
  ggplot(aes(x = date_of_onset)) +
  geom_histogram(binwidth = 7, fill = "gray", color = "black") +
  geom_line(data = binned_data, aes(x = as.Date(week), y = count), 
            color = "red", size = 1) +
  geom_point(data = binned_data, aes(x = as.Date(week), y = count), 
             color = "red", size = 2)
