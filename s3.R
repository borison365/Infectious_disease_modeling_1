
library(cleanepi)
library(tidyverse)
library(incidence2)


data <- read_csv(
  here::here("data", 
             "simulated_ebola_2.csv")
)

clean_data <- data %>% 
  cleanepi::standardize_column_names()%>% # to standardize column names
  cleanepi::standardize_column_names(rename = c("index"="x1") 
                                     ) %>% 
  cleanepi::remove_constants(cutoff = 1) %>% 
  cleanepi::remove_duplicates() %>% 
  cleanepi::replace_missing_values(na_strings = "") %>% 
  cleanepi::convert_to_numeric(target_columns = "age") %>% 
  cleanepi::standardize_dates(                             # make all dates in one format
    target_columns = c("date_onset", "date_sample"),
    timeframe = c(as.Date("2014-01-1"), as.Date("2016-12-30")
                  ))  
view(clean_data) # after proper cleaning, this become a linelist


##########################################


df_incd <- incidence2::incidence(
  clean_data, 
  date_index = "date_onset",
  interval = 7,  # to get the incidence per week
  group = "status"   # to get the incidence by group: suspected , confirmed etc..
 
)
view(df_incd)

df_incd %>%
  ggplot()+ 
  geom_col(
    mapping = aes(x = date_index, y = count,
           fill = status), col = "black")+
  
  labs(x= "Weekly reports", y = "Count", title = "Weekly Incidence of Ebola", caption = "(B) - simulated data")+
  theme()+
  theme_bw()
  
  
  
  
 
 
