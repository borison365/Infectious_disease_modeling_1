library(tidyverse)

cases
cases %>%
  dplyr::count(outcome) %>%
  tidyr::pivot_wider(names_from = outcome, values_from = n) %>%
  cleanepi::standardize_column_names() %>%
  dplyr::mutate(cases_known_outcome = death + recover)

# Load necessary libraries
library(ggplot2)
library(readr)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
linelist <- read_csv("data/linelist.csv")
# Read the dataset
linelist

inelist$date_of_hospitalisation <- as.Date(linelist$date_of_hospitalisation)
linelist$date_of_outcome <- as.Date(linelist$date_of_outcome)

# Filter the data for cases that appeared in May and June
linelist_filtered <- linelist %>%
  filter(format(date_of_hospitalisation, "%B") %in% c("May", "June") |
           format(date_of_outcome, "%B") %in% c("May", "June"))

# Filter for cases in May and June
linelist_filtered <- linelist %>%
  filter(format(date_of_hospitalisation, "%Y-%m") %in% c("2023-05", "2023-06") | 
           format(date_of_outcome, "%Y-%m") %in% c("2023-05", "2023-06"))

linelist_filtered$case_id
# Take a random sample of 30 unique case IDs
set.seed(123)  # For reproducibility
sampled_case_ids <- sample(unique(linelist_filtered$case_id), 30)

# Filter the dataset for the sampled case IDs
linelist_sampled <- linelist_filtered %>%
  filter(case_id %in% sampled_case_ids)
view(linelist_sampled)

# Reshape the data for ggplot
linelist_long <- linelist_sampled %>%
  pivot_longer(
    cols = c(date_of_hospitalisation, date_of_outcome),
    names_to = "date_type",
    values_to = "date"
  )
view(linelist_long)

# Create the plot
ggplot(linelist_long, aes(x = date, y = factor(case_id), color = date_type)) +
  geom_point(size = 3) + # Add points
  geom_line(data = linelist_long %>% arrange(case_id, date), # Connect points with lines
            aes(x = date, y = factor(case_id), group = case_id),
            inherit.aes = FALSE, color = "gray70", size = 0.8) +
  scale_color_manual(values = c("date_of_hospitalisation" = "purple", "date_of_outcome" = "red")) + # Customize colors
  scale_x_date(date_labels = "%b", date_breaks = "1 month") + # Format x-axis to show months
  labs(x = "Date", y = "Case ID", color = "Event type", caption = "(A) - linelist_data") + # Add labels
  theme_minimal() + # Use minimal theme
  theme(
    axis.text.x = element_text(size = 8), # Customize x-axis text size
    axis.text.y = element_text(size = 7.2), # Customize y-axis text size
    axis.title = element_text(size = 10), # Customize axis title size
    legend.title = element_text(size = 8), # Customize legend title size
    legend.text = element_text(size = 8) # Customize legend text size
  )

ggsave("final_plot.png", final_plot, dpi = 300, width = 12, height = 8, units = "in") # Save as PNG
ggsave("final_plot.pdf", final_plot, width = 12, height = 8) # Save as PDF (scalable)
############## Alternatively

# Enhanced Plot for Conference Presentation
ggplot(linelist_long, aes(x = date, y = factor(case_id), color = date_type)) +
  # Add points
  geom_point(size = 4, alpha = 0.8) + 
  # Connect points with lines
  geom_line(data = linelist_long %>% arrange(case_id, date),
            aes(x = date, y = factor(case_id), group = case_id),
            inherit.aes = FALSE, color = "gray70", size = 0.9, linetype = "dashed") +
  # Customize color palette
  scale_color_manual(
    values = c("date_of_hospitalisation" = "#0073C2FF", "date_of_outcome" = "#EFC000FF"),
    labels = c("Hospitalization Date", "Outcome Date")
  ) +
  # Customize x-axis for dates
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0.01, 0)) +
  # Add labels and title
  labs(
    x = "Month",
    y = "Case ID",
    color = "Event Type",
    title = "Hospitalization and Outcome Dates for Randomly Sampled Cases",
    caption = "Source: Linelist Data"
  ) +
  # Aesthetic improvements
  theme_light(base_size = 14) + # Light theme with base font size
  theme(
    panel.grid.major = element_line(color = "gray90", size = 0.5), # Major gridlines
    panel.grid.minor = element_blank(), # Remove minor gridlines
    axis.text.x = element_text(size = 12, face = "bold", color = "black"), # X-axis labels
    axis.text.y = element_text(size = 10, face = "italic", color = "black"), # Y-axis labels
    axis.title = element_text(size = 14, face = "bold"), # Axis titles
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # Title
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5), # Subtitle
    legend.position = "top", # Move legend to the top
    legend.title = element_text(size = 12, face = "bold"), # Legend title
    legend.text = element_text(size = 11) # Legend text
  )

