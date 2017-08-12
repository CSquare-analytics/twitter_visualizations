library(data.table)
library(dplyr)
library(dtplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(foreach)

# Read data
data.path = "data/Youth literacy rate, population 15-24 years (%)/UNdata_Export_20170811_222341607.csv"
data = fread(data.path, encoding = "UTF-8")
data$`Observation Value` %<>% as.numeric()

# Preprare data table
data %<>% filter(.,  `Time Period` == 2015) 

# Sort countries
countries = data %>% 
  arrange(`Observation Value`) %>% 
  filter(Sex == "All genders") %>% 
  .[['Reference Area']]

# Add country as factor, soted by the literacy
data$country = factor(data$`Reference Area`, countries)

# Filter data and add the recatangle width
data %<>% filter(Sex != "All genders")
data %<>% select(-`Time Period`, -`Units of measurement`, - `Age group`) 
data$plot.val = data$`Observation Value`/2

# Generate plots
plots = foreach(i = 1:length(countries)) %do% {
  # Get country's data
  .country = levels(data$country)[i]
  country.df = filter(data, country == .country)
  
  # Generate plot
  plot = ggplot(country.df,
         aes(xmin = -plot.val, xmax = plot.val, ymin = -plot.val, ymax = plot.val,
             fill = Sex, color = Sex, frame = Sex)) +
    # Squares
    geom_rect(aes(xmin = -50, xmax = 50, ymin = -50, ymax = 50), color = "black", fill = "white", linetype = "dashed") +
    geom_rect(aes(xmin = -37.5, xmax = 37.5, ymin = -37.5, ymax = 37.5), fill = "white", linetype = "dashed", color = "grey80") +
    geom_rect(data = filter(country.df, Sex == "Male"), aes(xmin = 0), color = "white") +
    geom_rect(data = filter(country.df, Sex == "Female"), aes(xmax = 0), color = "white") +
    
    # Scales
    coord_equal() +
    scale_fill_manual(values = c("Male" = "#1B2F40", "Female" = "#FAA589")) +
    scale_color_manual(values = c("Male" = "#1B2F40", "Female" = "#FAA589")) +
    guides(fill = FALSE, color = FALSE) +
    
    # Themes
    ggtitle(.country) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          panel.spacing = unit(0.2, "lines"),
          axis.text = element_blank())
  
  return(plot)
}

# jpeg('literacy_youth.jpeg', width = 1500, height = 1200)
grid.arrange(grobs = plots[1:15], ncol = 5)
# dev.off()
