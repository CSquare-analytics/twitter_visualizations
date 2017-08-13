library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggrepel)

# Read data 
data.girls = fread("child-protection_FGMC_daughter-prevalence-database_updated-feb-2016-1.csv")
data.women = fread("child-protection_FGMC_women-prevalence-database_updated-feb-2016-1.csv")

data.girls %<>%  filter(., Country != "")
data.women %<>% filter(., Country != "")


# Join women and girls data, keep countries were both exist
percentages = dplyr::full_join(select(data.girls, 1:2), select(data.women, 1:2), by = "Country")
percentages %<>% arrange(-`FGM/C prevalence among girls and women (%)`)
percentages %<>% filter(complete.cases(.))
percentages %<>% mutate(diff = `FGM/C prevalence among girls and women (%)` - `FGM/C prevalence among girls (%)`)


# Prepare Labels 
labels.girls = percentages[,1:2] %>% 
  group_by(girls = round(`FGM/C prevalence among girls (%)`)) %>% 
  summarise(countries = paste(Country, collapse = ", ")) %>% 
  mutate(countries = paste0(girls, '% - ', countries))

labels.women = percentages[,c(1:3)] %>% 
  group_by(women = round(`FGM/C prevalence among girls and women (%)`)) %>% 
  summarise(countries = paste(paste(Country, collapse = ", "))) %>% 
  mutate(countries = paste0(countries, ' - ', women, '%'))

# Plot 
women.x = 1.5
ggplot(percentages) + 
  geom_segment(aes(x =  women.x, xend = women.x, y =  0, yend = 100), color = "grey85") + 
  geom_segment(aes(x =  3, xend = 3, y =  0, yend = 100), color = "grey85") + 
  
  geom_segment(aes(x = women.x, xend = 3, 
                   y = `FGM/C prevalence among girls and women (%)`, yend = `FGM/C prevalence among girls (%)`, 
                   color = diff)) +
  
  geom_point(aes(x = women.x, y = `FGM/C prevalence among girls and women (%)`, color = diff), size = 2)  +
  geom_text(data = labels.women, aes(x = women.x, y = women, label = countries), 
            hjust = "right", nudge_x = -0.1, vjust = 0.3 ,size=2.3) + 
  
  geom_point(aes(x = 3, y = `FGM/C prevalence among girls (%)`, color = diff), size = 2) + 
  geom_text(data = labels.girls, aes(x = 3, y = girls, label = countries), 
            hjust = "left", vjust = 0.5, nudge_x = 0.1,size=2.3) + 
  
  ggtitle("\n     Female genital mutilation prevalence\n") + 
  geom_text(aes(x = 3, y = 105, label = "Among girls aged\n0 to 14 years"), hjust = "center", size=3.5) + 
  geom_text(aes(x = women.x, y = 105, label = "Among girls & women aged \n15 to 49 years"), hjust = "center", size=3.5) + 
  
  scale_x_continuous(limits = c(0,5)) + 
  viridis::scale_color_viridis(option = "C", begin = 0.1, end = 0.9, direction = -1, name = "Difference:  \n",  
                               labels = function(x){paste0(x, "%")}, limits = c(0, 100)) + 
  
  theme_minimal() + 
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        
        plot.title = element_text(size = 19, face = "bold"),
        panel.grid = element_blank(),
        
        legend.position = "bottom",
        legend.justification = c(0.85, 0.5), 
        legend.key.height = unit(0.5, 'lines'),
        legend.key.width = unit(2, 'lines'))


  
