library(dplyr)
library(ggplot2)
library(viridis)
library(ggpubr)
library(sunburstR)
library(tidyverse)
library(stringr)


#Has species names
UniqueSpecies = read.csv("Unique_species.csv")



#################################FIGURE 3############################################
#####################################################################################



#create dataframe summary to be graphed



# get a dataframe which extracts publication year into new column
# and counts the total number of devices used for each year to be graphed
manus_devices <- UniqueSpecies %>%
  mutate(
    Year = as.numeric(str_extract(Manuscript, "\\d{4}"))
  ) %>%
  group_by(Manuscript, Year) %>%
  summarise(
    Num_Devices = Abbreviation_Spec %>%
      str_split("\\s*\\+\\s*") %>%
      unlist() %>%
      n_distinct(),
    .groups = "drop"
  )



#Define bins, to group years by 3 to better represent on x axis
breaks <- c(seq(1986, 2025, by = 3), 2026 + 1)  

# Define labels to match bins exactly
labels <- c(paste(seq(1986, 2022, by = 3), 
                  seq(1988, 2024, by = 3), sep = "-"), "2025-2026")

#Cut dataframe into defined bins
manus_devices$Year_bin <- cut(
  manus_devices$Year,
  breaks = breaks,
  labels = labels,
  right = FALSE,
  include.lowest = TRUE
)





#calculate the total studies per each year bin and device number to show
#temporal trends in pairing devices, including 2,3,4 devices
plot_data <- manus_devices %>%
  group_by(Year_bin, Num_Devices) %>%
  summarise(n_studies = n(), .groups = "drop") %>%
  # fill missing combinations with 0
  complete(
    Year_bin,
    Num_Devices = 2:4,  # adjust if you have more device counts
    fill = list(n_studies = 0)
  )


#graph the temporal trends of how papers have been pairing devices
temporal_trends = ggplot(plot_data, aes(x = Year_bin,
                      y = n_studies,
                      fill = factor(Num_Devices))) +
  geom_col(width = .75) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    limits = c(0, 50),
    expand = expansion(mult = c(0, 0))
  ) +
  
  theme_bw() +
  theme(
    # get rid of grey grid marks in the back
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    # set horizontal grid marks to follow y-values across
    panel.grid.major.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1,
                               size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    
    axis.title.x = element_text(
      size = 20, 
      margin = margin(30, 0, 0, 0)
    ),
    axis.title.y = element_text(
      size = 20, 
      margin = margin(0, 30, 0, 0)
    ),
    
    
    plot.margin = unit(c(1.5,0, .87, 0.1),
                       "inches")) +
  
  
  # change axis titles
  xlab("Publication Year") +
  ylab("Total Number of Studies") +
  labs(fill = "Total Devices \nPaired")


temporal_trends
################################################################################






























#############################SUMMARY DATA FOR FIGURE 3##########################
#################################################################################

#Summry of the total manuscripts across year bins
manus_bin_summary <- manus_devices %>%
  group_by(Year_bin) %>%
  summarise(
    n_manuscripts = n(),
    .groups = "drop"
  )  %>%
  mutate(
    percent_total = round(n_manuscripts / sum(n_manuscripts) * 100, 1)
  )





#Summary of total devices reported by manuscript
manus_device_summary <- manus_devices %>%
  group_by(Num_Devices) %>%
  summarise(
    n_manuscripts = n(),
    .groups = "drop"
  )  %>%
  mutate(
    percent_total = round(n_manuscripts / sum(n_manuscripts) * 100, 1)
  )
########################################################################################