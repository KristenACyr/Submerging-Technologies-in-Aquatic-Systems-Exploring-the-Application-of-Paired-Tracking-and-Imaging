library(dplyr)
library(ggplot2)
library(stringr)
library("ggh4x")
library(forcats)
library(ggpubr)
library(grid)
library(tidyr)
library(viridis)
library(patchwork)
################################################################################



SDC = read.csv("BT_Master.csv")



#Calculate total number of unique publications per groups
SDC = SDC %>%
  group_by(Length_Groups, Depth_groups, Water_Type, 
           Device_Pairing_Abbreviation) %>%
  summarise(total = n_distinct(Manuscript))



#Change the names of abbreviations for device pairings to make clearer
SDC$Device_Pairing_Abbreviation = gsub("AC", "Acoustic",SDC$Device_Pairing_Abbreviation)
SDC$Device_Pairing_Abbreviation = gsub("R", "Radio",SDC$Device_Pairing_Abbreviation)

SDC$Device_Pairing_Abbreviation = gsub("SA", "Satellite",SDC$Device_Pairing_Abbreviation)
SDC$Device_Pairing_Abbreviation = gsub("AB", "Animal-borne",SDC$Device_Pairing_Abbreviation)
SDC$Device_Pairing_Abbreviation = gsub("DD", "Data Logger",SDC$Device_Pairing_Abbreviation)
SDC$Device_Pairing_Abbreviation = gsub("B", "Baited",SDC$Device_Pairing_Abbreviation)
SDC$Device_Pairing_Abbreviation = gsub("Baitedaited", "Baited",SDC$Device_Pairing_Abbreviation)



#create dataframe to plotted just looking at max depth cameras were deployed
depth = SDC %>% 
  group_by(Depth_groups,Device_Pairing_Abbreviation) %>% 
  summarise(total = sum(total))

#create dataframe to be plotted just looking at min length species were tagged
length = SDC %>% 
  group_by(Length_Groups, Device_Pairing_Abbreviation) %>% 
  summarise(total = sum(total))


#create a dataframe to be plooted just looking at percentage of studies in
#different aquatic bodies, fresh, salt, and brackish
watertype <- SDC %>% 
  group_by(Water_Type) %>% 
  summarise(total = sum(total)) %>%
  mutate(proportion = (total / 208) * 100)








#calculate total articles that deployed each device pairing to later be merged
device_totals <- length %>%
  group_by(Device_Pairing_Abbreviation) %>%
  summarise(total_articles = sum(total)) %>%
  ungroup()

# Merge total counts back into original dataframe for length
length <- length %>%
  left_join(device_totals, by = "Device_Pairing_Abbreviation") %>%
  mutate(Length_Groups = replace_na(as.character(Length_Groups), "Not Reported"))

# Merge total counts back into original dataframe for depth
device_totals <- depth %>%
  group_by(Device_Pairing_Abbreviation) %>%
  summarise(total_articles = sum(total)) %>%
  ungroup()





#########Create label for the plot to be formatted later in powerpoint########
# create labels for depth group to be clearer for legend
depth <- depth %>%
  left_join(device_totals, by = "Device_Pairing_Abbreviation") %>%
  mutate(
    Depth_groups = as.character(Depth_groups),             # convert factor to character
    Depth_groups = replace_na(Depth_groups, "Not Reported"), # replace NAs
    Depth_groups = factor(Depth_groups,                     # convert back to factor with all levels
                          levels = c("Surface to 2m", "2m to 61m", ">61m", "Not Reported"))
  )

#Arrange the labels from most commonly used to least
device_labels <- tibble(
  Device_Pairing_Abbreviation = unique(depth$Device_Pairing_Abbreviation)  # Get unique values
) %>%
  left_join(
    depth %>%
      group_by(Device_Pairing_Abbreviation) %>%
      summarise(total = sum(total)),  # Summarize total counts
    by = "Device_Pairing_Abbreviation"
  ) %>%
  arrange(total) %>%  # Order by total count (ascending)
  mutate(Device_Pairing_Abbreviation = fct_reorder(Device_Pairing_Abbreviation, total))

##############################################################################



























#####################################Figure 5######################################



##################
#plot the labels in ordered format, from above code,
#for the figure to later be used in powerpoint and edited
device_labels_plot <- 
  device_labels |>
  ggplot(
    aes(
      x = 1,
      y = Device_Pairing_Abbreviation,
      label = Device_Pairing_Abbreviation
    )
  ) +
  geom_text(size = 4.5) +
  theme_void()

device_labels_plot
######################






#####Depth plot for the left side
p <- ggplot(data = depth,
            aes(x = total, 
                y = reorder(Device_Pairing_Abbreviation, total_articles),
                fill = factor(Depth_groups, levels = c("Surface to 2m", "2m to 61m", ">61m", "Not Reported")))) +
  geom_col(width = .5) +
  scale_fill_viridis(discrete = TRUE, 
                     breaks = c("Surface to 2m", "2m to 61m", ">61m", "Not Reported"),
                     direction = -1, end = .75) +   # Clip the data (without removing)
  scale_x_continuous(lim = c(65,0), expand = c(0, 0), trans = "reverse") +
  scale_y_discrete(c(levels(length$Device_Pairing_Abbreviation), "extra_space"),
                   expand = expansion(mult = c(0, 0.1)),
                   position = "right"
  ) + # Move y-axis to the right side of the plot
  theme_classic() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_blank(),  # Remove y-axis labels for this plot
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.spacing = unit(1.5, "lines"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 20),
        plot.margin = margin(1,3,5,1, "cm"),
        plot.background = element_rect(fill = "lightblue", color = NA)) + 
  annotate(
    geom = "label",
    x = 35.5,
    y = 35.75,
    label = "Surface to 2m",
    fill = "#5ec962",
    color = "white",
    label.padding = unit(0.3, "lines"),
    size = 5
  ) + 
  annotate(
    geom = "label",
    x = 25.5,
    y = 35.75,
    label = "2m to 61m",
    fill = "#21918c",
    color = "white",
    label.padding = unit(0.3, "lines"),
    size = 5
  ) + 
  annotate(
    geom = "label",
    x = 18.55,
    y = 35.75,
    label = ">61m",
    fill = "#3b528b",
    color = "white",
    label.padding = unit(0.3, "lines"),
    size = 5
  ) + 
  annotate(
    geom = "label",
    x = 10.55,
    y = 35.75,
    label = "Not Reported",
    fill = "#440154",
    color = "white",
    label.padding = unit(0.3, "lines"),
    size = 5
  ) 

##########




####Length plot for the right side
b <- ggplot(data = length,
            aes(x = total, 
                y = reorder(Device_Pairing_Abbreviation, total_articles),
                fill = factor(Length_Groups, levels = c("Small (<28.5cm)", "Medium (28.5-61cm", "Large (>61cm)", "Not Reported")))) +
  geom_col(width = .5) +
  scale_fill_viridis(discrete = TRUE, 
                     breaks = c("Small (<28.5cm)", "Medium (28.5-61cm", "Large (>61cm)", "Not Reported"),
                     labels = c("Small (<28.5cm)", "Medium (28.5-61cm", "Large (>61cm)", "Not Reported"),
                     option = "plasma",
                     direction = -1, end = 0.75) +
  scale_x_continuous(lim = c(0,65), expand = c(0, 0)) +
  scale_y_discrete(c(levels(length$Device_Pairing_Abbreviation), "extra_space"),
                   expand = expansion(mult = c(0, 0.1))# Add more space at the top (10% more)
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.spacing = unit(1.5, "lines"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 20),
        plot.margin = margin(1,1,5,3, "cm"),
        plot.background = element_rect(fill = "lightblue", color = NA)) +
  annotate(
    geom = "label",
    x = 21.75,
    y = 35.75,
    label = "Small (<28.5cm)",
    fill = "#f89540",
    color = "white",
    label.padding = unit(0.3, "lines"),
    size = 5) + 
  
  annotate(
    geom = "label",
    x = 36.85,
    y = 35.75,
    label = "Medium (28.5-120cm)",
    fill = "#cc4778",
    color = "white",
    label.padding = unit(0.3, "lines"),
    size = 5
  ) + 
  annotate(
    geom = "label",
    x = 52,
    y = 35.75,
    label = "Large (>120cm)",
    fill = "#7e03a8",
    color = "white",
    label.padding = unit(0.3, "lines"),
    size = 5
  ) + 
  annotate(
    geom = "label",
    x = 9.55,
    y = 35.75,
    label = "Not Reported",
    fill = "#440154",
    color = "white",
    label.padding = unit(0.3, "lines"),
    size = 5
  ) 
###############



# Combine depth and length into single plot
figure = ggpubr::ggarrange(p, b, ncol = 2, nrow = 1, widths = c(1, 1), common.legend = FALSE) 
figure






#######plot portion of different water body types 
#######that were studied using different pairings, edit later in powerpoint
f = ggplot(watertype, aes(y = "Water Types", fill = Water_Type, x = proportion)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_viridis_d() +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(title = "Proportion of Different Water Types", x = "Proportion", y = NULL) +
  theme(axis.text.y = element_blank()) +  
  theme_void() +
  theme(
    title = element_blank(),
    legend.position = "none"
  )
f
##########

#######################################################################################
#######################################################################################







































###############################summary of depths/length and devices############
#####################################################################################
Fig5sum = read.csv("BT_Master.csv")

Tot_UniqueDEV = Fig5sum %>%
  select(Device_Pairing_Abbreviation) %>%
  unique() %>%
  count()

#count total number of unique device pairings
Tot_ModeDeviPair <- Fig5sum %>%
  group_by(Device_Pairing_Abbreviation) %>%
  summarise(total = n_distinct(Manuscript),
            porportion = (total/208)*100)


Tot_UniqueDEV = Fig5sum %>%
  select(Device_Pairing_Abbreviation) %>%
  unique() %>%
  count()


#percentage of articles reporting fresh, salt, and brackish water
watertype <- Fig5sum %>% 
  group_by(Water_Type) %>% 
  summarise(total = sum(n_distinct(Manuscript))) %>%
  mutate(proportion = (total / 208) * 100)

