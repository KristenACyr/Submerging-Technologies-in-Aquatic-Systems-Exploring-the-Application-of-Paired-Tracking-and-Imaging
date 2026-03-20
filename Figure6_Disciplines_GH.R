library(ggsankey)
library("tokenizers")
library(dplyr)
library(tidyr)
library(viridisLite)
library(viridis)
library(ggplot2)
library(forcats)

Master = read.csv("BT_Master.csv")






#Change the name of ecological disciplines to better fit on sankey plot
Master$Discipline = gsub("Other Drivers of Movement and Behavior", 
                         "Other Drivers",Master$Discipline)

#Make better abbreviations for device pairings to fit better on sankey plot
replacements <- c(
  "Animal-borne" = "AB",
  "Satellite" = "SA",
  "Acoustic" = "AC",
  "DD" = "DL",
  "Radio" = "R",
  "Baited" = "B"
)

for(i in names(replacements)){
  Master$Device_Pairing_Abbreviation <- gsub(i, replacements[i], Master$Device_Pairing_Abbreviation)
}

































##################################figure 6#########################################
########################################################################################
#Get the dataframe which has total number of Manuscripts for
#each Discipline, and what method was used so it can be plotted
#in ggalluvia
Alluvial <- Master %>%
  dplyr::select(Discipline, Device_Pairing_Abbreviation, Manuscript) %>%
  distinct(Discipline, Device_Pairing_Abbreviation, Manuscript)

#Aggregate the data to count how many manuscripts report each combination of tools
Alluvial_count <- Alluvial %>%
  count(Device_Pairing_Abbreviation, Discipline)



#Prepare the data for an alluvial plot
df <- Alluvial_count %>%
  make_long(Device_Pairing_Abbreviation, Discipline)

Alluvial_count %>% 
  select(Device_Pairing_Abbreviation) %>% 
  unique() %>% 
  count()
sum = Alluvial_count %>% 
  group_by(Device_Pairing_Abbreviation) %>% 
  summarise(total = sum(n))


unique(df$node)
df <- df %>% filter(node!="")  


sum = df %>% 
  filter(x == "Device_Pairing_Abbreviation") %>%
  group_by(node) %>%
  summarise(total = n())
######################


#Manually order the device pairings and ecological disciplines so the more
#commonly reported ones are at the top, and least are at the bottom in the plot
df$node <- factor(
  df$node,
  levels =
    c("DL + SA + Mobile", "AC + PIT + B", "R + Mobile + Stationary",
       "R + Mobile", "R + DL + AB", "PIT + Mobile",
       "DL + PIT + Mobile", "DL + Mobile", "AC + AB", 
       "PIT + SA + Mobile", "PIT + R + Stationary", "DL + SA + B",  
       "DL + PIT + AB", "DL + AB + Mobile","AC + SA + Mobile",
       "AC + R + Mobile", "AC + PIT + Stationary", "AC + PIT + R + Stationary",
        "AC + Mobile + Stationary", "AC + B + Mobile", "SA + Stationary",   
       "SA + Mobile", "SA + AB", "AC + SA + B", "AC + DL + Stationary",
       "DL + SA + AB", "DL + AB", "AC + DL + AB", "R + Stationary", "AC + Mobile",
       "AC + B", "PIT + Stationary", "DL + Stationary","AC + Stationary", 
       "Species Management","Resource-Use Management", "Conservation Measures", 
       "Reproductive Ecology", "Other Drivers", "Methodological","Behavioural Ecology")
)


############################################################################












































###################################Figure 6###################################
##############################################################################

#Format the alluvial plot

width <- .4

p <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = 1, node.color = "black", show.legend = FALSE, width = width) +
  theme_void() +
  theme(
    plot.margin = unit(rep(5.5, 4), "pt")
  ) +
  #I have tried to play around with this code to change scale but to no avail
  scale_fill_viridis_d(begin = 1.0, end = 0)


# Get the data from the flows layer
dat <- layer_data(last_plot(), 1) |>
  filter(x == 2 - width / 2) |>
  distinct(fill, flow_end_ymax, .keep_all = TRUE)



p = p +
  geom_rect(data = dat, aes(
    xmin = x, xmax = x + width,
    ymin = flow_end_ymin, ymax = flow_end_ymax,
    fill = label
  ), inherit.aes = FALSE) +
  geom_sankey_label(size = 5, color = "black", fill = "white") +
  guides(fill = "none") 



p

########################################################################################
















































####################################Summary stats for fig 6################################
############################################################################################

#Has species names
UniqueSpecies = read.csv("Unique_species.csv")
#########Figure 2##########


#total device pairings used here
Master %>%
  distinct(Device_Pairing_Abbreviation) %>%
  count()


##Calcuate the average eco discipline addressed per manuscript
AvEcoDisc_Per_Manu = Master %>%
  select(Manuscript, Discipline) %>%
  unique() %>%
  group_by(Manuscript) %>%
  count()

summary(AvEcoDisc_Per_Manu)


##Total disciplines and sub-disciplines each article was categorized under
Ecos_tots <- Master  %>%
  group_by(Manuscript) %>%
  summarise(
    Total_Disciplines = n_distinct(Discipline),
    Total_Subdisciplines = n_distinct(Sub_discipline),
    .groups = "drop"
  )
summary(Manus_tots$Total_Subdisciplines)


#total taxa, species, and devices studied/used in each article
SpDev_tots = UniqueSpecies %>% 
  group_by(Manuscript) %>% 
  summarise(
    Total_taxo = n_distinct(Taxa),
    Total_species = n_distinct(Species),
    Total_Devices = Abbreviation_Spec %>%
      str_split("\\s*\\+\\s*") %>%  # split by + with optional spaces
      unlist() %>%                   # flatten the list
      n_distinct(),                  # count unique devices
    .groups = "drop"
  )


#combine tables for totals of disciplines and studies to create summary table 
#for each article
Manuscript_Summary <- left_join(Manus_tots, manus_totsB, by = "Manuscript")


#calculate min, max, and mean of totals across articles
Overall_Summary <- Manuscript_Summary %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~ round(mean(.x, na.rm = TRUE), 0),
        min  = ~ min(.x, na.rm = TRUE),
        max  = ~ max(.x, na.rm = TRUE)
      )
    )
  )


#Calculate the total number of manuscripts 
#that addressed each eco discipline category
TotalManu_EcoDisc = Master %>%
  select(Manuscript, Discipline) %>%
  unique() %>%
  group_by(Discipline) %>%
  count() %>%
  ungroup() %>%
  mutate(prop = n/sum(n)*100,
         propmanu = n/208*100)


#calculate the total number of mansuscripts that
#addressed each sub-discipline category
TotalManu_EcoSUBDisc = Master %>%
  select(Manuscript, Sub_discipline, Discipline) %>%
  unique() %>%
  group_by(Discipline) %>%
  count(Sub_discipline) %>%
  ungroup() %>%
  mutate(prop = n/sum(n)*100,
         propmanu = n/208*100)



#Calculate the total number of manuscripts 
#that used each paired device method
TotalManu_PerGroup = Master %>%
  select(Manuscript, Device_Pairing_Abbreviation) %>%
  unique() %>%
  group_by(Device_Pairing_Abbreviation) %>%
  count() %>%
  ungroup() %>%
  mutate(prop = n/sum(n)*100)

#Caclulate the number of diverse pairings used for each eco discipline
TotalEcoDis_PerCombo <- Master %>%
  group_by(Discipline) %>%
  summarise(n_device_combos = n_distinct(Device_Pairing_Abbreviation))



##########################################################################
