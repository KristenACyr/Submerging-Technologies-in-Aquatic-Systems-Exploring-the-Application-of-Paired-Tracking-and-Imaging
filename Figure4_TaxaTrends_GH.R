library("rphylopic")
library("viridis")
library(dplyr)
library("ggtext")
library(extrafont)
loadfonts(device = "win")
library("patchwork")
library(stringr)
library(tidyverse)

########NEED OLDER GGPLOT TO FIT WITH RPHYLOPIC FOR THIS
######As of March 12, 2026. However, ggplot may make changes.
remotes::install_version("ggplot2", version = "3.4.4")
library("egg")
library(ggplot2)
################################################################

combo_diversity = read.csv("Unique_species.csv")


#remove entries that just have sp., and not specific species names
combo_diversity = combo_diversity %>%
  filter(!grepl(" sp\\.$", Species))






















#############################################FIGURE 4 PLOTTING##########################
#####################################################################################

##Import phylopics for taxa classifications

#Bivalve
Bivalve = get_uuid(name = "Bivalvia")
Bivalve_pic <- get_phylopic(Bivalve)
#attribution for phylopic artist
get_attribution(uuid = Bivalve)

#Cephalopoda
Cephalopoda = get_uuid(name="Alloteuthis africana")
Cephalopoda_pic = get_phylopic(Cephalopoda)
#attribution for phylopic artist
get_attribution(uuid = Cephalopoda)


#Cetacea
cetacea = get_uuid(name = "Kogia simus")
cetacea_pic = get_phylopic(cetacea)
#attribution for phylopic artist
get_attribution(uuid = cetacea)


#Chondrichthyes
Chondrichthyes = get_uuid(name = "Strophodus rebecae")
Chondrichthyes_pic <- get_phylopic(Chondrichthyes)
#attribution for phylopic artist
get_attribution(uuid = Chondrichthyes)


##Chondrostei
Chondrostei = get_uuid(name = "Acipenser")
Chondrostei_pic <- get_phylopic(Chondrostei)
#attribution for phylopic artist
get_attribution(uuid = Chondrostei)

#Decapoda
Decapoda = get_uuid((name = "Cancer magister"))
decapoda_pic = get_phylopic(Decapoda)
#attribution for phylopic artist
get_attribution(uuid = Decapoda)

##Gastropoda
Gastropoda = get_uuid(name = "Gastropoda")
Gastropoda_pic <- get_phylopic(Gastropoda)
#attribution for phylopic artist
get_attribution(uuid = Gastropoda)


#Phocidae
Phocidae = get_uuid(name = "Phocidae")
Phocidae_pic = get_phylopic(Phocidae)
#attribution for phylopic artist
get_attribution(uuid = Phocidae)


##Reptilia
Reptilia = get_uuid(name = "Eretmochelys imbricata")
Reptilia_pic <- get_phylopic(Reptilia)
#attribution for phylopic artist
get_attribution(uuid = Reptilia)


##Teleostei
Teleostei = get_uuid(name = "Salmoninae")
Teleostei_pic <- get_phylopic(Teleostei)
#attribution for phylopic artist
get_attribution(uuid = Teleostei)


##Mustelidae
Mustelidae = get_uuid(name = "Aonyx perspicillatus")
Mustelidae_pic <- get_phylopic(Mustelidae)
#attribution for phylopic artist
get_attribution(uuid = Mustelidae)


##Petromyzontida
Petromyzontida = get_uuid(name = "Petromyzon marinus")
Petromyzontida_pic <- get_phylopic(Petromyzontida)
#attribution for phylopic artist
get_attribution(uuid = Petromyzontida)


##Scyphozoa
Scyphozoa = get_uuid(name = "Scyphozoa")
Scyphozoa_pic <- get_phylopic(Scyphozoa)
#attribution for phylopic artist
get_attribution(uuid = Scyphozoa)





#####colours of phylopics in graph
taxa_cols <- c(
  "Bivalvia" = "#f89540",
  "Cephalopoda" = "#b5de2b",
  "Cetacea" = "#26828e",
  "Chondrichthyes" = "#440154FF",
  "Chondrostei" = "#414487FF",
  "Decapoda" = "#b218d8",
  "Gastropoda" = "#22A884FF",
  "Phocidae" = "#7AD151FF",
  "Reptilia" = "#FDE725FF",
  "Teleostei" = "#d51167",
  "Mustelidae" = "#6C2DC7",
  "Petromyzontida" = "#FF6D33",
  "Scyphozoa" = "#FF61CC"
)
###################################


#calculate total unique species study by each device, and taxa
species_per_taxa <- combo_diversity %>%
  group_by(Taxa, Abbreviation_Spec) %>%
  summarise(
    n_species = n_distinct(Species),
    .groups = "drop"
  )

##CREATE COLUMN WITH fuller names for device pairings, so its easier
##to see what exact devices were used
abbr_map <- c(
  "AC"   = "Acoustic",
  "PIT"  = "PIT",
  "DD"   = "Data Logger",
  "BRUV" = "BRUV",
  "R"    = "Radio",
  "SA"   = "Satellite",
  "AB"   = "Animal-borne",
  "ACA"  = "Autonomous Camera",
  "ACOA" = "Acoustic Camera",
  "AV"   = "Autonomous Video",
  "DR"   = "Drone",
  "SC"   = "Stationary Camera",
  "SV"   = "Stationary Video",
  "TOC"  = "Towed Camera",
  "TOV"  = "Towed Video",
  "ROV"   = "ROV",
  "DOV"   = "DOV",
  "SB"   = "Stationary Baited"
)





# format how the device pairings are listed in graph, to make more clear and 
# consistent
species_per_taxa <- species_per_taxa %>%
  rowwise() %>%
  mutate(
    Abbreviation_Spec_Full = str_split(Abbreviation_Spec, "\\s*\\+\\s*") %>% 
      map_chr(~ abbr_map[.x] %>% unname() %>% str_c(collapse = " + "))
  ) %>%
  ungroup()




#Plot the different taxa that were studied using different device pairings,
#as total species per taxa
TaxaCombo_Plot = ggplot(
  data = species_per_taxa,
  aes(x = reorder(Abbreviation_Spec_Full, n_species, FUN = base::sum),
      y = n_species,
      fill = Taxa))  +
  scale_fill_manual(values = taxa_cols) +
  geom_col(width = .75) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    limits = c(0,55),
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
    axis.text.y = element_text(size = 25),
    axis.text.x = element_text(size = 25),
    
    axis.title.x = element_text(size = 25, 
      margin = margin(20, 0, 0, 0)
    ),
    axis.title.y = element_text(size = 25, 
      margin = margin(0, 20, 0, 0)
    ),
    
    
    plot.margin = unit(c(1, 0.5, .75, 1.25),
                       "inches"),
    
    legend.position = "none") +
  # change axis titles
  xlab("Device Pairings") +
  ylab("Total Number of Species")

#put the plot so the bars are coming from y-axis so its easier to 
#read device pairings
TaxaCombo_Plot = TaxaCombo_Plot + coord_flip() 

TaxaCombo_Plot



###Add the phylopics and annotations to create legend for plot
TaxaCombo_Plot1 = TaxaCombo_Plot +
  #Bivalve Call
  add_phylopic(img = Bivalve_pic, alpha = 1, x = 52, y = 40, height = 2.25, fill =  "#f89540") +
  annotate("text", x = 52, y=44.5, label= "Bivalvia", size = 20/.pt, hjust = 0) +
  
  
  #Cephalopoda Call
  add_phylopic(Cephalopoda_pic, alpha = 1, x = 49.5, y = 40, height = 2, fill =  "#b5de2b") +
  annotate("text", x = 49.5, y=44.5, label= "Cephalopoda", size = 20/.pt, hjust = 0) +
  
  #Cetacea Call
  add_phylopic(cetacea_pic, alpha = 1, x = 47, y = 40, height = 2, fill =  "#26828e") +
  annotate("text", x = 47, y=44.5, label= "Cetacea", size = 20/.pt, hjust = 0) +
  
  #Chondrichthyan calling
  add_phylopic(Chondrichthyes_pic, alpha = 1, x = 44.5,y = 40, height = 2.5, fill =  "#440154FF") +
  annotate("text", x = 44.5, y=44.5, size = 20/.pt, label= "Chondricthyes", hjust = 0) +
  
  #Chondrostei calling
  add_phylopic(Chondrostei_pic, alpha = 1, x = 42,y = 40, height = 1.75, fill =  "#414487FF") +
  annotate("text", x = 42, y=44.5, label= "Chondrostei", size = 20/.pt, hjust = 0) +
  
  
  #Decapoda calling
  add_phylopic(decapoda_pic, alpha = 1, x = 40,y = 40, height = 2.5, fill =  "#b220d8") +
  annotate("text", x = 40, y=44.5, label= "Decapoda", size = 20/.pt, hjust = 0) +
  
  
  #Gastropoda calling
  add_phylopic(Gastropoda_pic, alpha = 1, x = 37.5,y = 40, height = 2.25, fill =  "#22A884FF") +
  annotate("text", x = 37.5, y=44.5, label= "Gastropoda", size = 20/.pt, hjust = 0) +
  
  
  #Mustelidae calling
  add_phylopic(Mustelidae_pic, alpha = 1, x = 35,y = 40, height = 2, fill =  "#6C2DC7") +
  annotate("text", x = 35, y=44.5, label= "Mustelidae", size = 20/.pt, hjust = 0) +
  
  
 #Petromyzontida calling
  add_phylopic(Petromyzontida_pic, alpha = 1,  x = 32.5,y = 40, height = 2, fill =  "#FF6D33") +
  annotate("text", x = 32.5, y=44.5, label= "Petromyzontida", size = 20/.pt, hjust = 0) +
  
  #Phocidae calling
  add_phylopic(Phocidae_pic, alpha = 1, x = 30,y = 40, height = 2.25, fill =  "#7AD151FF") +
  annotate("text", x = 30, y=44.5, label= "Phocidae", size = 20/.pt, hjust = 0) +
  
  
  #Reptilia calling
  add_phylopic(Reptilia_pic, alpha = 1, x = 27.5,y = 40, height = 2.5, fill =  "#FDE725FF") +
  annotate("text", x = 27.5, y = 44.5, label= "Reptilia", size = 20/.pt, hjust = 0) +
  
  #Scyphozoa calling
  add_phylopic(Scyphozoa_pic, alpha = 1, x = 25,y = 40, height = 2.5, fill =  "#FF61CC") +
  annotate("text", x = 25, y=44.5, label= "Scyphozoa", size = 20/.pt, hjust = 0) +
  
  #Teleostei calling
  add_phylopic(Teleostei_pic, alpha = 1, x =22.5,y = 40, height = 2, fill =  "#d51167") +
  annotate("text", x = 22.5, y=44.5, label= "Teleostei", size = 20/.pt, hjust = 0) 
  
######################################################################################################













































#######################################Summary Data for Figure 4#####################
##################################################################################
#count total number of unique device pairings
Tot_UniqueDEV = combo_diversity %>%
  select(Abbreviation_Spec) %>%
  unique() %>%
  count()



#Get the total species that have been studied, and total taxonmic groups
Tot_UniqueSp = combo_diversity %>%
  select(Species) %>%
  unique() %>%
  count()

#count total taxa
Tot_Uniquetax = combo_diversity %>%
  select(Taxa) %>%
  unique() %>%
  count()

#Count number of unique species and taxonomic groups per device pairings
summary_speciesPERdevice = combo_diversity %>%
  group_by(Abbreviation_Spec) %>%
  summarize(total_taxa = n_distinct(Taxa),
         total_species = n_distinct(Species),
         .groups = "drop"
  ) %>%
  mutate(
    taxa_percent = 100 * total_taxa /13,
    species_percent = 100 * total_species /273
  )

summary_species = combo_diversity %>%
  group_by(Taxa, Abbreviation_Spec) %>%
  summarize(n_articles = n_distinct(Manuscript), .groups = "drop")

sum(summary_species$n_articles)



taxaperdevice = summary_species = combo_diversity %>%
  group_by(Abbreviation_Spec) %>%
  summarize(n_taxa = n_distinct(Taxa), .groups = "drop")
