
library("viridis")
library(ggplot2)
library(dplyr)
library("ggalluvial")
library(stringr)
library("ggh4x")
library(forcats)
library(ggpubr)
library(tidyr)
library("DescTools")
library(sunburstR)
library(htmlwidgets)
#Setting working directory for Masters Chapter folder
setwd("D:/Documents/Masters/Chapters/Chapter 1/Tables")


DM = read.csv("BT_Master.csv")


  
###################################################################
########Change "not recorded" to NA's to calculate total NA's
DM = DM %>%
  mutate(Depth_groups = na_if(Depth_groups, "Not Reported"),
         Length_Groups = na_if(Length_Groups, "Not Reported"))



DM = DM %>%
  mutate(Recording_Hours = na_if(Recording_Hours, "Not Reported"),
         Tracking_Days = na_if(Tracking_Days, "Not Reported"))

DM = DM %>%
  mutate(Total_Field_Seasons = na_if(Total_Field_Seasons, "Not Reported"))
#############################################################



###Make a dataframe with columns that have grouped breaks for sunchart
Decision_sunchart = DM %>%
  select(Manuscript, Taxa, Sub_discipline, Discipline, Total_Field_Seasons,
         Water_Type,Depth_groups, Length_Groups, Tracking_Days, Recording_Hours, 
         Intermediate_Device_Pairing)


#Get rid of entries that had NA's
#If you want to see the full list of metadata, even with NA's then REMOVE
Decision_sunchart = Decision_sunchart %>%
  drop_na(Depth_groups|Length_Groups|Tracking_Days|Recording_Hours)
 

##Get rid of duplicates
Decision_sunchartdf = Decision_sunchart %>%
  group_by(Taxa, Water_Type, Discipline, Sub_discipline, Total_Field_Seasons,
           Depth_groups, Length_Groups, Tracking_Days, Recording_Hours, 
           Intermediate_Device_Pairing) %>%
  summarise(total = n_distinct(Manuscript))


#rename certain entries that use -, to make sure entries show up in the same layer
#of the suburst chart, and don't get automatically seperated
Decision_sunchartdf$Intermediate_Device_Pairing = gsub("Animal-borne", "AnimalBorne", 
                                     Decision_sunchartdf$Intermediate_Device_Pairing)
Decision_sunchartdf$Discipline = gsub("Resource-Use Management", "Resource Use Management", 
                                                     Decision_sunchartdf$Discipline)

Decision_sunchartdf$Length_Groups <- gsub("(\\d+\\.?\\d*)-(\\d+\\.?\\d*)", "\\1 to \\2",
                                        Decision_sunchartdf$Length_Groups)

Decision_sunchartdf$Depth_groups <- gsub("(\\d+\\.?\\d*)-(\\d+\\.?\\d*)", "\\1 to \\2",
                                       Decision_sunchartdf$Depth_groups)

Decision_sunchartdf$Tracking_Days <- gsub("(\\d+\\.?\\d*)-(\\d+\\.?\\d*)", "\\1 to \\2",
                                        Decision_sunchartdf$Tracking_Days)

Decision_sunchartdf$Recording_Hours <- gsub("(\\d+\\.?\\d*)-(\\d+\\.?\\d*)", "\\1 to \\2",
                                          Decision_sunchartdf$Recording_Hours)


####add entries into right format, and order for sunburst chart
Decision_sunchartdf$V1 = paste(Decision_sunchartdf$Taxa,"-", Decision_sunchartdf$Water_Type)
Decision_sunchartdf$V1 = paste(Decision_sunchartdf$V1,"-", Decision_sunchartdf$Discipline)
Decision_sunchartdf$V1 = paste(Decision_sunchartdf$V1,"-", Decision_sunchartdf$Sub_discipline)
Decision_sunchartdf$V1 = paste(Decision_sunchartdf$V1,"-", Decision_sunchartdf$Total_Field_Seasons)
Decision_sunchartdf$V1 = paste(Decision_sunchartdf$V1,"-", Decision_sunchartdf$Depth_groups)
Decision_sunchartdf$V1 = paste(Decision_sunchartdf$V1,"-", Decision_sunchartdf$Length_Groups)
Decision_sunchartdf$V1 = paste(Decision_sunchartdf$V1,"-", Decision_sunchartdf$Tracking_Days)
Decision_sunchartdf$V1 = paste(Decision_sunchartdf$V1,"-", Decision_sunchartdf$Recording_Hours)
Decision_sunchartdf$V1 = paste(Decision_sunchartdf$V1,"-", Decision_sunchartdf$Intermediate_Device_Pairing)

Decision_sunchartdf = Decision_sunchartdf %>%
  ungroup() %>%
  select(V1, total)
###############################################################################

































######################################Figure 8#####################################
p = sunburst(
  Decision_sunchartdf,
  withD3 = TRUE,
  colors= htmlwidgets::JS("
function() {
  debugger
  // this will be d3 version 4
  const node = d3.select(this).datum()
  let color
  
  if(node.depth > 0) {  // 2nd level
    const ancestors = node.depth === 1 ? [node.parent, node] : node.ancestors().reverse()
    // inefficient to define every time but will do this way for now
    color = d3.scaleOrdinal(d3.schemeCategory10)
      .domain(ancestors[0].children.map(d=>d.data.name))
    return(d3.color(color(ancestors[1].data.name)).brighter((node.depth - 1)/4))
  }
}
  ")
)

p































############################SUMMARY STATS FOR FIG.8############################
################################################################################
#count the total number of articles that were included in the summary metadata 
#table
df = DM %>% 
  drop_na(Depth_groups|Length_Groups|Tracking_Days|Recording_Hours) %>% 
  select(Manuscript) %>% 
  unique()

#calculate total number of NAs per each column
NA_summary <- DM %>%
  summarise(
    Total_Field_Seasons_NA = n_distinct(Manuscript[is.na(Total_Field_Seasons)]),
    Max_C_Depth_m_NA = n_distinct(Manuscript[is.na(Max_C_Depth_m)]),
    Min_length_Tag_cm_NA = n_distinct(Manuscript[is.na(Min_length_Tag_cm)]),
    Max_Days_Tracked_NA = n_distinct(Manuscript[is.na(Max_Days_Tracked)]),
    Max_Hours_Recorded_NA = n_distinct(Manuscript[is.na(Max_Hours_Recorded)])
  )
