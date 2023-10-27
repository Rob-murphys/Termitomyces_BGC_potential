setwd("D:/University of Copenhagen/UCPH_Social & Symbiotic Evolution Group - Documents/Manuscripts in preparation/Schmidt Termitomyces/SI_v7")

library(dplyr)
library(tibble)
library(ggplot2)
library(readxl)
library(reshape2)
library(ComplexHeatmap)
library(circlize)

#==============================#
## Loading and formating data ##
#==============================#

# Reading in data
total_BGCs = read_excel("SI_TableS6_AllBGC_v7.xlsx")

# Selecting only the columns needed and filtering to main BGC classes
reduced_BGCs = total_BGCs %>%
  select("Sample ID", Species, Plotted_class, GCF) %>%
  filter(Plotted_class %in% c("NI-siderophore", "indole", "NRPS", "NRPS-like", "terpene", "RiPP")) %>%
  filter(GCF != "Singleton") %>%
  mutate(GCF_2 = paste("GCF", GCF, sep = "")) %>%
  select(!GCF)

colnames(reduced_BGCs) = c("Sample_ID", "species", "class", "GCF") # setting column names

#==============================#
## Making a heatmap dataframe ##
#==============================#

reduced_BGCs$present = 1 # introducing a column by which we can strech the dataframe into long b
heatmap_df = dcast(data = reduced_BGCs, formula = species ~ GCF, value.var = "present") # turning dataframe from long with wide
heatmap_df = column_to_rownames(.data = heatmap_df, var = "species") # setting row names
heatmap_df[heatmap_df > 0] = 1 # changing any value above 0 to 1 as we only care about + or -

#=======================#
## Making the heatmap  ##
#=======================#

## Class colours ##
#=================#

class_colours = read.csv("D:/University of Copenhagen/UCPH_Social & Symbiotic Evolution Group - Documents/Manuscripts in preparation/Schmidt Termitomyces/Figures/Figure 2/class_colours.csv") %>%
  select("class", "new.colours")

# generating a dataframe of annotation colours
column_ann = select(reduced_BGCs, c("GCF", "class", "species")) %>% # filtering only needed columns
  inner_join(class_colours, by = c("class"= "class")) %>% # merging with colour dataframe by class
  group_by(GCF) %>% # group by GFC
  slice(1) %>% # taking the first occurance of the grouped by variable ONLY
  ungroup

#making the Heatmap annotation funtion
col_ann = data.frame(class = column_ann$class)

colours = list("class" = c("indole" = "#FDF5A2",
                           "RiPP" =  "#98B5D5",
                           "terpene" = "#C38FC0", 
                           "NRPS-like" = "#93CD8F", 
                           "NI-siderophore" = "#F4837C",
                           "NRPS" = "#499A88"))

 
colAnn <- HeatmapAnnotation(df = col_ann,
                            which = 'col',
                            col = colours,
                            show_legend = FALSE,
                            show_annotation_name = FALSE)
## Host species colours ##
#========================#
species_data = read_excel("SI_TableS1_strains_ID.xlsx")
species_colours = read.csv("host_species_colours.csv")

species_data = inner_join(species_data, species_colours, by = c("Host" = "species"))

row_ann = select(reduced_BGCs, c("species", "Sample_ID")) %>% # filtering only needed columns
  inner_join(species_data, by = c("Sample_ID"= "ID")) %>% # merging with colour dataframe by class
  group_by(species) %>% # group by GFC
  slice(1) %>% # taking the first occurance of the grouped by variable ONLY
  ungroup

#making the Heatmap annotation funtion
ann  = data.frame(row_ann$Host)

colnames(ann) = "Host"
colours = list("Host" = c("Ancistrotermes cavithorax" = "#85d2d3", 
                           "Ancistrotermes guineensis" = "#85d2d3", 
                           "Ancistrotermes sp. " = "#85d2d3", 
                           "Macrotermes natalensis" = "#e8acae", 
                           "Macrotermes subhyalinus" = "#e8acae",
                          "Macrotermes bellicosus"= "#e8acae",
                          "Macrotermes gilvus" = "#e8acae",
                          "Odontotermes badius" = "#b3cea7",
                          "Odontotermes sp." = "#b3cea7",
                          "Odontotermes transvaalensis" = "#b3cea7",
                          "Microtermes sp." = "#d8b4dc", 
                          "Pseudacanthotermes"= "#a7c1e9",
                          "Uknown" = "#000000"))


rowAnn <- HeatmapAnnotation(df = ann,
                            which = 'row',
                            col = colours,
                            show_legend = FALSE,
                            show_annotation_name = FALSE)

## Making the actual heapmap ##
#=============================#
col_fun = colorRamp2(c(1, 0), c("#2a78b8", "white")) # colours for inside theheatmap

pdf(width=15, height=8, file = "D:/University of Copenhagen/UCPH_Social & Symbiotic Evolution Group - Documents/Manuscripts in preparation/Schmidt Termitomyces/Figures/Figure 2/GCF_heatmap_non_cluster.pdf")
Heatmap(heatmap_df, 
        col = col_fun,
        show_heatmap_legend = FALSE,
        row_dend_side = "left",
        row_names_side = "left",
        bottom_annotation = colAnn,
        row_dend_width = unit(2, "cm"),
        column_dend_height = unit(2, "cm"),
        cluster_rows = FALSE) # clusterd by "complete linkage method"

dev.off()

