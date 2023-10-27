setwd("D:/University of Copenhagen/UCPH_Social & Symbiotic Evolution Group - Documents/Manuscripts in preparation/Schmidt Termitomyces/SI_v7/")

library(dplyr)
library(tibble)
library(ggplot2)
library(readxl)
library(reshape2)
library(vegan)
library(ape)
source("D:/OneDrive - University of Copenhagen/PhD/Projects/Termite_metagenomes_anvio/scripts/veganCovEllipse.R")
#==============================#
## Loading and formating data ##
#==============================#

# Reading in data
total_BGCs = read_excel("SI_TableS6_AllBGC_v7.xlsx")

# Selecting only the columns needed and filtering to main BGC classes
reduced_BGCs = total_BGCs %>%
  select("Sample ID", Species, Plotted_class, GCF) %>%
  filter(Plotted_class %in% c("NI-siderophore", "indole", "NRPS", "NRPS-like", "terpene", "RiPP")) %>%
  filter(GCF != "Singleton")

colnames(reduced_BGCs) = c("Sample_ID", "species", "class", "GCF") # setting column names
## Metadata ##

metadata = read_excel("SI_TableS1_strains_ID.xlsx")
host_colours = read.csv("host_species_colours.csv")

metadata = metadata %>%
  select(c("ID", "species", "Host"))
  # left_join(host_colours, by = c("Host" = "species")) %>%
  # arrange(ID)
  # group_by(Clade) %>%
  # slice(1) %>%
  # ungroup()

metadata$Host = sapply(metadata$Host, function(x){ # Removing species from the Host to only keep genus
  strsplit(as.character(x), " ", fixed = TRUE)[[1]][1]
})

metadata$Host = as.factor(metadata$Host)
metadata$species = as.factor(metadata$species)
#==============================#
## Making a profile dataframe ##
#==============================#

reduced_BGCs$present = 1 # introducing a column by which we can strech the dataframe into long b
GCF_profiles = dcast(data = reduced_BGCs, formula = Sample_ID ~ GCF, value.var = "present") # turning dataframe from long with wide
GCF_profiles = GCF_profiles %>%
  arrange(Sample_ID) %>%
  left_join(metadata, by = c("Sample_ID" = "ID")) %>%
  filter(Host != "Uknown" ) %>%
  column_to_rownames(var = "Sample_ID") %>% # setting row names
  select(!c("species", "Host"))


#==================================#
## Permanova of the GCF profiles ##
#==================================#
genome_metrics = read.csv("SI_TableS2_genome_stats_actual_csv.csv") %>%
  arrange(Sample) %>%
  filter(Termite.host != "unknown" )

metadata_all = metadata %>%
  left_join(genome_metrics, by = c("ID" = "Sample")) %>%
  filter(Host != "Uknown") %>%
  arrange(ID) %>%
  mutate_at(c("N50", "Busco.score"), as.numeric)

adonis_GCF_res = adonis2(GCF_profiles ~ Host + species + N50 + Busco.score, method = "bray", data = metadata_all, permutations = 10000)


