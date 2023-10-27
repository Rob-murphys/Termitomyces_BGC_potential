setwd("D:/University of Copenhagen/UCPH_Social & Symbiotic Evolution Group - Documents/Manuscripts in preparation/Schmidt Termitomyces")

library(dplyr)
library(tibble)
library(ggplot2)
library(readxl)
library(reshape2)
library(ggbeeswarm)
library(ggpubr)

#### Pannel A violin plot ####
#============================#

total_BGCs = read_excel("SI_v7/SI_TableS6_AllBGC_v7.xlsx")

# selecting only the columns we need to only the classes we care about
reduced_BGCs = total_BGCs %>%
  select("Sample ID", Species, Plotted_class, Complete) %>%
  filter(Plotted_class %in% c("NI-siderophore", "indole", "NRPS", "NRPS-like", "terpene", "RiPP"))

species_list = NULL
for(i in unique(reduced_BGCs$Plotted_class)){
  df = reduced_BGCs %>%
    filter(Plotted_class == i) # selecting only the current class
  # for each species in that has this class get % of contings that are NOT near a contig edge
  species_list[[i]] = sapply(unique(reduced_BGCs$Species), function(x){ # sapply over the species
    df2 = df %>% # filtering only that current species
      filter(Species == x)
    nrow(df2[df2$Complete == "Yes",])/nrow(df2) # number of BGCs not at contig edge over all BGCS of that class n that species
  })
}

species_df = do.call(cbind, species_list)
species_df_long = melt(species_df, na.rm =TRUE, varnames = c( "species", "class"))
species_df_long$class = factor(species_df_long$class, levels = c("indole", "terpene", "NRPS-like", "NRPS", "NI-siderophore", "RiPP"))


cols = c("indole" = "#FDF5A2",
         "RiPP" =  "#98B5D5",
         "terpene" = "#C38FC0", 
         "NRPS-like" = "#93CD8F", 
         "NI-siderophore" = "#F4837C",
         "NRPS" = "#499A88")

class_shapes = c("NI-siderophore" = 15, "indole" = 16, 
                 "NRPS" = 17, "NRPS-like" = 18, 
                 "terpene" = 3, "RiPP" = 4)

BGC_completness_violin = ggplot(species_df_long, aes(x = class, y = value*100, fill = class))+
  geom_violin()+
  # geom_jitter(aes(shape = class))+
  geom_beeswarm(aes(shape = class), size = 2, cex = 1.5)+
  scale_fill_manual(values = cols)+
  scale_shape_manual(values = class_shapes)+
  theme_pubr()+
  theme(axis.text.y = element_text(colour = "black", size = 18), 
        axis.text.x = element_text(colour = "black", size = 18), 
        legend.position = "none", axis.title.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18, colour = "black"))+
  xlab("BCG class")+
  ylab("Precent of complete BGC")+
  scale_x_discrete(labels = c("Indole", "Terpene", "NRPS-like", "NRPS", "NI-siderophore", "RiPP"))


ggsave(plot = BGC_completness_violin, filename = "D:/University of Copenhagen/UCPH_Social & Symbiotic Evolution Group - Documents/Manuscripts in preparation/Schmidt Termitomyces/Figures/Figure 1/Panel_C_BGC_completness_violin.pdf")
#### Pannel A violin plot end ####

#### Pannel D heatmap ####
#========================#

total_BGCs = read_excel("SI_v7/SI_TableS6_AllBGC_v7.xlsx")

# selecting only the columns we need to only the classes we care about
reduced_BGCs = total_BGCs %>%
  select("Sample ID", Species, Plotted_class, Complete) %>%
  filter(Plotted_class %in% c("NI-siderophore", "indole", "NRPS", "NRPS-like", "terpene", "RiPP"))


colnames(reduced_BGCs) = c("Sample_ID", "species", "class", "Complete") 
reduced_BGCs$present = 1

class_profiles = dcast(data = reduced_BGCs, formula = Sample_ID ~ class, value.var = "present") # turning dataframe from long with wide

order = c("D44", "GCA3313075", "GCA3316525", "GCA3313675", "D8", "D27", "D9", "D25", "IC0020", "D45", "IC0026", "Micro", "GCA3313785", 
          "D42", "IC0035-1", "ANC-H", "IC0024", "ANC-D", "ANC-I", "IC0010", "IC0001", "IC0034", "IC0038", "IC0027", "IC0032", 
          "IC0033", "GCA1263195", "D11", "T112", "T153", "D46", "IC0019", "GCA3313055", "D59", "GCA1972325", "D52", "D28", "D58", "PseudaA")


class_profiles_ordered = class_profiles[match(order, class_profiles$Sample_ID),]

rownames(class_profiles_ordered) = NULL
class_profiles_ordered = column_to_rownames(class_profiles_ordered, var = "Sample_ID")

pdf(width=4, height=15, file = "D:/University of Copenhagen/UCPH_Social & Symbiotic Evolution Group - Documents/Manuscripts in preparation/Schmidt Termitomyces/Figures/Figure 1/Pannel D Class heatmap.pdf")
ComplexHeatmap::pheatmap(class_profiles_ordered, 
                         cluster_rows = FALSE,
                         show_rownames = FALSE, 
                         show_colnames = FALSE)
dev.off()
#### Pannel D heatmap end ####