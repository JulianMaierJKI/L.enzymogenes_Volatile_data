#Analysis of aVOCs in vitro data

# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggpubr)
library(broom)
library(openxlsx)
library(FSA)

# Create Data sets


# set workdictionary
setwd("//do-fs-01v/bi/FG-Phytopatho/Julian Maier-OptiLyso/Publishing/Maier et al. 1/Mode of action/Abb_Graphen_Bilder/Media_screening")

#Read data
library(readxl)
aVOCs_media_test_ST_JM_R <- read_excel("VOCs_media_screening.xlsx")

# Cleaningthe data sets
#1 Create df with necesary columns
aVOCs_media_test_ST_JM_R <- aVOCs_media_test_ST_JM_R[,c(1:10)]
#1.2 Remove all contaminated plates
aVOCs_media_test_ST_JM_R_rmFungi <- aVOCs_media_test_ST_JM_R %>% filter(Cont == "0")
#1.3 Count the number of leftover plates per fungi and Ctrl
Replicate_number_media_test <- aVOCs_media_test_ST_JM_R_rmFungi %>%
  group_by(Fungi, Ctrl, dpi, Medium) %>%
  summarize(Replicates =sum(!is.na(Ctrl)))

#Check which fungi are still present
aVOCs_media_test_ST_JM_R_rmFungi$Fungi %>% unique()
#"A. radici" "R. solani"

#################

## Look for NAs in dfs
NA_rows_aVOCs_media_test_ST_JM_R <- which(rowSums(is.na(aVOCs_media_test_ST_JM_R_rmFungi)) > 0) #non
aVOCs_media_test_ST_JM_R_rmFungi_noCont_noNA <- aVOCs_media_test_ST_JM_R_rmFungi

#Give unified names
#### Run3
aVOCs_media_test_ST_JM_R_rmFungi$Fungi %>% unique()

aVOCs_media_test_ST_JM_R_rmFungi_noCont_noNA$Fungi <- dplyr::recode(aVOCs_media_test_ST_JM_R_rmFungi_noCont_noNA$Fungi,
                 "A. radici" = "A. radicina",
                 "R. solani" = "R. solani")

### Reorder them
aVOCs_media_test_ST_JM_R_rmFungi_noCont_noNA$Fungi  <- factor(aVOCs_media_test_ST_JM_R_rmFungi_noCont_noNA$Fungi, 
                                                               levels = c("A. radicina",
                                                                          "R. solani"))

#Order df alphabetically
aVOCs_media_test_ST_JM_R_rmFungi_noCont_noNA <- aVOCs_media_test_ST_JM_R_rmFungi_noCont_noNA[order(aVOCs_media_test_ST_JM_R_rmFungi_noCont_noNA$Fungi), ]

##NOTE: The package "car" has a recode function too. It could mask the recode function in dplyr

#Datensatz überprüfen
## Identify outliers

#Convert multiple columns to factors
aVOCs_media_test_ST_JM_R_rmFungi_noCont_noNA %>% colnames()
cols_aVOCs <- c("Fungi","Number","Ctrl","dpi","Finished", "Medium")
cols_aVOCs_num <- c("GR1","GR2","GR3")

library(magrittr)
library(dplyr)
aVOCs_media_test_ST_JM_R_rmFungi_noCont_noNA <- aVOCs_media_test_ST_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs, factor)

#Calculate the mean for each plate
df_aVOCs_media_test_ST_mean_GR <- aVOCs_media_test_ST_JM_R_rmFungi_noCont_noNA %>%
  mutate(across(c(GR1, GR2, GR3), ~ . - 0.3)) %>% #Subtract the radius of the plug (0.3 cm)
  mutate(mean_GR = rowMeans(across(c("GR1",   "GR2",   "GR3")), na.rm = TRUE)) %>% #Calculate the mean of the GR per 3 radii measured 
  dplyr::select("Fungi","Number","Ctrl","dpi","Finished","Medium", "mean_GR")

#Calculate statistics
###3
library(FSA)
df_aVOCs_media_test_ST_mean_GR_stat <- df_aVOCs_media_test_ST_mean_GR %>%
  group_by(Fungi, Ctrl, dpi, Medium) %>%
  summarize(mean=mean(mean_GR, na.rm = TRUE),
            median=median(mean_GR, na.rm = TRUE),
            Replicates =sum(!is.na(mean_GR)))

# Select the last measurement per fungi to calculate the relative inhibition
# Calculate relative change from the first time point in each group

#####3
df_aVOCs_media_test_ST_rel_Inh <- df_aVOCs_media_test_ST_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl, Medium) %>%
  slice(1) %>% 
  group_by(Fungi) %>%
  mutate(
    baseline = last(mean),  # baseline per group
    rel_pct_Inh = ifelse(
      baseline == 0, NA,      # avoid division by zero
      ((mean - baseline) / baseline)*100)*(-1)) %>%
  ungroup() %>%
  filter(Ctrl == "0") %>%
  dplyr::select(!(c("baseline", "Ctrl", "dpi", "median")))

# Save the dfs in a single excel file
#####3
All_DataSets_aVOCs_media_test_ST <- list("Raw_Data" = aVOCs_media_test_ST_JM_R,
                                                   "Cleaned_Data" = aVOCs_media_test_ST_JM_R_rmFungi_noCont_noNA, 
                                                   "Mean_GRs" = df_aVOCs_media_test_ST_mean_GR,
                                                   "Stats_by_dpi" = df_aVOCs_media_test_ST_mean_GR_stat,
                                                   "Data_Rel_Inh" = df_aVOCs_media_test_ST_rel_Inh)
write.xlsx(All_DataSets_aVOCs_media_test_ST, file = "All_DataSets_aVOCs_media_test_ST_JM_R.xlsx")
