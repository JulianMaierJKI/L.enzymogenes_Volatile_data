#Analysis of aVOCs in vitro data - Long term effects

# Load libraries
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggpubr)
library(purrr)
library(magrittr)
library(FSA)

# Create Data sets

# set workdictionary
setwd("C:/Users/Julian.Maier/Documents/Manuscript_Urlaub/Fig_2")

# Read data
# Load multiple excel sheets into separate dfs

library(purrr)
df_list <- map(purrr::set_names(excel_sheets("Zusammenfassung_aller_Daten_RAW.xlsx")),
               read_excel, path = "Zusammenfassung_aller_Daten_RAW.xlsx")

purrr::map(names(df_list),
           ~assign(.x, df_list[[.x]], envir = .GlobalEnv))

##R9   ST_300126, LT_300126
##R8   ST_230725, LT_240725
##R7   ST_120625, LT_120625
##R6   ST_160525, LT_160525
##R5   ST_100425, 
##R4   ST_060325,
##R3   ST_310125,
##R2   ST_250724 (Empty)
##rm(ST_250724)

# Cleaning the LT data sets
#Run6_LT
#1 Create df with necesary columns
aVOCs_Run6_LT_160525_JM_R <- LT_160525[,c(1:9)]
#1.2 Remove all contaminated plates
aVOCs_Run6_LT_160525_JM_R_rmFungi <- aVOCs_Run6_LT_160525_JM_R %>% filter(Cont == "0")
#1.3 Count the number of leftover plates per fungi and Ctrl
Replicate_number_Run6_LT <- aVOCs_Run6_LT_160525_JM_R_rmFungi %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(Replicates =sum(!is.na(Ctrl)))
#1.4 Remove all fungi with a Replicates number < 3 (less than 3 Petri dishes per treatement and fungus)
Names_of_rm_fungi6_LT <- as.data.frame((Replicate_number_Run6_LT %>% filter(Replicates < 3))[,1] %>% unique())
aVOCs_Run6_LT_160525_JM_R_rmFungi_rmRep_low <- aVOCs_Run6_LT_160525_JM_R_rmFungi %>% filter(!(Fungi %in% list(Names_of_rm_fungi6_LT$Fungi %>% unique())))

#Check which fungi are still present
aVOCs_Run6_LT_160525_JM_R_rmFungi_rmRep_low$Fungi %>% unique()
#"R. solani"    "P. infestans" "A. solani"    "P. lingam"

#################
#Run7_LT
#1 Create df with necesary columns
aVOCs_Run7_LT_120625_JM_R <- LT_120625[,c(1:9)]
#1.2 Remove all contaminated plates
aVOCs_Run7_LT_120625_JM_R_rmFungi <- aVOCs_Run7_LT_120625_JM_R %>% filter(Cont == "0")
#1.3 Count the number of leftover plates per fungi and Ctrl
Replicate_number_Run7_LT <- aVOCs_Run7_LT_120625_JM_R_rmFungi %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(Replicates =sum(!is.na(Ctrl)))
#1.4 Remove all fungi with a Replicates number < 3
Names_of_rm_fungi7_LT <- as.data.frame((Replicate_number_Run7_LT %>% filter(Replicates < 3))[,1] %>% unique())
aVOCs_Run7_LT_120625_JM_R_rmFungi_rmRep_low <- aVOCs_Run7_LT_120625_JM_R_rmFungi %>% filter(!(Fungi %in% list(Names_of_rm_fungi7_LT$Fungi %>% unique())))
#Check which fungi are still present
aVOCs_Run7_LT_120625_JM_R_rmFungi_rmRep_low$Fungi %>% unique()
#"F. cul."      "P. ult."      "P. infestans"

#################
#Run8_LT
#1 Create df with necesary columns
aVOCs_Run8_LT_240725_JM_R <- LT_240725[,c(1:9)]
#1.2 Remove all contaminated plates
aVOCs_Run8_LT_240725_JM_R_rmFungi <- aVOCs_Run8_LT_240725_JM_R %>% filter(Cont == "0")
#1.3 Count the number of leftover plates per fungi and Ctrl
Replicate_number_Run8_LT <- aVOCs_Run8_LT_240725_JM_R_rmFungi %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(Replicates =sum(!is.na(Ctrl)))
#1.4 Remove all fungi with a Replicates number < 3
Names_of_rm_fungi8_LT <- as.data.frame((Replicate_number_Run8_LT %>% filter(Replicates < 3))[,1] %>% unique())
aVOCs_Run8_LT_240725_JM_R_rmFungi_rmRep_low <- aVOCs_Run8_LT_240725_JM_R_rmFungi %>% filter(!(Fungi %in% list(Names_of_rm_fungi8_LT$Fungi %>% unique())))
#Check which fungi are still present
aVOCs_Run8_LT_240725_JM_R_rmFungi_rmRep_low$Fungi %>% unique()
#"P. ult."   "F. cul."   "R. solani" "F. gre"    "P. lingam"

#################
#Run9_LT
#1 Create df with necesary columns
aVOCs_Run9_LT_300126_JM_R <- LT_300126[,c(1:9)]
#1.2 Remove all contaminated plates
aVOCs_Run9_LT_300126_JM_R_rmFungi <- aVOCs_Run9_LT_300126_JM_R %>% filter(Cont == "0")
#1.3 Count the number of leftover plates per fungi and Ctrl
Replicate_number_Run9_LT <- aVOCs_Run9_LT_300126_JM_R_rmFungi %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(Replicates =sum(!is.na(Ctrl)))
#1.4 Remove all fungi with a Replicates number < 3
Names_of_rm_fungi9_LT <- as.data.frame((Replicate_number_Run9_LT %>% filter(Replicates < 3))[,1] %>% unique())
aVOCs_Run9_LT_300126_JM_R_rmFungi_rmRep_low <- aVOCs_Run9_LT_300126_JM_R_rmFungi %>% filter(!(Fungi %in% list(Replicate_number_Run9_LT$Fungi %>% unique())))
#Check which fungi are still present
aVOCs_Run9_LT_300126_JM_R_rmFungi_rmRep_low$Fungi %>% unique()
#"F. gra"    "A. solani"

## Remove NA in Finished 
NA_rows_Run6 <- which(rowSums(is.na(aVOCs_Run6_LT_160525_JM_R_rmFungi_rmRep_low)) > 0) #non
aVOCs_Run6_LT_160525_JM_R_rmFungi_noCont_noNA <- aVOCs_Run6_LT_160525_JM_R_rmFungi_rmRep_low

NA_rows_Run7 <- which(rowSums(is.na(aVOCs_Run7_LT_120625_JM_R_rmFungi_rmRep_low)) > 0) #non
aVOCs_Run7_LT_120625_JM_R_rmFungi_noCont_noNA <- aVOCs_Run7_LT_120625_JM_R_rmFungi_rmRep_low

NA_rows_Run8 <- which(rowSums(is.na(aVOCs_Run8_LT_240725_JM_R_rmFungi_rmRep_low)) > 0) #non
aVOCs_Run8_LT_240725_JM_R_rmFungi_noCont_noNA <- aVOCs_Run8_LT_240725_JM_R_rmFungi_rmRep_low

NA_rows_Run9 <- which(rowSums(is.na(aVOCs_Run9_LT_300126_JM_R_rmFungi_rmRep_low)) > 0) #10 A. solani: In column "Cont" the 1 was not placed. It is therefore removed here.
aVOCs_Run9_LT_300126_JM_R_rmFungi_noCont_noNA <- aVOCs_Run9_LT_300126_JM_R_rmFungi_rmRep_low[-NA_rows_Run9, ]

#Give unified names
#### Run6_LT
aVOCs_Run6_LT_160525_JM_R_rmFungi_noCont_noNA$Fungi %>% unique()
#"R. solani"    "P. infestans" "A. solani"    "P. lingam" 

aVOCs_Run6_LT_160525_JM_R_rmFungi_noCont_noNA$Fungi <- dplyr::recode(aVOCs_Run6_LT_160525_JM_R_rmFungi_noCont_noNA$Fungi,
                                                                     "R. solani" = "R. solani",
                                                                     "A. solani" = "A. solani",
                                                                     "P. infestans" = "P. infestans",
                                                                     "P. lingam" = "P. lingam")

### Reorder them
aVOCs_Run6_LT_160525_JM_R_rmFungi_noCont_noNA$Fungi  <- factor(aVOCs_Run6_LT_160525_JM_R_rmFungi_noCont_noNA$Fungi, 
                                                               levels = c("R. solani",
                                                                          "A. solani",
                                                                          "P. infestans",
                                                                          "P. lingam"))

#Order df alphabetically
aVOCs_Run6_LT_160525_JM_R_rmFungi_noCont_noNA <- aVOCs_Run6_LT_160525_JM_R_rmFungi_noCont_noNA[order(aVOCs_Run6_LT_160525_JM_R_rmFungi_noCont_noNA$Fungi), ]

#### Run7_LT
aVOCs_Run7_LT_120625_JM_R_rmFungi_noCont_noNA$Fungi %>% unique()
#"F. cul."      "P. infestans" "P. ult." 
aVOCs_Run7_LT_120625_JM_R_rmFungi_noCont_noNA$Fungi <- dplyr::recode(aVOCs_Run7_LT_120625_JM_R_rmFungi_noCont_noNA$Fungi,
                                                                    "F. cul." = "F. culmorum",
                                                                   "P. infestans" = "P. infestans",
                                                                   "P. ult." = "P. ultimum")

### Reorder them
aVOCs_Run7_LT_120625_JM_R_rmFungi_noCont_noNA$Fungi  <- factor(aVOCs_Run7_LT_120625_JM_R_rmFungi_noCont_noNA$Fungi, 
                                                               levels = c("F. culmorum",
                                                                          "P. infestans",
                                                                          "P. ultimum"))

#Order df alphabetically
aVOCs_Run7_LT_120625_JM_R_rmFungi_noCont_noNA <- aVOCs_Run7_LT_120625_JM_R_rmFungi_noCont_noNA[order(aVOCs_Run7_LT_120625_JM_R_rmFungi_noCont_noNA$Fungi), ]

### Run8_LT
aVOCs_Run8_LT_240725_JM_R_rmFungi_noCont_noNA$Fungi %>% unique()
#"F. cul."   "P. lingam" "R. solani" "P. ult."   "F. gre" 
aVOCs_Run8_LT_240725_JM_R_rmFungi_noCont_noNA$Fungi <- dplyr::recode(aVOCs_Run8_LT_240725_JM_R_rmFungi_noCont_noNA$Fungi,
                                                                     "F. cul." = "F. culmorum",
                                                                     "P. lingam" = "P. lingam",
                                                                     "R. solani" = "R. solani",
                                                                     "P. ult." = "P. ultimum",
                                                                     "F. gre" = "F. graminearum")

### Reorder them
aVOCs_Run8_LT_240725_JM_R_rmFungi_noCont_noNA$Fungi  <- factor(aVOCs_Run8_LT_240725_JM_R_rmFungi_noCont_noNA$Fungi, 
                                                               levels = c("F. culmorum",
                                                                          "P. lingam",
                                                                          "R. solani",
                                                                          "P. ultimum",
                                                                          "F. graminearum"))

#Order df alphabetically
aVOCs_Run8_LT_240725_JM_R_rmFungi_noCont_noNA <- aVOCs_Run8_LT_240725_JM_R_rmFungi_noCont_noNA[order(aVOCs_Run8_LT_240725_JM_R_rmFungi_noCont_noNA$Fungi), ]

### Run9_LT
aVOCs_Run9_LT_300126_JM_R_rmFungi_noCont_noNA$Fungi %>% unique()
#"F. gra"    "A. solani"
aVOCs_Run9_LT_300126_JM_R_rmFungi_noCont_noNA$Fungi <- dplyr::recode(aVOCs_Run9_LT_300126_JM_R_rmFungi_noCont_noNA$Fungi,
                                                                     "A. solani" = "A. solani",
                                                                     "F. gra" = "F. graminearum")

### Reorder them
aVOCs_Run9_LT_300126_JM_R_rmFungi_noCont_noNA$Fungi  <- factor(aVOCs_Run9_LT_300126_JM_R_rmFungi_noCont_noNA$Fungi, 
                                                               levels = c("A. solani",
                                                                          "F. graminearum"))

#Order df alphabetically
aVOCs_Run9_LT_300126_JM_R_rmFungi_noCont_noNA <- aVOCs_Run9_LT_300126_JM_R_rmFungi_noCont_noNA[order(aVOCs_Run9_LT_300126_JM_R_rmFungi_noCont_noNA$Fungi), ]

##NOTE: The package "car" has a recode function too. It could mask the recode function in dplyr

#Convert multiple columns to factors
aVOCs_Run8_LT_240725_JM_R_rmFungi_noCont_noNA %>% colnames()
cols_aVOCs <- c("Fungi","Number","Ctrl","dpi","Finished")
cols_aVOCs_num <- c("GR1","GR2","GR3")

library(magrittr)
library(dplyr)
aVOCs_Run6_LT_160525_JM_R_rmFungi_noCont_noNA <- aVOCs_Run6_LT_160525_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs, factor)
aVOCs_Run6_LT_160525_JM_R_rmFungi_noCont_noNA <- aVOCs_Run6_LT_160525_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs_num, as.numeric)

aVOCs_Run7_LT_120625_JM_R_rmFungi_noCont_noNA <- aVOCs_Run7_LT_120625_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs, factor)
aVOCs_Run8_LT_240725_JM_R_rmFungi_noCont_noNA <- aVOCs_Run8_LT_240725_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs, factor)
aVOCs_Run9_LT_300126_JM_R_rmFungi_noCont_noNA <- aVOCs_Run9_LT_300126_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs, factor)

#Calculate the mean for each plate (NOTE: R sometimes returns an error when using the following code. Just restart the Session)
aVOCs_Run6_LT_160525_mean_GR <- aVOCs_Run6_LT_160525_JM_R_rmFungi_noCont_noNA %>%
  mutate(across(c(GR1, GR2, GR3), ~ . - 0.3)) %>% #Subtract the radius of the plug (0.3 cm)
  mutate(mean_GR = rowMeans(across(c("GR1",   "GR2",   "GR3")), na.rm = TRUE)) %>% #Calculate the mean of the GR per 3 radii measured 
  dplyr::select("Fungi","Number","Ctrl","dpi","mean_GR")

aVOCs_Run7_LT_120625_mean_GR <- aVOCs_Run7_LT_120625_JM_R_rmFungi_noCont_noNA %>%
  mutate(across(c(GR1, GR2, GR3), ~ . - 0.3)) %>% #Subtract the radius of the plug (0.3 cm)
  mutate(mean_GR = rowMeans(across(c("GR1",   "GR2",   "GR3")), na.rm = TRUE)) %>% #Calculate the mean of the GR per 3 radii measured 
  dplyr::select("Fungi","Number","Ctrl","dpi","mean_GR")

aVOCs_Run8_LT_240725_mean_GR <- aVOCs_Run8_LT_240725_JM_R_rmFungi_noCont_noNA %>%
  mutate(across(c(GR1, GR2, GR3), ~ . - 0.3)) %>% #Subtract the radius of the plug (0.3 cm)
  mutate(mean_GR = rowMeans(across(c("GR1",   "GR2",   "GR3")), na.rm = TRUE)) %>% #Calculate the mean of the GR per 3 radii measured 
  dplyr::select("Fungi","Number","Ctrl","dpi","mean_GR")

aVOCs_Run9_LT_300126_mean_GR <- aVOCs_Run9_LT_300126_JM_R_rmFungi_noCont_noNA %>%
  mutate(across(c(GR1, GR2, GR3), ~ . - 0.3)) %>% #Subtract the radius of the plug (0.3 cm)
  mutate(mean_GR = rowMeans(across(c("GR1",   "GR2",   "GR3")), na.rm = TRUE)) %>% #Calculate the mean of the GR per 3 radii measured 
  dplyr::select("Fungi","Number","Ctrl","dpi","mean_GR")

#Calculate statistics
library(FSA) #Needed for se()
###6
aVOCs_Run6_LT_160525_mean_GR_stat <- aVOCs_Run6_LT_160525_mean_GR %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(mean=mean(mean_GR, na.rm = TRUE),
            median=median(mean_GR, na.rm = TRUE),
            min=min(mean_GR, na.rm = TRUE),
            max=max(mean_GR, na.rm = TRUE),
            sd=sd(mean_GR, na.rm = TRUE),
            var=var(mean_GR, na.rm = TRUE),
            se=se(mean_GR, na.rm = TRUE),
            Replicates =sum(!is.na(mean_GR)))

###7
aVOCs_Run7_LT_120625_mean_GR_stat <- aVOCs_Run7_LT_120625_mean_GR %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(mean=mean(mean_GR, na.rm = TRUE),
            median=median(mean_GR, na.rm = TRUE),
            min=min(mean_GR, na.rm = TRUE),
            max=max(mean_GR, na.rm = TRUE),
            sd=sd(mean_GR, na.rm = TRUE),
            var=var(mean_GR, na.rm = TRUE),
            se=se(mean_GR, na.rm = TRUE),
            Replicates =sum(!is.na(mean_GR)))

###8
aVOCs_Run8_LT_240725_mean_GR_stat <- aVOCs_Run8_LT_240725_mean_GR %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(mean=mean(mean_GR, na.rm = TRUE),
            median=median(mean_GR, na.rm = TRUE),
            min=min(mean_GR, na.rm = TRUE),
            max=max(mean_GR, na.rm = TRUE),
            sd=sd(mean_GR, na.rm = TRUE),
            var=var(mean_GR, na.rm = TRUE),
            se=se(mean_GR, na.rm = TRUE),
            Replicates =sum(!is.na(mean_GR)))

###9
aVOCs_Run9_LT_300126_mean_GR_stat <- aVOCs_Run9_LT_300126_mean_GR %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(mean=mean(mean_GR, na.rm = TRUE),
            median=median(mean_GR, na.rm = TRUE),
            min=min(mean_GR, na.rm = TRUE),
            max=max(mean_GR, na.rm = TRUE),
            sd=sd(mean_GR, na.rm = TRUE),
            var=var(mean_GR, na.rm = TRUE),
            se=se(mean_GR, na.rm = TRUE),
            Replicates =sum(!is.na(mean_GR)))

###
#Put the following info into a single df:
#Per fungi and treatment:
#Select the last day
#Keep: Fungi, Ctrl, dpi, mean, median, Replicates

#####6_LT
df_aVOCs_Run6_LT_160525_abs_GR <- aVOCs_Run6_LT_160525_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl) %>%
  slice(1) %>% 
  group_by(Fungi)

#####7_LT
df_aVOCs_Run7_LT_120625_abs_GR <- aVOCs_Run7_LT_120625_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl) %>%
  slice(1) %>% 
  group_by(Fungi)

#####8_LT
df_aVOCs_Run8_LT_240725_abs_GR <- aVOCs_Run8_LT_240725_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl) %>%
  slice(1) %>% 
  group_by(Fungi)

#####9_LT
df_aVOCs_Run9_LT_300126_abs_GR <- aVOCs_Run9_LT_300126_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl) %>%
  slice(1) %>% 
  group_by(Fungi)

##Save all dfs in a single df

df_aVOCs_Run6_LT_160525_abs_GR$run <- 6
df_aVOCs_Run7_LT_120625_abs_GR$run <- 7
df_aVOCs_Run8_LT_240725_abs_GR$run <- 8
df_aVOCs_Run9_LT_300126_abs_GR$run <- 8

Data_abs_GR_LT <- rbind(df_aVOCs_Run6_LT_160525_abs_GR,
                     df_aVOCs_Run7_LT_120625_abs_GR,
                     df_aVOCs_Run8_LT_240725_abs_GR,
                     df_aVOCs_Run9_LT_300126_abs_GR)

# Select the last measurement per fungi to calculate the relative inhibition
# Calculate relative change from the first time point in each group

#####6_LT
df_aVOCs_Run6_LT_160525_rel_Inh <- aVOCs_Run6_LT_160525_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl) %>%
  slice(1) %>% 
  group_by(Fungi) %>%
  mutate(
    baseline = last(mean),  # baseline per group
    rel_pct_Inh = ifelse(
      baseline == 0, NA,      # avoid division by zero
      ((mean - baseline) / baseline)*100)*(-1)) %>%
  ungroup() %>%
  filter(Ctrl == "0") %>%
  dplyr::select(!(c("baseline", "Ctrl")))
#####7_LT
df_aVOCs_Run7_LT_120625_rel_Inh <- aVOCs_Run7_LT_120625_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl) %>%
  slice(1) %>% 
  group_by(Fungi) %>%
  mutate(
    baseline = last(mean),  # baseline per group
    rel_pct_Inh = ifelse(
      baseline == 0, NA,      # avoid division by zero
      ((mean - baseline) / baseline)*100)*(-1)) %>%
  ungroup() %>%
  filter(Ctrl == "0") %>%
  dplyr::select(!(c("baseline", "Ctrl")))
#####8_LT
df_aVOCs_Run8_LT_240725_rel_Inh <- aVOCs_Run8_LT_240725_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl) %>%
  slice(1) %>% 
  group_by(Fungi) %>%
  mutate(
    baseline = last(mean),  # baseline per group
    rel_pct_Inh = ifelse(
      baseline == 0, NA,      # avoid division by zero
      ((mean - baseline) / baseline)*100)*(-1)) %>%
  ungroup() %>%
  filter(Ctrl == "0") %>%
  dplyr::select(!(c("baseline", "Ctrl")))
#####9_LT
df_aVOCs_Run9_LT_300126_rel_Inh <- aVOCs_Run9_LT_300126_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl) %>%
  slice(1) %>% 
  group_by(Fungi) %>%
  mutate(
    baseline = last(mean),  # baseline per group
    rel_pct_Inh = ifelse(
      baseline == 0, NA,      # avoid division by zero
      ((mean - baseline) / baseline)*100)*(-1)) %>%
  ungroup() %>%
  filter(Ctrl == "0") %>%
  dplyr::select(!(c("baseline", "Ctrl")))

# Save the dfs in a single excel file
library(openxlsx)
#####6_LT
list_of_datasets_Run6_LT_160525 <- list("Raw_Data" = aVOCs_Run6_LT_160525_JM_R,
                                        "Cleaned_Data" = aVOCs_Run6_LT_160525_JM_R_rmFungi_noCont_noNA, 
                                        "Mean_GRs" = aVOCs_Run6_LT_160525_mean_GR,
                                        "Stats_by_dpi" = aVOCs_Run6_LT_160525_mean_GR_stat,
                                        "Data_Rel_Inh" = df_aVOCs_Run6_LT_160525_rel_Inh)
write.xlsx(list_of_datasets_Run6_LT_160525, file = "All_DataSets_aVOCs_Run6_LT_160525_JM_R.xlsx")
#####7_LT
list_of_datasets_Run7_LT_120625 <- list("Raw_Data" = aVOCs_Run7_LT_120625_JM_R,
                                        "Cleaned_Data" = aVOCs_Run7_LT_120625_JM_R_rmFungi_noCont_noNA, 
                                        "Mean_GRs" = aVOCs_Run7_LT_120625_mean_GR,
                                        "Stats_by_dpi" = aVOCs_Run7_LT_120625_mean_GR_stat,
                                        "Data_Rel_Inh" = df_aVOCs_Run7_LT_120625_rel_Inh)
write.xlsx(list_of_datasets_Run7_LT_120625, file = "All_DataSets_aVOCs_Run7_LT_120625_JM_R.xlsx")
#####8_LT
list_of_datasets_Run8_LT_240725 <- list("Raw_Data" = aVOCs_Run8_LT_240725_JM_R,
                                        "Cleaned_Data" = aVOCs_Run8_LT_240725_JM_R_rmFungi_noCont_noNA, 
                                        "Mean_GRs" = aVOCs_Run8_LT_240725_mean_GR,
                                        "Stats_by_dpi" = aVOCs_Run8_LT_240725_mean_GR_stat,
                                        "Data_Rel_Inh" = df_aVOCs_Run8_LT_240725_rel_Inh)
write.xlsx(list_of_datasets_Run8_LT_240725, file = "All_DataSets_aVOCs_Run8_LT_240725_JM_R.xlsx")
#####9_LT
list_of_datasets_Run9_LT_300126 <- list("Raw_Data" = aVOCs_Run9_LT_300126_JM_R,
                                        "Cleaned_Data" = aVOCs_Run9_LT_300126_JM_R_rmFungi_noCont_noNA, 
                                        "Mean_GRs" = aVOCs_Run9_LT_300126_mean_GR,
                                        "Stats_by_dpi" = aVOCs_Run9_LT_300126_mean_GR_stat,
                                        "Data_Rel_Inh" = df_aVOCs_Run9_LT_300126_rel_Inh)
write.xlsx(list_of_datasets_Run9_LT_300126, file = "All_DataSets_aVOCs_Run9_LT_300126_JM_R.xlsx")

#######
# Save the dfs in a single excel file
df_aVOCs_Run6_LT_160525_rel_Inh$run <- 6
df_aVOCs_Run7_LT_120625_rel_Inh$run <- 7
df_aVOCs_Run8_LT_240725_rel_Inh$run <- 8
df_aVOCs_Run9_LT_300126_rel_Inh$run <- 9

Raw_Data <- rbind(df_aVOCs_Run6_LT_160525_rel_Inh,
                  df_aVOCs_Run7_LT_120625_rel_Inh,
                  df_aVOCs_Run8_LT_240725_rel_Inh,
                  df_aVOCs_Run9_LT_300126_rel_Inh)

Data_Rel_Inh <- Raw_Data %>%
  group_by(Fungi) %>%
  summarise(mean=mean(rel_pct_Inh, na.rm = TRUE),
            median=median(rel_pct_Inh, na.rm = TRUE),
            min=min(rel_pct_Inh, na.rm = TRUE),
            max=max(rel_pct_Inh, na.rm = TRUE),
            sd=sd(rel_pct_Inh, na.rm = TRUE),
            var=var(rel_pct_Inh, na.rm = TRUE),
            se=se(rel_pct_Inh, na.rm = TRUE),
            Replicates =sum(!is.na(rel_pct_Inh)))

Data_abs_GR_Stat <- Data_abs_GR_LT %>%
  group_by(Fungi, Ctrl) %>%
  summarise(mean=mean(mean, na.rm = TRUE),
            median=median(mean, na.rm = TRUE),
            min=min(mean, na.rm = TRUE),
            max=max(mean, na.rm = TRUE),
            sd=sd(mean, na.rm = TRUE),
            var=var(mean, na.rm = TRUE),
            se=se(mean, na.rm = TRUE),
            Replicates =sum(!is.na(mean)))

list_of_datasets_aVOCs_Run6to9_LT_JM_R <- list("Raw_Data" = Raw_Data,
                                               "Data_abs_GR" = Data_abs_GR_LT,
                                               "Data_abs_GR_Stat" = Data_abs_GR_Stat,
                                               "Data_Rel_Inh" = Data_Rel_Inh)

write.xlsx(list_of_datasets_aVOCs_Run6to9_LT_JM_R, file = "All_DataSets_aVOCs_Run6to9_LT_JM_R.xlsx")
#########
#Generating graphs
# Load required libraries
library(ggplot2)
library(cowplot)
library(ggplot2)
library(png)
library(grid)
library(patchwork)

#set workdictionary
setwd("C:/Users/Julian.Maier/Documents/Manuscript_Urlaub/Fig_2")
#Load Data sets
aVOCs_Run6to9_LT_JM_R <- read_excel("All_DataSets_aVOCs_Run6to9_LT_JM_R.xlsx", 
                                    sheet = "Data_abs_GR")

#Dataset
All_DataSets_aVOCs_Run6to9_LT_JM_R <- aVOCs_Run6to9_LT_JM_R %>% mutate_at(c("Fungi", "Ctrl", "dpi", "Replicates", "run"), as.factor)

Best_fungi_time_series_ST_JM_R <- read_excel("Best_fungi_VOCs_time_plot_JM_R.xlsx", 
                                             sheet = "Growth_Time")

# Generate Graphs
#Fig. 2 - Boxplot VOCs LT

x_Title_A <- expression(paste("Days post pathogen inoculation [dppi]"))
y_Title_A <- expression(paste("Absolute growth [cm]"))

A <- aVOCs_Run6to9_LT_JM_R %>% 
  group_by(Fungi, Ctrl) %>%
  ggplot(aes(x = Fungi, y = mean, group = Ctrl)) +
  geom_errorbar(stat = "summary", fun.data = mean_sdl, fun.args = list(mult = 1), width = 0.2, color = "black", alpha = 1, position = position_dodge(width = 0.9)) +
  stat_summary(geom = "point", fun = "mean", color = "red", shape = 15, position = position_dodge(width = 0.9)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.9), shape = 16, size = 1, color ="black") +
  theme_light(base_size = 16) +
  scale_y_continuous(limits = c(NA, 4.5), breaks=seq(0, 4.5, 0.5)) +
  coord_cartesian(clip = "off") +
  labs(
    x=x_Title_A,
    y=y_Title_A) +
  theme_bw() +
  theme(text = element_text(size = 12, family = "sans"),
        axis.text.x = element_text(size = 12, face = "italic", color = "black", angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=12, color = "black"),
        axis.title.y = element_text(size=12, color = "black"),#, hjust = 0),
        legend.position = "none")

ggsave("Figure2.tiff", plot = A, device = tiff, dpi = 1000, width = 17, height = 9, units = "cm", pointsize = 12)