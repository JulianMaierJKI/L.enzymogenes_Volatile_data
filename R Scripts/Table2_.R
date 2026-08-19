#Analysis of aVOCs in vitro data

# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggpubr)
library(broom)
library(openxlsx)
library(magrittr)
library(FSA)

# set workdictionary
setwd("C:/Users/Julian.Maier/Documents/Manuscript_Urlaub/Table_S3/RAW")

# Load multiple excel sheets into sepaerate dfs

library(purrr)
df_list <- map(purrr::set_names(excel_sheets("All_DataSets_Ventilation_ST.xlsx")), #call st_names() specifically from purrr. It is masked by the set_names from magrittr
               read_excel, path = "All_DataSets_Ventilation_ST.xlsx")

purrr::map(names(df_list),
           ~assign(.x, df_list[[.x]], envir = .GlobalEnv))
 
##R3  Run3_230426
##R2  Run2_170426

# Cleaning the data sets
####
#Run2
#1 Create df with necesary columns
aVOCs_Run2_ventilation_ST_170426_JM_R <- Run2_170426[,c(1:11)]
#1.2 Remove all contaminated plates
#1.2.1 Identify contaminated plates
anti_join_aVOCs_Run2_ventilation_ST_170426 <- aVOCs_Run2_ventilation_ST_170426_JM_R %>% filter(Cont == "1")
#2 anti_join
aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi <- anti_join(aVOCs_Run2_ventilation_ST_170426_JM_R, anti_join_aVOCs_Run2_ventilation_ST_170426, by=c("Fungi", "Number", "Ctrl", "Ventilation"))
#1.3 Count the number of leftover plates per fungi and Ctrl
Replicate_number_ventilation_Run2 <- aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi %>%
  group_by(Fungi, Ctrl, Ventilation, dpi) %>%
  summarize(Replicates =sum(!is.na(Ctrl)))
#1.4 Remove all fungi & conditions with a Replicates number < 3
anti_join_Fungus_and_condition_rm_ventilation_fungi2 <- (Replicate_number_ventilation_Run2 %>% filter(Replicates < 3))[,c(1,2,3)]
#1.5 Antijoin to remove only the conditions with replicate number < 2
aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_rmRep_low <- anti_join(aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi, anti_join_Fungus_and_condition_rm_ventilation_fungi2, by=c("Fungi", "Ctrl", "Ventilation"))

#Check which fungi are still present
aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_rmRep_low$Fungi %>% unique()
#"A. solani" "R. solani" "A. radicina" 
#

## Look for NAs in dfs
NA_rows_Run2_ventilation_ST <- which(rowSums(is.na(aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_rmRep_low[,-11])) > 0) #non (not including OD600)
aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA <- aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_rmRep_low

####
#Run3
#1 Create df with necesary columns
aVOCs_Run3_ventilation_ST_230426_JM_R <- Run3_230426[,c(1:11)]
#1.2 Remove all contaminated plates
#1.2.1 Identify contaminated plates
anti_join_aVOCs_Run3_ventilation_ST_230426 <- aVOCs_Run3_ventilation_ST_230426_JM_R %>% filter(Cont == "1")
#2 anti_join
aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi <- anti_join(aVOCs_Run3_ventilation_ST_230426_JM_R, anti_join_aVOCs_Run3_ventilation_ST_230426, by=c("Fungi", "Number", "Ctrl", "Ventilation"))
#1.3 Count the number of leftover plates per fungi and Ctrl
Replicate_number_ventilation_Run3 <- aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi%>%
  group_by(Fungi, Ctrl, Ventilation, dpi) %>%
  summarize(Replicates =sum(!is.na(Ctrl)))
#1.4 Remove all fungi & conditions with a Replicates number < 3
anti_join_Fungus_and_condition_rm_ventilation_fungi3 <- (Replicate_number_ventilation_Run3 %>% filter(Replicates < 3))[,c(1,2,3)]
#1.5 Antijoin to remove only the conditions with replicate number < 2
aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_rmRep_low <- anti_join(aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi, anti_join_Fungus_and_condition_rm_ventilation_fungi3, by=c("Fungi", "Ctrl", "Ventilation"))
#Check which fungi are still present
aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_rmRep_low$Fungi %>% unique()
#"A. solani" "R. solani" "A. radicina" 
#

## Look for NAs in dfs
NA_rows_Run3_ventilation_ST <- which(rowSums(is.na(aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_rmRep_low[,-11])) > 0) #non (not including OD600)
aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA <- aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_rmRep_low


#####

#Give unified names
## Run2
aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA$Fungi %>% unique()

aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA$Fungi <- dplyr::recode(aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA$Fungi,
                                   "A. radicina" = "A. radicina",
                                   "A. solani" = "A. solani",
                                   "R. solani" = "R. solani")
### Reorder them
aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA$Fungi  <- factor(aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA$Fungi, 
                             levels = c("A. radicina",
                                        "A. solani",
                                        "R. solani"))
#Order df alphabetically
aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA <- aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA[order(aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA$Fungi), ]

## Run3
aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA$Fungi %>% unique()

aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA$Fungi <- dplyr::recode(aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA$Fungi,
                                   "A. radicina" = "A. radicina",
                                   "A. solani" = "A. solani",
                                   "R. solani" = "R. solani")
### Reorder them
aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA$Fungi  <- factor(aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA$Fungi, 
                             levels = c("A. radicina",
                                        "A. solani",
                                        "R. solani"))
#Order df alphabetically
aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA <- aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA[order(aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA$Fungi), ]

##NOTE: The package "car" has a recode function too. It could mask the recode function in dplyr

#Datensatz überprüfen
## Identify outliers

#Convert multiple columns to factors
cols_aVOCs <- c("Fungi","Number","Ctrl", "Ventilation", "dpi","Finished")
cols_aVOCs_num <- c("GR1","GR2","GR3","OD600")

library(magrittr)
library(dplyr)

aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA <- aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs, factor)
aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA <- aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs_num, as.numeric)

aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA <- aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs, factor)
aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA <- aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs_num, as.numeric)


#Calculate the mean for each plate
aVOCs_Run2_ventilation_ST_170426_mean_GR <- aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA %>%
  mutate(across(c(GR1, GR2, GR3), ~ . - 0.3)) %>% #Subtract the radius of the plug (0.3 cm)
  mutate(mean_GR = rowMeans(across(c("GR1",   "GR2",   "GR3")), na.rm = TRUE)) %>% #Calculate the mean of the GR per 3 radii measured 
  dplyr::select("Fungi","Number","Ctrl","Ventilation", "dpi", "OD600", "mean_GR")

aVOCs_Run3_ventilation_ST_230426_mean_GR <- aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA %>%
  mutate(across(c(GR1, GR2, GR3), ~ . - 0.3)) %>% #Subtract the radius of the plug (0.3 cm)
  mutate(mean_GR = rowMeans(across(c("GR1",   "GR2",   "GR3")), na.rm = TRUE)) %>% #Calculate the mean of the GR per 3 radii measured 
  dplyr::select("Fungi","Number","Ctrl","Ventilation", "dpi", "OD600", "mean_GR")


#Calculate statistics

#Run2
aVOCs_Run2_ventilation_ST_170426_mean_GR_stat <- aVOCs_Run2_ventilation_ST_170426_mean_GR %>%
  group_by(Fungi, Ctrl, Ventilation, dpi) %>%
  summarize(meanGR=mean(mean_GR, na.rm = TRUE),
            medianGR=median(mean_GR, na.rm = TRUE),
            minGR=min(mean_GR, na.rm = TRUE),
            maxGR=max(mean_GR, na.rm = TRUE),
            sdGR=sd(mean_GR, na.rm = TRUE),
            varGR=var(mean_GR, na.rm = TRUE),
            meanOD=mean(OD600, na.rm = TRUE),
            sdOD=sd(OD600, na.rm = TRUE),
            varOD=var(OD600, na.rm = TRUE),
            Replicates =sum(!is.na(Number)))
#Run3
aVOCs_Run3_ventilation_ST_230426_mean_GR_stat <- aVOCs_Run3_ventilation_ST_230426_mean_GR %>%
  group_by(Fungi, Ctrl, Ventilation, dpi) %>%
  summarize(meanGR=mean(mean_GR, na.rm = TRUE),
            medianGR=median(mean_GR, na.rm = TRUE),
            minGR=min(mean_GR, na.rm = TRUE),
            maxGR=max(mean_GR, na.rm = TRUE),
            sdGR=sd(mean_GR, na.rm = TRUE),
            varGR=var(mean_GR, na.rm = TRUE),
            meanOD=mean(OD600, na.rm = TRUE),
            sdOD=sd(OD600, na.rm = TRUE),
            varOD=var(OD600, na.rm = TRUE),
            Replicates =sum(!is.na(Number)))

# Select the last measurement per fungi to calculate the relative inhibition
# Calculate relative change from the first time point in each group

##Run2
aVOCs_Run2_ventilation_ST_170426_rel_Inh <- aVOCs_Run2_ventilation_ST_170426_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl, Ventilation) %>%
  slice(1) %>% 
  group_by(Fungi,Ventilation) %>%
  mutate(
    baseline = last(meanGR),  # baseline per group
    rel_pct_Inh = ifelse(
      baseline == 0, NA,      # avoid division by zero
      ((meanGR - baseline) / baseline)*100)*(-1)) %>%
  ungroup() %>%
  filter(Ctrl == "0") %>%
  dplyr::select(!(c("baseline", "Ctrl")))

##Run3
aVOCs_Run3_ventilation_ST_230426_rel_Inh <- aVOCs_Run3_ventilation_ST_230426_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl, Ventilation) %>%
  slice(1) %>% 
  group_by(Fungi,Ventilation) %>%
  mutate(
    baseline = last(meanGR),  # baseline per group
    rel_pct_Inh = ifelse(
      baseline == 0, NA,      # avoid division by zero
      ((meanGR - baseline) / baseline)*100)*(-1)) %>%
  ungroup() %>%
  filter(Ctrl == "0") %>%
  dplyr::select(!(c("baseline", "Ctrl")))

#Combine the data sets of the two runs
##Add the respective run
###NOTE: The data was not combined before to be able to identify potential errors easier (which did happen :)

aVOCs_Run2_ventilation_ST_170426_JM_R$Run <- 2
aVOCs_Run3_ventilation_ST_230426_JM_R$Run <- 3
aVOCs_ventilation_RAW_summary <- rbind(aVOCs_Run2_ventilation_ST_170426_JM_R, aVOCs_Run3_ventilation_ST_230426_JM_R)

aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA$Run <- 2
aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA$Run <- 3
aVOCs_ventilation_rmFungi_noCont_noNA_summary <- rbind(aVOCs_Run2_ventilation_ST_170426_JM_R_rmFungi_noCont_noNA, aVOCs_Run3_ventilation_ST_230426_JM_R_rmFungi_noCont_noNA)

aVOCs_Run2_ventilation_ST_170426_mean_GR$Run <- 2
aVOCs_Run3_ventilation_ST_230426_mean_GR$Run <- 3
aVOCs_ventilation_mean_GR_summary <- rbind(aVOCs_Run2_ventilation_ST_170426_mean_GR, aVOCs_Run3_ventilation_ST_230426_mean_GR)

aVOCs_Run2_ventilation_ST_170426_mean_GR_stat$Run <- 2
aVOCs_Run3_ventilation_ST_230426_mean_GR_stat$Run <- 3
aVOCs_ventilation_mean_GR_stat_summary <- rbind(aVOCs_Run2_ventilation_ST_170426_mean_GR_stat, aVOCs_Run3_ventilation_ST_230426_mean_GR_stat)

aVOCs_Run2_ventilation_ST_170426_rel_Inh$Run <- 2
aVOCs_Run3_ventilation_ST_230426_rel_Inh$Run <- 3
aVOCs_ventilation_rel_Inh_summary <- rbind(aVOCs_Run2_ventilation_ST_170426_rel_Inh, aVOCs_Run3_ventilation_ST_230426_rel_Inh) # Run1 was a screening and was not included for further analysis.

#aVOCs_ventilation_rel_Inh_summary_NoR1 <- aVOCs_ventilation_rel_Inh_summary %>% filter(Run > 1)

aVOCs_Run2_3_ventilation_ST_stat <- aVOCs_ventilation_rel_Inh_summary  %>%
  group_by(Fungi, Ventilation) %>%
  summarize(mean_rel_Inh=mean(rel_pct_Inh, na.rm = TRUE),
            #se_rel_Inh=se(rel_pct_Inh, na.rm = TRUE),
            mean_OD=mean(meanOD, na.rm = TRUE),
            Replicates =sum(!is.na(rel_pct_Inh)))

# Save the dfs in a single excel file
##3

list_of_datasets_Run2_3_ventilation_ST <- list("Raw_Data" = aVOCs_ventilation_RAW_summary,
                                        "Cleaned_Data" = aVOCs_ventilation_rmFungi_noCont_noNA_summary, 
                                        "Mean_GRs" = aVOCs_ventilation_mean_GR_summary,
                                        "Stats_by_dpi" = aVOCs_ventilation_mean_GR_stat_summary,
                                        "Data_Rel_Inh" = aVOCs_ventilation_rel_Inh_summary,
                                        "Stats_summary" = aVOCs_Run2_3_ventilation_ST_stat)
write.xlsx(list_of_datasets_Run2_3_ventilation_ST, file = "All_DataSets_aVOCs_Run2_3_ventilation_ST_JM_R.xlsx")