#Analysis of aVOCs in vitro data

# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggpubr)
library(broom)
library(openxlsx)

# Create Data sets


# set workdictionary
setwd("C:/Users/julian.maier/Documents/JKI_OptiLyso/aVOCs/Analysis_of_all_datasets")

# Read data
# Load multiple excel sheets into sepaerate dfs

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

# Cleaningthe data sets
#Run3
#1 Create df with necesary columns
aVOCs_Run3_ST_310125_JM_R <- ST_310125[,c(1:9)]
#1.2 Remove all contaminated plates
aVOCs_Run3_ST_310125_JM_R_rmFungi <- aVOCs_Run3_ST_310125_JM_R %>% filter(Cont == "0")
#1.3 Count the number of leftover plates per fungi and Ctrl
Replicate_number_Run3 <- aVOCs_Run3_ST_310125_JM_R_rmFungi %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(Replicates =sum(!is.na(Ctrl)))
#1.4 Remove all fungi with a Replicates number < 3 (less than 3 Petri dishes per treatement and fungus)
Names_of_rm_fungi3 <- as.data.frame((Replicate_number_Run3 %>% filter(Replicates < 3))[,1] %>% unique())
aVOCs_Run3_ST_310125_JM_R_rmFungi_rmRep_low <- aVOCs_Run3_ST_310125_JM_R_rmFungi %>% filter(!(Fungi %in% list(Names_of_rm_fungi3$Fungi %>% unique())))

#Check which fungi are still present
aVOCs_Run3_ST_310125_JM_R_rmFungi_rmRep_low$Fungi %>% unique()
#"A. radici"      "A. solani"      "F. culmorum"    "F. graminearum" "P. lingam"      "P. ultimum"     "R. solani" 
#################
#Run4
#1 Create df with necesary columns
aVOCs_Run4_ST_060325_JM_R <- ST_060325[,c(1:9)]
#1.2 Remove all contaminated plates
aVOCs_Run4_ST_060325_JM_R_rmFungi <- aVOCs_Run4_ST_060325_JM_R %>% filter(Cont == "0")
#1.3 Count the number of leftover plates per fungi and Ctrl
Replicate_number_Run4 <- aVOCs_Run4_ST_060325_JM_R_rmFungi %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(Replicates =sum(!is.na(Ctrl)))
#1.4 Remove all fungi with a Replicates number < 3
Names_of_rm_fungi4 <- as.data.frame((Replicate_number_Run4 %>% filter(Replicates < 3))[,1] %>% unique())
aVOCs_Run4_ST_060325_JM_R_rmFungi_rmRep_low <- aVOCs_Run4_ST_060325_JM_R_rmFungi %>% filter(!(Fungi %in% list(Names_of_rm_fungi4$Fungi %>% unique())))
#Check which fungi are still present
aVOCs_Run4_ST_060325_JM_R_rmFungi_rmRep_low$Fungi %>% unique()
#"R. solani"      "A. radici"      "F. graminearum" "P. lingam" "A. fabea"
#Remove A. fabea
aVOCs_Run4_ST_060325_JM_R_rmFungi_rmRep_low <- aVOCs_Run4_ST_060325_JM_R_rmFungi_rmRep_low %>% filter(!(Fungi %in% c("A. fabea")))

#################
#Run5
#1 Create df with necesary columns
aVOCs_Run5_ST_100425_JM_R <- ST_100425[,c(1:9)]
#1.2 Remove all contaminated plates
aVOCs_Run5_ST_100425_JM_R_rmFungi <- aVOCs_Run5_ST_100425_JM_R %>% filter(Cont == "0")
#1.3 Count the number of leftover plates per fungi and Ctrl
Replicate_number_Run5 <- aVOCs_Run5_ST_100425_JM_R_rmFungi %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(Replicates =sum(!is.na(Ctrl)))
#1.4 Remove all fungi with a Replicates number < 3
Names_of_rm_fungi5 <- as.data.frame((Replicate_number_Run5 %>% filter(Replicates < 3))[,1] %>% unique())
aVOCs_Run5_ST_100425_JM_R_rmFungi_rmRep_low <- aVOCs_Run5_ST_100425_JM_R_rmFungi %>% filter(!(Fungi %in% list(Names_of_rm_fungi5$Fungi %>% unique())))
#Check which fungi are still present
aVOCs_Run5_ST_100425_JM_R_rmFungi_rmRep_low$Fungi %>% unique()
#"R. solani"   "A. radici"   "A. solani"   "F. culmorum"

#################
#Run6
#1 Create df with necesary columns
aVOCs_Run6_ST_160525_JM_R <- ST_160525[,c(1:9)]
#1.2 Remove all contaminated plates
aVOCs_Run6_ST_160525_JM_R_rmFungi <- aVOCs_Run6_ST_160525_JM_R %>% filter(Cont == "0")
#1.3 Count the number of leftover plates per fungi and Ctrl
Replicate_number_Run6 <- aVOCs_Run6_ST_160525_JM_R_rmFungi %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(Replicates =sum(!is.na(Ctrl)))
#1.4 Remove all fungi with a Replicates number < 3
Names_of_rm_fungi6 <- as.data.frame((Replicate_number_Run6 %>% filter(Replicates < 3))[,1] %>% unique())
aVOCs_Run6_ST_160525_JM_R_rmFungi_rmRep_low <- aVOCs_Run6_ST_160525_JM_R_rmFungi %>% filter(!(Fungi %in% list(Names_of_rm_fungi6$Fungi %>% unique())))
#Check which fungi are still present
aVOCs_Run6_ST_160525_JM_R_rmFungi_rmRep_low$Fungi %>% unique()
#"A. radici"    "A. solani"    "P. infestans" "P. lingam"    "R. solani"  

#################
#Run7
#1 Create df with necesary columns
aVOCs_Run7_ST_120625_JM_R <- ST_120625[,c(1:9)]
#1.2 Remove all contaminated plates
aVOCs_Run7_ST_120625_JM_R_rmFungi <- aVOCs_Run7_ST_120625_JM_R %>% filter(Cont == "0")
#1.3 Count the number of leftover plates per fungi and Ctrl
Replicate_number_Run7 <- aVOCs_Run7_ST_120625_JM_R_rmFungi %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(Replicates =sum(!is.na(Ctrl)))
#1.4 Remove all fungi with a Replicates number < 3
Names_of_rm_fungi7 <- as.data.frame((Replicate_number_Run7 %>% filter(Replicates < 3))[,1] %>% unique())
aVOCs_Run7_ST_120625_JM_R_rmFungi_rmRep_low <- aVOCs_Run7_ST_120625_JM_R_rmFungi %>% filter(!(Fungi %in% c(Names_of_rm_fungi7$Fungi %>% unique())))
#Check which fungi are still present
aVOCs_Run7_ST_120625_JM_R_rmFungi_rmRep_low$Fungi %>% unique()
#"F. cul."   "P. infestans"  "P. ult." 

#################
#Run8
#1 Create df with necesary columns
aVOCs_Run8_ST_230725_JM_R <- ST_230725[,c(1:9)]
#1.2 Remove all contaminated plates
aVOCs_Run8_ST_230725_JM_R_rmFungi <- aVOCs_Run8_ST_230725_JM_R %>% filter(Cont == "0")
#1.3 Count the number of leftover plates per fungi and Ctrl
Replicate_number_Run8 <- aVOCs_Run8_ST_230725_JM_R_rmFungi %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(Replicates =sum(!is.na(Ctrl)))
#1.4 Remove all fungi with a Replicates number < 3
Names_of_rm_fungi8 <- as.data.frame((Replicate_number_Run8 %>% filter(Replicates < 3))[,1] %>% unique())
aVOCs_Run8_ST_230725_JM_R_rmFungi_rmRep_low <- aVOCs_Run8_ST_230725_JM_R_rmFungi %>% filter(!(Fungi %in% list(Names_of_rm_fungi8[[1]])))
#Check which fungi are still present
aVOCs_Run8_ST_230725_JM_R_rmFungi_rmRep_low$Fungi %>% unique()
#"F. cul." "P. lingam" "R. solani" "P. ult."   "F. gre" 

#################
#Run9
#1 Create df with necesary columns
aVOCs_Run9_ST_300126_JM_R <- ST_300126[,c(1:9)]
#1.2 Remove all contaminated plates
aVOCs_Run9_ST_300126_JM_R_rmFungi <- aVOCs_Run9_ST_300126_JM_R %>% filter(Cont == "0")
#1.3 Count the number of leftover plates per fungi and Ctrl
Replicate_number_Run9 <- aVOCs_Run9_ST_300126_JM_R_rmFungi %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(Replicates =sum(!is.na(Ctrl)))
#1.4 Remove all fungi with a Replicates number < 3
Names_of_rm_fungi9 <- as.data.frame((Replicate_number_Run9 %>% filter(Replicates < 3))[,1] %>% unique())
aVOCs_Run9_ST_300126_JM_R_rmFungi_rmRep_low <- aVOCs_Run9_ST_300126_JM_R_rmFungi %>% filter(!(Fungi %in% list(Names_of_rm_fungi9[[1]])))
#Check which fungi are still present
aVOCs_Run9_ST_300126_JM_R_rmFungi_rmRep_low$Fungi %>% unique()
#"F. gra"       "P. infestans" "A. solani"

## Look for NAs in dfs
NA_rows_Run3_ST <- which(rowSums(is.na(aVOCs_Run3_ST_310125_JM_R_rmFungi_rmRep_low)) > 0) #non
aVOCs_Run3_ST_310125_JM_R_rmFungi_noCont_noNA <- aVOCs_Run3_ST_310125_JM_R_rmFungi_rmRep_low

NA_rows_Run4_ST <- which(rowSums(is.na(aVOCs_Run4_ST_060325_JM_R_rmFungi_rmRep_low)) > 0) #non
aVOCs_Run4_ST_060325_JM_R_rmFungi_noCont_noNA <- aVOCs_Run4_ST_060325_JM_R_rmFungi_rmRep_low

NA_rows_Run5_ST <- which(rowSums(is.na(aVOCs_Run5_ST_100425_JM_R_rmFungi_rmRep_low)) > 0) #non
aVOCs_Run5_ST_100425_JM_R_rmFungi_noCont_noNA <- aVOCs_Run5_ST_100425_JM_R_rmFungi_rmRep_low

NA_rows_Run6_ST <- which(rowSums(is.na(aVOCs_Run6_ST_160525_JM_R_rmFungi_rmRep_low)) > 0) #non
aVOCs_Run6_ST_160525_JM_R_rmFungi_noCont_noNA <- aVOCs_Run6_ST_160525_JM_R_rmFungi_rmRep_low

NA_rows_Run7_ST <- which(rowSums(is.na(aVOCs_Run7_ST_120625_JM_R_rmFungi_rmRep_low)) > 0) #non
aVOCs_Run7_ST_120625_JM_R_rmFungi_noCont_noNA <- aVOCs_Run7_ST_120625_JM_R_rmFungi_rmRep_low

NA_rows_Run8_ST <- which(rowSums(is.na(aVOCs_Run8_ST_230725_JM_R_rmFungi_rmRep_low)) > 0) #non
aVOCs_Run8_ST_230725_JM_R_rmFungi_noCont_noNA <- aVOCs_Run8_ST_230725_JM_R_rmFungi_rmRep_low

NA_rows_Run9_ST <- which(rowSums(is.na(aVOCs_Run9_ST_300126_JM_R_rmFungi_rmRep_low)) > 0) #non
aVOCs_Run9_ST_300126_JM_R_rmFungi_noCont_noNA <- aVOCs_Run9_ST_300126_JM_R_rmFungi_rmRep_low

#Give unified names
#### Run3
aVOCs_Run3_ST_310125_JM_R_rmFungi_noCont_noNA$Fungi %>% unique()

aVOCs_Run3_ST_310125_JM_R_rmFungi_noCont_noNA$Fungi <- dplyr::recode(aVOCs_Run3_ST_310125_JM_R_rmFungi_noCont_noNA$Fungi,
                 "A. radici" = "A. radicina",
                 "A. solani" = "A. solani",
                 "F. culmorum" = "F. culmorum",
                 "F. graminearum" = "F. graminearum",
                 "P. lingam" = "P. lingam",
                 "P. ultimum" = "P. ultimum",
                 "R. solani" = "R. solani")

### Reorder them
aVOCs_Run3_ST_310125_JM_R_rmFungi_noCont_noNA$Fungi  <- factor(aVOCs_Run3_ST_310125_JM_R_rmFungi_noCont_noNA$Fungi, 
                                                               levels = c("A. radicina",
                                                                          "A. solani",
                                                                          "F. culmorum",
                                                                          "F. graminearum",
                                                                          "P. lingam",
                                                                          "P. ultimum",
                                                                          "R. solani"))

#Order df alphabetically
aVOCs_Run3_ST_310125_JM_R_rmFungi_noCont_noNA <- aVOCs_Run3_ST_310125_JM_R_rmFungi_noCont_noNA[order(aVOCs_Run3_ST_310125_JM_R_rmFungi_noCont_noNA$Fungi), ]

#### Run4
aVOCs_Run4_ST_060325_JM_R_rmFungi_noCont_noNA$Fungi %>% unique()

aVOCs_Run4_ST_060325_JM_R_rmFungi_noCont_noNA$Fungi <- dplyr::recode(aVOCs_Run4_ST_060325_JM_R_rmFungi_noCont_noNA$Fungi,
                                                                     "R. solani" = "R. solani",
                                                                     "A. radici" = "A. radicina",
                                                                     "F. graminearum" = "F. graminearum",
                                                                     "P. lingam" = "P. lingam")

### Reorder them
aVOCs_Run4_ST_060325_JM_R_rmFungi_noCont_noNA$Fungi  <- factor(aVOCs_Run4_ST_060325_JM_R_rmFungi_noCont_noNA$Fungi, 
                                                               levels = c("R. solani",
                                                                          "A. radicina",
                                                                          "F. graminearum",
                                                                          "P. lingam"))

#Order df alphabetically
aVOCs_Run4_ST_060325_JM_R_rmFungi_noCont_noNA <- aVOCs_Run4_ST_060325_JM_R_rmFungi_noCont_noNA[order(aVOCs_Run4_ST_060325_JM_R_rmFungi_noCont_noNA$Fungi), ]

### Run5
aVOCs_Run5_ST_100425_JM_R_rmFungi_noCont_noNA$Fungi %>% unique()

aVOCs_Run5_ST_100425_JM_R_rmFungi_noCont_noNA$Fungi <- dplyr::recode(aVOCs_Run5_ST_100425_JM_R_rmFungi_noCont_noNA$Fungi,
                                                                     "A. radici" = "A. radicina",
                                                                     "F. culmorum" = "F. culmorum",
                                                                     "A. solani" = "A. solani",
                                                                     "R. solani" = "R. solani")

### Reorder them
aVOCs_Run5_ST_100425_JM_R_rmFungi_noCont_noNA$Fungi  <- factor(aVOCs_Run5_ST_100425_JM_R_rmFungi_noCont_noNA$Fungi, 
                                                               levels = c("A. radicina",
                                                                          "F. culmorum",
                                                                          "A. solani",
                                                                          "R. solani"))

#Order df alphabetically
aVOCs_Run5_ST_100425_JM_R_rmFungi_noCont_noNA <- aVOCs_Run5_ST_100425_JM_R_rmFungi_noCont_noNA[order(aVOCs_Run5_ST_100425_JM_R_rmFungi_noCont_noNA$Fungi), ]

### Run6
aVOCs_Run6_ST_160525_JM_R_rmFungi_noCont_noNA$Fungi %>% unique()

aVOCs_Run6_ST_160525_JM_R_rmFungi_noCont_noNA$Fungi <- dplyr::recode(aVOCs_Run6_ST_160525_JM_R_rmFungi_noCont_noNA$Fungi,
                                                                     "A. radici" = "A. radicina",
                                                                     "A. solani" = "A. solani",
                                                                     "P. infestans" = "P. infestans",
                                                                     "P. lingam" = "P. lingam",
                                                                     "R. solani" = "R. solani")
### Reorder them
aVOCs_Run6_ST_160525_JM_R_rmFungi_noCont_noNA$Fungi  <- factor(aVOCs_Run6_ST_160525_JM_R_rmFungi_noCont_noNA$Fungi, 
                                                               levels = c("A. radicina",
                                                                          "A. solani",
                                                                          "P. infestans",
                                                                          "P. lingam",
                                                                          "R. solani"))

#Order df alphabetically
aVOCs_Run6_ST_160525_JM_R_rmFungi_noCont_noNA <- aVOCs_Run6_ST_160525_JM_R_rmFungi_noCont_noNA[order(aVOCs_Run6_ST_160525_JM_R_rmFungi_noCont_noNA$Fungi), ]

### Run7
aVOCs_Run7_ST_120625_JM_R_rmFungi_noCont_noNA$Fungi %>% unique()

aVOCs_Run7_ST_120625_JM_R_rmFungi_noCont_noNA$Fungi <- dplyr::recode(aVOCs_Run7_ST_120625_JM_R_rmFungi_noCont_noNA$Fungi,
                                                                     "F. cul." = "F. culmorum",
                                                                     "P. infestans" = "P. infestans",
                                                                     "P. ult." = "P. ultimum")
### Reorder them
aVOCs_Run7_ST_120625_JM_R_rmFungi_noCont_noNA$Fungi  <- factor(aVOCs_Run7_ST_120625_JM_R_rmFungi_noCont_noNA$Fungi, 
                                                               levels = c("F. culmorum",
                                                                          "P. infestans",
                                                                          "P. ultimum"))

#Order df alphabetically
aVOCs_Run7_ST_120625_JM_R_rmFungi_noCont_noNA <- aVOCs_Run7_ST_120625_JM_R_rmFungi_noCont_noNA[order(aVOCs_Run7_ST_120625_JM_R_rmFungi_noCont_noNA$Fungi), ]

### Run8
aVOCs_Run8_ST_230725_JM_R_rmFungi_noCont_noNA$Fungi %>% unique()

aVOCs_Run8_ST_230725_JM_R_rmFungi_noCont_noNA$Fungi  <- dplyr::recode(aVOCs_Run8_ST_230725_JM_R_rmFungi_noCont_noNA$Fungi,
                                                                                      "F. cul." = "F. culmorum",
                                                                                      "P. lingam" = "P. lingam",
                                                                                      "R. solani" = "R. solani",
                                                                                      "P. ult." = "P. ultimum",
                                                                                      "F. gre" = "F. graminearum")
### Reorder them
aVOCs_Run8_ST_230725_JM_R_rmFungi_noCont_noNA$Fungi  <- factor(aVOCs_Run8_ST_230725_JM_R_rmFungi_noCont_noNA$Fungi, 
levels = c("F. culmorum",
           "P. lingam",
           "R. solani",
           "P. ultimum",
           "F. graminearum"))

#Order df alphabetically
aVOCs_Run8_ST_230725_JM_R_rmFungi_noCont_noNA <- aVOCs_Run8_ST_230725_JM_R_rmFungi_noCont_noNA[order(aVOCs_Run8_ST_230725_JM_R_rmFungi_noCont_noNA$Fungi), ]

### Run9
aVOCs_Run9_ST_300126_JM_R_rmFungi_noCont_noNA$Fungi %>% unique()

aVOCs_Run9_ST_300126_JM_R_rmFungi_noCont_noNA$Fungi  <- dplyr::recode(aVOCs_Run9_ST_300126_JM_R_rmFungi_noCont_noNA$Fungi,
                                                                      "A. solani" = "A. solani",
                                                                      "P. infestans" = "P. infestans",
                                                                      "F. gra" = "F. graminearum")
### Reorder them
aVOCs_Run9_ST_300126_JM_R_rmFungi_noCont_noNA$Fungi  <- factor(aVOCs_Run9_ST_300126_JM_R_rmFungi_noCont_noNA$Fungi, 
                                                               levels = c("A. solani",
                                                                          "P. infestans",
                                                                          "F. graminearum"))

#Order df alphabetically
aVOCs_Run9_ST_300126_JM_R_rmFungi_noCont_noNA <- aVOCs_Run9_ST_300126_JM_R_rmFungi_noCont_noNA[order(aVOCs_Run9_ST_300126_JM_R_rmFungi_noCont_noNA$Fungi), ]


##NOTE: The package "car" has a recode function too. It could mask the recode function in dplyr


#Datensatz überprüfen
## Identify outliers

#Convert multiple columns to factors
aVOCs_Run9_ST_300126_JM_R_rmFungi_noCont_noNA %>% colnames()
cols_aVOCs <- c("Fungi","Number","Ctrl","dpi","Finished")
cols_aVOCs_num <- c("GR1","GR2","GR3")

library(magrittr)
library(dplyr)
aVOCs_Run3_ST_310125_JM_R_rmFungi_noCont_noNA <- aVOCs_Run3_ST_310125_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs, factor)
aVOCs_Run4_ST_060325_JM_R_rmFungi_noCont_noNA <- aVOCs_Run4_ST_060325_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs, factor)
aVOCs_Run5_ST_100425_JM_R_rmFungi_noCont_noNA <- aVOCs_Run5_ST_100425_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs, factor)

aVOCs_Run6_ST_160525_JM_R_rmFungi_noCont_noNA <- aVOCs_Run6_ST_160525_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs, factor)
aVOCs_Run6_ST_160525_JM_R_rmFungi_noCont_noNA <- aVOCs_Run6_ST_160525_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs_num, as.numeric)

aVOCs_Run7_ST_120625_JM_R_rmFungi_noCont_noNA <- aVOCs_Run7_ST_120625_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs, factor)
aVOCs_Run8_ST_230725_JM_R_rmFungi_noCont_noNA <- aVOCs_Run8_ST_230725_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs, factor)
aVOCs_Run9_ST_300126_JM_R_rmFungi_noCont_noNA <- aVOCs_Run9_ST_300126_JM_R_rmFungi_noCont_noNA %>% mutate_at(cols_aVOCs, factor)

#Calculate the mean for each plate
df_aVOCs_Run3_ST_310125_mean_GR <- aVOCs_Run3_ST_310125_JM_R_rmFungi_noCont_noNA %>%
  mutate(across(c(GR1, GR2, GR3), ~ . - 0.3)) %>% #Subtract the radius of the plug (0.3 cm)
  mutate(mean_GR = rowMeans(across(c("GR1",   "GR2",   "GR3")), na.rm = TRUE)) %>% #Calculate the mean of the GR per 3 radii measured 
  dplyr::select("Fungi","Number","Ctrl","dpi","mean_GR")

df_aVOCs_Run4_ST_060325_mean_GR <- aVOCs_Run4_ST_060325_JM_R_rmFungi_noCont_noNA %>%
  mutate(across(c(GR1, GR2, GR3), ~ . - 0.3)) %>% #Subtract the radius of the plug (0.3 cm)
  mutate(mean_GR = rowMeans(across(c("GR1",   "GR2",   "GR3")), na.rm = TRUE)) %>% #Calculate the mean of the GR per 3 radii measured 
  dplyr::select("Fungi","Number","Ctrl","dpi","mean_GR")

df_aVOCs_Run5_ST_100425_mean_GR <- aVOCs_Run5_ST_100425_JM_R_rmFungi_noCont_noNA %>%
  mutate(across(c(GR1, GR2, GR3), ~ . - 0.3)) %>% #Subtract the radius of the plug (0.3 cm)
  mutate(mean_GR = rowMeans(across(c("GR1",   "GR2",   "GR3")), na.rm = TRUE)) %>% #Calculate the mean of the GR per 3 radii measured 
  dplyr::select("Fungi","Number","Ctrl","dpi","mean_GR")

df_aVOCs_Run6_ST_160525_mean_GR <- aVOCs_Run6_ST_160525_JM_R_rmFungi_noCont_noNA %>%
  mutate(across(c(GR1, GR2, GR3), ~ . - 0.3)) %>% #Subtract the radius of the plug (0.3 cm)
  mutate(mean_GR = rowMeans(across(c("GR1",   "GR2",   "GR3")), na.rm = TRUE)) %>% #Calculate the mean of the GR per 3 radii measured 
  dplyr::select("Fungi","Number","Ctrl","dpi","mean_GR")

df_aVOCs_Run7_ST_120625_mean_GR <- aVOCs_Run7_ST_120625_JM_R_rmFungi_noCont_noNA %>%
  mutate(across(c(GR1, GR2, GR3), ~ . - 0.3)) %>% #Subtract the radius of the plug (0.3 cm)
  mutate(mean_GR = rowMeans(across(c("GR1",   "GR2",   "GR3")), na.rm = TRUE)) %>% #Calculate the mean of the GR per 3 radii measured 
  dplyr::select("Fungi","Number","Ctrl","dpi","mean_GR")

df_aVOCs_Run8_ST_230725_mean_GR <- aVOCs_Run8_ST_230725_JM_R_rmFungi_noCont_noNA %>%
  mutate(across(c(GR1, GR2, GR3), ~ . - 0.3)) %>% #Subtract the radius of the plug (0.3 cm)
  mutate(mean_GR = rowMeans(across(c("GR1",   "GR2",   "GR3")), na.rm = TRUE)) %>% #Calculate the mean of the GR per 3 radii measured 
  dplyr::select("Fungi","Number","Ctrl","dpi","mean_GR")

df_aVOCs_Run9_ST_300126_mean_GR <- aVOCs_Run9_ST_300126_JM_R_rmFungi_noCont_noNA %>%
  mutate(across(c(GR1, GR2, GR3), ~ . - 0.3)) %>% #Subtract the radius of the plug (0.3 cm)
  mutate(mean_GR = rowMeans(across(c("GR1",   "GR2",   "GR3")), na.rm = TRUE)) %>% #Calculate the mean of the GR per 3 radii measured 
  dplyr::select("Fungi","Number","Ctrl","dpi","mean_GR")

#Calculate statistics
library(FSA)
###3
df_aVOCs_Run3_ST_310125_mean_GR_stat <- df_aVOCs_Run3_ST_310125_mean_GR %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(mean=mean(mean_GR, na.rm = TRUE),
            median=median(mean_GR, na.rm = TRUE),
            min=min(mean_GR, na.rm = TRUE),
            max=max(mean_GR, na.rm = TRUE),
            sd=sd(mean_GR, na.rm = TRUE),
            var=var(mean_GR, na.rm = TRUE),
            se=se(mean_GR, na.rm = TRUE),
            Replicates =sum(!is.na(mean_GR)))

###4
df_aVOCs_Run4_ST_060325_mean_GR_stat <- df_aVOCs_Run4_ST_060325_mean_GR %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(mean=mean(mean_GR, na.rm = TRUE),
            median=median(mean_GR, na.rm = TRUE),
            min=min(mean_GR, na.rm = TRUE),
            max=max(mean_GR, na.rm = TRUE),
            sd=sd(mean_GR, na.rm = TRUE),
            var=var(mean_GR, na.rm = TRUE),
            se=se(mean_GR, na.rm = TRUE),
            Replicates =sum(!is.na(mean_GR)))

###5
df_aVOCs_Run5_ST_100425_mean_GR_stat <- df_aVOCs_Run5_ST_100425_mean_GR %>%
  group_by(Fungi, Ctrl, dpi) %>%
  summarize(mean=mean(mean_GR, na.rm = TRUE),
            median=median(mean_GR, na.rm = TRUE),
            min=min(mean_GR, na.rm = TRUE),
            max=max(mean_GR, na.rm = TRUE),
            sd=sd(mean_GR, na.rm = TRUE),
            var=var(mean_GR, na.rm = TRUE),
            se=se(mean_GR, na.rm = TRUE),
            Replicates =sum(!is.na(mean_GR)))

###6
df_aVOCs_Run6_ST_160525_mean_GR_stat <- df_aVOCs_Run6_ST_160525_mean_GR %>%
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
df_aVOCs_Run7_ST_120625_mean_GR_stat <- df_aVOCs_Run7_ST_120625_mean_GR %>%
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
df_aVOCs_Run8_ST_230725_mean_GR_stat <- df_aVOCs_Run8_ST_230725_mean_GR %>%
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
df_aVOCs_Run9_ST_300126_mean_GR_stat <- df_aVOCs_Run9_ST_300126_mean_GR %>%
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

##3
df_aVOCs_Run3_ST_310125_abs_GR <- df_aVOCs_Run3_ST_310125_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl) %>%
  slice(1) %>% 
  group_by(Fungi)

##4
df_aVOCs_Run4_ST_060325_abs_GR <- df_aVOCs_Run4_ST_060325_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl) %>%
  slice(1) %>% 
  group_by(Fungi)

##5
df_aVOCs_Run5_ST_100425_abs_GR <- df_aVOCs_Run5_ST_100425_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl) %>%
  slice(1) %>% 
  group_by(Fungi)

##6
df_aVOCs_Run6_ST_160525_abs_GR <- df_aVOCs_Run6_ST_160525_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl) %>%
  slice(1) %>% 
  group_by(Fungi)

##7
df_aVOCs_Run7_ST_120625_abs_GR <- df_aVOCs_Run7_ST_120625_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl) %>%
  slice(1) %>% 
  group_by(Fungi)

##8
df_aVOCs_Run8_ST_230725_abs_GR <- df_aVOCs_Run8_ST_230725_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl) %>%
  slice(1) %>% 
  group_by(Fungi)

##9
df_aVOCs_Run9_ST_300126_abs_GR <- df_aVOCs_Run9_ST_300126_mean_GR_stat %>%                                      
  arrange(desc(dpi)) %>% 
  group_by(Fungi, Ctrl) %>%
  slice(1) %>% 
  group_by(Fungi)

##Save all dfs in a single df

df_aVOCs_Run3_ST_310125_abs_GR$run <- 3
df_aVOCs_Run4_ST_060325_abs_GR$run <- 4
df_aVOCs_Run5_ST_100425_abs_GR$run <- 5
df_aVOCs_Run6_ST_160525_abs_GR$run <- 6
df_aVOCs_Run7_ST_120625_abs_GR$run <- 7
df_aVOCs_Run8_ST_230725_abs_GR$run <- 8
df_aVOCs_Run9_ST_300126_abs_GR$run <- 9

Data_abs_GR <- rbind(df_aVOCs_Run3_ST_310125_abs_GR,
                  df_aVOCs_Run4_ST_060325_abs_GR,
                  df_aVOCs_Run5_ST_100425_abs_GR,
                  df_aVOCs_Run6_ST_160525_abs_GR,
                  df_aVOCs_Run7_ST_120625_abs_GR,
                  df_aVOCs_Run8_ST_230725_abs_GR,
                  df_aVOCs_Run9_ST_300126_abs_GR)

# Select the last measurement per fungi to calculate the relative inhibition
# Calculate relative change from the first time point in each group

#####3
df_aVOCs_Run3_ST_310125_rel_Inh <- df_aVOCs_Run3_ST_310125_mean_GR_stat %>%                                      
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
#####4
df_aVOCs_Run4_ST_060325_rel_Inh <- df_aVOCs_Run4_ST_060325_mean_GR_stat %>%                                      
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
#####5
df_aVOCs_Run5_ST_100425_rel_Inh <- df_aVOCs_Run5_ST_100425_mean_GR_stat %>%                                      
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
#####6
df_aVOCs_Run6_ST_160525_rel_Inh <- df_aVOCs_Run6_ST_160525_mean_GR_stat %>%                                      
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
#####7
df_aVOCs_Run7_ST_120625_rel_Inh <- df_aVOCs_Run7_ST_120625_mean_GR_stat %>%                                      
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
#####8
df_aVOCs_Run8_ST_230725_rel_Inh <- df_aVOCs_Run8_ST_230725_mean_GR_stat %>%                                      
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
#####9
df_aVOCs_Run9_ST_300126_rel_Inh <- df_aVOCs_Run9_ST_300126_mean_GR_stat %>%                                      
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

#######
# Save the dfs in a single excel file
df_aVOCs_Run3_ST_310125_rel_Inh$run <- 3
df_aVOCs_Run4_ST_060325_rel_Inh$run <- 4
df_aVOCs_Run5_ST_100425_rel_Inh$run <- 5
df_aVOCs_Run6_ST_160525_rel_Inh$run <- 6
df_aVOCs_Run7_ST_120625_rel_Inh$run <- 7
df_aVOCs_Run8_ST_230725_rel_Inh$run <- 8
df_aVOCs_Run9_ST_300126_rel_Inh$run <- 9

Raw_Data <- rbind(df_aVOCs_Run3_ST_310125_rel_Inh,
                  df_aVOCs_Run4_ST_060325_rel_Inh,
                  df_aVOCs_Run5_ST_100425_rel_Inh,
                  df_aVOCs_Run6_ST_160525_rel_Inh,
                  df_aVOCs_Run7_ST_120625_rel_Inh,
                  df_aVOCs_Run8_ST_230725_rel_Inh,
                  df_aVOCs_Run9_ST_300126_rel_Inh)

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

Data_abs_GR_Stat <- Data_abs_GR %>%
  group_by(Fungi, Ctrl) %>%
  summarise(mean=mean(mean, na.rm = TRUE),
            median=median(mean, na.rm = TRUE),
            min=min(mean, na.rm = TRUE),
            max=max(mean, na.rm = TRUE),
            sd=sd(mean, na.rm = TRUE),
            var=var(mean, na.rm = TRUE),
            se=se(mean, na.rm = TRUE),
            Replicates =sum(!is.na(mean)))

list_of_datasets_aVOCs_Run3to9_ST_JM_R <- list("Raw_Data" = Raw_Data,
                                               "Data_abs_GR" = Data_abs_GR,
                                               "Data_abs_GR_Stat" = Data_abs_GR_Stat,
                                              "Data_Rel_Inh" = Data_Rel_Inh)

write.xlsx(list_of_datasets_aVOCs_Run3to9_ST_JM_R, file = "All_DataSets_aVOCs_Run3to9_ST_JM_R_PS.xlsx")

#######
#Putting together all fungi into one df to visualize the data

dfaVOCs3 <- df_aVOCs_Run3_ST_310125_mean_GR_stat %>% filter(Fungi %in% c("A. solani", "F. graminearum", "P. lingam", "R. solani"))
dfaVOCs6 <- df_aVOCs_Run6_ST_160525_mean_GR_stat %>% filter(Fungi == "A. radicina")
dfaVOCs7 <- df_aVOCs_Run7_ST_120625_mean_GR_stat %>% filter(Fungi == "P. infestans")
dfaVOCs8 <- df_aVOCs_Run8_ST_230725_mean_GR_stat %>% filter(Fungi %in% c("F. culmorum", "P. ultimum"))

aVOCs_all_fungi_best <- rbind(dfaVOCs3, dfaVOCs6, dfaVOCs7, dfaVOCs8)

write.xlsx(Best_fungi_VOCs_time_plot_JM_R, file = "Best_fungi_VOCs_time_plot_JM_R.xlsx")

################
#Statistical analysis
# Analyzing multiple independent experiments together using the "glmmTMB" and DHARMa package
#VOCs_ST

#1. Generate a df with the pooled data indicating the respective experiment

#Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggpubr)
library(moments)
library(broom)
library(rcompanion) #Dependency: "DescTools"
library(openxlsx)

library(glmmTMB)
library(DHARMa)
library(ggeffects)
library(emmeans)
library(scico)
library(multcomp)
library(multcompView)
library(ggeffects)

#set workdictionary
setwd("C:/Users/Julian.Maier/Documents/Manuscript_Urlaub/Fig_1/Skripte_und_Daten")

#Read data
aVOCs_Run3to9_ST_JM_R <- read_excel("All_DataSets_aVOCs_Run3to9_ST_JM_R_PS.xlsx", 
                                    sheet = "Data_abs_GR")

#Dataset
All_DataSets_aVOCs_Run3to9_ST_JM_R <- aVOCs_Run3to9_ST_JM_R %>% mutate_at(c("Fungi", "Ctrl", "dpi", "Replicates", "run"), as.factor)

#Looking  data distributions
hist(All_DataSets_aVOCs_Run3to9_ST_JM_R$mean, breaks = 30)

####Fit models
#All family functions: https://glmmtmb.github.io/glmmTMB/reference/nbinom2.html 
##Bisher bestes model: Keine Fehler, Alle Behandlungen inklusive (ohne CU 3h ATLAS)
#NOTE: All_DataSets_aVOCs_Run3to9_ST_JM_R_beta$rel_inf_area[All_DataSets_aVOCs_Run3to9_ST_JM_R_beta$rel_inf_area == 1] <- 1 - 1e-6 was not include --> better fit

aVOCs_Run3to9_ST_gaussian_Zi0_dispCtrl <- glmmTMB(mean ~ Ctrl * Fungi + (1 | run),
                                                  ziformula = ~ 0,   # models excess zeros/ones
                                                  dispformula = ~ 1,
                                                  data = All_DataSets_aVOCs_Run3to9_ST_JM_R,
                                                  family = gaussian())

simulationOutput_aVOCs_Run3to9_ST <- simulateResiduals(aVOCs_Run3to9_ST_gaussian_Zi0_dispCtrl_newform)
#Fig. S1 Upper
plot(simulationOutput_aVOCs_Run3to9_ST) #Takes simulateResiduals() output
#Fig. S1 Middel
testDispersion(simulationOutput_aVOCs_Run3to9_ST) #Takes simulateResiduals() output
testQuantiles(simulationOutput_aVOCs_Run3to9_ST)  #Takes simulateResiduals() output
#Fig. S1 Lower
testOutliers(simulationOutput_aVOCs_Run3to9_ST)  #Takes simulateResiduals() output
outliers(simulationOutput_aVOCs_Run3to9_ST) #Takes simulateResiduals() output
summary(aVOCs_Run3to9_ST_gaussian_Zi0_dispCtrl_newform) #Takes glmmTMB() output
logLik(aVOCs_Run3to9_ST_gaussian_Zi0_dispCtrl_newform) #Takes glmmTMB() output

#Calculating the 95% CI (Supplements Table S1)
library(ggeffects)
pred_results <- predict_response(aVOCs_Run3to9_ST_gaussian_Zi0_dispCtrl_newform, terms = c("Ctrl", "Fungi"))
print(pred_results, collapse_table = TRUE, collapse_ci = TRUE)

AIC(aVOCs_Run3to9_ST_gaussian_Zi0_dispCtrl_newform, aVOCs_Run3to9_ST_gaussian_Zi0_dispCtrl) # Gütevergleich unterschiedlicher Modelle; Can only be used for the same data sets; the lower the AIC the better the model

#Pairwise comparison between treated and untreated fungi
#warp.emm_pair <- emmeans(aVOCs_Run3to9_ST_gaussian_Zi0_dispCtrl_newform,~ Ctrl | Fungi) #~ factor1 | factor2: Marginal means of factor1 grouped by factor2.
#cont.emm_pair <- contrast(warp.emm_pair, method = "pairwise")

#Comparison between all samples
warp.emm_over <- emmeans(aVOCs_Run3to9_ST_gaussian_Zi0_dispCtrl_newform,~ Ctrl * Fungi)
cont.emm_over <- contrast(warp.emm_over,  method = "pairwise")

#CLD for down stream plotting of data (Sig. letters for Figure 1)
#cld_aVOCs_ST_pair <- cld(emmeans(aVOCs_Run3to9_ST_gaussian_Zi0_dispCtrl_newform,~ Ctrl | Fungi)) #Takes glmmTMB() output #Pairwise comparison
cld_aVOCs_ST_over <- cld(emmeans(aVOCs_Run3to9_ST_gaussian_Zi0_dispCtrl_newform,~ Ctrl * Fungi)) #Takes glmmTMB() output #Overall comparison

##############
#Generating paper graphs
# Load required libraries
library(ggplot2)
library(cowplot)
library(ggplot2)
library(png)
library(grid)
library(patchwork)
library(readxl)
library(multcomp)
library(multcompView)
library(emmeans)
library(ggplotify)
library(magick)
library(tiff)

#set workdictionary
setwd("C:/Users/Julian.Maier/Documents/Manuscript_Urlaub/Fig_1/Skripte_und_Daten")

#Load Data sets

#Read data
aVOCs_Run3to9_ST_JM_R <- read_excel("All_DataSets_aVOCs_Run3to9_ST_JM_R_PS.xlsx", 
                                    sheet = "Data_abs_GR")

#Dataset
All_DataSets_aVOCs_Run3to9_ST_JM_R <- aVOCs_Run3to9_ST_JM_R %>% mutate_at(c("Fungi", "Ctrl", "dpi", "Replicates", "run"), as.factor)

Best_fungi_time_series_ST_JM_R <- read_excel("Best_fungi_VOCs_time_plot_JM_R.xlsx", 
                                             sheet = "Growth_Time")

# Generate Graphs
#A - Boxplot VOCs (Fig. 1A)

x_Title_A <- expression(paste("Days post pathogen inoculation [dppi]"))
y_Title_A <- expression(paste("Absolute growth [cm]"))

# Put Sig letters 
# Split sig. dif. groups with more than one number from cld(emmeans)

convert_cld_numbers_to_letters <- function(groups) { 
  cleaned <- gsub("\\s+", "", groups) # Remove ALL whitespace (spaces, tabs, newlines) 
  split_groups <- strsplit(cleaned, "") # Split strings like "12" into c("1","2")
  map <- setNames(letters[1:9], as.character(1:9)) # Map numbers to letters
  sapply(split_groups, function(x) paste(map[x], collapse = ""))
}
#To run this part first run the script "glmmTMB_DHARMa_analysis_All_DataSets"
letter_df <- data.frame(Ctrl = cld(emmeans(aVOCs_Run3to9_ST_gaussian_Zi0_dispCtrl_newform,~ Ctrl * Fungi))$Ctrl, Fungi = cld(emmeans(aVOCs_Run3to9_ST_gaussian_Zi0_dispCtrl_newform,~ Ctrl * Fungi))$Fungi,  Letters = convert_cld_numbers_to_letters((cld_aVOCs_ST_over)$.group))
y_pos <- max(All_DataSets_aVOCs_Run3to9_ST_JM_R$mean) * 1.15
geom_text(data = letter_df, aes(x = Fungi, y = y_pos, label = Letters), size = 5)

A <- All_DataSets_aVOCs_Run3to9_ST_JM_R %>% 
  group_by(Fungi, Ctrl) %>%
  ggplot(aes(x = Fungi, y = mean, group = Ctrl)) +
  geom_errorbar(stat = "summary", fun.data = mean_sdl, fun.args = list(mult = 1), width = 0.2, color = "black", alpha = 1, position = position_dodge(width = 0.9)) +
  stat_summary(geom = "point", fun = "mean", color = "red", shape = 15, position = position_dodge(width = 0.9)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.9), shape = 16, size = 1, color ="black") +
  geom_text(data = letter_df, aes(x = Fungi, y = y_pos, label = Letters, group = Ctrl), size = 4, color = "black", position = position_dodge(0.9)) +
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

setwd("//do-fs-01v/bi/FG-Phytopatho/Julian Maier-OptiLyso/Publishing/Maier et al. 1/Mode of action/Abb_Graphen_Bilder/Abb_manuscript/ST")

ggsave("Figure1A.tiff", plot = A, device = tiff, dpi = 1000, width = 17, height = 9, units = "cm", pointsize = 12)

#B - Line Graph - VOCs
#Supplements (Fig. S4)

x_Title_B <- expression(paste("Days post pathogen inoculation [dppi]"))
y_Title_B <- expression(paste("Mean radial growth [cm]"))

B <- aVOCs_all_fungi_best %>% 
  group_by(Fungi, dpi) %>%
  ggplot(aes(x = as.numeric(dpi), y = mean, group = Ctrl, shape = Ctrl)) +
  geom_point(size = 1.5) + 
  scale_x_continuous(breaks=seq(1, 13, 2)) +
  scale_y_continuous(limits = c(NA, 4), breaks=seq(0, 4, 2)) +
  #geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd, color = Ctrl)) +
  facet_wrap(~ Fungi) +
  labs(
    x=x_Title_B,
    y=y_Title_B) +
  theme_bw() +
  theme(text = element_text(size = 12, family = "sans"), # sans = Arial
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        legend.position = "none",
        strip.background = element_rect(fill = "white"), 
        strip.text = element_text(face = "italic", color = "black",size = 12))
ggsave("FigureS4.tiff", plot = B, device = tiff, dpi = 1000, width = 17, height = 9, units = "cm", pointsize = 12)

#C - Picture Fungi
#library(ggplotify)
#library(magick)
#library(tiff)

# Read the TIFF image (Fig. 1B)
C_raw <- image_read("VOCs_ST_Fungi_Examples_wide.tif")
# Convert to a ggplot object 
C <- as.ggplot(C_raw)

# Combine the plots into a single layout
library(cowplot)
combined_plot <- plot_grid(A, C, labels = c("A)", "B)"), ncol = 1, rel_heights = c(1,0.5) , rel_widths = c(1,4))

ggsave("Fugure1.tiff", plot = combined_plot, device = tiff, dpi = 1000, width = 17, height = 16, units = "cm", pointsize = 12)
