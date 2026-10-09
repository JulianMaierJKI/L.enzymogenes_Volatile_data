#aVOCs in planta
#1. and 2. run
#13.08.25 & 19.11.25
#Analysis of the leaf exposed optimally to the VOCs compared to the rest

#Load libraries
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggpubr)
library(moments)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(multcomp)
library(multcompView)
library(FSA)

#set workdictionary
setwd("C:/Users/Julian.Maier/Documents/JKI_OptiLyso/Relative infected leaf area/aVOCs_ad_planta/Cucumber/Paper")

#Read data
library(readxl)
aVOCs_ad_planta_Run1_130825_raw <- read_excel("Boniturschema_130825.xlsx")
aVOCs_ad_planta_Run2_191125_raw <- read_excel("Boniturschema_191125.xlsx")

#remove unnecessary columns
aVOCs_ad_planta_Run1_130825_raw <- aVOCs_ad_planta_Run1_130825_raw[,c(1:4)]
aVOCs_ad_planta_Run2_191125_raw <- aVOCs_ad_planta_Run2_191125_raw[,c(1:4)]

#Use commune column names
colnames(aVOCs_ad_planta_Run1_130825_raw)[colnames(aVOCs_ad_planta_Run1_130825_raw) == "rel_inf_area"] <- "inf.area"
colnames(aVOCs_ad_planta_Run2_191125_raw)[colnames(aVOCs_ad_planta_Run2_191125_raw) == "Rel_inf_area"] <- "inf.area"

#Find all missing rel. inf. values
#This converts all non.numeric rel. inf. values to NA and returns the respective row number
x_nonum_Run1 <- which(is.na(as.numeric(aVOCs_ad_planta_Run1_130825_raw$inf.area))) #All good
aVOCs_ad_planta_Run1_130825_new <- aVOCs_ad_planta_Run1_130825_raw

x_nonum_Run2 <- which(is.na(as.numeric(aVOCs_ad_planta_Run2_191125_raw$inf.area))) #Plant #3 and #6 of the LEC treated group were dead (they dried up)
aVOCs_ad_planta_Run2_191125_new <- aVOCs_ad_planta_Run2_191125_raw[-x_nonum_Run2,]

#Combine the two dfs and add a column indicating the Experiment (Run)
aVOCs_ad_planta_Run1_130825_new$Run <- 1
aVOCs_ad_planta_Run2_191125_new$Run <- 2

aVOCs_ad_planta_Run1and2 <- rbind(aVOCs_ad_planta_Run1_130825_new, aVOCs_ad_planta_Run2_191125_new)

#Check for unified names
aVOCs_ad_planta_Run1and2$Treatment %>% unique()

#Column inf.area is not numeric -> convert
aVOCs_ad_planta_Run1and2$inf.area <- as.numeric(aVOCs_ad_planta_Run1and2$inf.area)

#Convert multiple columns to factors
aVOCs_ad_planta_Run1and2 %>% colnames()
cols_aVOCs_ip <- c("Treatment","Plant","Leaf","Run")

library(magrittr)
library(dplyr)
aVOCs_ad_planta_Run1and2 <- aVOCs_ad_planta_Run1and2 %>% mutate_at(cols_aVOCs_ip, factor)

#Compute summary statistics:
#1. Select the leaf which was exposed optimally to the VOCs = leaf with the lowest infection value
#2. Remove them from the original df
#3. Rename treatments
#4. Combine the two data sets again
#5. Calculate the parameters by groups - mean, sd, var, count: 

#1.
df_LEC20_leafs_exposed <- aVOCs_ad_planta_Run1and2 %>% 
  group_by(Treatment, Plant, Run) %>%
  filter(Treatment == "20LEC") %>%
  slice_min(inf.area, with_ties = FALSE)         # Select row with minimum inf.area in each group (corresponds to VOC exposed leaf)
#For the plants "20LEC, Plant:4, Leaf:1, Run:1" and "20LEC, Plant:8, Leaf:1, Run:1" no reduction in infection was observed. They are both flagged by the outlier detection downstream. However, they were still included in the anmalysis because they represent true biological data/ plant reactions to the treatment. 

#2 anti_join
aVOCs_ad_planta_Run1and2_noExposure <- anti_join(aVOCs_ad_planta_Run1and2, df_LEC20_leafs_exposed, by=c("Treatment", "Plant", "Leaf", "inf.area", "Run"))

#3
df_LEC20_leafs_exposed$Treatment <- recode(df_LEC20_leafs_exposed$Treatment,
                                                "20LEC" = "+LEC \n volatiles")

aVOCs_ad_planta_Run1and2_noExposure$Treatment <- recode(aVOCs_ad_planta_Run1and2_noExposure$Treatment,
                                           "CON+" = 'CON+', 
                                           "CON-" = 'CON-',
                                           "CU" = "Cuprozin Progress \n 0.52%",
                                           "20LEC" = "-LEC \n volatiles")
#4 
df_aVOCs_in_planta_one_leaf_1and2 <- rbind(df_LEC20_leafs_exposed, aVOCs_ad_planta_Run1and2_noExposure)

df_aVOCs_in_planta_one_leaf_1and2$Treatment <- factor(df_aVOCs_in_planta_one_leaf_1and2$Treatment, 
                                                                      levels = c("CON+", 
                                                                                 "CON-",
                                                                                 "Cuprozin Progress \n 0.52%",
                                                                                 "-LEC \n volatiles",
                                                                                 "+LEC \n volatiles"))

#Use to order the table according to the factor levels
df_aVOCs_in_planta_one_leaf_1and2 <- with(df_aVOCs_in_planta_one_leaf_1and2, df_aVOCs_in_planta_one_leaf_1and2[order(Treatment),])

#5 Calculate the mean by plant

df_aVOCs_in_planta_one_leaf_1and2_mean_by_plant <- df_aVOCs_in_planta_one_leaf_1and2 %>%
  group_by(Treatment, Plant, Run) %>% summarize(inf.area = mean(inf.area))

#Include relative protection efficiency
##Formula: 1-[median(x)/median(CON+)]

median_H2O_one_leaf <- median(unlist((df_aVOCs_in_planta_one_leaf_1and2_mean_by_plant %>%
                                        filter(Treatment == "CON+"))[,4]))

df_aVOCs_in_planta_one_leaf_1and2_stats_Run <- df_aVOCs_in_planta_one_leaf_1and2_mean_by_plant %>%
  group_by(Treatment, Run) %>%
  summarise(mean=mean(inf.area, na.rm = TRUE),
            median=median(inf.area, na.rm = TRUE),
            min=min(inf.area, na.rm = TRUE),
            max=max(inf.area, na.rm = TRUE),
            sd=sd(inf.area, na.rm = TRUE),
            var=var(inf.area, na.rm = TRUE),
            se=se(inf.area, na.rm = TRUE), # Need library(FSA)
            rel_prot_eff=(1-(median(inf.area, na.rm = TRUE)/median_H2O_one_leaf))*100,
            plants =sum(!is.na(inf.area)))

df_aVOCs_in_planta_one_leaf_1and2_stats_Treatment <- df_aVOCs_in_planta_one_leaf_1and2_mean_by_plant %>%
  group_by(Treatment) %>%
  summarise(mean=mean(inf.area, na.rm = TRUE),
            median=median(inf.area, na.rm = TRUE),
            min=min(inf.area, na.rm = TRUE),
            max=max(inf.area, na.rm = TRUE),
            sd=sd(inf.area, na.rm = TRUE),
            var=var(inf.area, na.rm = TRUE),
            se=se(inf.area, na.rm = TRUE), # Need library(FSA)
            rel_prot_eff=(1-(median(inf.area, na.rm = TRUE)/median_H2O_one_leaf))*100,
            plants =sum(!is.na(inf.area)))

library(xlsx)
library(openxlsx)

list_of_datasets_one_leaf <- list("Raw data" = aVOCs_ad_planta_Run1and2,
                                "Data_one_leaf" = df_aVOCs_in_planta_one_leaf_1and2,
                                "Data_one_leaf_mean" = df_aVOCs_in_planta_one_leaf_1and2_mean_by_plant,
                                "Statistics_Run" = df_aVOCs_in_planta_one_leaf_1and2_stats_Run,
                                "Statistics_Treatment" = df_aVOCs_in_planta_one_leaf_1and2_stats_Treatment)
write.xlsx(list_of_datasets_one_leaf, file = "aVOCs_in_Planta_cucumber_one_leaf_Exp1and2_data.xlsx")

#####
#Data visualization (Not the final plot!)
my_title_130825 <- expression(paste("Effect of aVOCs against" , italic(" P. cubensis"), "in cucumber plants"))
x_title_130825 <- expression(paste(bold("Treatments")))
y_title_130825 <- expression(paste(bold("Relative infected leaf area [%]")))

df_aVOCs_in_planta_one_leaf_1and2_mean_by_plant %>%
  group_by(Treatment) %>%
  ggplot(aes(x = Treatment, y = inf.area)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(height = 0, width = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "red") +
  scale_y_continuous(limits = c(NA, 100), breaks=seq(0, 100, 10)) +
  theme_bw() +
  theme(plot.title = element_text(size=22, face="bold", color = "black"), 
        plot.subtitle = element_text(size=15),
        axis.text.x = element_text(size=18, face="bold", color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18, face="bold", color = "black"),
        axis.title.y = element_text(size=18, face="bold", color = "black"),
        legend.position="none")

####
#Statistical analysis

#Values must be given as % in an Interval [0;1]
df_aVOCs_in_planta_one_leaf_1and2_mean_by_plant$perc_infec <- df_aVOCs_in_planta_one_leaf_1and2_mean_by_plant$inf.area/100

####
####Fit models
hist(df_aVOCs_in_planta_one_leaf_1and2_mean_by_plant$perc_infec) #Hist shows clear accumulation of data points at 0 and 1. --> ordbeta model.

#Remove outliers according to the downstream test outliers()

modelZ1_mean <- glmmTMB(perc_infec ~ Treatment + (1 | Run) + (1 | Run:Plant), # Random effect: Run and random nested effect: (Plant) are specified as random effects.
                        ziformula = ~1,   # models excess zeros/ones
                        data = df_aVOCs_in_planta_one_leaf_1and2_mean_by_plant,
                        family = ordbeta(link="logit"),  #Accounts for real biological zeros
                        dispformula = ~ Run) # Variance differs by Run therefor run was included as a dispersion formula
simulationOutput_modelZ1_mean <- simulateResiduals(modelZ1_mean)
#Fig. S2 Upper
plot(simulationOutput_modelZ1_mean) #Takes simulateResiduals() output
plotResiduals(simulationOutput_modelZ1_mean)
#Fig. S2 Middel
testDispersion(simulationOutput_modelZ1_mean) #Takes simulateResiduals() output
summary(modelZ1_mean) #Takes glmmTMB() output
#Fig. S2 Lower
outliers(simulationOutput_modelZ1_mean) #Takes simulateResiduals() output
testOutliers(simulationOutput_modelZ1_mean)  #Takes simulateResiduals() output
summary(modelZ1_mean) #Takes glmmTMB() output
logLik(modelZ1_mean) #Takes glmmTMB() output

#Supplements Table S2
library(ggeffects)
pred_results <- predict_response(modelZ1_mean)
print(pred_results, collapse_table = TRUE, collapse_ci = TRUE)


em <- emmeans(modelZ1_mean,~Treatment, type="response") # type="response" gives results on the original proportion scale; Takes glmmTMB() output
#NOTE: df = Inf does not indicate an error; It is due to the used family = beta_family or ordbeta.
#Adjusting for multiple comparison: adjust options in emmeans include "tukey", "bonferroni", "holm", "fdr", etc.
cld(emmeans(modelZ1_mean,~Treatment, type="response"))
cld_aVOCs_inpla <- cld(emmeans(modelZ1_mean,~Treatment, type="response"))
#########################

#Plot data according to stat. analysis --> All experiments together
x_title_aVOCs_inpla <- expression(paste("Treatments"))
y_title_aVOCs_inpla <- expression(paste("Relative infected leaf area [%]"))

# Put Sig letters 
# Split sig. dif. groups with more tham one number from cld(emmeans)

convert_cld_numbers_to_letters <- function(groups) { 
  cleaned <- gsub("\\s+", "", groups) # Remove ALL whitespace (spaces, tabs, newlines) 
  split_groups <- strsplit(cleaned, "") # Split strings like "12" into c("1","2")
  map <- setNames(letters[1:9], as.character(1:9)) # Map numbers to letters
  sapply(split_groups, function(x) paste(map[x], collapse = ""))
}

letter_df <- data.frame( Treatment = cld(emmeans(modelZ1_mean,~Treatment, type="response"))$Treatment, Letters = convert_cld_numbers_to_letters((cld_aVOCs_inpla)$.group))
y_pos <- max(df_aVOCs_in_planta_one_leaf_1and2_mean_by_plant$inf.area) * 1.05
geom_text(data = letter_df, aes(x = Treatment, y = y_pos, label = Letters), size = 5)

D <- df_aVOCs_in_planta_one_leaf_1and2_mean_by_plant %>%
  group_by(Treatment) %>%
  ggplot(aes(x = Treatment, y = inf.area)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(height = 0, width = 0.2, size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 15, size = 1, color = "red") +
  geom_text(data = letter_df, aes(x = Treatment, y = y_pos, label = Letters), size = 5, color = "black") +
  scale_y_continuous(limits = c(NA, NA), breaks=seq(0, 110, 20)) +
  labs(
    x=x_title_aVOCs_inpla,
    y=y_title_aVOCs_inpla) +
  theme_bw() +
  theme(text = element_text(size = 12, family = "sans"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=12, color = "black"),
        axis.title.y = element_text(size=12, color = "black"),
        legend.position = "none")

ggsave("Figure3.tiff", plot = D, device = tiff, dpi = 1000, width = 17, height = 9, units = "cm", pointsize = 12)
