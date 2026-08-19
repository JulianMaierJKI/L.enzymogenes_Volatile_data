#Analysis of the GC-MS data

#1. Load libraries

library(data.table)
library(reshape2)
library(ggplot2)
library(dplyr)
#NMDS
library(lattice)
library(vegan)
library(MASS)
library(reshape2)
library(tidyverse)
library(RVAideMemoire)

library(FSA)
library(pheatmap)
library(ComplexHeatmap)
library(gtable)
library(grid)
library(ggplotify)

library(cowplot)

#2. Load data

setwd("//do-fs-01v/bi/FG-Phytopatho/Julian Maier-OptiLyso/Publishing/Maier et al. 1/Mode of action/HS_Analysis_by_J.Galliger/Data_Jannicke")
vocs <- read.delim("//do-fs-01v/bi/FG-Phytopatho/Julian Maier-OptiLyso/Publishing/Maier et al. 1/Mode of action/HS_Analysis_by_J.Galliger/Data_Jannicke/Bak_VOCs_report.csv")

#3. Check data structure and remove unnecessary compounds

vocs$filename <- as.factor(vocs$filename)
vocs$name <- as.factor(vocs$name)
#levels(vocs$name)
vocs$sn <- as.integer(vocs$sn)
#removing the peaks/components that are classified as impurities based on their fragmentation.
names_rm <- c("RI=1003.0_Silox","RI=1076.5_Silox","RI=1162.7_Silox","RI=1336.1_Silox","RI=850.7_Silox","RI=912.9_Silox","RI=969.6_Silox")
vocs1 <- subset(vocs, !(name %in% names_rm))
vocs1$name <- droplevels(vocs1$name)
#levels(vocs1$name)

#4. Only keep components with S/n > 20

vocs2 <- subset(vocs1, vocs1$sn>=20)
#levels(vocs2$name)
vocs2$name <- droplevels(vocs2$name)
#levels(vocs2$name)

#5. Transpose the data frame and set the filenames/ids as row names

dt <- as.data.table(vocs2) 
setkey(dt, filename, name )     # order data table by columns: 1st. filename and 2nd. name, requierd for the next step 

final.table <- dt[, .SD[CJ(unique(filename), unique(name))] ][is.na(area), area := 0][] # creat rows for not detected compounds, 

as <- final.table %>%
  count(filename, name) %>%
  arrange(n)

dfx <- as.data.frame(c(tapply(final.table$area, final.table$name, as.vector)))    
rownames(dfx) <- levels(final.table$filename)   
dfx$filename <- as.factor(rownames(dfx))

#6. Add additonal variables, treatments, etc.

samplelist <- read.delim("//do-fs-01v/bi/FG-Phytopatho/Julian Maier-OptiLyso/Publishing/Maier et al. 1/Mode of action/HS_Analysis_by_J.Galliger/Data_Jannicke/Bak_SampleList.csv")
#str(samplelist)
samplelist$filename <- as.factor(samplelist$filename)
samplelist$sample_ID <- as.factor(samplelist$sample_ID)
samplelist$bak <- as.factor(samplelist$bak)
nd <- merge(samplelist, dfx, by = "filename")

#7. Generate compositional dataset

nd$sum <- apply(nd[,c(4:37)], 1, sum)                                # create a new column and calculate the sum of all area values for one sample 
                                                                    # (1:5) = define columns which should be included (area values)
my.pct <- function (x){(x)/ nd$sum*100}                              # function for calculation of proportions 
pct <- as.data.frame(lapply(4:37, function(x) my.pct(nd[[x]])))     # calculate proportion of single compounds and store it in a new data frame 
                                                                    # apply the created function to every column, which should be included
names(pct) = names(nd)[4:37]                                        # name columns of the new data frame with names of VOCs
rownames(pct) -> nd$sample_ID                                        # set sample id as rownames

pct$groups <- as.factor(nd$bak)
#str(pct)

#8. NMDS
#meta-NMDS: robuster als iso-NMDS kleinster gemeinsamer Stress wird von mehreren Punkten aus gesucht. Auch f?r unterschiedlich skalierte Daten geeignet. 
#Transformiert die Daten automatisch mit Wisconcin double standardization. USE/Read data table without groups column 35

mds1 <- metaMDS(pct[,-35]  , "robust.aitchison",2, autotransform = F) # due to a lot of values being 0. robust.aitchison # Gruppenfaktor zur Berchnung auslassen: pct[,-35] 
mds1$stress # should be small
stressplot(mds1) # Generally, stress < 0.05 provides an excellent representation in reduced dimensions, < 0.1 is great, < 0.2 is good, and stress > 0.3 provides a poor representation

#9. Plotting data
##Read Data.scores from matrix -> coordinaten zum ploten der daten punkte pro sample dann mit ggplot hübsch plotten

data.scores <- as.data.frame(vegan::scores(mds1$points))  
data.scores$sample <- as.factor(rownames(data.scores))  
data.scores$grp<-pct$group
levels(pct$group)

#Sub figure A)
A <- data.scores %>%
  ggplot() +
  stat_ellipse(aes(x=MDS1,y=MDS2, colour=grp),level = 0.50) + 
  geom_point(aes(x=MDS1,y=MDS2,shape=grp),size=2) + 
  scale_colour_manual(values =c("black","black", "black"),
                      breaks = c("C", "D", "L"),
                      labels=c("C", "D" , "L"))+
  scale_shape_manual(values =c(15,17,19),
                     breaks = c("C", "D", "L"),
                     labels=c("C", "D", "L"))+
  theme_classic()+
  labs(colour = "", shape = "")+
  theme(text = element_text(size = 12, family = "sans"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size=12, color = "black"),
        axis.text.y = element_text(size=12, color = "black"),
        axis.title.y = element_text(size=12, color = "black"),
        legend.position="bottom",
        legend.text = element_text(size=12, color = "black"))

#10. Stats: Permanova
#Calculating the distance/dissimilarity matrix -> Euclidean/Bray-Curtis

dist_ait <- vegdist(pct[,-35], method = "robust.aitchison")
mean(dist_ait)  ### Mean distance of all data in the distance matrix

##10.1 Permutational multivariate analysis of variance  -> PERMANOVA (Anderson 2001)

PERMANOVA_ait <- adonis2(dist_ait ~ pct$groups, permutations=10000)
pairwise.perm.manova(vegdist(pct[,-c(35)], method="robust.aitchison"),pct$groups, nperm=10000, p.method="bonferroni")

#11. Multivariate homogeneity of groups dispersions (variances) -> PERMDISP (Anderson 2006)
##11.1 Calculate multivariate dispersions

pctbdis <- betadisper(dist_ait, pct$groups) ### Use "euclidean"" or  "bray" in vegdist

##11.2 Perform test
anova(pctbdis)

##11.3 Permutation test for F
permutest(pctbdis, pairwise = TRUE,permu=1000)

## 12.4 Tukey's Honest Significant Differences
#pctbdis.HSD <- TukeyHSD(pctbdis)
#plot(pctbdis.HSD)

#13. Plot the groups and distances to centroids on the first two PCoA axes
#plot(pctbdis)

#14. Draw a boxplot of the distances to centroid for each group
#boxplot(pctbdis, log="y", main="centered log-ratio")

#15. Identifying sig. dif. components
#Table S6
kruskal_pvalue <- sapply(4:37, function(x) {
  kruskal.test(nd[[x]]~ nd$bak)$p.value
})
names(kruskal_pvalue) <- names(nd)[4:37]
substance<-names(nd)[4:37]

kruskal_vocs <- data.frame(
  substance = substance,
  pvalue = kruskal_pvalue
)
kruskal_vocs$p_adj <- p.adjust(kruskal_vocs$pvalue, method = "BH")

kruskal_vocs$p_adj <- round(as.numeric(kruskal_vocs$p_adj), digits = 3)

kruskal_vocs$substance <- as.factor(kruskal_vocs$substance)
sig_vocs <- subset(kruskal_vocs, kruskal_vocs$p_adj <=  0.05)
sig_vocs_una <- subset(kruskal_vocs, kruskal_vocs$pvalue <=  0.05)

rownames(kruskal_vocs) <- NULL
Kruskal_Vocs <- kruskal_vocs

Kruskal_Vocs$substance <- recode(Kruskal_Vocs$substance,
                              "X1.Hexanol..2.ethyl.._userJKI" = "2-ethylhexanol",
                              "X1.Pentene..2.4.4.trimethyl." = "2,4,4-trimethylpentene",
                              "X2.Decanone" = "2-decanone",
                              "X2.Heptanone..userJKI" = "2-heptanone",
                              "X2.Heptanone..6.methyl." = "6-methyl-2-heptanone",
                              "X2.methyl.1.Butanol_userJKI" = "2-methyl-1-butanol",
                              "X2.Nonanone" = "2-nonanone",
                              "X2.Tetradecanone" = "RI 1563",
                              "X3.Furaldehyde" = "3-furaldehyde",
                              "Acetophenone" = "acetophenone",
                              "Benzaldehyd_userJKI" = "benzaldehyde",
                              "Cyclopentanone" = "cyclopentanone",
                              "Decane" = "decane",
                              "Furan..2.methoxy." = "2-methoxyfuran",
                              "n.Dodecan_userJKI" = "n-dodecane",
                              "p.Xylen...userJKI" = "p-xylene",
                              "Phenylacetaldehyd_userJKI" = "phenylacetaldehyde",
                              "Pyrazine..2.5.dimethyl." = "pyrazin (RI 919)",
                              "Pyrazine_RI.1052" = "pyrazin (RI 1052)",
                              "Pyrazine_RI..1172" = "pyrazin (RI 1172)",
                              "Pyrazine_RI.1058" = "pyrazin (RI 1058)",
                              "RI.1115.5..unknown" = "RI 1116",
                              "RI.1170.2.Dodecanol." = "RI 1170",
                              "RI.1232.2..unknown" = "RI 1232",
                              "RI.1294.9...Undecanone." = "undecanone",
                              "RI.1359.7.2.dodecanone." = "2-dodecanone",
                              "RI.1401.0_unknown.acid." = "RI 1401",
                              "RI.1472.5_unknown" = "RI 1473",
                              "RI.1600.7_unknown" = "RI 1601",
                              "RI.875.1.unknown" = "RI 875",
                              "RI.883.6_unknown" = "RI 884",
                              "RI.930.6_unknown" = "RI 931",
                              "RI.949.4_unkonwn" = "RI 949",
                              "xylene." = "xylene")

write.xlsx(Kruskal_Vocs, file = "VOCs_GCMS_data_kruskal_results.xlsx")

#kruskal_vocs
#sig_vocs

#16. Post hoc test
# column names of VOCs sig. according to Kruskal.test
#Table S6
sig_voc_names <- kruskal_vocs$substance[kruskal_vocs$p_adj <= 0.05]

dunn_results <- lapply(sig_voc_names, function(voc) {
  res <- dunnTest(as.formula(paste(voc, "~ bak")),
                  data = nd,
                  method = "bh")$res
  res$VOC <- voc
  return(res)
})

dunn_results_df <- bind_rows(dunn_results)
sig_dunn <- dunn_results_df %>%
  filter(P.adj <= 0.05)

Dunn_results_df <- sig_dunn
rownames(Dunn_results_df) <- NULL
Dunn_results_df <- Dunn_results_df[,c(1,2,4,5)]

Dunn_results_df$VOC <- recode(Dunn_results_df$VOC,
                              "X2.Decanone" = "2-decanone",
                              "X2.Heptanone..userJKI" = "2-heptanone",
                              "X2.Heptanone..6.methyl." = "6-methyl-2-heptanone",
                              "X2.methyl.1.Butanol_userJKI" = "2-methyl-1-butanol",
                              "X2.Nonanone" = "2-nonanone",
                              "X3.Furaldehyde" = "3-furaldehyde",
                              "Benzaldehyd_userJKI" = "benzaldehyde",
                              "Decane" = "decane",
                              "n.Dodecan_userJKI" = "n-dodecane",
                              "Phenylacetaldehyd_userJKI" = "phenylacetaldehyde",
                              "Pyrazine_RI.1052" = "pyrazin (RI 1052)",
                              "Pyrazine_RI.1058" = "pyrazin (RI 1058)",
                              "RI.1294.9...Undecanone." = "undecanone",
                              "RI.1359.7.2.dodecanone." = "2-dodecanone",
                              "RI.1401.0_unknown.acid." = "RI 1401",
                              "RI.1600.7_unknown" = "RI 1601")

colnames(Dunn_results_df)
Dunn_results_df <- Dunn_results_df[c("VOC", "Comparison", "Z", "P.adj")]

write.xlsx(Dunn_results_df, file = "VOCs_GCMS_data_dunn_test_results.xlsx")

# biological relevnt comparisons only 
##1. BAC-specific (D vs C & L vs C significant)

bac_specific <- sig_dunn %>%
  filter(Comparison %in% c("C - D", "C - L", "D - C", "L - C")) %>%
  mutate(comp = case_when(
    grepl("C.*D|D.*C", Comparison) ~ "C-D",
    grepl("C.*L|L.*C", Comparison) ~ "C-L"
  )) %>%
  group_by(VOC) %>%
  summarise(n = n_distinct(comp)) %>%
  filter(n == 2) %>%
  pull(VOC)

bac_table <- dunn_results_df %>%
  filter(VOC %in% bac_specific)

##1. LEC-specific (L vs C & L vs D significant)

L_specific <- sig_dunn %>%
  filter(Comparison %in% c("C - L", "L - C", "D - L", "L - D")) %>%
  mutate(comp = case_when(
    grepl("C.*L|L.*C", Comparison) ~ "C-L",
    grepl("D.*L|L.*D", Comparison) ~ "D-L"
  )) %>%
  group_by(VOC) %>%
  summarise(n = n_distinct(comp)) %>%
  filter(n == 2) %>%
  pull(VOC)

L_table <- dunn_results_df %>%
  filter(VOC %in% L_specific)

#box plot of LEC-specific VOCs

L_vocs <- unique(L_table$VOC)
LECvocs <- nd %>%
  dplyr::select(bak, all_of(L_vocs)) %>%
  tidyr::pivot_longer(-bak, names_to = "variable", values_to = "value")

#VOC names publication ready 
LECvocs$variable  <- recode(LECvocs$variable,
                            "X2.Decanone" = "2-decanone",
                            "X2.Heptanone..userJKI" = "2-heptanone",
                            "X2.Heptanone..6.methyl." = "6-methyl \n -2-heptanone",
                            "X2.Nonanone" = "2-nonanone",
                            "Pyrazine_RI.1058" = "pyrazine \n (RI 1058)",
                            "RI.1294.9...Undecanone." = "undecanone",
                            "RI.1359.7.2.dodecanone." = "2-dodecanone")

#Sub figure B)
B <- ggplot(LECvocs, aes(x = bak, y = value)) + 
  geom_boxplot(aes(fill = bak), outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.8, alpha = 0.8) +
  scale_fill_manual(
    values = c("white","white", "white"),
    breaks = c("C","D","L")) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Peak area", labels = scales::label_number(scale_cut = scales::cut_short_scale())))+
  theme_bw() +
  labs(x = "", y = " ", fill="") +
  facet_wrap(~ variable, scales = "free_y", nrow = 2, 
             labeller = labeller(variable = label_wrap_gen(width = 15))) +
  theme(text = element_text(size = 12, family = "sans"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=12, color = "black"),
        axis.title.y = element_text(size=12, color = "black"),
        axis.title.y.right = element_text(angle = 90),
        axis.text.y.left = element_blank(),
        strip.background = element_rect(fill = "white"), 
        strip.text = element_text(face = "italic", color = "black",size = 12),
        legend.position = "none")

#Generating heatmaps
df<-nd[,1:37]

#average peak area / bacteria bzw. media
df_mean <- df %>%
  group_by(bak) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

mat <- as.data.frame(df_mean)
rownames(mat) <- mat$bak
mat$bak <- NULL

colnames(mat) <- recode(colnames(mat),
                        "X1.Hexanol..2.ethyl.._userJKI" = "2-ethylhexanol",
                        "X1.Pentene..2.4.4.trimethyl." = "2,4,4-trimethylpentene",
                        "X2.Decanone" = "2-decanone",
                        "X2.Heptanone..userJKI" = "2-heptanone",
                        "X2.Heptanone..6.methyl." = "6-methyl-2-heptanone",
                        "X2.methyl.1.Butanol_userJKI" = "2-methyl-1-butanol",
                        "X2.Nonanone" = "2-nonanone",
                        "X2.Tetradecanone" = "RI 1563",
                        "X3.Furaldehyde" = "3-furaldehyde",
                        "Acetophenone" = "acetophenone",
                        "Benzaldehyd_userJKI" = "benzaldehyde",
                        "Cyclopentanone" = "cyclopentanone",
                        "Decane" = "decane",
                        "Furan..2.methoxy." = "2-methoxyfuran",
                        "n.Dodecan_userJKI" = "n-dodecane",
                        "p.Xylen...userJKI" = "p-xylene",
                        "Phenylacetaldehyd_userJKI" = "phenylacetaldehyde",
                        "Pyrazine..2.5.dimethyl." = "pyrazin (RI 919)",
                        "Pyrazine_RI.1052" = "pyrazin (RI 1052)",
                        "Pyrazine_RI..1172" = "pyrazin (RI 1172)",
                        "Pyrazine_RI.1058" = "pyrazin (RI 1058)",
                        "RI.1115.5..unknown" = "RI 1116",
                        "RI.1170.2.Dodecanol." = "RI 1170",
                        "RI.1232.2..unknown" = "RI 1232",
                        "RI.1294.9...Undecanone." = "undecanone",
                        "RI.1359.7.2.dodecanone." = "2-dodecanone",
                        "RI.1401.0_unknown.acid." = "RI 1401",
                        "RI.1472.5_unknown" = "RI 1473",
                        "RI.1600.7_unknown" = "RI 1601",
                        "RI.875.1.unknown" = "RI 875",
                        "RI.883.6_unknown" = "RI 884",
                        "RI.930.6_unknown" = "RI 931",
                        "RI.949.4_unkonwn" = "RI 949",
                        "xylene." = "xylene")


mat <- as.matrix(mat)

mat_log <- log10(mat + 0.1)
mat_log_t <- t(mat_log)

specificity_L <- mat_log["L", ] - rowMeans(mat_log[c("D", "C"), ])
specificity_L <- sort(specificity_L, decreasing = TRUE)

top_L <- names(specificity_L)
mat_L <- mat_log[, top_L]
mat_L <- mat_L[, order(specificity_L[top_L], decreasing = TRUE)]

# # Specify which column names should be bold
#myCols <- c("pyrazin \n (RI 919)","pyrazin \n (RI 1052)","pyrazin \n (RI 1172)","pyrazin \n (RI 1058)")

#Sub figure C)
library(grid)
library(ggplotify)

C <- pheatmap(mat_L,
              cluster_rows = FALSE,
              cluster_cols = FALSE,
              fontsize = 12,
              fontfamily = "sans",
              angle_col = c("90"),
              legend_labels = NA,
              legend_breaks = NA,
              name = "Scale")

D <- as.ggplot(grid.grabExpr(ComplexHeatmap::draw(C)))

# Combine the plots into a single layout
library(cowplot)
combined_plot <- plot_grid(
  plot_grid(A, B,
            labels = c("A)", "B)"),
            nrow = 1, 
            ncol = 2,
            rel_widths = c(0.3, 1)),
  plot_grid(NULL, D,
            labels = c("C)"),
            nrow = 2, 
            ncol = 1,
            rel_heights = c(0.1, 1),
            rel_widths = c(1, 0.5)),
  nrow = 2,
  rel_heights = c(1, 1))

ggsave("Figure_3_A_B_C_2.tiff", plot = combined_plot, device = tiff, dpi = 1000, width = 24, height = 18, units = "cm", pointsize = 12)
