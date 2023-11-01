# c4_vpd_invasion_analysis.R

## load libraries
library(tidyverse)
library(car)

## load vpd curve data
vpd_data <- read.csv('../data/raw_data_curves_clean.csv')
head(vpd_data)

## get mean values for each VPD for each curve

### add a new column for the grouping VPD and T for each curve
vpd_data$tvpd_group <- 'NULL'
vpd_data$tvpd_group[vpd_data$Tleaf < 32 & vpd_data$VPDleaf < 1.8] <- 'T1_VPD1'
vpd_data$tvpd_group[vpd_data$Tleaf < 32 & vpd_data$VPDleaf > 1.8 & vpd_data$VPDleaf < 2.8] <- 'T1_VPD2'
vpd_data$tvpd_group[vpd_data$Tleaf < 32 & vpd_data$VPDleaf > 2.8 & vpd_data$VPDleaf < 3.6] <- 'T1_VPD3'
vpd_data$tvpd_group[vpd_data$Tleaf < 32 & vpd_data$VPDleaf > 3.6] <- 'T1_VPD4'
vpd_data$tvpd_group[vpd_data$Tleaf > 32 & vpd_data$VPDleaf < 4] <- 'T2_VPD1'
vpd_data$tvpd_group[vpd_data$Tleaf > 32 & vpd_data$VPDleaf > 4 & vpd_data$VPDleaf < 5] <- 'T2_VPD2'
vpd_data$tvpd_group[vpd_data$Tleaf > 32 & vpd_data$VPDleaf > 5 & vpd_data$VPDleaf < 5.7] <- 'T2_VPD3'
vpd_data$tvpd_group[vpd_data$Tleaf > 32 & vpd_data$VPDleaf > 5.7] <- 'T2_VPD4'

### make unique id for each vpd within each curve
plant_id1 <- substr(vpd_data$id, start = 1, stop = 3)
plant_id2 <- gsub(" ", "", plant_id1)
plant_id3 <- gsub("i", "", plant_id2)
vpd_data$plant_id <- plant_id3

### group and get mean values
vpd_data_groupby <- group_by(vpd_data, plant_id, tvpd_group)
vpd_data_means <- summarise(vpd_data_groupby, 
                            VPDleaf_mean = mean(VPDleaf, na.rm = T),
                            Tleaf_mean = mean(Tleaf, na.rm = T),
                            A_mean = mean(A, na.rm = T),
                            gsw_mean = mean(gsw, na.rm = T),
                            Ci_mean = mean(Ci, na.rm = T),
                            E_mean = mean(E, na.rm = T))
head(vpd_data_means)

## read in plant information and connect to data
plant_id_data <- read.csv('../data/plant_ID.csv')
head(plant_id_data)
plant_id_data$plant_id <- as.character(plant_id_data$Individual)

vpd_data_combined <- left_join(vpd_data_means, plant_id_data, by = "plant_id")
head(vpd_data_combined)

## analyze the data
A_lm <- lm(A_mean ~ water_treatment * VPD_treatment * tvpd_group, data = vpd_data_combined)
plot(resid(A_lm) ~ fitted(A_lm))
summary(A_lm)
Anova(A_lm)



