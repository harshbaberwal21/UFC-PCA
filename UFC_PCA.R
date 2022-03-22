
## Reading in the project dataset

setwd("path")
ufc <- read.csv("project-data.csv")

len_ini = nrow(ufc)
ufc <- ufc %>% mutate(date = as.Date.character(date)) %>% filter(date >= '2001-05-04')
len_fin = nrow(ufc)

# Dropped Rows
len_fin - len_ini
rm(len_ini,len_fin)


## Checking Dataset
head(ufc)
colnames(ufc)
str(ufc)
dim(ufc)

## Loading the library for data manipulation.
library(dplyr)

## Selecting the required variables and reforming the data.
selected_cols <- c("R_age","R_avg_BODY_landed","R_avg_CLINCH_landed","R_avg_GROUND_landed","R_avg_HEAD_landed","R_avg_KD","R_avg_LEG_landed","R_avg_opp_GROUND_landed","R_avg_opp_KD","R_avg_opp_LEG_landed","B_age","B_avg_BODY_landed","B_avg_CLINCH_landed","B_avg_GROUND_landed","B_avg_HEAD_landed","B_avg_KD","B_avg_LEG_landed","B_avg_opp_GROUND_landed","B_avg_opp_KD","B_avg_opp_LEG_landed")
ufc1 <- ufc %>% select(all_of(selected_cols))


## Checking for any missing values and removing using listwise deletion
dim(ufc1)
sum(is.na(ufc1))

ufc2 <- na.omit(ufc1)

sum(is.na(ufc2))
dim(ufc2)

################## PCA Run 1

#Test KMO Sampling Adequacy

library(psych)
KMO(ufc2)
#Overall MSA = 0.7

#Test Bartlett's Test of Sphericity
library(REdaS)
bart_spher(ufc2)
#p-value < 2.22e-16 (Very Small Number)

## Test for Reliability Analysis using Cronbach's Alpha
library(psych)
alpha(ufc2, check.keys = TRUE)
#raw_alpha = 0.65

library(fmsb)
CronbachAlpha(ufc2)
# 0.595


## Creating PCA for scree
p_ufc = prcomp(ufc2, center=T, scale=T)
#Check Scree Plot
plot(p_ufc)
abline(1, 0)


###### Running  PCA for 7 factos based on the eigenvalue method
pca_ufc_1 = psych::principal(ufc2, rotate="promax", nfactors=7, scores=TRUE)
pca_ufc_1
print(pca_ufc_1$loadings, cutoff=.5, sort=T)


################## PCA Run 2

# Removing avg_GROUND_landed variable
ufc3 <- ufc2 %>% select(!c("R_avg_GROUND_landed","B_avg_GROUND_landed"))

## Create PCA for scree
p_ufc_1 = prcomp(ufc3, center=T, scale=T)
#Check Scree Plot
plot(p_ufc_1)
abline(1, 0)


pca_ufc_2 = psych::principal(ufc3, rotate="promax", nfactors=5, scores=TRUE)
pca_ufc_2
print(pca_ufc_2$loadings, cutoff=.5, sort=T)


