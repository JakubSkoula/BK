# statistical assessment of VIs usint paired t-test
# 24. 06. 2026
# Author: Jakub Skoula

# packages:
install.packages("readxl") # if not installed
library(readxl); library(dplyr)

# clear Global Environment
rm(list = ls())
# load data
phenex <- read_excel("Data/STATs/phenex.xlsx")

# check data structure and data types
head(phenex)

# checking normal distribution of data
VI <- phenex |> 
  filter(phenex$VI == "MCARI")
nrow(VI)
head(VI)
# calculates SOS - PL10
diff <- VI$SOS - VI$PL10

# plots
qqnorm(diff)
qqline(diff)
hist(diff)

# shapiro-wilk test of data normality
shapiro.test(VI$SOS - VI$PL10)$p.value

# if shapiro-wilk test of normality shows that distribution is:
# not normal -> wilcox pair test

# wilcox<-wilcox.test(VI$SOS, VI$PL10, paired = TRUE)
# wilcox

# normal -> students pair t-test

student.t = t.test(VI$SOS,VI$PL10, paired = TRUE)
student.t
