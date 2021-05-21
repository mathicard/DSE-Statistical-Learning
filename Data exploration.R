#############################
#  Group 4: Above the norm  #
#  Statistical Learning DSE #
#  Final project - Module 1 #
#############################


library(readr)
library(dplyr)
library(readxl)
library(ggpubr)
library(ggplot2)
library(Hmisc)


#############################
##  1.Exploratory analysis
#############################

setwd("~/Documents/DSE/Statistical Learning/Projects")
dataset <- read.csv("usa_final.csv", sep=',', header = TRUE, dec = ".")

str(dataset)
summary(dataset)

colSums(is.na(dataset)) #We have no NAs

# 1. Describe the variable AQI

## Check normality ##

(d1 <-  dataset %>%
    ggplot(aes(x=aqi)) +
    geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8))

shapiro.test(dataset$aqi) #p-value<0.01

# AQI is not Normal distributed, we reject the null hypothesis of the Shapiro-Wilks test. 

## Logarithm transformation ##

dataset <- dataset %>% mutate(ln_aqi=log(dataset$aqi))

shapiro.test(dataset$ln_aqi) #p-value>0.05
# We reject the null hypothesis that ln(AQI) is not Normal distributed at 95% of confidence. 

#install.packages("ggpubr")
ggqqplot(dataset$ln_aqi, title = "QQ Plot of log AQI")

# As almost all the points fall approximately along the reference line, we can assume Normality.


## log(AQI) density plot ##

d2 <- dataset %>%
  ggplot(aes(x=ln_aqi)) +
  geom_density(fill="orangered3", color=FALSE, alpha=0.5)

ggarrange(d1, d2, 
          ncol = 2, nrow = 1)


# 2.	Correlation matrix between the variables of our dataset 

## Scatter plot matrix ## 

## Correlation matrix ##

res<-cor(dataset[,-c(1,2)]) 
round(res, 2)

#install.packages('Hmisc')
rcorr(as.matrix(dataset[,-1])) # Seems that correlation between lockdown and mining with AQI is not significative.

## Correlation heatmap ##

col<- colorRampPalette(c("darkgreen", "white", "darkred"))(20)
heatmap(x = res, col = col, symm = TRUE, Colv = NA, Rowv = NA)

library("PerformanceAnalytics")
chart.Correlation(dataset[,-1], histogram=TRUE, pch=19)

#Boxpot of AQI
library(car)
Boxplot(~aqi, data=dataset, id=list(labels=dataset$state))
#Possible outliers: California, DC and Wyoming.


# ANOVA with lockdown

# Perform the test
compare_means(aqi ~ lockdown,  data = dataset,
              ref.group = ".all.", method = "t.test")

# Visualize the expression profile
ggboxplot(dataset, x = "lockdown", y = "aqi", color = "lockdown", 
          add = "jitter", legend = "none") +
  geom_hline(yintercept = mean(dataset$aqi), linetype = 2)+ # Add horizontal line at base mean
  ylim(0, 200)+
  stat_compare_means(method = "anova", label.y = 200)+        # Add global ANOVA p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.", hide.ns = TRUE)      # Pairwise comparison against all


# We can conclude that AQI is not significantly different between States by lockdown measures.
