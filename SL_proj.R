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
##  1.Data cleaning
#############################

dataset <- read.csv("dataset_US.csv", sep=',', header = TRUE, dec = ".", skip=1)

dataset <- read_csv("dataset_US.csv")

dataset <- read_table("dataset_US.txt")

dataset <- read_xlsx("dataset_US.xlsx")

names(dataset)[3] <- paste("value")

dataset$value <- as.numeric(dataset$value)

construction <- subset(dataset, Description=="Construction")
manufacturing <- subset(dataset, Description=="Manufacturing")
mining <- subset(dataset, Description=="Mining,quarrying,andoilandgasextraction")
transportation <- subset(dataset, Description=="Transportationandwarehousing")
retail <- subset(dataset, Description=="Retailtrade")
finance <- subset(dataset, Description=="Finance,insurance,realestate,rental,andleasing")
utilities <- subset(dataset, Description=="Utilities")
information <- subset(dataset, Description=="Information")
professional <- subset(dataset, Description=="Professionalandbusinessservices")
education <- subset(dataset, Description=="Educationalservices")
healthcare <- subset(dataset, Description=="Healthcareandsocialassistance")
waste <- subset(dataset, Description=="Administrativeandsupportandwastemanagementandremediationservices")
accommodation <- subset(dataset, Description=="Accommodationandfoodservices")

usa <- cbind(accommodation, construction, education, finance, healthcare, information, manufacturing, mining, 
             professional, retail, transportation, utilities, waste)

summary(usa)
View(usa)

usa_final <- usa[, c(1,3,6,9,12,15,18,21,24,27,30,33,36,39)] 

colnames(usa_final) <- c("state","accommodation", "construction", "education", "finance", "healthcare", 
                         "information", "manufacturing", "mining", 
                         "professional", "retail", "transportation", "utilities", "waste")
View(usa_final)

write.csv(usa_final, "usa_final.csv")
aqi <- read.csv("annual_aqi_by_county_2020.")



#############################
##  2.Exploratory analysis
#############################

setwd("~/Documents/DSE/Statistical Learning/Projects")
dataset <- read.csv("usa_final_v2.csv", sep=';', header = TRUE, dec = ".")

str(dataset)
summary(dataset)

# 1. Describe the variable AQI

## Check normality ##

(d1 <-  dataset %>%
  ggplot(aes(x=aqi)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8))

shapiro.test(dataset$aqi) #p-value<0.01

# AQI is not Normal distributed, we reject the null hypothesis of the Shapiro-Wilks test. 

## Logarithm transformation ##

dataset <- dataset %>% mutate(log_aqi=log(dataset$aqi))

shapiro.test(dataset$log_aqi) #p-value>0.05
# We reject the null hypothesis that ln(AQI) is not Normal distributed at 95% of confidence. 

#install.packages("ggpubr")
ggqqplot(dataset$log_aqi, title = "QQ Plot of log AQI")

# As almost all the points fall approximately along the reference line, we can assume Normality.


## log(AQI) density plot ##

d2 <- dataset %>%
  ggplot(aes(x=log_aqi)) +
  geom_density(fill="orangered3", color=FALSE, alpha=0.5)

ggarrange(d1, d2, 
          ncol = 2, nrow = 1)


# 2.	Correlation matrix between the variables of our dataset 

colSums(is.na(dataset)) #We have NAs in mining (2) and precipitations (3)


## Scatter plot matrix ## 

## Correlation matrix ##

res<-cor(dataset[,-1]) 
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



######

bikes <- readxl::read_xls("PlacesForBikes.xls")


######

pairs(dataset[2:19], 
      main = "bla bla",
      pch = 21, 
      bg = c("#1b9e77", "#d95f02", "#7570b3")[unclass(dataset$state)])

#install.packages('GGally')
library(GGally)

ggpairs(dataset, columns=c(2,9,10,13,15,16,17,18,19), aes(color=state)) 

ggplot(dataset, aes(x=mining, y=aqi)) + geom_boxplot() + geom_text(aes(label = state))




## Best Subset selection

install.packages('leaps')
library(leaps)
regfit.full=regsubsets(aqi~.-state, dataset)
reg.summary=summary(regfit.full)

reg.summary$rsq #R2
reg.summary$adjr2 #R2 aDJ

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab = '# variables', ylab = 'RSS', type="l")
plot(reg.summary$adjr2,xlab = '# variables', ylab = 'Adj R squared', type="l")
plot(reg.summary$bic,xlab = '# variables', ylab = 'BIC', type="l")
plot(reg.summary$Cp,xlab = '# variables', ylab = 'Cp', type="l")



par(mfrow=c(1,1))
?plot.regsubsets
