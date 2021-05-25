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

library(RCurl)
x <- getURL("https://raw.githubusercontent.com/mathicard/Statistical-Learning-DSE/main/usa_final.csv")
dataset <- read.csv(text = x)

str(dataset)
summary(dataset)

colSums(is.na(dataset)) #We have no NAs

#AQI Categorization
dataset$polluted <- cut(dataset$aqi, breaks = c(50,100,150,200),
               labels = c('yellow', 'orange', 'red'))


# 1. Describe the variable AQI

## Descriptive statistics ##

# AQI

dataset %>% summarise(
  count = n(), 
  mean = mean(aqi, na.rm = TRUE),
  median = median(aqi),
  min = min(aqi),
  max = max(aqi),
  sd = sd(aqi, na.rm = TRUE)
)


## By group ##

group_by(dataset, polluted) %>% 
  summarise(
    count = n(),
    share = n()/51*100,
    mean = mean(aqi, na.rm = TRUE),
    median = median(aqi),
    min = min(aqi),
    max = max(aqi),
    sd = sd(aqi, na.rm = TRUE)
  )

#Box plot

ggboxplot(dataset, x = "polluted", y = "aqi", color = "cl",
          palette = c("#E7B800", "#FC4E07", "red"), 
          add = "jitter", legend = "none") +
  geom_hline(yintercept = mean(dataset$aqi), linetype = 2)+ # Add horizontal line at base mean
  ylim(0, 200)


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


## Summary ##

str(dataset)
summary(dataset)

## Boxplot ##

library(car)
par(mfrow=c(1,1))
Boxplot(~aqi, data=dataset, col="#69b3a2", id=list(labels=dataset$state))

# 2.	Correlation matrix between the variables of our dataset 

## Scatter plot matrix ## 

## Correlation matrix ##
data_corr <- dataset[,-c(1,20:22)]

res <-cor(data_corr) 
round(res, 2)

#install.packages('Hmisc')
rcorr(as.matrix(data_corr)) # Seems that correlation between lockdown and mining with AQI is not significative.

## Correlation heatmap ##

col<- colorRampPalette(c("darkgreen", "white", "darkred"))(20)
heatmap(x = res, col = col, symm = TRUE, Colv = NA, Rowv = NA)

## Correlation chart ##

#library("PerformanceAnalytics")
#chart.Correlation(data_corr, histogram=TRUE, pch=19)

library(corrplot)

data_corr <- dataset[,-c(1,20:23)]
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
M <- cor(data_corr)

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# matrix of the p-value of the correlation
p.mat <- cor.mtest(data_corr)
head(p.mat[, 1:5])
colnames(M) <- c("aqi", "accom", "const", "edu", "fin", "health", "info", 
                 "manuf", "mining", "prof", "retail", "trans", "util", "waste",
                 "precip", "lock", "p_rural", "n_fact")

corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=45, 
         p.mat = p.mat, sig.level = 0.05, 
         number.cex = .6,
         diag=FALSE)



#Boxpot of AQI
library(car)
Boxplot(~aqi, data=data, col="#69b3a2", id=list(labels=data$state))

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






