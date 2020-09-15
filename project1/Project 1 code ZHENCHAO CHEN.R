
# Reading in data
data <- read.table("/Users/sktt1teddyfakerkhan/OneDrive - Emory University/bios521/project1_data-1.csv", 
                   quote="\"", comment.char="")

# Renaming variables
colnames(data) <- c("caseid", "time", "status", "drug", "age", "sex", "ascites",
                    "hepatomegaly", "spiders", "edema", "bilirubin", "cholesterol",
                    "albumin", "urine_copper", "alk_phosphatase", "sgot", 
                    "triglicerides", "platelets", "prothrombin", "hist_stage")

# Combining censored cases
data$dead <- ifelse(data$status == 0 | data$status == 1, 0, 1)
table(data$dead)

# Creating catergorical factor for bilirubin
data$bili_cat <- ifelse(data$bilirubin < 1.1, "< 1.1", 
                        ifelse(data$bilirubin >= 1.1 & data$bilirubin <= 3.3, "1.1-3.3", ">3.3"))
data$bili_cat <- factor(data$bili_cat, levels = c("< 1.1", "1.1-3.3", ">3.3"))
table( data$bili_cat)

# Creating age in years
data$ageinyear <- data$age/365.25

## Analyses
# Linear regression - excluding censored (univariate+multivariate)
summary(lm(time ~ drug, data = data[data$dead==1,]))
summary(lm(time ~ ageinyear, data = data[data$dead==1,]))
summary(lm(time ~ bili_cat, data = data[data$dead==1,]))
summary(lm(time ~ drug + ageinyear + bili_cat, data = data[data$dead==1,]))

# Linear regression - using censored times as death times
summary(lm(time ~ drug, data = data))
summary(lm(time ~ ageinyear, data = data))
summary(lm(time ~ bili_cat, data = data))
summary(lm(time ~ drug + ageinyear + bili_cat, data = data))

# Logistic regression 
summary(glm(dead ~ drug, family = "binomial", data = data))
summary(glm(dead ~ ageinyear, family = "binomial", data = data))
summary(glm(dead ~ bili_cat, family = "binomial", data = data))
summary(glm(dead ~ drug + ageinyear + bili_cat, family = "binomial", data = data))

# Survival analysis assuming Weibull distribution
library(flexsurv)
flexsurvreg(Surv(time, dead) ~ drug, data = data, dist = "Weibull")
flexsurvreg(Surv(time, dead) ~ ageinyear, data = data, dist = "Weibull")
flexsurvreg(Surv(time, dead) ~ bili_cat, data = data, dist = "Weibull")
flexsurvreg(Surv(time, dead) ~ drug + ageinyear + bili_cat, data = data, dist = "Weibull")