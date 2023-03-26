library(tidyverse)
library(caret)
library(pROC)
library(dplyr)
library(XLConnect)


# Load data
data <- read.csv(file.choose(), header=T)

# drop ethnicity=4 and combine highest_edu=1 to 2
data[data$ethnicity==4,]$ethnicity <- 1
# data[data$highest_edu==1,]$highest_edu <- 2

# convert certain variables to factor
data$ethnicity <- as.factor(data$ethnicity)
# data$highest_edu<- as.factor(data$highest_edu)
data$household_income <- as.factor(data$household_income)
data$pc_vig <- as.factor(data$pc_vig)
data$pc_mvpa <- factor(data$pc_mvpa)
data$pg_vig <- as.factor(data$pg_vig)
data$pg_mvpa <- as.factor(data$pg_mvpa)
data$delivery_mode <- data$delivery_mode-1

data_std <- data %>% mutate_at(c("delivery_age", 'ga', 'stai_state_pw26', 'stai_trait_pw26', 'stai_state_m3',
                     'stai_trait_m3', 'epds_tot_pw26', 'epds_tot_m3', 'parity', 'weight_birth',
                     'bisq_q3_m6', 'bisq_totslphrs_m6', 'bisq_q3_m12', 'bisq_totslphrs_m12'), ~(scale(.) %>% as.vector))

str(data)
summary(data)

data$stai_trait_pw26_cat <- cut(data$stai_trait_pw26,
                       breaks=c(20, 31, 36, 41, 68),
                       labels=c('A', 'B', 'C', 'D'))

# fit logistic regression model using selected variables
# eczema edu 12
col <- c("bf_m6", "alcohol_pc", "epds_tot_pw26", "stai_trait_m3", "relevel(pc_mvpa, ref = '0')", "maternal_allergy_pw11", "parity", "infection_m3", "stai_trait_pw26")
# rhinitis edu 12
col <- c("bf_m6", "bisq_q3_m12", "delivery_age", "infection_m3", "relevel(pg_mvpa, ref = '0')", "alcohol_pc", "parity", "epds_tot_pw26")
# wheeze edu 12
col <- c("highest_edu12", "stai_state_pw26", "infection_m3", "ga", "stai_trait_m3", "bf_m6", "maternal_allergy_pw11", "epds_tot_pw26")

formula <- as.formula(paste0("wheeze_label ~ ", paste0(col, collapse = " + ")))

model <- glm(formula, data = data, family = 'binomial')
summary(model)
res <- as.data.frame(coef(summary(model)))
res
res$Estimate

write.csv(res,"/Users/dongyizhi/RA/Current/Exposome/code/r_regression/result/wheeze.csv", row.names = T)
ci <- as.data.frame(confint(model))
ci
write.csv(ci,"/Users/dongyizhi/RA/Current/Exposome/code/r_regression/result/wheeze_ci.csv", row.names = T)


## evaluation of model
# likelihood ratio test
library(lmtest)
md0 <- glm(eczema_label ~ 1, data=data, family = 'binomial')
summary(md0)
lrtest(md0, model)

# Hosmer-Lemeshow test
# library(MKmisc)
# HLgof.test(fit = fitted(model), obs = data$eczema_label)
library(ResourceSelection)
hoslem.test(data$eczema_label, fitted(model), g=10)

# Wald test
library(survey)
regTermTest(model, "ga")
