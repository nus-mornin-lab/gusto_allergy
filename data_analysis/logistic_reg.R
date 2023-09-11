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
                     'epds_tot_pw26', 'epds_tot_m3', 'parity', 'weight_birth',
                     'bisq_q3_m6', 'bisq_totslphrs_m6', 'bisq_q3_m12', 'bisq_totslphrs_m12',
                     'neopterin', 'riboflavin', 
                     'trigonelline', 'pyridoxal', 'pyridoxalphosphate', 'vitamind3',
                     'arginine', 'adma', 'creatine', 'cystathionine', 'm3histidine',
                     'histidine', 'methionine', 'hydroxyanthralinicac', 'kynurenicacid',
                     'kynurenine', 'tryptophan', 'betaine', 'choline', 'dimethylglycine',
                     'trimethylaminenoxide'), ~(scale(.) %>% as.vector))

str(data)
summary(data)

# data$stai_trait_pw26_cat <- cut(data$stai_trait_pw26,
#                        breaks=c(20, 31, 36, 41, 68),
#                        labels=c('A', 'B', 'C', 'D'))

# fit logistic regression model using selected variables
# eczema v3
col <- c('bf_m6', 'epds_tot_pw26', 'neopterin', 'alcohol_pc', 'infection_m3', 'stai_trait_pw26', 'choline', 'ga')
# rhinitis v3
col <- c('bf_m6', 'alcohol_pc', 'maternal_allergy_pw11', 'delivery_age', 'cystathionine', 'infection_m3', 'methionine', "relevel(pg_mvpa, ref = '0')", 'stai_state_m3')
# wheeze v3
col <- c('maternal_allergy_pw11', 'neopterin', 'ga', 'kynurenicacid', 'delivery_age', 'creatine', 'highest_edu12')

formula <- as.formula(paste0("rhinitis_label ~ ", paste0(col, collapse = " + ")))

model <- glm(formula, data = data_std, family = 'binomial')
summary(model)
res <- as.data.frame(coef(summary(model)))
res
res$Estimate

write.csv(res,"../result/wheeze_v3.csv", row.names = T)
ci <- as.data.frame(confint(model))
ci
write.csv(ci,"../result/wheeze_ci_v3.csv", row.names = T)



