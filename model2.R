# Model
library(data.table)
library(caret)

# Partition training vs validation (70:30, time series)
attrs <- c("PURCHASE_FLG", "VISIT", "G_VISIT", "G_PURCH", "Cl_VISIT", "Cl_PURCH","PRICE_RATE", "DISCOUNT_PRICE", "CATALOG_PRICE", "C_PURCH", "Same_Pref")
set.seed(1234)
rand <- runif(nrow(w_table), 0, 1)
visit.train <- w_table[rand < 0.7, names(w_table) %in% attrs]
visit.val <- w_table[rand >= 0.7, names(w_table) %in% attrs]
rm(rand)

visit.train$G_PURCH[visit.train$G_PURCH > 0] <- 1
visit.train$Cl_PURCH[visit.train$Cl_PURCH > 0] <- 1
visit.train$G_PURCH <- as.factor(visit.train$G_PURCH)
visit.train$Cl_PURCH <- as.factor(visit.train$Cl_PURCH)
visit.val$G_PURCH[visit.val$G_PURCH > 0] <- 1
visit.val$Cl_PURCH[visit.val$Cl_PURCH > 0] <- 1
visit.val$G_PURCH <- as.factor(visit.val$G_PURCH)
visit.val$Cl_PURCH <- as.factor(visit.val$Cl_PURCH)

# Logistic Regression
lm_model <- glm(PURCHASE_FLG ~ G_PURCH + Cl_PURCH + PRICE_RATE + DISCOUNT_PRICE + C_PURCH + Same_Pref, 
               data = visit.train,
               family = binomial(link = "logit"))
summary(lm_model)

