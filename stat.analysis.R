####################################################################################################
# Copyright (C) 2022 Michael Pokojovy                                                              #
#                                                                                                  #
# Data preparation for COVID-19 patient hospital length-of-stay (LOS) analysis reported in:        #
#                                                                                                  #
# Y. Wen, M.F. Rahman, Y. Zhuang, M. Pokojovy et al (2022)                                         #
# Time-to-event modeling for hospital length of stay prediction for COVID-19 patients              #
# Machine Learning with Applications. Volume 9, 15 September 2022, 100365                          #
# https://www.sciencedirect.com/science/article/pii/S2666827022000603?via%3Dihub                   #
#                                                                                                  #
####################################################################################################

library("randomForestSRC")
library("ggRandomForests")
library("ggplot2")

LOS.train.df = read.csv(file = "imputed.tables/LOS.train.imp.rf.df.csv", header = TRUE, stringsAsFactors = FALSE, sep = ",")
LOS.test.df  = read.csv(file = "imputed.tables/LOS.test.imp.rf.df.csv",  header = TRUE, stringsAsFactors = FALSE, sep = ",")

# Vitals
vital.names    = c("BMI", "BP_DIASTOLIC", "BP_SYSTOLIC", "PULSE", "PULSE.OXIMETRY", "RESPIRATIONS", "TEMPERATURE")
vital.names.x5 = as.vector(sapply(vital.names, function(vital.name) sapply(1:5, function(ind) paste(vital.name, ind, sep = ""))))

# Top 20 ICD10 codes
ICD10.names = colnames(LOS.train.df)[(max(which(is.element(colnames(LOS.train.df), vital.names.x5))) + 1L):ncol(LOS.train.df)]
freqs = colSums(LOS.train.df[, ICD10.names])
ICD10.top20.names = ICD10.names[order(freqs, decreasing = TRUE)[1:20]]

for (ICD10.name in ICD10.top20.names)
{
  LOS.train.df[, ICD10.name] = factor(LOS.train.df[, ICD10.name])
  LOS.test.df[,  ICD10.name] = factor(LOS.test.df[,  ICD10.name])
}

# All predictors

predictor.names = c("SEX", "ETHNICITY", "AGE", vital.names.x5, ICD10.top20.names)

LOS.train.df[, "SEX"] = factor(LOS.train.df[, "SEX"])
LOS.test.df[, "SEX"]  = factor(LOS.test.df[, "SEX"])
LOS.train.df[, "ETHNICITY"] = factor(LOS.train.df[, "ETHNICITY"])
LOS.test.df[, "ETHNICITY"]  = factor(LOS.test.df[, "ETHNICITY"])

# Survival forest

LOS.train.df$response = LOS.train.df$LOS_30D
LOS.test.df$response  = LOS.test.df$LOS_30D

LOS.train.df$status = 1
LOS.test.df$status = 1

surv.model <- rfsrc(as.formula(paste("Surv(response, status)", "~", paste(predictor.names, collapse = "+"))),
                               LOS.train.df, importance=TRUE)

plot(surv.model)

surv.test = predict(surv.model, newdata = LOS.test.df)
plot(gg_rfsrc(surv.test), alpha = 0.2) + labs(x = "time (days)", y = "probability of no discharge (%)") 

# Random forest

lambda = 0.0606

trafo     = function(x) (x^lambda - 1.0)/lambda
inv.trafo = function(x) (1.0 + lambda*x)^(1/lambda)

LOS.train.df$response = trafo(pmin(LOS.train.df$LOS_30D, 30.0))
LOS.test.df$response  = trafo(pmin(LOS.test.df$LOS_30D, 30.0))

qqnorm(LOS.train.df$response)
beta.hat = lm(sort(LOS.train.df$response) ~ qnorm((0.5 + (1:length(LOS.train.df$response)))/(length(LOS.train.df$response) + 1)))$coefficients
abline(a = beta.hat[1], b = beta.hat[2], col = "red")

rf.model = randomForest::randomForest(as.formula(paste("response", "~", paste(predictor.names, collapse = "+"))), 
                                      data = LOS.train.df, ntree = 500, mtry = 5, importance = TRUE)

randomForest::varImpPlot(rf.model, col = "blue", pch = 2)

LOS.test.pred = predict(rf.model, newdata = LOS.test.df, interval = "prediction", level = 0.9)
plot(inv.trafo(LOS.test.pred), inv.trafo(LOS.test.df$response),
     main = "Nonparametric RF", xlab = "predicted LOS", ylab = "observed LOS")

abline(a = 0, b = 1, col = "red")

lq = quantreg::rq(LOS.test.df$response ~ LOS.test.pred, tau = 0.05)
lines(inv.trafo(sort(LOS.test.pred)), inv.trafo(sort(lq$fitted.values)), col = "blue")

uq = quantreg::rq(LOS.test.df$response ~ LOS.test.pred, tau = 0.95)
lines(inv.trafo(sort(LOS.test.pred)), inv.trafo(sort(uq$fitted.values)), col = "blue")

abline(a = 0, b = 1, col = "red")

# Linear model

LOS.train.df$response = pmin(LOS.train.df$LOS_30D, 30.0)
LOS.test.df$response  = pmin(LOS.test.df$LOS_30D, 30.0)

boxcox.model = MASS::boxcox(as.formula(paste("response", "~", paste(predictor.names, collapse = "+"))), data = LOS.train.df)
lambda = boxcox.model$x[which.max(boxcox.model$y)]

trafo     = function(x) (x^lambda - 1.0)/lambda
inv.trafo = function(x) ifelse(x <= 0, 0, (1.0 + lambda*x)^(1/lambda))

LOS.train.df$response = trafo(LOS.train.df$response)
LOS.test.df$response  = trafo(LOS.test.df$response)

lm.model = lm(as.formula(paste("response", "~", paste(predictor.names, collapse = "+"))), data = LOS.train.df)

LOS.test.pred = predict(lm.model, newdata = LOS.test.df, interval = "prediction", level = 0.9)

plot(inv.trafo(LOS.test.pred[, "fit"]), inv.trafo(LOS.test.df$response), type = "p", 
     main = "Linear Regression", xlab = "predicted LOS", ylab = "observed LOS",
     xlim = c(0, 30), ylim = c(0, 30))
abline(a = 0, b = 1, col = "red")