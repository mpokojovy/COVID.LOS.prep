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

LOS.df = read.csv(file = "preproc.tables/LOS.df.csv", stringsAsFactors = FALSE)

## Coerce factors

LOS.df$SEX       = factor(LOS.df$SEX,       levels = c("MALE", "FEMALE"), labels = c("MALE", "FEMALE"))
LOS.df$ETHNICITY = factor(LOS.df$ETHNICITY, levels = c("NOT HISPANIC OR LATINO", "HISPANIC OR LATINO"), labels = c("NOT HISPANIC OR LATINO", "HISPANIC OR LATINO"))

varnames    = c("BMI", "BP_DIASTOLIC", "BP_SYSTOLIC", "PULSE", "PULSE.OXIMETRY", "RESPIRATIONS", "TEMPERATURE")
varnames.x5 = as.vector(sapply(varnames, function(varname) sapply(1:5, function(ind) paste(varname, ind, sep = ""))))

outcome.var.names = c("LOS_30D", "LOS_60D", "DECEASED_30D", "DECEASED_60D")
basic.var.names   = c("SEX", "ETHNICITY", "AGE", varnames.x5)
ICD10.var.names   = setdiff(colnames(LOS.df), c("hashed_mrn", "hashed_accession",	"HOSP_ADMSN_TIME", outcome.var.names, basic.var.names))

for (ICD in ICD10.var.names) {
  LOS.df[, ICD] = factor(LOS.df[, ICD], levels = c(FALSE, TRUE), labels = c("FALSE", "TRUE"))
}

## Split into training & test datasets
train2test.ratio = 0.8

set.seed(1)

n = nrow(LOS.df)

n.train = ceiling(n*train2test.ratio)
n.test  = n - n.train

I.train = sample(1:n, n.train, replace = FALSE)
I.test  = sample(setdiff(1:n, I.train), replace = FALSE)

LOS.train.df = LOS.df[I.train, ]
LOS.test.df  = LOS.df[I.test, ]

write.csv(LOS.train.df, file = "preproc.tables/LOS.train.df.csv", row.names = FALSE)
write.csv(LOS.test.df,  file = "preproc.tables/LOS.test.df.csv",  row.names = FALSE)

## Detect top ICD10 codes

prev = colSums(ifelse(LOS.train.df[, ICD10.var.names] == "TRUE", 1.0, 0.0))/nrow(LOS.train.df)
top  = 20L
top.ICD10.var.names = ICD10.var.names[order(abs(prev - 0.5))[1:top]]

cat("Top ICD 10 codes: \n")
print(top.ICD10.var.names)

## Impute train

## Vars to impute
imp.var.names = c(outcome.var.names, basic.var.names, top.ICD10.var.names)

set.seed(2)

for (method in c("cart", "rf")) {
  imp.train = mice::mice(LOS.train.df[, imp.var.names], m = 5L, method = method, maxit = 5L,
                         remove.collinear = FALSE, remove.constant = FALSE)

  LOS.train.imp.df = LOS.train.df
  LOS.train.imp.df[, imp.var.names] = mice::complete(imp.train)

  write.csv(LOS.train.imp.df, file = paste("imputed.tables/LOS.train.imp.", method, ".df.csv", sep = ""),
            row.names = FALSE)
}

## Impute test

## Vars to impute
imp.var.names = c(basic.var.names, top.ICD10.var.names)

set.seed(3)

for (method in c("cart", "rf")) {
  LOS.train.imp.df = read.csv(file = paste("imputed.tables/LOS.train.imp.", method, ".df.csv", sep = ""), 
                                          header = TRUE, stringsAsFactors = FALSE, sep = ",")
  
  LOS.test.imp.df = rbind(LOS.train.imp.df, LOS.test.df)
  
  LOS.test.imp.df$SEX       = factor(LOS.test.imp.df$SEX,       levels = c("MALE", "FEMALE"), labels = c("MALE", "FEMALE"))
  LOS.test.imp.df$ETHNICITY = factor(LOS.test.imp.df$ETHNICITY, levels = c("NOT HISPANIC OR LATINO", "HISPANIC OR LATINO"), labels = c("NOT HISPANIC OR LATINO", "HISPANIC OR LATINO"))
  
  for (ICD in ICD10.var.names) {
    LOS.test.imp.df[, ICD] = factor(LOS.test.imp.df[, ICD], levels = c(FALSE, TRUE), labels = c("FALSE", "TRUE"))
  }
  
  imp.test = mice::mice(LOS.test.imp.df[, imp.var.names], m = 5L, method = method, maxit = 5L,
                        remove.collinear = FALSE, remove.constant = FALSE)
    
  LOS.test.imp.df[, imp.var.names] = mice::complete(imp.test)[, imp.var.names]
  LOS.test.imp.df = LOS.test.imp.df[(n.train + 1):n, ]
  
  write.csv(LOS.test.imp.df, file = paste("imputed.tables/LOS.test.imp.", method, ".df.csv", sep = ""),
            row.names = FALSE)
}