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

plot.flag = FALSE

raw_tables_dir = "raw.tables"

all.vitals.df       = read.csv(file = paste(raw_tables_dir, "/", "all_vitals.tsv", sep = ""),                  
                               header = TRUE, stringsAsFactors = FALSE, sep = "\t")
all.diagnoses.df    = read.csv(file = paste(raw_tables_dir, "/", "all_diagnoses.tsv", sep = ""),               
                               header = TRUE, stringsAsFactors = FALSE, sep = "\t")
demographics.df     = read.csv(file = paste(raw_tables_dir, "/", "all_demographics.tsv", sep = ""),            
                               header = TRUE, stringsAsFactors = FALSE, sep = "\t")
LOS.outcomes.df     = read.csv(file = paste(raw_tables_dir, "/", "April_2021_encounters_film.tsv", sep = ""), 
                                header = TRUE, stringsAsFactors = FALSE, sep = "\t")

## Coerce data types
all.diagnoses.df$DATE         = as.Date(all.diagnoses.df$DATE)

# Remove entries w/ NA dates (only very few exist)
all.vitals.df = all.vitals.df[-which(all.vitals.df$RECORDED_TIME == ""), ]
all.vitals.df$RECORDED_TIME   = as.POSIXlt(all.vitals.df$RECORDED_TIME, tryFormats = "%Y-%m-%d %H:%M:%OS")

## Prepare vitals and outcomes

all.diagnoses.df$CURRENT_ICD10_LIST = trimws(all.diagnoses.df$CURRENT_ICD10_LIST, which = "both")
ICD10.unique = sort(unique(all.diagnoses.df$CURRENT_ICD10_LIST))
ICD10.unique.names = rep("", length(ICD10.unique))

for (i in 1:length(ICD10.unique)) {
  ICD10.unique.names[i] = all.diagnoses.df$DIAGNOSIS.NAME[which(all.diagnoses.df$CURRENT_ICD10_LIST == ICD10.unique[i])[1]]
}

history.ICD    = 365.0 # 356 days = 1 year
history.vitals = 14.0  # 14 days = 2 weeks
future  = 1.0          # 1 day

prepare.table = function(ind.array) {
  VITALS_DATE = all.vitals.df$RECORDED_TIME
  ICD_DATE    = all.diagnoses.df$DATE
  
  varnames    = c("BMI", "BP_DIASTOLIC", "BP_SYSTOLIC", "PULSE", "PULSE.OXIMETRY", "RESPIRATIONS", "TEMPERATURE")
  varnames.x5 = as.vector(sapply(varnames, function(varname) sapply(1:5, function(ind) paste(varname, ind, sep = ""))))
  
  res = data.frame(matrix(0.0, nrow = length(ind.array), ncol = 3L + 4L + 3L + 5L*length(varnames) + length(ICD10.unique)))
  colnames(res) = c("hashed_mrn", "hashed_accession", "HOSP_ADMSN_TIME",
                    "LOS_30D", "LOS_60D", "DECEASED_30D", "DECEASED_60D",
                    "SEX", "ETHNICITY", "AGE", varnames.x5, ICD10.unique)

  res$HOSP_ADMSN_TIME = as.POSIXlt(LOS.outcomes.df$HOSP_ADMSN_TIME[ind.array], tryFormats = "%Y-%m-%d %H:%M:%OS")
  
  for (i in 1:length(ind.array)) {
    cat("Status: ", i*100/length(ind.array), "% ...\n", sep = "")
    
    obs.ind = ind.array[i]
    
    hashed_mrn       = LOS.outcomes.df$hashed_mrn[obs.ind]
    hashed_accession = LOS.outcomes.df$hashed_accession[obs.ind]
    
    res$hashed_mrn[i]       = LOS.outcomes.df$hashed_mrn[obs.ind]
    res$hashed_accession[i] = LOS.outcomes.df$hashed_accession[obs.ind]
    
    # Response
    res$LOS_30D[i] = LOS.outcomes.df$sum_30_day_los[obs.ind]
    res$LOS_60D[i] = LOS.outcomes.df$sum_60_day_los[obs.ind]
    
    res$DECEASED_30D[i] = LOS.outcomes.df$died_within_30_days[obs.ind]
    res$DECEASED_60D[i] = LOS.outcomes.df$died_within_60_days[obs.ind]
    
    # Demographics
    I = which(demographics.df$hashed_mrn == hashed_mrn)
    
    stopifnot(length(I) <= 1)
    
    if (length(I) == 0) {
      res[i, "SEX"]       = NA
      res[i, "ETHNICITY"] = "UNKNOWN"
      res[i, "AGE"]       = NA
    } else {
      res[i, "SEX"]       = demographics.df[I, "SEX"]
      res[i, "ETHNICITY"] = demographics.df[I, "ETHNICITY"]
      res[i, "AGE"]       = demographics.df[I, "AGE"]
      
      if (res[i, "ETHNICITY"] == "UNKNOWN") {
        res[i, "ETHNICITY"] = NA
      }
    }
    
    # Vitals
    I = which(all.vitals.df$hashed_mrn == hashed_mrn)
    Date = res$HOSP_ADMSN_TIME[i]
    
    I.Dates = I[which((VITALS_DATE[I] <= Date + 24*60*60*future) & (VITALS_DATE[I] >= Date - 24*60*60*history.vitals))]
    
    if (length(I.Dates) > 0L) {
      res[i, varnames.x5] = as.vector(sapply(1:length(varnames), function(varind) fivenum(all.vitals.df[I.Dates, varnames[varind]], na.rm = TRUE)))
    } else {
      res[i, varnames.x5] = NA
    }
    
    # ICD
    I = which(all.diagnoses.df$hashed_mrn == hashed_mrn)
    
    I.Dates = I[which((ICD_DATE[I] <= as.Date(Date) + future) & (ICD_DATE[I] >= as.Date(Date) - history.ICD))]
    res[i, intersect(ICD10.unique, unique(all.diagnoses.df$CURRENT_ICD10_LIST[I.Dates]))] = TRUE
  }
  
  res$DECEASED_30D = ifelse(res$DECEASED_30D == "True", TRUE, FALSE)
  res$DECEASED_60D = ifelse(res$DECEASED_60D == "True", TRUE, FALSE)
  
  for (ICD in ICD10.unique) {
    res[, ICD] = factor(res[, ICD], levels = c(0, 1), labels = c("FALSE", "TRUE"))
  }
  
  if (plot.flag) {
    IDs = unique(res$hashed_mrn)
    I = which(is.element(all.diagnoses.df$hashed_mrn, IDs))
    
    plot(density(as.numeric(ICD_DATE[I])), col = "black", ylim = c(0, 1E-2), xlab = "Date as numeric", main = "ICD 10 dates")
    lines(density(as.numeric(as.Date(res$HOSP_ADMSN_TIME))), col = "red")
    legend(x = "topleft", legend = c("Dates ICD 10 recorded", "Admission dates"), lty = c(1, 1), col = c("black", "red"))
    
    IDs = unique(res$hashed_mrn)
    I = which(is.element(all.vitals.df$hashed_mrn, IDs))
    
    plot(density(as.numeric(as.Date(VITALS_DATE[I]))), col = "black", xlab = "Date as numeric", main = "Vitals dates")
    lines(density(as.numeric(as.Date(res$HOSP_ADMSN_TIME))), col = "red")
    legend(x = "topright", legend = c("Dates vitals recorded", "Admission dates"), lty = c(1, 1), col = c("black", "red"))
  }
  
  return(res)
}

# LOS dataframe
LOS.df = prepare.table(1:nrow(LOS.outcomes.df))
write.csv(LOS.df, file = "preproc.tables/LOS.df.csv", row.names = FALSE)