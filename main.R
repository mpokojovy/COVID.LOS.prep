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


## !!!
# Set working directory
path = NULL # Replace with actual directory path!
setwd(path)
## !!!

githib.path = "https://raw.githubusercontent.com/pmccaffrey6/COVID-LOS/main/"
github.raw.files = c("April_2021_encounters_film.tsv",
                     "all_demographics.tsv",
                     "all_diagnoses.tsv",
                     "all_vitals.tsv",
                     "cxr_covid_films.tsv",
                     "study_patient_outcome.tsv",
                     "study_vitals_at_exam.tsv")

if (!dir.exists("raw.tables")) {
  dir.create("raw.tables")
}

if (!dir.exists("preproc.tables")) {
  dir.create("preproc.tables")
}

if (!dir.exists("imputed.tables")) {
  dir.create("imputed.tables")
}

for (file in github.raw.files) {
  url = paste(githib.path, file, sep = "")
  destfile = paste("raw.tables/", file, sep = "")
  download.file(url, destfile)
}

source("auxil.R")

cat("Preprocessing...\n")
source("preprocess.R")

cat("\nImputing...\n")
source("impute.R")

cat("\nAnalyzing LOS...\n")
source("stat.analysis.R")