To create `LOS.train.imp.rf.df.csv` and `LOS.test.imp.rf.df.csv`,

Clone https://github.com/mpokojovy/COVID.LOS.prep .
Download R 4.4.1 for Windows via https://lib.stat.cmu.edu/R/CRAN/ .
Install R.
Add Rscript to system PATH.
Open Git Bash as administrator.
Navigate to root of `COVID.LOS.prep`.
Install package `mice` via `R -e "install.packages('mice', repos='http://cran.rstudio.com/')"`.
Install package `ranger` via `R -e "install.packages('ranger', repos='http://cran.rstudio.com/')"`.
Install package `randomForestSRC` via `R -e "install.packages('randomForestSRC', repos='http://cran.rstudio.com/')
"`.
Install package `ggRandomForests` via `R -e "install.packages('ggRandomForests', repos='http://cran.rstudio.com/')".
Install package `quantreg` via `R -e "install.packages('quantreg', repos='http://cran.rstudio.com/')".
Run `Rscript main.R`.
