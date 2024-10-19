To create `LOS.train.imp.rf.df.csv` and `LOS.test.imp.rf.df.csv`,

1. Clone https://github.com/mpokojovy/COVID.LOS.prep .

2. Download R 4.4.1 for Windows via https://lib.stat.cmu.edu/R/CRAN/ .

3. Install R.

4. Add Rscript to system PATH.

5. Open Git Bash as administrator.

6. Navigate to root of `COVID.LOS.prep`.

7. Install package `mice` via `R -e "install.packages('mice', repos='http://cran.rstudio.com/')"`.

8. Install package `ranger` via `R -e "install.packages('ranger', repos='http://cran.rstudio.com/')"`.

9. Install package `randomForestSRC` via `R -e "install.packages('randomForestSRC', repos='http://cran.rstudio.com/')
"`.

10. Install package `ggRandomForests` via `R -e "install.packages('ggRandomForests', repos='http://cran.rstudio.com/')".

11. Install package `quantreg` via `R -e "install.packages('quantreg', repos='http://cran.rstudio.com/')".

12. Run `Rscript main.R`.
