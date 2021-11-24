## ----include=FALSE-------------------------------------------------------
library("knitr")
options(prompt="R> ", continue = "+  ", width = 75, useFancyQuotes = FALSE)
opts_chunk$set(fig.path = "figures/figure-", fig.width = 4.5, fig.height = 4.5,
               out.width = "0.5\\textwidth", fig.align = "center",
               fig.lp = "fig:", fig.pos = "h!", tidy = FALSE)
render_sweave()             # use Sweave environments
set_header(highlight = "")  # do not use the Sweave.sty package

## ----results="hide", message=FALSE, warning=FALSE------------------------
library("r2spss")

## ----results="hide", message=FALSE, warning=FALSE------------------------
data("Eredivisie")
data("Exams")

## ------------------------------------------------------------------------
Eredivisie$logMarketValue <- log(Eredivisie$MarketValue)

## ----results="asis"------------------------------------------------------
descriptives(Eredivisie, c("Age", "Minutes", "logMarketValue"))

## ------------------------------------------------------------------------
histSPSS(Eredivisie, "logMarketValue")

## ------------------------------------------------------------------------
boxplotSPSS(Eredivisie, "logMarketValue")

## ------------------------------------------------------------------------
plotSPSS(Eredivisie, c("Age", "logMarketValue"))

## ------------------------------------------------------------------------
plotSPSS(Eredivisie, c("Age", "Minutes", "logMarketValue"))

## ----results="asis"------------------------------------------------------
tTest(Exams, "Resit", mu = 5.5)

## ----results="asis"------------------------------------------------------
tTest(Exams, c("Resit", "Regular"))

## ----results="asis"------------------------------------------------------
wilcoxonTest(Exams, c("Regular", "Resit"))

## ----results="asis"------------------------------------------------------
signTest(Exams, c("Regular", "Resit"))

## ------------------------------------------------------------------------
boxplotSPSS(Exams, c("Regular", "Resit"))

## ----eval=FALSE----------------------------------------------------------
#  tTest(Eredivisie, "logMarketValue", group = "Foreign")

## ----echo=FALSE, results="asis"------------------------------------------
tTest(Eredivisie, "logMarketValue", group = "Foreign")

## ----results="asis"------------------------------------------------------
wilcoxonTest(Eredivisie, "MarketValue", group = "Foreign")

## ------------------------------------------------------------------------
boxplotSPSS(Eredivisie, "logMarketValue", group = "Foreign")

## ----results="asis"------------------------------------------------------
oneway <- ANOVA(Eredivisie, "logMarketValue", group = "Position")
oneway

## ------------------------------------------------------------------------
plot(oneway)

## ----results="asis"------------------------------------------------------
kruskalTest(Eredivisie, "MarketValue", group = "Position")

## ----results="asis"------------------------------------------------------
twoway <- ANOVA(Eredivisie, "logMarketValue",
                group = c("Position", "Foreign"))
twoway

## ----fig.width=6, out.width = "0.7\\textwidth"---------------------------
plot(twoway)

## ------------------------------------------------------------------------
Eredivisie$AgeSq <- Eredivisie$Age^2

## ----results="asis"------------------------------------------------------
regression(logMarketValue ~ Age + AgeSq,
           logMarketValue ~ Age + AgeSq + Contract + Foreign,
           data = Eredivisie)

## ----results="asis"------------------------------------------------------
fit <- regression(logMarketValue ~ Age + AgeSq,
                  logMarketValue ~ Age + AgeSq + Contract + Foreign,
                  data = Eredivisie, change = TRUE)
print(fit, statistics = "summary")

## ------------------------------------------------------------------------
plot(fit, which = "histogram")

## ------------------------------------------------------------------------
plot(fit, which = "scatter")

