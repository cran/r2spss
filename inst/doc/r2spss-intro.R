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

## ----eval=FALSE----------------------------------------------------------
#  r2spss.sty(path = ".")

## ------------------------------------------------------------------------
r2spss_options$set(version = "legacy", minor = FALSE)

## ----results="hide", message=FALSE, warning=FALSE------------------------
data("Eredivisie")
data("Exams")

## ------------------------------------------------------------------------
Eredivisie$logMarketValue <- log(Eredivisie$MarketValue)

## ----results="asis"------------------------------------------------------
descriptives(Eredivisie, c("Age", "Minutes", "logMarketValue"))

## ------------------------------------------------------------------------
histogram(Eredivisie, "logMarketValue")

## ------------------------------------------------------------------------
box_plot(Eredivisie, "logMarketValue")

## ------------------------------------------------------------------------
scatter_plot(Eredivisie, c("Age", "logMarketValue"))

## ------------------------------------------------------------------------
scatter_plot(Eredivisie, c("Age", "Minutes", "logMarketValue"))

## ----results="asis"------------------------------------------------------
t_test(Exams, "Resit", mu = 5.5)

## ----results="asis"------------------------------------------------------
t_test(Exams, c("Resit", "Regular"))

## ----results="asis"------------------------------------------------------
wilcoxon_test(Exams, c("Regular", "Resit"))

## ----results="asis"------------------------------------------------------
sign_test(Exams, c("Regular", "Resit"))

## ------------------------------------------------------------------------
box_plot(Exams, c("Regular", "Resit"))

## ----eval=FALSE----------------------------------------------------------
#  t_test(Eredivisie, "logMarketValue", group = "Foreign")

## ----echo=FALSE, results="asis"------------------------------------------
t_test(Eredivisie, "logMarketValue", group = "Foreign")

## ----results="asis"------------------------------------------------------
wilcoxon_test(Eredivisie, "MarketValue", group = "Foreign")

## ------------------------------------------------------------------------
box_plot(Eredivisie, "logMarketValue", group = "Foreign")

## ----results="asis"------------------------------------------------------
oneway <- ANOVA(Eredivisie, "logMarketValue", group = "Position")
oneway

## ------------------------------------------------------------------------
plot(oneway)

## ----results="asis"------------------------------------------------------
kruskal_test(Eredivisie, "MarketValue", group = "Position")

## ----results="asis"------------------------------------------------------
twoway <- ANOVA(Eredivisie, "logMarketValue",
                group = c("Position", "Foreign"))
twoway

## ----fig.width=6, out.width = "0.7\\textwidth"---------------------------
plot(twoway)

## ----results="asis"------------------------------------------------------
chisq_test(Eredivisie, "Position", p = c(1, 4, 3, 3)/11)

## ----results="asis"------------------------------------------------------
chisq_test(Eredivisie, c("Position", "Foreign"))

## ------------------------------------------------------------------------
Eredivisie$AgeSq <- Eredivisie$Age^2

## ----results="asis"------------------------------------------------------
fit <- regression(logMarketValue ~ Age + AgeSq,
                  logMarketValue ~ Age + AgeSq + Contract + Foreign,
                  data = Eredivisie)
fit

## ----results="asis"------------------------------------------------------
print(fit, statistics = "summary", change = TRUE)

## ------------------------------------------------------------------------
plot(fit, which = "histogram")

## ------------------------------------------------------------------------
plot(fit, which = "scatter")

