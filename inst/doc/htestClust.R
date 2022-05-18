## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(htestClust)

## ----eval=FALSE---------------------------------------------------------------
#  ## syntax for *stats* function
#  	prop.test(x, n, p = NULL, alternative = c("two.sided", "less", "greater"),
#  	conf.level = 0.95, correct = TRUE)
#  	
#  ## syntax for *htestClust* function
#  	proptestClust(x, id, p = NULL, alternative = c("two.sided", "less",
#  	"greater"), variance = c("sand.null", "sand.est", "emp", "MoM"),
#  	conf.level = 0.95)

## -----------------------------------------------------------------------------
library(htestClust)
data(screen8)
head(screen8)	
	
(tab <- table(screen8$sch.id)) 
summary(as.vector(tab))

## ---- fig.width = 7, fig.height = 4-------------------------------------------
### Figure 1
par(mfrow = c(1,2))
icsPlot(x = screen8$math, id = screen8$sch.id, FUN = "mean", pch = 20)
icsPlot(x = screen8$read, id = screen8$sch.id, FUN = "mean", pch = 20)

## ---- fig.width = 7, fig.height = 4-------------------------------------------
### Figure 2
par(mfrow = c(1,2))
icsPlot(x = screen8$gender, id = screen8$sch.id, FUN = "prop", ylab = "P(Female)", pch = 20)
icsPlot(x = screen8$activity, id = screen8$sch.id, FUN = "prop")

## ----eval=FALSE---------------------------------------------------------------
#  ## example code to perform test for ICS (not run due to computational time)
#  set.seed(100)
#  ics.math <- icstestClust(screen8$math, screen8$sch.id, B = 1000, print.it = FALSE)
#  	
#  ics.math
#  Test of informative cluster size (TF)
#  data:  screen8$math
#  TF = 0.029686, p-value < 2.2e-16

## -----------------------------------------------------------------------------
screen8$math.p <- 1*(screen8$math >= 65)
proptestClust(screen8$math.p, screen8$sch.id, p = .75, alternative = "great")

## -----------------------------------------------------------------------------
tab <- table(screen8$gender, screen8$activity, screen8$sch.id) 
ptab <- prop.table(tab, c(1,3))
apply(ptab, c(1,2), mean)

## -----------------------------------------------------------------------------
chisqtestClust(screen8$gender, screen8$activity, screen8$sch.id)

## -----------------------------------------------------------------------------
prop.table(table(screen8$gender, screen8$activity), 1)

## -----------------------------------------------------------------------------
ttestClust(math ~ gender, id = sch.id, data = screen8)

## ----eval=FALSE---------------------------------------------------------------
#  ## code to run group-weighted Wilcoxon test analogue (not run due to computational time)
#  wilcoxtestClust(math ~ gender, id = sch.id, data = screen8, method = "group")

## -----------------------------------------------------------------------------
onewaytestClust(read ~ activity, id = sch.id, data = screen8)

