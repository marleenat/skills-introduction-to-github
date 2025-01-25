# Wooldridge chapter 8, empirical exercise C8-2.i

# LOAD PACKAGES

library(stargazer)
library(sandwich)
library(lmtest)

# SET WORKING DIRECTORY 
  
setwd("E:/Dropbox/4750/empiricalexamples_R")

# WRITE OUTPUT TO A LOG FILE

sink(file="jw.c8-2.out",append=FALSE,split=TRUE)   

# IMPORT DATASET FROM .CSV FILE

hprice1 <- read.csv("hprice1.csv")

# (C8-2.i) COMPUTE ROBUST SE'S FOR HOUSE PRICE REGRESSION
# AND COMPARE WITH CONVENTIONAL STANDARD ERRORS

pricereg_1ev <- lm( price ~  lotsize +  sqrft + bdrms, data=hprice1)
summary(pricereg_1ev)
coeftest(pricereg_1ev, vcov = vcovHC(pricereg_1ev, type="HC1"))

# WRITE THE ROBUST COVARIANCE MATRIX TO AN OBJECT
robustcov_lev <- vcovHC(pricereg_1ev, type = "HC1")
robustse_lev <- sqrt(diag(robustcov_lev))

# REPORTS RESULTS WITHOUT AND WITH ROBUST SE'S

stargazer(pricereg_1ev, pricereg_1ev, 
          type="text", out="jw.c8-2.i.txt",
          se = list(NULL,robustse_lev),
          omit.stat = "f")

# CLOSE THE LOG FILE

sink()
