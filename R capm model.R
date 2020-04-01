# First we load the package ¡§haven¡¨ which enable R to read SAS data
library(haven)

# read the data and subtract those columns we need
data<-read_sas("firm_stock.sas7bdat")
files<-read_sas("market_data.sas7bdat")

# view the data, run basic EDA
str(data)
head(data)
summary(data)

str(file)
head(file)
summary(file)

# extract market and risk-free data from file
# randomly choose three company to run capm model, in this case UBS,AXP,GE
MKTRF<-files["MKTRF"]
RF<-files["RF"]
MKTRF<-unlist(MKTRF)
RF<-unlist(RF)

UBS<-data[c(5641:5760),3]
AXP<-data[c(601:720),3]
GE<-data[c(2041:2160),3]

# the data of UBS in Nov. 2014 is missing, we found the actual data (3.509%) and fill in the missing value
UBS<-replace(UBS,83,0.03509)

# run EDA, view the basic statistics of each variable
summary(UBS)
summary(AXP)
summary(GE)
summary(MKTRF)
summary(RF)

# run CAPM regression using the lm() funcntion in R
y1<-UBS-RF
y2<-AXP-RF
y3<-GE-RF
x<-MKTRF-RF
capm_ubs<-lm(y1~x)
capm_axp<-lm(y2~x)
capm_ge<-lm(y3~x)

# view the capm models, the intercepts are the part that can't be explained by capm model
summary(capm_ubs)
summary(capm_axp)
summary(capm_ge)


# predicted premium of each firms under 5/-5% market premium
# UBS
c(-0.011287+1.567623*0.05, -0.011287+(1.567623*-0.05))
# AXP
c(-0.0005387+1.6325751*0.05, -0.0005387+(1.6325751*-0.05))
# GE
c(-0.010524+1.414527*0.05, -0.010524+(1.414527*-0.05))

# Residual standard error of three CAPM model above:
# UBS: 0.08796 AXP: 0.08553 GE: 0.05767

# Critical t value under the condition of two-tail, alpha=5%, degree of freedom about
# 120: 1.98 (can¡¦t find a table that contain the t value of 118 degree of
# freedom, so we just make do with the value under degree of freedom=120)

# 95% confidence interval: predicted premium value +/- critical t value*residual
# standard error (forecast error)

# UBS; 5% market premium
c(0.06709415+1.98*0.08796, 0.06709415-1.98*0.08796)
# UBS; -5% market premium
c(-0.08966815+1.98*0.08796, -0.08966815-1.98*0.08796)

# AXP; 5% market premium
c(0.08109005+1.98*0.08553, 0.08109005-1.98*0.08553)
# AXP; -5% market premium
c(-0.08216746+1.98*0.08553, -0.08216746-1.98*0.08553)

# GE; 5% market premium
c(0.06020235+1.98*0.05767, 0.06020235-1.98*0.05767)
# GE; -5% market premium
c(-0.08125035+1.98*0.05767, -0.08125035-1.98*0.05767)