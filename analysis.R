library(tidyverse)
library(dplyr)
library(ggplot2)
library(here)
library(rio)
library(stargazer)
library(bife)
library(survival)
library(fixest)

# Load data
msf <- import(here("data", "MSFS-estimates_full-3x2000.csv"))
gmfd <- import(here("data", "GMFD_V2.csv"))
media <- merge(msf, gmfd, by = c("ccode", "year")) %>% arrange(ccode, year)
couplist <- import(here("data", "Master_Coup List_all regimes_basic.dta"))
powell2012 <- import(here("data", "determinants_replication.dta"))
powell2012 <- powell2012 %>% filter(ccode %in% unique(couplist$ccode))

# merge data
df_coup <- merge(media, couplist, all = TRUE)
df_coup$coup[is.na(df_coup$coup)] <- 0 # replace NA with 0
df_coup <- df_coup %>% filter(ccode %in% unique(couplist$ccode)) #only countries in couplist
df_coup <- merge(df_coup, powell2012, by = c("ccode", "year")) # for control variables
df_coup$postcw <- ifelse(df_coup$year > 1991, 1, 0) # cold war dummy
df_coup <- df_coup %>% arrange(ccode, year) # sort by country and year

# regression
reg1 <- clogit(coup ~ MSF + postcw + MSF*postcw + soquall + lmilper + lgdppcl + dem + auth + powthy_peace + instab + strata(ccode), data = df_coup)
summary(reg1)
stargazer(reg1, type = "text")

reg2 <- feglm(coup ~ MSF + MSF*postcw + soquall + lmilper + lgdppcl + dem + auth + powthy_peace + I(powthy_peace^2) + I(powthy_peace^3) + instab | ccode + year, family = binomial(link = "logit"), data = df_coup)
summary(reg2, se = "twoway")
etable(reg2, se = "twoway", tex = T)

reg3 <- glm(coup ~ MSF + postcw + MSF*postcw + soquall + lmilper + chgdp + lgdppcl + dem + auth + powthy_peace + instab + factor(ccode) + factor(year), family = binomial(link = "logit"), data = df_coup)
summary(reg3)
stargazer(reg3, type = "text")
