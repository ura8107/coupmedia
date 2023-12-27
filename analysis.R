library(tidyverse)
library(dplyr)
library(ggplot2)
library(here)
library(rio)
library(stargazer)
library(bife)
library(survival)
# Load data
msf <- import(here("data", "MSFS-estimates_full-3x2000.csv"))
msf$MSF <- msf$MSF * 100
couplist <- import(here("data", "Master_Coup List_all regimes_basic.dta"))
powell2012 <- import(here("data", "determinants_replication.dta"))
powell2012 <- powell2012 %>% filter(ccode %in% unique(couplist$ccode))

# merge data
df_coup <- merge(msf, couplist, all = TRUE)
df_coup$coup[is.na(df_coup$coup)] <- 0
df_coup <- df_coup %>% filter(ccode %in% unique(couplist$ccode))
df_coup <- merge(df_coup, powell2012, by = c("ccode", "year"), all = TRUE)
df_coup <- df_coup %>% filter(year >= 1990)

# regression
reg1 <- glm(coup ~ MSF, data = df_coup, family = binomial(link = "logit"))
reg1 <- bife(coup ~ MSF | ccode, data = df_coup, model = "logit")
reg1 <- clogit(coup ~ MSF + soquall + lmilper + lgdppcl + dem + auth + powthy_peace + instab + strata(ccode), data = df_coup)
summary(reg1)
stargazer(reg1, out = here("figure", "reg1.html"))
