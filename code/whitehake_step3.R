# whitehake_step3.R
# calculate F40% and conduct short term projection at this F

# set working directory to source file location to begin

# if needed
#devtools::install_github("cmlegault/rose")

library(rose)
library(dplyr)

br <- read.csv("..\\saved\\WHbestruns.csv", header = TRUE)
br
nscen <- length(br$scenario)
scennum <- rep(NA, nscen)
for (i in 1:nscen){
  scennum[i] <- ifelse(i < 10, paste0("0", i), as.character(i))
}

nrecentyears <- 5
myrecruityears <- seq(2008, 2017)
mynpy <- 3 # number of projection years
target.spr <- 0.40
Ftarget <- rep(NA, nscen)
Fterm <- rep(NA, nscen)
Fratio <- rep(NA, nscen)
ssbtarget <- rep(NA, nscen)
ssbterm <- rep(NA, nscen)
ssbratio <- rep(NA, nscen)
projcatch <- matrix(NA, nrow = nscen, ncol = mynpy)

myname <- "whitehake.dat"
asap.name <- substr(myname, 1, nchar(myname)-4)
asap <- dget(paste0("..\\saved\\", asap.name, "_000.rdat"))
orig <- rose::get_asap_recent_vals(asap, nrecentyears)
orig.rho.table <- read.csv(paste0("..\\saved\\Retro.rho.values_", asap.name, "_000.csv"), header = TRUE)

# compute F to achieve target spawning potential ratio and project catch
ssbpr0 <- rose::calc_ssb_per_r(orig$nAge, orig$ssbwaa, orig$maturity, orig$maa, 0, orig$selx, orig$spawntime)
F.start <- 0.11
spr.f <- function(F.start){
  abs(calc_ssb_per_r(nAge = orig$nAge, ssbwaa = orig$ssbwaa, maturity = orig$maturity, maa = orig$maa, fmult = F.start, selx = orig$selx, spawntime = orig$spawntime) / ssbpr0 - target.spr)
}
yyy <- nlminb(start = F.start, objective = spr.f, lower = 0, upper = 5)
Ftarget.orig <- yyy$par
Fterm.orig <- asap$F.report[asap$parms$nyears]
Fratio.orig <- Fterm.orig / Ftarget.orig
orig.rho.F <- orig.rho.table$f.rho[orig.rho.table$X == "Mohn.rho"]
Fterm.orig.rhoadj <- Fterm.orig / (1 + orig.rho.F)
Fratio.orig.rhoadj <- Fterm.orig.rhoadj / Ftarget.orig

naa.orig <- rose::get_starting_naa(asap, myrecruityears)
projcatch.orig <- rose::project_short_term(naa.orig, orig$maa, Ftarget.orig, orig$selx, orig$cwaa, mynpy, naa.orig[1])

# apply SSBrho and NAArho adjustments to orig and get projected catch 
orig.rho.SSB <- orig.rho.table$ssb.rho[orig.rho.table$X == "Mohn.rho"] 
nAge <- asap$parms$nages
myncols <- length(orig.rho.table[1, ])
mynrows <- length(orig.rho.table[, 1])
orig.rho.NAA <- rep(NA, nAge)
iage <- 0
for (icol in (myncols - nAge + 1):myncols){
  iage <- iage + 1
  orig.rho.NAA[iage] <- orig.rho.table[mynrows, icol]
}
naa.orig.ssbrhoadj <- naa.orig / (1 + orig.rho.SSB)
naa.orig.naarhoadj <- naa.orig / (1 + orig.rho.NAA)
rbind(naa.orig, naa.orig.ssbrhoadj, naa.orig.naarhoadj)
projcatch.orig.ssbrhoadj <- project_short_term(naa.orig.ssbrhoadj, orig$maa, Ftarget.orig, orig$selx, orig$cwaa, mynpy, naa.orig[1])
projcatch.orig.naarhoadj <- project_short_term(naa.orig.naarhoadj, orig$maa, Ftarget.orig, orig$selx, orig$cwaa, mynpy, naa.orig[1])
ssbterm.orig <- asap$SSB[asap$parms$nyears]
ssbterm.orig.rhoadj <- ssbterm.orig / (1 + orig.rho.SSB)
ssbprtarget.orig <- calc_ssb_per_r(orig$nAge, orig$ssbwaa, orig$maturity, orig$maa, Ftarget.orig, orig$selx, orig$spawntime)
ssbtarget.orig <- ssbprtarget.orig * quantile(asap$N.age[,1], 0.75)
ssbratio.orig <- ssbterm.orig / ssbtarget.orig
ssbratio.orig.rhoadj <- ssbterm.orig.rhoadj / ssbtarget.orig

orig.status.df <- data.frame(Scenario = rep(c("orig", "orig.rhoadj"), 3),
                             Metric = rep(c("Terminal", "Target", "Ratio"), each = 2),
                             SSB = c(ssbterm.orig, ssbterm.orig.rhoadj,
                                     ssbtarget.orig, ssbtarget.orig, 
                                     ssbratio.orig, ssbratio.orig.rhoadj),
                             F = c(Fterm.orig, Fterm.orig.rhoadj, 
                                   Ftarget.orig, Ftarget.orig, 
                                   Fratio.orig, Fratio.orig.rhoadj)) %>%
  mutate(Scenario = as.factor(Scenario))

# loop through the scenarios
for (i in 1:nscen){
  asap <- dget(paste0("..\\saved\\WHscen", scennum[i], ".rdat"))
  mya <- rose::get_asap_recent_vals(asap, nrecentyears)
  
  # compute F to achieve target spawning potential ratio (NOTE: using original M in calcs)
  ssbpr0 <- rose::calc_ssb_per_r(mya$nAge, mya$ssbwaa, mya$maturity, orig$maa, 0, mya$selx, mya$spawntime)
  F.start <- 0.11
  spr.f <- function(F.start){
    abs(rose::calc_ssb_per_r(nAge = mya$nAge, ssbwaa = mya$ssbwaa, maturity = mya$maturity, maa = orig$maa, fmult = F.start, selx = mya$selx, spawntime = mya$spawntime) / ssbpr0 - target.spr)
  }
  yyy <- nlminb(start = F.start, objective = spr.f, lower = 0, upper = 5)
  Ftarget[i] <- yyy$par
  Fterm[i] <- asap$F.report[asap$parms$nyears]
  Fratio[i] <- Fterm[i] / Ftarget[i]
  
  ssbprtarget <- rose::calc_ssb_per_r(mya$nAge, mya$ssbwaa, mya$maturity, orig$maa, Ftarget[i], mya$selx, mya$spawntime)
  ssbtarget[i] <- ssbprtarget * quantile(asap$N.age[,1], 0.75)
  ssbterm[i] <- asap$SSB[asap$parms$nyears]
  ssbratio[i] <- ssbterm[i] / ssbtarget[i]
  
  naa <- rose::get_starting_naa(asap, myrecruityears)
  projcatch[i, ] <- rose::project_short_term(naa, mya$maa, Ftarget[i], mya$selx, mya$cwaa, mynpy, naa[1]) / br$cmult[i]
  
}

# save data
scen.status.df <- data.frame(Scenario = rep(paste0(br$case, br$ramplab, br$change.year), 3),
                             Metric = rep(c("Terminal", "Target", "Ratio"), each = nscen),
                             SSB = c(ssbterm, ssbtarget, ssbratio),
                             F = c(Fterm, Ftarget, Fratio)) %>%
  mutate(Scenario = as.factor(Scenario))

rose.df <- scen.status.df %>%
  group_by(Metric) %>%
  summarize(SSB = mean(SSB), F = mean(F)) %>%
  mutate(Scenario = "Rose") %>%
  select(Scenario, Metric, SSB, F)

status.df <- rbind(orig.status.df, scen.status.df, rose.df) 
write.csv(status.df, file = "..\\saved\\WHstatus.csv", row.names = FALSE)

projyear <- asap$parms$endyr + seq(1, mynpy)

orig.c.df <- data.frame(Scenario = rep(c("orig", "orig.SSBrhoadj", "orig.NAArhoadj"), each = 3),
                        Year = rep(projyear, 3),
                        Catch = c(projcatch.orig, projcatch.orig.ssbrhoadj, projcatch.orig.naarhoadj))

scen.c.df <- data.frame(Scenario = rep(paste0(br$case, br$ramplab, br$change.year), 3),
                        Year = rep(projyear, each = nscen),
                        Catch = as.vector(projcatch))

rose.c.df <- scen.c.df %>%
  group_by(Year) %>%
  summarize(Catch = mean(Catch)) %>%
  mutate(Scenario = "Rose") %>%
  select(Scenario, Year, Catch)

c.df <- rbind(orig.c.df, scen.c.df, rose.c.df)
write.csv(c.df, file = "..\\saved\\WHcatch_advice.csv", row.names = FALSE)
