# whitehake_step2.R
# figure out which runs have zero retro, rerun these runs saving needed info

# set working directory to source file location to begin

# if needed
#devtools::install_github("cmlegault/ASAPplots")
#devtools::install_github("cmlegault/rose")

library(dplyr)
library(ASAPplots)
library(rose)

rhodbWH <- read.csv("..\\saved\\ssbrhodatabaseWH.csv")
dim(rhodbWH)

bestrho <- rhodbWH %>%
  filter(scenario != "Base Case") %>%
  mutate(multiplier = cmult * mmult) %>%
  mutate(absssbrho = abs(ssbrho)) %>%
  group_by(scenario, change.year, ramp, mselxlab) %>%
  summarize(min = min(absssbrho)) 

nscenarios <- length(bestrho$scenario)

for (i in 1:nscenarios){
  myrun <- rhodbWH %>%
    mutate(absssbrho = abs(ssbrho)) %>%
    filter(scenario == bestrho$scenario[i],
           change.year == bestrho$change.year[i],
           ramp == bestrho$ramp[i],
           mselxlab == bestrho$mselxlab[i],
           absssbrho == bestrho$min[i]) %>%
    mutate(Multiplier = cmult * mmult) %>%
    mutate(case = ifelse(cmult > 1, "Cmult", paste("Mmult", mselxlab)),
           ramplab = ifelse(ramp == 0, "Sudden", paste0("Ramp", ramp))) %>%
    mutate(scenlab = paste(case, ramplab))
  if (i == 1){
    bestruns <- myrun
  }else{
    bestruns <- rbind(bestruns, myrun)
  }
}
bestruns <- bestruns %>%
  mutate(ramplab = factor(ramplab, levels = c("Sudden", "Ramp4", "Ramp9"))) %>%
  arrange(case, ramplab, change.year)
bestruns
write.csv(bestruns, file = "..\\saved\\WHbestruns.csv", row.names = FALSE)

# change back to working directory to run these cases
setwd(file.path(getwd(), "..\\workingWH"))
myname <- "whitehake.dat"
n.peels <- 5
nages <- 9
scennum <- rep(NA, nscenarios)
for (icase in 1:nscenarios){
  if (icase < 10){
    scennum[icase] <- paste0("0", icase)
  } else {
    scennum[icase] <- as.character(icase)
  }
  myscen <- paste0(bestruns$case[icase], bestruns$ramplab[icase], bestruns$change.year[icase])
  myramp <- bestruns$ramp[icase]
  myyear <- bestruns$change.year[icase]
  mycmult <- bestruns$cmult[icase]
  mymmult <- bestruns$mmult[icase]
  mymselxlab <- bestruns$mselxlab[icase]
  if (mymselxlab == "All Ages"){
    mymselx <- rep(1, nages)
  } else {
    if (mymselxlab == "Young Ages"){
      mymselx <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
    } else { 
      if (mymselxlab == "Old Ages"){
        mymselx <- c(0, 0, 0, 0, 0, 1, 1, 1, 1)
      } else {
        stop("something wrong")
      }
    }
  }
  runRetroMults(myscen, myname, n.peels, myramp, myyear, mycmult, mymmult, mymselx, mymselxlab, 1, TRUE)
  fname <- paste0("y", myyear, "r", myramp, "c", mycmult, "m", mymmult, "s", paste0(mymselx, collapse = ""), "i1")
  print(fname)
  print(scennum[icase])
}

# save the necessary files, don't delete anything yet
ts.df <- data.frame(case = character(),
                    ramplab = character(),
                    change.year = integer(),
                    Year = integer(),
                    SSB = double(),
                    F = double(),
                    R = double())

for (icase in 1:nscenarios){
  myscen <- paste0(bestruns$case[icase], bestruns$ramplab[icase], bestruns$change.year[icase])
  myramp <- bestruns$ramp[icase]
  myyear <- bestruns$change.year[icase]
  mycmult <- bestruns$cmult[icase]
  mymmult <- bestruns$mmult[icase]
  mymselxlab <- bestruns$mselxlab[icase]
  if (mymselxlab == "All Ages"){
    mymselx <- rep(1, nages)
  } else {
    if (mymselxlab == "Young Ages"){
      mymselx <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
    } else { 
      if (mymselxlab == "Old Ages"){
        mymselx <- c(0, 0, 0, 0, 0, 1, 1, 1, 1)
      } else {
        step("something wrong")
      }
    }
  }
  fname <- paste0("y", myyear, "r", myramp, "c", mycmult, "m", mymmult, "s", paste0(mymselx, collapse = ""), "i1")
  print(fname)
  file.copy(from = paste0(fname, "_000.rdat"), to = paste0("..\\saved\\WHscen", scennum[icase], ".rdat"))

  asap <- dget(paste0(fname, "_000.rdat"))
  this.df <- data.frame(case = ifelse(mycmult > 1, "Cmult", paste("Mmult", mymselxlab)),
                        ramplab = ifelse(myramp == 0, "Sudden", paste0("Ramp", myramp)),
                        change.year = myyear,
                        Year = seq(asap$parms$styr, asap$parms$endyr),
                        SSB = asap$SSB,
                        F = asap$F.report,
                        R = asap$N.age[, 1])
  
  ts.df <- rbind(ts.df, this.df)
}
ts.df
write.csv(ts.df, file = "..\\saved\\time_seriesWH.csv", row.names = FALSE)
