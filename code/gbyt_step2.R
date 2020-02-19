# gbyt_rose_step2.R
# figure out which runs have zero retro, rerun these runs saving needed info

# set working directory to source file location to begin

# if needed
#devtools::install_github("cmlegault/ASAPplots")
#devtools::install_github("cmlegault/rose")

library(dplyr)
library(ASAPplots)
library(rose)

rhodbGBYT <- read.csv("..\\saved\\ssbrhodatabaseGBYT.csv")
dim(rhodbGBYT)

## 12 scenarios with lowest abs(ssbrho) for each combination of fix and ramp
bestbestrho <- rhodbGBYT %>%
  filter(scenario != "Base Case") %>%
  mutate(multiplier = cmult * mmult) %>%
  mutate(absssbrho = abs(ssbrho)) %>%
  group_by(scenario, ramp, mselxlab) %>%
  summarize(min = min(absssbrho)) 

bestbestrho

for (i in 1:length(bestbestrho$scenario)){
  myrun <- rhodbGBYT %>%
    mutate(absssbrho = abs(ssbrho)) %>%
    filter(scenario == bestbestrho$scenario[i],
           ramp == bestbestrho$ramp[i],
           mselxlab == bestbestrho$mselxlab[i],
           absssbrho == bestbestrho$min[i])
  if (i == 1){
    useruns <- myrun
  }else{
    useruns <- rbind(useruns, myrun)
  }
}
useruns
myorder <- c("08", "09", "07", "02", "05", "03", "06", "01", "04", "11", "12", "10")
useruns$scen <- myorder
useruns <- useruns[order(useruns$scen), ]
nscen <- length(useruns$scen)
write.csv(useruns, file = paste0("..\\saved\\GBYTuse12table.csv"), row.names = FALSE)

# change back to working directory to run these 12 cases
setwd(file.path(getwd(), "..\\workingGBYT"))
myname <- "GBYT.dat"
n.peels <- 5
nages <- 6
scennum <- rep(NA, nscen)
for (icase in 1:nscen){
  if (icase < 10){
    scennum[icase] <- paste0("0", icase)
  } else {
    scennum[icase] <- as.character(icase)
  }
  myscen <- paste(useruns$scenario[icase], useruns$change.year[icase])
  myramp <- useruns$ramp[icase]
  myyear <- useruns$change.year[icase]
  mycmult <- useruns$cmult[icase]
  mymmult <- useruns$mmult[icase]
  mymselxlab <- useruns$mselxlab[icase]
  if (mymselxlab == "All Ages"){
    mymselx <- rep(1, nages)
  } else {
    if (mymselxlab == "Young"){
      mymselx <- c(1, 1, 1, 0, 0, 0)
    } else { 
      if (mymselxlab == "Old"){
        mymselx <- c(0, 0, 0, 1, 1, 1)
      } else {
        step("something wrong")
      }
    }
  }
  runRetroMults(myscen, myname, n.peels, myramp, myyear, mycmult, mymmult, mymselx, mymselxlab, 1, TRUE)
  fname <- paste0("y", myyear, "r", myramp, "c", mycmult, "m", mymmult, "s", paste0(mymselx, collapse = ""))
  print(fname)
}

# save the necessary files (don't delete anything yet)
ts.df <- data.frame(case = character(),
                    ramplab = character(),
                    Year = integer(),
                    SSB = double(),
                    F = double(),
                    R = double())

for (icase in 1:nscen){
  myscen <- paste(useruns$scenario[icase], useruns$change.year[icase])
  myramp <- useruns$ramp[icase]
  myyear <- useruns$change.year[icase]
  mycmult <- useruns$cmult[icase]
  mymmult <- useruns$mmult[icase]
  mymselxlab <- useruns$mselxlab[icase]
  if (mymselxlab == "All Ages"){
    mymselx <- rep(1, nages)
  } else {
    if (mymselxlab == "Young"){
      mymselx <- c(1, 1, 1, 0, 0, 0)
    } else { 
      if (mymselxlab == "Old"){
        mymselx <- c(0, 0, 0, 1, 1, 1)
      } else {
        step("something wrong")
      }
    }
  }
  fname <- paste0("y", myyear, "r", myramp, "c", mycmult, "m", mymmult, "s", paste0(mymselx, collapse = ""), "i1")
  print(fname)
  file.copy(from = paste0(fname, "_000.rdat"), to = paste0("..\\saved\\GBYTscen", scennum[icase], ".rdat"))

  asap <- dget(paste0(fname, "_000.rdat"))
  this.df <- data.frame(case = ifelse(mycmult > 1, "Cmult", paste("Mmult", mymselxlab)),
                        ramplab = ifelse(myramp == 0, "Sudden", paste0("Ramp", myramp)),
                        Year = seq(asap$parms$styr, asap$parms$endyr),
                        SSB = asap$SSB,
                        F = asap$F.report,
                        R = asap$N.age[, 1])
  
  ts.df <- rbind(ts.df, this.df)
}
write.csv(ts.df, file = "..\\saved\\time_seriesGBYT.csv", row.names = FALSE)
