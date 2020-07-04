# 02_pick_best.R
# determine best runs for each scenario (closest to zero retro), rerun these runs saving needed info

# set working directory to source file location to begin

# if needed
#devtools::install_github("cmlegault/ASAPplots")
#devtools::install_github("cmlegault/rose")

library(dplyr)
library(ASAPplots)
library(rose)

# function to standardize the bestruns.csv file among stocks
get_bestruns <- function(bestrho, rhodb, maxssbrho){
  nscenarios <- length(bestrho$scenario)
  for (i in 1:nscenarios){
    myrun <- rhodb %>%
      mutate(absssbrho = abs(ssbrho)) %>%
      filter(scenario == bestrho$scenario[i],
             change.year == bestrho$change.year[i],
             ramp == bestrho$ramp[i],
             mselxlab == bestrho$mselxlab[i],
             absssbrho == bestrho$min[i]) %>%
      mutate(Multiplier = cmult * mmult) %>%
      mutate(case = ifelse(cmult != 1, "Cmult", paste("Mmult", mselxlab)),
             ramplab = ifelse(ramp == 0, "Sudden", paste0("Ramp", ramp))) %>%
      mutate(scenlab = paste(case, ramplab))
    if (i == 1){
      bestruns <- myrun
    }else{
      bestruns <- rbind(bestruns, myrun)
    }
  }
  bestruns <- bestruns %>%
    filter(absssbrho <= maxssbrho) %>%
    mutate(ramplab = factor(ramplab, levels = c("Sudden", "Ramp4", "Ramp9"))) %>%
    arrange(case, ramplab, change.year)
  return(bestruns)
}

# mystock = character name of stock
# useruns = selected scenarios for the stock
# young.ages = vector of selectivity for young ages case
# old.ages = vector of selectivity for old ages case
# n.peels = number of retrospective peels (default = 5)
# runs ASAP for selected scenarios and copies rdat files to saved directory
# with filename mystock scen ## .rdat (e.g., GBYTscen03.rdat)
run_selected <- function(mystock, useruns, young.ages, old.ages, n.peels=5){
  orig.dir <- getwd()
  setwd(file.path(getwd(), paste0("..\\working", mystock)))
  myname <- paste0(mystock, ".dat")
  nages <- length(young.ages)
  nscen <- length(useruns$scenario)
  scennum <- rep(NA, nscen)
  for (icase in 1:nscen){
    if (icase < 10){
      scennum[icase] <- paste0("0", icase)
    } else {
      scennum[icase] <- as.character(icase)
    }
    myscen <- paste0(useruns$case[icase], useruns$ramplab[icase], useruns$change.year[icase])
    myramp <- useruns$ramp[icase]
    myyear <- useruns$change.year[icase]
    mycmult <- useruns$cmult[icase]
    mymmult <- useruns$mmult[icase]
    mymselxlab <- useruns$mselxlab[icase]
    if (mymselxlab == "All Ages"){
      mymselx <- rep(1, nages)
    } else {
      if (mymselxlab == "Young"){
        mymselx <- young.ages
      } else { 
        if (mymselxlab == "Old"){
          mymselx <- old.ages
        } else {
          step("something wrong")
        }
      }
    }
    runRetroMults(myscen, myname, n.peels, myramp, myyear, mycmult, mymmult, mymselx, mymselxlab, 1, TRUE)
    fname <- paste0("y", myyear, "r", myramp, "c", mycmult, "m", mymmult, "s", paste0(mymselx, collapse = ""), "i1")
    print(fname)
    file.copy(from = paste0(fname, "_000.rdat"), to = paste0("..\\saved\\", mystock, "scen", scennum[icase], ".rdat"))
  }
  setwd(orig.dir)
  return(NULL)
}

# mystock = character name of stock
# useruns = selected scenarios for the stock
# creates csv file of SSB, F, and R time series from each scenario in saved dir
get_time_series <- function(mystock, useruns){
  ts.df <- data.frame(case = character(),
                      ramplab = character(),
                      change.year = integer(),
                      Year = integer(),
                      SSB = double(),
                      F = double(),
                      R = double())
  nscen <- length(useruns$scenario)
  scennum <- rep(NA, nscen)
  for (icase in 1:nscen){
    if (icase < 10){
      scennum[icase] <- paste0("0", icase)
    } else {
      scennum[icase] <- as.character(icase)
    }
    myramp <- useruns$ramp[icase]
    myyear <- useruns$change.year[icase]
    mycmult <- useruns$cmult[icase]
    mymmult <- useruns$mmult[icase]
    mymselxlab <- useruns$mselxlab[icase]
    
    asap <- dget(paste0("..\\saved\\", mystock, "scen", scennum[icase], ".rdat"))
    this.df <- data.frame(case = ifelse(mycmult != 1, "Cmult", paste("Mmult", mymselxlab)),
                          ramplab = ifelse(myramp == 0, "Sudden", paste0("Ramp", myramp)),
                          change.year = myyear,
                          Year = seq(asap$parms$styr, asap$parms$endyr),
                          SSB = asap$SSB,
                          F = asap$F.report,
                          R = asap$N.age[, 1])
    
    ts.df <- rbind(ts.df, this.df)
  }
  write.csv(ts.df, file = paste0("..\\saved\\time_series_", mystock, ".csv"), row.names = FALSE)
  return(NULL)
}

##################################
# run the four stocks
##################################

# Georges Bank Yellowtail Flounder
rhodbGBYT <- read.csv("..\\saved\\ssbrhodatabaseGBYT.csv")
dim(rhodbGBYT)

## 12 scenarios with lowest abs(ssbrho) for each combination of fix and ramp
rhodbGBYT1 <- rhodbGBYT %>%
  mutate(absssbrho = abs(ssbrho)) 

bestrhoGBYT <- rhodbGBYT %>%
  filter(scenario != "Base Case") %>%
  mutate(multiplier = cmult * mmult) %>%
  mutate(absssbrho = abs(ssbrho)) %>%
  group_by(scenario, ramp, mselxlab) %>%
  summarize(min = min(absssbrho)) 

bestrhoGBYT$change.year <- NA

for (i in 1:length(bestrhoGBYT$scenario)){
  mychangeyear <- rhodbGBYT1 %>%
    filter(scenario == bestrhoGBYT$scenario[i],
           ramp == bestrhoGBYT$ramp[i],
           absssbrho == bestrhoGBYT$min[i]) 
  bestrhoGBYT$change.year[i] <- mychangeyear$change.year
}

bestrunsGBYT <- get_bestruns(bestrhoGBYT, rhodbGBYT, 0.03)
write.csv(bestrunsGBYT, file = "..\\saved\\bestrunsGBYT.csv", row.names = FALSE)

young.agesGBYT <- c(1, 1, 1, 0, 0, 0)
old.agesGBYT <- c(0, 0, 0, 1, 1, 1)
run_selected("GBYT", bestrunsGBYT, young.agesGBYT, old.agesGBYT)
get_time_series("GBYT", bestrunsGBYT)
  
# Gulf of Maine Haddock
rhodbGOMH <- read.csv("..\\saved\\ssbrhodatabaseGOMH.csv")
dim(rhodbGOMH)

bestrhoGOMH <- rhodbGOMH %>%
  filter(cmult >= 0.1) %>%
  filter(mmult >= 0.1) %>%
  filter(scenario != "Base Case") %>%
  mutate(multiplier = cmult * mmult) %>%
  mutate(absssbrho = abs(ssbrho)) %>%
  group_by(scenario, change.year, ramp, mselxlab) %>%
  summarize(min = min(absssbrho)) 

bestrunsGOMH <- get_bestruns(bestrhoGOMH, rhodbGOMH, 0.03)
write.csv(bestrunsGOMH, file = "..\\saved\\bestrunsGOMH.csv", row.names = FALSE)

young.agesGOMH <- c(rep(1, 4), rep(0, 5))
old.agesGOMH <- c(rep(0, 4), rep(1, 5))
run_selected("GOMH", bestrunsGOMH, young.agesGOMH, old.agesGOMH)
get_time_series("GOMH", bestrunsGOMH)

# White Hake
rhodbWH <- read.csv("..\\saved\\ssbrhodatabaseWH.csv")
dim(rhodbWH)

bestrhoWH <- rhodbWH %>%
  filter(scenario != "Base Case") %>%
  mutate(multiplier = cmult * mmult) %>%
  mutate(absssbrho = abs(ssbrho)) %>%
  group_by(scenario, change.year, ramp, mselxlab) %>%
  summarize(min = min(absssbrho)) 

bestrunsWH <- get_bestruns(bestrhoWH, rhodbWH, 0.03)
write.csv(bestrunsWH, file = "..\\saved\\bestrunsWH.csv", row.names = FALSE)

young.agesWH <- c(1,1,1,1,0,0,0,0,0)
old.agesWH <- c(0,0,0,0,0,1,1,1,1)
run_selected("WH", bestrunsWH, young.agesWH, old.agesWH)
get_time_series("WH", bestrunsWH)

# Witch Flounder
rhodbWitch <- read.csv("..\\saved\\ssbrhodatabaseWitch.csv")
dim(rhodbWitch)

bestrhoWitch <- rhodbWitch %>%
  filter(scenario != "Base Case") %>%
  mutate(multiplier = cmult * mmult) %>%
  mutate(absssbrho = abs(ssbrho)) %>%
  group_by(scenario, change.year, ramp, mselxlab) %>%
  summarize(min = min(absssbrho)) 

bestrunsWitch <- get_bestruns(bestrhoWitch, rhodbWitch, 0.03)
write.csv(bestrunsWitch, file = "..\\saved\\bestrunsWitch.csv", row.names = FALSE)

young.agesWitch <- c(rep(1, 5), rep(0, 6))
old.agesWitch <- c(rep(0, 5), rep(1,6))
run_selected("Witch", bestrunsWitch, young.agesWitch, old.agesWitch)
get_time_series("Witch", bestrunsWitch)



