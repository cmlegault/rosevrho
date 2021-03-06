# 01_GOMH.R
# apply Rose approach to 2019 Gulf of Maine Haddock assessment and compare to rho adjustment

# set working directory to source file location to begin

# if needed
#devtools::install_github("cmlegault/ASAPplots")
#devtools::install_github("cmlegault/rose")

library(dplyr)
library(ASAPplots)
library(rose)

# get starting ASAP file
myname <- "GOMH.dat"
setwd(file.path(getwd(), "..\\workingGOMH"))

# copy ASAP executables into current and working directories
rose.dir <- find.package("rose")
file.copy(from = file.path(rose.dir, "ASAPexecutables", "ASAP3.exe"), to = "ASAP3.exe")
file.copy(from = file.path(rose.dir, "ASAPexecutables", "ASAPRETRO.exe"), to = "ASAPRETRO.exe")

# copy starting ASAP file into working directory, get info about run, and run retro
file.copy(from = file.path("..\\data", myname), to = myname)
asap.dat <- ASAPplots::ReadASAP3DatFile(myname)
nages <- asap.dat$dat$n_ages
terminal.year <- as.numeric(asap.dat$dat[1]) + as.numeric(asap.dat$dat[2]) - 1
n.peels <- 5 # ICES default
retro.first.year <- terminal.year - n.peels
shell(paste("ASAPRETRO.exe", myname, retro.first.year), intern = TRUE)

# run ASAPplots on original data
asap.name <- substr(myname, 1, nchar(myname)-4)
ASAPplots::PlotASAP(wd = ".", asap.name = asap.name)
file.copy(from = paste0(asap.name, "_000.rdat"), to = paste0("..\\saved\\", asap.name, "_000.rdat"))
file.copy(from = paste0("plots\\Retro.rho.values_", asap.name, "_000.csv"), 
          to = paste0("..\\saved\\Retro.rho.values_", asap.name, "_000.csv"))

# save time series of SSB and F from each peel
peel.df <- data.frame(Peel = integer(),
                      Year = integer(),
                      metric = character(),
                      value = double())

for (ipeel in 0:n.peels){
  fname <- paste0(asap.name, "_00", ipeel, ".rdat")
  asap <- dget(fname)
  thisdf <- data.frame(Peel = ipeel,
                       Year = seq(asap$parms$styr, asap$parms$endyr),
                       metric = rep(c("SSB", "F"), each = asap$parms$nyears),
                       value = c(asap$SSB, asap$F.report))
  peel.df <- rbind(peel.df, thisdf)
}
write.csv(peel.df, file = paste0("..\\saved\\peeldf_", asap.name, ".csv"), row.names = FALSE)

# runRetroMults on base case so have needed data frame to combine later
orig.df <- runRetroMults("Base Case", myname, n.peels, 0, 2018, 1, 1, rep(1, nages), "All Ages", 1, FALSE)

# common values
year.vals <- c(2000, 2005, 2010, 2015)
mymults <- seq(0.1, 0.9, 0.1)
young.ages <- c(rep(1, 4), rep(0, 5))
old.ages <- c(rep(0, 4), rep(1, 5))

# run sudden catch mults
myscenario <- "Sudden Cmults"
scenario1.df <- runRetroMults(myscenario, myname, n.peels, 0, year.vals, mymults, 1, rep(1, nages), "All Ages", 1, FALSE)

# save work so far
sofar.df <- rbind(orig.df, scenario1.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# dial in cmults
scenario1b.df <- runRetroMults(myscenario, myname, n.peels, 0, 2000, seq(0.01, 0.09, 0.01), 1, rep(1, nages), "All Ages", 1, FALSE)

scenario1c.df <- runRetroMults(myscenario, myname, n.peels, 0, c(2005, 2010, 2015), seq(0.01, 0.09, 0.01), 1, rep(1, nages), "All Ages", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario1b.df, scenario1c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# cmults ramp4
myscen2 <- "Ramp4 Cmults"
scenario2.df <- runRetroMults(myscen2, myname, n.peels, 4, year.vals, mymults, 1, rep(1, nages), "All Ages", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario2.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# dial in cmults ramp4
scenario2b.df <- runRetroMults(myscen2, myname, n.peels, 4, year.vals, seq(0.01, 0.05, 0.01), 1, rep(1, nages), "All Ages", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario2b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# cmults ramp9
myscen3 <- "Ramp9 Cmults"
scenario3.df <- runRetroMults(myscen3, myname, n.peels, 9, year.vals, mymults, 1, rep(1, nages), "All Ages", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario3.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# dial in cmults ramp9
scenario3b.df <- runRetroMults(myscen3, myname, n.peels, 9, year.vals, seq(0.01, 0.05, 0.01), 1, rep(1, nages), "All Ages", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario3b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# sudden Mmults
myscen4 <- "Sudden Mmults"
scenario4.df <- runRetroMults(myscen4, myname, n.peels, 0, year.vals, 1, mymults, rep(1, nages), "All Ages", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario4.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# dial in sudden Mmults 
# change year 2005 nailed it with mult=0.3
scenario4b.df <- runRetroMults(myscen4, myname, n.peels, 0, 2000, 1, seq(0.15, 0.19, 0.01), rep(1, nages), "All Ages", 1, FALSE)

scenario4c.df <- runRetroMults(myscen4, myname, n.peels, 0, 2010, 1, seq(0.25, 0.29, 0.01), rep(1, nages), "All Ages", 1, FALSE)

scenario4d.df <- runRetroMults(myscen4, myname, n.peels, 0, 2015, 1, seq(0.01, 0.09, 0.02), rep(1, nages), "All Ages", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario4b.df, scenario4c.df, scenario4d.df)
write.csv(sofar.df, file = "sofar.csv", row.names = 1, FALSE)

# Mmult ramp4
myscen5 <- "Ramp4 Mmults"
scenario5.df <- runRetroMults(myscen5, myname, n.peels, 4, year.vals, 1, mymults, rep(1, nages), "All Ages", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario5.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# dial in Mmults ramp4
scenario5b.df <- runRetroMults(myscen5, myname, n.peels, 4, 2000, 1, seq(0.12, 0.16, 0.01), rep(1, nages), "All Ages", 1, FALSE)

scenario5c.df <- runRetroMults(myscen5, myname, n.peels, 4, 2005, 1, seq(0.23, 0.28, 0.01), rep(1, nages), "All Ages", 1, FALSE)

scenario5d.df <- runRetroMults(myscen5, myname, n.peels, 4, 2010, 1, seq(0.31, 0.33, 0.01), rep(1, nages), "All Ages", 1, FALSE)

scenario5e.df <- runRetroMults(myscen5, myname, n.peels, 4, 2015, 1, seq(0.05, 0.09, 0.01), rep(1, nages), "All Ages", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario5b.df, scenario5c.df, scenario5d.df, scenario5e.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# Mmult ramp9
myscen6 <- "Ramp9 Mmults"
scenario6.df <- runRetroMults(myscen6, myname, n.peels, 9, year.vals, 1, mymults, rep(1, nages), "All Ages", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario6.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# dial in Mmults ramp9
# change year 2005 nailed it with mult=0.2

scenario6b.df <- runRetroMults(myscen6, myname, n.peels, 9, 2000, 1, seq(0.05, 0.09, 0.01), rep(1, nages), "All Ages", 1, FALSE)

scenario6c.df <- runRetroMults(myscen6, myname, n.peels, 9, 2010, 1, seq(0.26, 0.29, 0.01), rep(1, nages), "All Ages", 1, FALSE)

scenario6d.df <- runRetroMults(myscen6, myname, n.peels, 9, 2015, 1, seq(0.15, 0.19, 0.01), rep(1, nages), "All Ages", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario6b.df, scenario6c.df, scenario6d.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# Old Sudden Mmult (ages 5-9+)
myscen7 <- "Old Sudden Mmults"
scenario7.df <- runRetroMults(myscen7, myname, n.peels, 0, year.vals, 1, mymults, old.ages, "Old", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario7.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# dial in Old Sudden Mmults
scenario7b.df <- runRetroMults(myscen7, myname, n.peels, 0, 2000, 1, seq(0.01, 0.09, 0.02), old.ages, "Old", 1, FALSE)

scenario7c.df <- runRetroMults(myscen7, myname, n.peels, 0, c(2005, 2010, 2015), 1, seq(0.01, 0.09, 0.02), old.ages, "Old", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario7b.df, scenario7c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# Old Ramp4 Mmults
myscen8 <- "Old Ramp4 Mmults"
scenario8.df <- runRetroMults(myscen8, myname, n.peels, 4, year.vals, 1, mymults, old.ages, "Old", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario8.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# dial in Old Ramp4 Mmults
scenario8b.df <- runRetroMults(myscen8, myname, n.peels, 4, year.vals, 1, seq(0.01, 0.09, 0.01), old.ages, "Old", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario8b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# Old Ramp9 Mmults
myscen9 <- "Old Ramp9 Mmults"
scenario9.df <- runRetroMults(myscen9, myname, n.peels, 9, year.vals, 1, mymults, old.ages, "Old", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario9.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# dial in Old Ramp9 Mmults
scenario9b.df <- runRetroMults(myscen9, myname, n.peels, 9, year.vals, 1, seq(0.01, 0.09, 0.02), old.ages, "Old", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario9b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# Young Sudden Mmults (ages 1-4)
myscen10 <- "Young Sudden Mmults"
scenario10.df <- runRetroMults(myscen10, myname, n.peels, 0, year.vals, 1, mymults, young.ages, "Young", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario10.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# dial in Young Sudden Mmults
scenario10b.df <- runRetroMults(myscen10, myname, n.peels, 0, year.vals, 1, seq(0.01, 0.09, 0.02), young.ages, "Young", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario10b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# Young Ramp4 Mmults (ages 1-4)
myscen11 <- "Young Ramp4 Mmults"
scenario11.df <- runRetroMults(myscen11, myname, n.peels, 4, year.vals, 1, mymults, young.ages, "Young", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario11.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# dial in Young Ramp4 Mmults
scenario11b.df <- runRetroMults(myscen11, myname, n.peels, 4, year.vals, 1, seq(0.01, 0.09, 0.02), young.ages, "Young", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario11b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# Young Ramp9 Mmults (ages 1-4)
myscen12 <- "Young Ramp9 Mmults"
scenario12.df <- runRetroMults(myscen12, myname, n.peels, 9, year.vals, 1, mymults, young.ages, "Young", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario12.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# dial in Young Ramp9 Mmults
scenario12b.df <- runRetroMults(myscen12, myname, n.peels, 9, year.vals, 1, seq(0.01, 0.09, 0.02), young.ages, "Young", 1, FALSE)

# save work so far
sofar.df <- rbind(sofar.df, scenario12b.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

# copy results file so not lost
file.copy(from = "sofar.csv", to = "..\\saved\\ssbrhodatabaseGOMH.csv")

####
# # try index multipliers - didn't work
# year.vals <- c(2000, 2005, 2010, 2015)
# mymults <- seq(1.5, 5, 0.5)
# myscen13 <- "Sudden Imults"
# scenario13.df <- runRetroMults(myscen13, myname, n.peels, 0, year.vals, 1, 1, rep(1, nages), "All Ages", mymults, FALSE)




