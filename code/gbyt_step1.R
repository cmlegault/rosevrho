# gbyt_step1.R
# Georges Bank yellowtail flounder for Rose v. rho paper
# step 1: run original model and retro, compute rho-adjusted values, and find the multipliers that eliminate the retrospective pattern for 12 combinations of fix (catch, M all ages, M old ages, M young ages) and ramp (sudden, 4 year, 9 year)

# set working directory to source file location to begin

# if have not already installed these packages
# devtools::install_github("cmlegault/ASAPplots")
# devtools::install_github("cmlegault/rose")

library(ASAPplots)
library(rose)
library(dplyr)

# get starting ASAP file
myname <- "GBYT.dat"

# change to working directory
setwd(file.path(getwd(), "..\\workingGBYT"))

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

# save teime series of SSB and F from each peel
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

# common values to use initially
year.vals <- c(1995, 2000, 2005, 2010, 2013, 2015)
mymults <- seq(2, 10, 1)

### run catch mults for 6 sudden change years
myscenario <- "Sudden Cmults"
scenario1.df <- runRetroMults(myscenario, myname, n.peels, 0, year.vals, mymults, 1, rep(1, nages), "All Ages", 1, FALSE)
sofar.df <- rbind(orig.df, scenario1.df)

# now try cmults with 4 year ramp
myscen2 <- "Ramp4 Cmults"
scenario2.df <- runRetroMults(myscen2, myname, n.peels, 4, year.vals, mymults, 1, rep(1, nages), "All Ages", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario2.df)

# now try cmults with 4 year ramp
myscen2a <- "Ramp9 Cmults"
scenario2a.df <- runRetroMults(myscen2a, myname, n.peels, 9, year.vals, mymults, 1, rep(1, nages), "All Ages", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario2a.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

### M mults for all ages
myscen3 <- "Sudden Mmults"
scenario3.df <- runRetroMults(myscen3, myname, n.peels, 0, year.vals, 1, mymults, rep(1, nages), "All Ages", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario3.df)

# dial in sudden M mults
scenario3b.df <- runRetroMults(myscen3, myname, n.peels, 0, c(1995, 2000, 2005, 2010), 1, seq(2.1, 2.4, 0.1), rep(1, nages), "All Ages", 1, FALSE)
scenario3c.df <- runRetroMults(myscen3, myname, n.peels, 0, 2013, 1, seq(2.6, 2.9, 0.1), rep(1, nages), "All Ages", 1, FALSE)
scenario3d.df <- runRetroMults(myscen3, myname, n.peels, 0, 2015, 1, seq(4.1, 4.4, 0.1), rep(1, nages), "All Ages", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario3b.df, scenario3c.df, scenario3d.df)

# now 4 year M ramp
myscen4 <- "Ramp4 Mmults"
scenario4.df <- runRetroMults(myscen4, myname, n.peels, 4, year.vals, 1, mymults, rep(1, nages), "All Ages", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario4.df)

# dial in the Mramp4 cases
# 1995 nailed already
scenario4b.df <- runRetroMults(myscen4, myname, n.peels, 4, c(2000, 2005, 2010, 2013), 1, seq(2.1, 2.4, 0.1), rep(1, nages), "All Ages", 1, FALSE)
scenario4c.df <- runRetroMults(myscen4, myname, n.peels, 4, 2015, 1, seq(2.6, 2.9, 0.1), rep(1, nages), "All Ages", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario4b.df, scenario4c.df)

# now 9 year M ramp
myscen5 <- "Ramp9 Mmults"
scenario5.df <- runRetroMults(myscen5, myname, n.peels, 9, year.vals, 1, mymults, rep(1, nages), "All Ages", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario5.df)

# dial in the Mramp9 cases
scenario5b.df <- runRetroMults(myscen5, myname, n.peels, 9, 1995, 1, seq(2.1, 2.4, 0.1), rep(1, nages), "All Ages", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario5b.df)
scenario5c.df <- runRetroMults(myscen5, myname, n.peels, 9, c(2000, 2005, 2010, 2013, 2015), 1, seq(2.1, 2.4, 0.1), rep(1, nages), "All Ages", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario5b.df, scenario5c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

### Mmults for young fish (ages 1-3) only
myscen6 <- "Young Ramp9 Mmults"
scenario6.df <- runRetroMults(myscen6, myname, n.peels, 9, year.vals, 1, mymults, c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario6.df)

# dial in young ramp9 mmults cases
scenario6b.df <- runRetroMults(myscen6, myname, n.peels, 9, 1995, 1, seq(5.5, 5.9, 0.1), c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
scenario6c.df <- runRetroMults(myscen6, myname, n.peels, 9, c(2000, 2015), 1, seq(4.5, 4.9, 0.1), c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
scenario6d.df <- runRetroMults(myscen6, myname, n.peels, 9, c(2005, 2013), 1, seq(4.1, 4.5, 0.1), c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
scenario6e.df <- runRetroMults(myscen6, myname, n.peels, 9, 2010, 1, seq(3.5, 3.9, 0.1), c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario6b.df, scenario6c.df, scenario6d.df, scenario6e.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

## now try Mmults for old fish (ages 4-6+) only
myscen7 <- "Old Ramp9 Mmults"
scenario7.df <- runRetroMults(myscen7, myname, n.peels, 9, year.vals, 1, mymults, c(0, 0, 0, 1, 1, 1), "Old", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario7.df)

# dial in old ramp9 mmults cases
scenario7b.df <- runRetroMults(myscen7, myname, n.peels, 9, c(1995, 2000, 2005, 2010, 2013), 1, seq(2.5, 2.9, 0.1), c(0, 0, 0, 1, 1, 1), "Old", 1, FALSE)
scenario7c.df <- runRetroMults(myscen7, myname, n.peels, 9, 2015, 1, seq(2.5, 2.9, 0.1), c(0, 0, 0, 1, 1, 1), "Old", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario7b.df, scenario7c.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

## young sudden Mmults
myscen8 <- "Young Sudden Mmults"
scenario8.df <- runRetroMults(myscen8, myname, n.peels, 0, year.vals, 1, mymults, c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario8.df)

# dial in young sudden Mmults
scenario8b.df <- runRetroMults(myscen8, myname, n.peels, 0, 1995, 1, seq(5.1, 5.5, 0.1), c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
scenario8c.df <- runRetroMults(myscen8, myname, n.peels, 0, c(2000, 2010), 1, seq(4.1, 4.5, 0.1), c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
scenario8d.df <- runRetroMults(myscen8, myname, n.peels, 0, 2005, 1, seq(3.5, 3.9, 0.1), c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
scenario8e.df <- runRetroMults(myscen8, myname, n.peels, 0, 2013, 1, seq(8.5, 8.9, 0.1), c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario8b.df, scenario8c.df, scenario8d.df, scenario8e.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

## now old sudden Mmults
myscen9 <- "Old Sudden Mmults"
scenario9.df <- runRetroMults(myscen9, myname, n.peels, 0, year.vals, 1, mymults, c(0, 0, 0, 1, 1, 1), "Old", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario9.df)

# dial in old sudden Mmults
scenario9b.df <- runRetroMults(myscen9, myname, n.peels, 0, c(1995, 2000, 2005, 2010), 1, seq(2.5, 2.9, 0.1), c(0, 0, 0, 1, 1, 1), "Old", 1, FALSE)
scenario9c.df <- runRetroMults(myscen9, myname, n.peels, 0, 2013, 1, seq(3.1, 3.5, 0.1), c(0, 0, 0, 1, 1, 1), "Old", 1, FALSE)
scenario9d.df <- runRetroMults(myscen9, myname, n.peels, 0, 2015, 1, seq(4.5, 4.9, 0.1), c(0, 0, 0, 1, 1, 1), "Old", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario9b.df, scenario9c.df, scenario9d.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

## Ramp4 Mmults for young fish (ages 1-3) only
myscen10 <- "Young Ramp4 Mmults"
scenario10.df <- runRetroMults(myscen10, myname, n.peels, 4, year.vals, 1, mymults, c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario10.df)

# dial in young ramp4 mmults cases
scenario10b.df <- runRetroMults(myscen10, myname, n.peels, 4, 1995, 1, seq(5.1, 5.5, 0.1), c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
scenario10c.df <- runRetroMults(myscen10, myname, n.peels, 4, 2000, 1, seq(4.3, 4.7, 0.1), c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
scenario10d.df <- runRetroMults(myscen10, myname, n.peels, 4, c(2005, 2010), 1, seq(3.5, 3.9, 0.1), c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
scenario10e.df <- runRetroMults(myscen10, myname, n.peels, 4, 2013, 1, seq(4.5, 4.9, 0.1), c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
scenario10f.df <- runRetroMults(myscen10, myname, n.peels, 4, 2015, 1, seq(8.1, 8.5, 0.1), c(1, 1, 1, 0, 0, 0), "Young", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario10b.df, scenario10c.df, scenario10d.df, scenario10e.df, scenario10f.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

## now try ramp4 Mmults for old fish (ages 4-6+) only
myscen11 <- "Old Ramp4 Mmults"
scenario11.df <- runRetroMults(myscen11, myname, n.peels, 4, year.vals, 1, mymults, c(0, 0, 0, 1, 1, 1), "Old", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario11.df)

# dial in old ramp4 mmults
scenario11b.df <- runRetroMults(myscen11, myname, n.peels, 4, c(1995, 2000, 2005, 2010), 1, seq(2.5, 2.9, 0.1), c(0, 0, 0, 1, 1, 1), "Old", 1, FALSE)
scenario11c.df <- runRetroMults(myscen11, myname, n.peels, 4, 2013, 1, seq(3.1, 3.5, 0.1), c(0, 0, 0, 1, 1, 1), "Old", 1, FALSE)
scenario11d.df <- runRetroMults(myscen11, myname, n.peels, 4, 2015, 1, seq(3.3, 3.7, 0.1), c(0, 0, 0, 1, 1, 1), "Old", 1, FALSE)
sofar.df <- rbind(sofar.df, scenario11b.df, scenario11c.df, scenario11d.df)
write.csv(sofar.df, file = "sofar.csv", row.names = FALSE)

### index multipliers did not work
# run a set of Index mults for range of years
# myscen13 <- "Sudden Imults"
# scenario13.df <- runRetroMults(myscen13, myname, n.peels, 0, year.vals, 1, 1, rep(1, nages), "All Ages", mymults, FALSE)

# move and rename results file
file.copy(from = "sofar.csv", to = "..\\saved\\ssbrhodatabaseGBYT.csv")
