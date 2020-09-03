# 04_make_figures.R
# makes all figures for paper

# set working directory to source file location to begin

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

myext <- "jpg"
mycase <- c("Cmult", "Mmult All Ages", "Mmult Old", "Mmult Young")
myramp <- c("Sudden", "Ramp4", "Ramp9")
myyear <- c(1995, 2000, 2005, 2010, 2013, 2015)
nc <- length(mycase)
nr <- length(myramp)
ny <- length(myyear)
myscen <- rep(NA, (nc * nr * ny))
icount <- 0
for (ic in 1:nc){
  for (ir in 1:nr){
    for (iy in 1:ny){
      icount <- icount + 1
      myscen[icount] <- paste(mycase[ic], myramp[ir], myyear[iy])
    }
  }
}

myapproach <- c("orig", "orig.rhoadj", "orig.NAArhoadj", "orig.SSBrhoadj", "Rose")

myscenorder <- c(rev(myapproach), rev(myscen))

# retro plot -----------------------------------------------------
# basic retro plot with rho-adjusted value
peeldf_GBYT <- read.csv("..\\saved\\peeldf_GBYT.csv") %>%
  mutate(Peel = factor(Peel)) %>%
  mutate(stock = "GBYT")

peeldf_WH <- read.csv("..\\saved\\peeldf_WH.csv") %>%
  mutate(Peel = factor(Peel)) %>%
  mutate(stock = "White Hake")

peeldf_Witch <- read.csv("..\\saved\\peeldf_Witch.csv") %>%
  mutate(Peel = factor(Peel)) %>%
  mutate(stock = "Witch")

peeldf_GOMH <- read.csv("..\\saved\\peeldf_GOMH.csv") %>%
  mutate(Peel = factor(Peel)) %>%
  mutate(stock = "GOM Haddock")

peeldf <- rbind(peeldf_GBYT, peeldf_WH, peeldf_Witch, peeldf_GOMH)

rhoGBYT <- read.csv("..\\saved\\Retro.rho.values_GBYT_000.csv") %>%
  filter(X == "Mohn.rho") %>%
  select(f.rho, ssb.rho)

rhoWH <- read.csv("..\\saved\\Retro.rho.values_WH_000.csv") %>%
  filter(X == "Mohn.rho") %>%
  select(f.rho, ssb.rho)

rhoWitch <- read.csv("..\\saved\\Retro.rho.values_Witch_000.csv") %>%
  filter(X == "Mohn.rho") %>%
  select(f.rho, ssb.rho)

rhoGOMH <- read.csv("..\\saved\\Retro.rho.values_GOMH_000.csv") %>%
  filter(X == "Mohn.rho") %>%
  select(f.rho, ssb.rho)

rhoadjustGBYT <- peeldf_GBYT %>%
  filter(Peel == 0, Year == max(Year)) %>%
  mutate(rhoadj = ifelse(metric == "SSB", value / (1 + rhoGBYT$ssb.rho), 
                         value / (1 + rhoGBYT$f.rho)))

rhoadjustWH <- peeldf_WH %>%
  filter(Peel == 0, Year == max(Year)) %>%
  mutate(rhoadj = ifelse(metric == "SSB", value / (1 + rhoWH$ssb.rho), 
                         value / (1 + rhoWH$f.rho)))

rhoadjustWitch <- peeldf_Witch %>%
  filter(Peel == 0, Year == max(Year)) %>%
  mutate(rhoadj = ifelse(metric == "SSB", value / (1 + rhoWitch$ssb.rho), 
                         value / (1 + rhoWitch$f.rho)))

rhoadjustGOMH <- peeldf_GOMH %>%
  filter(Peel == 0, Year == max(Year)) %>%
  mutate(rhoadj = ifelse(metric == "SSB", value / (1 + rhoGOMH$ssb.rho), 
                         value / (1 + rhoGOMH$f.rho)))

rhoadjust <- rbind(rhoadjustGBYT, rhoadjustWH, rhoadjustWitch, rhoadjustGOMH)

plot_retro <- function(peeldf, rhoadjust, mymetric, mytype){
  peeldf <- filter(peeldf, metric == mymetric)
  rhoadjust <- filter(rhoadjust, metric == mymetric)
  retroplot <- ggplot(peeldf, aes(x=Year, y=value, group=Peel)) +
    geom_line() +
    geom_point(data = rhoadjust, aes(x=Year, y=rhoadj), color = "black") +
    facet_wrap(~stock, scales = "free_y", ncol = 2) +
    expand_limits(y = 0) +
    ylab(mymetric) +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
    theme_bw()
  print(retroplot)
  ggsave(filename = paste0("..\\figs\\retroplot", mymetric, ".", mytype), width=4, height = 4, units = "in", dpi=500)
  return(retroplot)
}

retroplotSSB <- plot_retro(peeldf, rhoadjust, "SSB", myext)
retroplotF <- plot_retro(peeldf, rhoadjust, "F", myext)

# rho table ------------------------------------------------------
# Mohn's rho table for SSB, F, N at age for both stocks
rhoGBYT <- read.csv("..\\saved\\Retro.rho.values_GBYT_000.csv") %>%
  filter(X == "Mohn.rho")
rhoGBYT$Age.7 <- NA
rhoGBYT$Age.8 <- NA
rhoGBYT$Age.9 <- NA
rhoGBYT$Age.10 <- NA
rhoGBYT$Age.11 <- NA

rhoWH <- read.csv("..\\saved\\Retro.rho.values_WH_000.csv") %>%
  filter(X == "Mohn.rho")
rhoWH$Age.10 <- NA
rhoWH$Age.11 <- NA

rhoWitch <- read.csv("..\\saved\\Retro.rho.values_Witch_000.csv") %>%
  filter(X == "Mohn.rho")

rhoGOMH <- read.csv("..\\saved\\Retro.rho.values_GOMH_000.csv") %>%
  filter(X == "Mohn.rho")
rhoGOMH$Age.10 <- NA
rhoGOMH$Age.11 <- NA

rhotab <- rbind(rhoGBYT, rhoGOMH, rhoWH, rhoWitch) %>%
  select(-c("X", "recr.rho", "jan1b.rho", "explb.rho", "stockn.rho")) %>%
  t(.)
colnames(rhotab) <- c("GBYT", "GOM Haddock", "White Hake", "Witch")
rownames(rhotab) <- c("F", "SSB", paste("N age", 1:11))
write.csv(rhotab, file = "..\\saved\\rhotable.csv")

# remove retro plot ----------------------------------------------
# Mohn's rho for SSB vs multiplier
plot_remove_retro <- function(mystock, myylim, myext){
  rhodb <- read.csv(paste0("..\\saved\\ssbrhodatabase", mystock, ".csv"))

  tdf <- rhodb %>%
    mutate(Multiplier = mmult * cmult) %>%
    filter(scenario != "Base Case") %>%
    mutate(case = ifelse(cmult != 1, "Cmult", paste("Mmult", mselxlab))) %>%
    mutate(ramplab = ifelse(ramp == 0, "Sudden", paste0("Ramp", ramp))) %>%
    mutate(ramplab = factor(ramplab, levels = c("Sudden", "Ramp4", "Ramp9")))

  remove_retro_plot <- ggplot(tdf, aes(x=Multiplier, y=ssbrho, color=factor(change.year))) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, color = "red") +
    coord_cartesian(ylim = myylim) +
    facet_grid(ramplab ~ case) +
    ylab("Mohn's rho for SSB") +
    labs(color = "Change Year") +
    scale_fill_manual(name = "Change Year") +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
    theme_bw()
  print(remove_retro_plot)
  ggsave(filename = paste0("..\\figs\\remove_retro_", mystock, ".", myext), remove_retro_plot, dpi = 500)
  return(remove_retro_plot)
}

remove_retro_plotGBYT <- plot_remove_retro("GBYT", c(-1, 3), myext)
remove_retro_plotGOMH <- plot_remove_retro("GOMH", c(-0.3, 0.2), myext)
remove_retro_plotWH <- plot_remove_retro("WH", c(-0.5, 0.3), myext)
remove_retro_plotWitch <- plot_remove_retro("Witch", c(-0.5, 0.6), myext)

remove_retro_plotGOMH_rev <- remove_retro_plotGOMH +
  scale_x_continuous(breaks = c(0, 0.3, 0.6, 0.9), labels = c("0.0", "0.3", "0.6", "0.9"))
print(remove_retro_plotGOMH_rev)
ggsave(filename = paste0("..\\figs\\remove_retro_GOMH.", myext), remove_retro_plotGOMH_rev, dpi = 500)

#remove_retro_all4 <- plot_grid(remove_retro_plotGBYT, remove_retro_plotGOMH_rev, remove_retro_plotWH, remove_retro_plotWitch)
#print(remove_retro_all4)
#ggsave(filename = paste0("..\\figs\\remove_retro_all4.", myext), remove_retro_all4, width = 8, height = 6, units = "in", dpi = 500)

# time series plot -----------------------------------------------
# time series plots from selected models compared to original run
plot_time_series <- function(mystock, myext){
  res <- list()
  
  ts <- read.csv(paste0("..\\saved\\time_series_", mystock, ".csv"), header = TRUE)  %>%
    mutate(ramplab = factor(ramplab, levels = c("Sudden", "Ramp4", "Ramp9")))
  
  asap <- dget(paste0("..\\saved\\", mystock, "_000.rdat"))

  base <- data.frame(Year = seq(asap$parms$styr, asap$parms$endyr),
                     SSB = asap$SSB,
                     F = asap$F.report,
                     R = asap$N.age[, 1])
  
  myts <- c("SSB", "F", "R")
  for (i in 1:3){
    ts_plot <- ggplot(ts, aes(x=Year, y=get(myts[i]), color=factor(change.year))) +
      geom_point() +
      geom_line() +
      geom_line(data=base, aes(x=Year, y=get(myts[i])), color = "blue") +
      expand_limits(y = 0) +
      facet_grid(ramplab ~ case) +
      labs(color = "Change Year") +
      ylab(myts[i]) +
      scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
      theme_bw()
    print(ts_plot)
    res[[i]] <- ts_plot
    ggsave(filename = paste0("..\\figs\\time_series_", mystock, "_", myts[i], ".", myext), ts_plot, dpi = 500)
  }
  return(res)
}

tsGBYT <- plot_time_series("GBYT", myext)
tsGOMH <- plot_time_series("GOMH", myext)
tsWH <- plot_time_series("WH", myext)
tsWitch <- plot_time_series("Witch", myext)

# status plot ----------------------------------------------------
plot_status <- function(mystock, mytypes){
  myfigs <- list()
  mymetrics <- c("Terminal", "Target", "Ratio")
  myramplabs <- c("Sudden", "Ramp4", "Ramp9", "None", "Many")
  myxlab <- c("SSB", "SSB", "SSB/SSBtarget")
  myylab <- c("F", "F", "F/Ftarget")
  mycolors <- c("#D95F02", "#1B9E77", "#7570B3", "#66A61E", "#666666", "#A6761D", "#E7298A")
  mystatus <- read.csv(file = paste0("..\\saved\\status_", mystock, ".csv")) %>%
    transform(Scenario = factor(Scenario, levels = myscenorder)) %>%
    transform(Metric = factor(Metric, levels = mymetrics)) %>%
    transform(RampLab = factor(RampLab, levels = myramplabs))
  myorig.status <- filter(mystatus, substr(Scenario, 1, 4) == "orig")
  myrose <- filter(mystatus, Scenario == "Rose")
  col2 <- guide_legend(ncol = 2)
  
  for (i in 1:3){
    mymetric <- mymetrics[i]
    myfigs[[i]] <- ggplot(filter(mystatus, Metric == mymetric), aes(x = SSB, y = F, color = Case, shape = ChangeYear, size = RampLab)) +
      geom_point() +
      scale_color_manual(breaks=c("Cmult", "Mmult All Ages", "Mmult Old", "Mmult Young", "orig", "orig.rhoadj", "Rose"), values=mycolors) +
      scale_shape_manual(breaks=c(1995, 2000, 2005, 2010, 2013, 2015, "Many", "None"), values=c(0, 2, 3, 4, 5, 6, 19, 15)) +
      geom_segment(data=filter(myorig.status, Metric == mymetric), aes(x=SSB[1], y=F[1], xend=SSB[2], yend=F[2], linetype="rho adjustment"), arrow = arrow(), color="black", size=1) +
      scale_linetype_manual("rho adjustment", values = c("rho adjustment"=1), name="", guide = col2) +
      {if (i == 3) geom_hline(yintercept = 1, linetype = 2)} +
      {if (i == 3) geom_vline(xintercept = 0.5, linetype = 2)} +
      xlab(myxlab[i]) +
      ylab(myylab[i]) +
      ggtitle(mymetric) +
      scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
      theme_bw() +
      theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
      guides(linetype = FALSE)
    print(myfigs[[i]])
#    ggsave(filename = paste0("..\\figs\\", mymetric, "_", mystock, ".", mytypes[i]), myfigs[[i]], width = 6, height = 8, units = "in", dpi = 500)
  }
  
  f3way_plot <- ggplot(mystatus, aes(x=F, y=Scenario)) +
    geom_point() +
    facet_wrap(~ Metric, scales = "free_x") +
    ylab("") +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
    theme_bw()
  myfigs[[4]] <- f3way_plot
  print(myfigs[[4]])
  ggsave(filename = paste0("..\\figs\\f3way_", mystock, ".", mytypes[4]), f3way_plot, width = 6.5, height = 6.5, units = "in", dpi = 500)
  
  ssb3way_plot <- ggplot(mystatus, aes(x=SSB, y=Scenario)) +
    geom_point() +
    facet_wrap(~ Metric, scales = "free_x") +
    ylab("") +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
    theme_bw()
  myfigs[[5]] <- ssb3way_plot
  print(myfigs[[5]])
  ggsave(filename = paste0("..\\figs\\ssb3way_", mystock, ".", mytypes[5]), ssb3way_plot, width = 6.5, height = 6.5, units = "in", dpi = 500)
  
  return(myfigs)
}

GBYT_status_figs <- plot_status("GBYT", rep(myext, 5))
WH_status_figs <- plot_status("WH", rep(myext, 5))
Witch_status_figs <- plot_status("Witch", rep(myext, 5))
GOMH_status_figs <-  plot_status("GOMH", rep(myext, 5))

# status plot facetted ------------------------------------------
mymetrics <- c("Terminal", "Target", "Ratio")
myramplabs <- c("Sudden", "Ramp4", "Ramp9", "None", "Many")
myxlab <- c("SSB", "SSB", "SSB/SSBtarget")
myylab <- c("F", "F", "F/Ftarget")
mycolors <- c("#D95F02", "#1B9E77", "#7570B3", "#66A61E", "#666666", "#A6761D", "#E7298A")
statusGBYT <- read.csv(file = "..\\saved\\status_GBYT.csv") %>%
  mutate(stock = "GBYT")
statusGOMH <- read.csv(file = "..\\saved\\status_GOMH.csv") %>%
  mutate(stock = "GOM Haddock")
statusWH <- read.csv(file = "..\\saved\\status_WH.csv") %>%
  mutate(stock = "White Hake")
statusWitch <- read.csv(file = "..\\saved\\status_Witch.csv") %>%
  mutate(stock = "Witch")
mystatus <- rbind(statusGBYT, statusGOMH, statusWH, statusWitch) %>%
  transform(Scenario = factor(Scenario, levels = myscenorder)) %>%
  transform(Metric = factor(Metric, levels = mymetrics)) %>%
  transform(RampLab = factor(RampLab, levels = myramplabs))
myorig.status <- filter(mystatus, substr(Scenario, 1, 4) == "orig") %>%
  select(-Scenario) %>%
  pivot_wider(names_from = Case, values_from = c(SSB, F))
myrose <- filter(mystatus, Scenario == "Rose")
col2 <- guide_legend(ncol = 2)
myfigs <- list()
for (i in 1:3){
  mymetric <- mymetrics[i]
  myfigs[[i]] <- ggplot(filter(mystatus, Metric == mymetric), aes(x = SSB, y = F, color = Case, shape = ChangeYear, size = RampLab)) +
    geom_point() +
    scale_color_manual(breaks=c("Cmult", "Mmult All Ages", "Mmult Old", "Mmult Young", "orig", "orig.rhoadj", "Rose"), values=mycolors) +
    scale_shape_manual(breaks=c(1995, 2000, 2005, 2010, 2013, 2015, "Many", "None"), values=c(0, 2, 3, 4, 5, 6, 19, 15)) +
    geom_segment(data=filter(myorig.status, Metric == mymetric), aes(x=SSB_orig, y=F_orig, xend=SSB_orig.rhoadj, yend=F_orig.rhoadj, linetype="rho adjustment"), arrow = arrow(), color="black", size=1) +
    scale_linetype_manual("rho adjustment", values = c("rho adjustment"=1), name="", guide = col2) +
    {if (i == 3) geom_hline(yintercept = 1, linetype = 2)} +
    {if (i == 3) geom_vline(xintercept = 0.5, linetype = 2)} +
    xlab(myxlab[i]) +
    ylab(myylab[i]) +
    facet_wrap(~stock, scales = "free") +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
    theme_bw() +
    theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
    guides(linetype = FALSE)
  print(myfigs[[i]])
  ggsave(filename = paste0("..\\figs\\", mymetric, "_all4.", myext), myfigs[[i]], width = 6, height = 8, units = "in", dpi = 500)
}

# catch plot -----------------------------------------------------
plot_catch <- function(mystock, myext, is.small=FALSE){

  mycatch <- read.csv(paste0("..\\saved\\catch_advice_", mystock, ".csv")) %>%
    transform(Scenario = factor(Scenario, levels = myscenorder))
  
  if (is.small == TRUE){
    mycatch <- mycatch %>%
      filter(Scenario %in% c("orig", "orig.NAArhoadj", "orig.SSBrhoadj", "Rose"))
  }
  
  smlab <- ifelse(is.small == TRUE, "sm", "")
  
  catch_plot <- ggplot(mycatch, aes(x=Catch, y=Scenario)) +
    geom_point() +
    facet_wrap(~Year) +
    ylab("") +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
    theme_bw() +
    theme(legend.position = "none")
  print(catch_plot)
  ggsave(filename = paste0("..\\figs\\catch_advice_", mystock, smlab, ".", myext), catch_plot, dpi = 500)
  return(catch_plot)
}

catch_plot_GBYT <- plot_catch("GBYT", myext)
catch_plot_GOMH <- plot_catch("GOMH", myext)
catch_plot_WH <- plot_catch("WH", myext)
catch_plot_Witch <- plot_catch("Witch", myext)

catch_plot_all4 <- plot_grid(catch_plot_GBYT, catch_plot_GOMH, catch_plot_WH, catch_plot_Witch, labels = "AUTO")
print(catch_plot_all4)
ggsave(filename = paste0("..\\figs\\catch_advice_all4.", myext), catch_plot_all4, width = 10.5, height = 12, units = "in", dpi = 500)

# now make reduced catch plot showing just orig, 2 Rho, and Rose results
catch_plot_GBYTsm <- plot_catch("GBYT", myext, is.small=TRUE)
catch_plot_GOMHsm <- plot_catch("GOMH", myext, is.small=TRUE)
catch_plot_WHsm <- plot_catch("WH", myext, is.small=TRUE)
catch_plot_Witchsm <- plot_catch("Witch", myext, is.small=TRUE)

catch_plot_all4sm <- plot_grid(catch_plot_GBYTsm, catch_plot_GOMHsm, catch_plot_WHsm, catch_plot_Witchsm, labels = "AUTO")
print(catch_plot_all4sm)
ggsave(filename = paste0("..\\figs\\catch_advice_all4sm.", myext), catch_plot_all4sm, width = 6, height = 4, units = "in", dpi = 500)

# compare expanded biomass plots ---------------------------------

# GBYT
# get the expanded survey data
surveyb <- read.csv("..\\data\\surveyB.csv", header = TRUE) %>%
  mutate(surveyB = (DFO + Spring + Fall_lag) / 3,
         surveyBcv = (CVDFO + CVSpring + CVFall_lag) / 3)
surveyb <- surveyb %>%
  mutate(surveyBlow = surveyB * (1.0 - 1.96 * surveyBcv),
         surveyBhigh = surveyB * (1.0 + 1.96 * surveyBcv)) %>%
  filter(Year <= 2018) %>%
  mutate(stock = "GBYT")

# get SSB time series
ts.df <- read.csv("..\\saved\\time_series_GBYT.csv", header = TRUE) %>%
  transform(ramplab = factor(ramplab, 
                             levels = c("Sudden", "Ramp4", "Ramp9"))) %>%
  mutate(stock = "GBYT")

# get orig run
orig <- dget("..\\saved\\GBYT_000.rdat")
orig.df <- data.frame(Year = seq(orig$parms$styr, orig$parms$endyr),
                      SSB = orig$SSB) %>%
  mutate(stock = "GBYT")

# rho-adjust orig run
orig.rho.table <- read.csv("..\\saved\\Retro.rho.values_GBYT_000.csv", header = TRUE)
orig.rho.SSB <- orig.rho.table$ssb.rho[orig.rho.table$X == "Mohn.rho"]

orig_SSB_2018 <- filter(orig.df, Year == 2018) %>%
  select(SSB)

rhoadj <- data.frame(Year = 2018,
                     SSB = orig_SSB_2018 / (1 + orig.rho.SSB)) %>%
  mutate(stock = "GBYT")

# make plots
ts_ssb_survey_plot <- ggplot(filter(ts.df, Year >= 2010), aes(x=Year)) +
  geom_point(aes(y=SSB)) +
  geom_line(aes(y=SSB)) +
  geom_line(data=surveyb, aes(x=Year, y=surveyB), color = "blue") +
  geom_ribbon(data=surveyb, aes(x=Year, ymin=surveyBlow, ymax=surveyBhigh), fill = "blue", alpha=0.3) +
  geom_line(data=filter(orig.df, Year >= 2010), aes(x=Year, y=SSB), color = "red") +
  geom_point(data=rhoadj, aes(x=Year, y=SSB), color = "red", alpha = 0.6) +
  facet_grid(ramplab ~ case) +
  ylab("SSB or Survey Biomass") +
  coord_cartesian(ylim = c(0, 60000)) +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
  theme_bw()
print(ts_ssb_survey_plot)
ggsave(filename = paste0("..\\figs\\GBYTcompare_survey12.", myext), ts_ssb_survey_plot, dpi = 500)

# now just Rose SSB
rose_ssb <- ts.df %>%
  group_by(Year) %>%
  summarize(roseSSB = mean(SSB)) %>%
  mutate(stock = "GBYT")

ts_rose_ssb_survey_plot <- ggplot(filter(rose_ssb, Year >= 2010), aes(x=Year)) +
  geom_point(aes(y=roseSSB)) +
  geom_line(aes(y=roseSSB)) +
  geom_line(data=surveyb, aes(x=Year, y=surveyB), color = "blue") +
  geom_ribbon(data=surveyb, aes(x=Year, ymin=surveyBlow, ymax=surveyBhigh), fill = "blue", alpha=0.3) +
  geom_line(data=filter(orig.df, Year >= 2010), aes(x=Year, y=SSB), color = "red") +
  geom_point(data=rhoadj, aes(x=Year, y=SSB), color = "red", alpha = 0.6) +
  ylab("SSB or Survey Biomass") +
  coord_cartesian(ylim = c(0, 60000)) +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
  theme_bw()
print(ts_rose_ssb_survey_plot)
ggsave(filename = paste0("..\\figs\\GBYTcompare_survey_rose.", myext), ts_rose_ssb_survey_plot, dpi = 500)

# witch flounder
# get the expanded survey
surveybwitch <- read.csv("..\\data\\surveyB_witch.csv", header = TRUE) %>%
  mutate(surveyB = (Spring + Fall_lag) / 2,
         surveyBcv = (CVSpring + CVFall_lag) / 2)
surveybwitch <- surveybwitch %>%
  mutate(surveyBlow = surveyB * (1.0 - 1.96 * surveyBcv),
         surveyBhigh = surveyB * (1.0 + 1.96 * surveyBcv)) %>%
  filter(Year <= 2015) %>%
  mutate(stock = "Witch")

# get SSB time series
tswitch <- read.csv("..\\saved\\time_series_Witch.csv", header = TRUE) %>%
  transform(ramplab = factor(ramplab, 
                             levels = c("Sudden", "Ramp4", "Ramp9"))) %>%
  mutate(stock = "Witch")

# get orig run
orig_witch <- dget("..\\saved\\Witch_000.rdat")
orig_witch.df <- data.frame(Year = seq(orig_witch$parms$styr, 
                                       orig_witch$parms$endyr),
                            SSB = orig_witch$SSB) %>%
  mutate(stock = "Witch")

# rho adjust orig run
orig.rho.table_witch <- read.csv("..\\saved\\Retro.rho.values_Witch_000.csv", header = TRUE)
orig.rho.SSB_witch <- orig.rho.table_witch$ssb.rho[orig.rho.table_witch$X == "Mohn.rho"] 

orig_SSB_2015_witch <- filter(orig_witch.df, Year == 2015) %>%
  select(SSB)

rhoadj_witch <- data.frame(Year = 2015,
                           SSB = orig_SSB_2015_witch / (1 + orig.rho.SSB_witch)) %>%
  mutate(stock = "Witch")

# make plots
ts_witch_ssb_survey_plot <- ggplot(filter(tswitch, Year >= 2010), aes(x=Year)) +
  geom_point(aes(y=SSB, color=factor(change.year))) +
  geom_line(aes(y=SSB, color=factor(change.year))) +
  geom_line(data=surveybwitch, aes(x=Year, y=surveyB), color = "blue") +
  geom_ribbon(data=surveybwitch, aes(x=Year, ymin=surveyBlow, ymax=surveyBhigh), fill = "blue", alpha=0.3) +
  geom_line(data=filter(orig_witch.df, Year >= 2010), aes(x=Year, y=SSB), color = "red") +
  geom_point(data=rhoadj_witch, aes(x=Year, y=SSB), color = "red", alpha = 0.6) +
  facet_grid(ramplab ~ case) +
  ylab("SSB or Survey Biomass") +
  labs(color = "Change Year") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
  theme_bw()
print(ts_witch_ssb_survey_plot)
ggsave(filename = paste0("..\\figs\\Witchcompare_survey12.", myext), ts_witch_ssb_survey_plot, dpi = 500)

# now just Rose SSB
rose_ssb_witch <- tswitch %>%
  group_by(Year) %>%
  summarize(roseSSB = mean(SSB)) %>%
  mutate(stock = "Witch")

ts_witch_rose_ssb_survey_plot <- ggplot(filter(rose_ssb_witch, Year >= 2010), aes(x=Year)) +
  geom_point(aes(y=roseSSB)) +
  geom_line(aes(y=roseSSB)) +
  geom_line(data=surveybwitch, aes(x=Year, y=surveyB), color = "blue") +
  geom_ribbon(data=surveybwitch, aes(x=Year, ymin=surveyBlow, ymax=surveyBhigh), fill = "blue", alpha=0.3) +
  geom_line(data=filter(orig_witch.df, Year >= 2010), aes(x=Year, y=SSB), color = "red") +
  geom_point(data=rhoadj_witch, aes(x=Year, y=SSB), color = "red", alpha = 0.6) +
  ylab("SSB or Survey Biomass") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
  theme_bw()
print(ts_witch_rose_ssb_survey_plot)
ggsave(filename = paste0("..\\figs\\Witchcompare_survey_rose.", myext), ts_witch_rose_ssb_survey_plot, dpi = 500)

# combined rose plot
rose_ssb_2 <- rbind(rose_ssb, rose_ssb_witch)
surveyb2 <- rbind(surveyb, mutate(surveybwitch, DFO = NA, CVDFO = NA))
orig_2 <- rbind(orig.df, orig_witch.df)
rhoadj_2 <- rbind(rhoadj, rhoadj_witch)

ts_2_rose_ssb_survey_plot <- ggplot(filter(rose_ssb_2, Year >= 2010), aes(x=Year)) +
  geom_point(aes(y=roseSSB)) +
  geom_line(aes(y=roseSSB)) +
  geom_line(data=surveyb2, aes(x=Year, y=surveyB), color = "blue") +
  geom_ribbon(data=surveyb2, aes(x=Year, ymin=surveyBlow, ymax=surveyBhigh), fill = "blue", alpha=0.3) +
  geom_line(data=filter(orig_2, Year >= 2010), aes(x=Year, y=SSB), color = "red") +
  geom_point(data=rhoadj_2, aes(x=Year, y=SSB), color = "red", alpha = 0.6) +
  facet_wrap(~stock, scales = "free", ncol = 2) +
  ylab("SSB or Survey Biomass") +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
  theme_bw()
print(ts_2_rose_ssb_survey_plot)
ggsave(filename = paste0("..\\figs\\Bothcompare_survey_rose.", myext), ts_2_rose_ssb_survey_plot, dpi = 500)

