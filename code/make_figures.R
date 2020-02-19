# make_figures.R
# makes all figures for paper

# set working directory to source file location to begin

library(dplyr)
library(ggplot2)
library(viridis)

# retro plot --------------------------------------------------------------
# basic retro plot with rho-adjusted value
peeldf_GBYT <- read.csv("..\\saved\\peeldf_GBYT.csv") %>%
  mutate(Peel = factor(Peel)) %>%
  mutate(stock = "GBYT")

peeldf_WH <- read.csv("..\\saved\\peeldf_whitehake.csv") %>%
  mutate(Peel = factor(Peel)) %>%
  mutate(stock = "White Hake")

peeldf <- rbind(peeldf_GBYT, peeldf_WH)

rhoGBYT <- read.csv("..\\saved\\Retro.rho.values_GBYT_000.csv") %>%
  filter(X == "Mohn.rho") %>%
  select(f.rho, ssb.rho)

rhoWH <- read.csv("..\\saved\\Retro.rho.values_whitehake_000.csv") %>%
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

rhoadjust <- rbind(rhoadjustGBYT, rhoadjustWH)

retroplot <- ggplot(peeldf, aes(x=Year, y=value, color=Peel)) +
  geom_line() +
  geom_point(data = rhoadjust, aes(x=Year, y=rhoadj), color = "black") +
  facet_grid(metric~stock, scales = "free_y") +
  expand_limits(y = 0) +
  ylab("") +
  scale_color_viridis(discrete=TRUE) +
  theme_bw()
print(retroplot)
ggsave(filename = "..\\figs\\retroplot.png", width=6, height = 4, units = "in")


# rho table ---------------------------------------------------------------
# Mohn's rho table for SSB, F, N at age for both stocks
rhoGBYT <- read.csv("..\\saved\\Retro.rho.values_GBYT_000.csv") %>%
  filter(X == "Mohn.rho")
rhoGBYT$Age.7 <- NA
rhoGBYT$Age.8 <- NA
rhoGBYT$Age.9 <- NA

rhoWH <- read.csv("..\\saved\\Retro.rho.values_whitehake_000.csv") %>%
  filter(X == "Mohn.rho")

rhotab <- rbind(rhoGBYT, rhoWH) %>%
  select(-c("X", "recr.rho", "jan1b.rho", "explb.rho", "stockn.rho")) %>%
  t(.)
colnames(rhotab) <- c("GBYT", "White Hake")
rownames(rhotab) <- c("F", "SSB", paste("N age", 1:9))
write.csv(rhotab, file = "..\\saved\\rhotable.csv")

# remove retro plot -------------------------------------------------------
# Mohn's rho for SSB vs multiplier
rhodbGBYT <- read.csv("..\\saved\\ssbrhodatabaseGBYT.csv")
dim(rhodbGBYT)

tdfGBYT <- rhodbGBYT %>%
  mutate(Multiplier = mmult * cmult) %>%
  filter(scenario != "Base Case") %>%
  mutate(case = ifelse(cmult > 1, "Cmult", paste("Mmult", mselxlab))) %>%
  mutate(ramplab = ifelse(ramp == 0, "Sudden", paste0("Ramp", ramp))) %>%
  mutate(ramplab = factor(ramplab, levels = c("Sudden", "Ramp4", "Ramp9")))

remove_retro_plotGBYT <- ggplot(tdfGBYT, aes(x=Multiplier, y=ssbrho, color=factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  coord_cartesian(ylim = c(-1, 3)) +
  facet_grid(ramplab ~ case) +
  ylab("Mohn's rho for SSB") +
  labs(color = "Change Year") +
  scale_fill_manual(name = "Change Year") +
  theme_bw()
print(remove_retro_plotGBYT)
ggsave(filename = "..\\figs\\remove_retroGBYT.png", remove_retro_plotGBYT)

rhodbWH <- read.csv("..\\saved\\ssbrhodatabaseWH.csv")
dim(rhodbWH)

tdfWH <- rhodbWH %>%
  mutate(Multiplier = mmult * cmult) %>%
  filter(scenario != "Base Case") %>%
  mutate(case = ifelse(cmult > 1, "Cmult", paste("Mmult", mselxlab))) %>%
  mutate(ramplab = ifelse(ramp == 0, "Sudden", paste0("Ramp", ramp))) %>%
  mutate(ramplab = factor(ramplab, levels = c("Sudden", "Ramp4", "Ramp9")))

remove_retro_plotWH <- ggplot(tdfWH, aes(x=Multiplier, y=ssbrho, color=factor(change.year))) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  coord_cartesian(ylim = c(-0.5, 0.3)) +  # better than ylim approach
  facet_grid(ramplab ~ case) +
  ylab("Mohn's rho for SSB") +
  labs(color = "Change Year") +
  scale_fill_manual(name = "Change Year") +
  theme_bw()
print(remove_retro_plotWH)
ggsave(filename = "..\\figs\\remove_retroWH.png", remove_retro_plotWH)

WHbestruns <- read.csv("..\\saved\\WHbestruns.csv")
dim(WHbestruns)

WHbestdf <- WHbestruns %>%
  mutate(ramplab = factor(ramplab, levels = c("Sudden", "Ramp4", "Ramp9")))

WHbestplot <- ggplot(WHbestdf, aes(x=change.year, y=Multiplier, fill=as.factor(change.year))) +
  geom_bar(stat="identity") +
  expand_limits(y = 0) +
  facet_grid(ramplab ~ case) +
  xlab("Change Year") +
  theme_bw() +
  theme(legend.position = "none")
print(WHbestplot)
ggsave(filename = "..\\figs\\WHbest.png", WHbestplot)

# time series plot --------------------------------------------------------
# time series plots from selected models compared to original run
tsGBYT <- read.csv("..\\saved\\time_seriesGBYT.csv", header = TRUE)

tsGBYT <- tsGBYT %>%
  mutate(ramplab = factor(ramplab, levels = c("Sudden", "Ramp4", "Ramp9")))

asap <- dget("..\\saved\\GBYT_000.rdat")
baseGBYT <- data.frame(Year = seq(asap$parms$styr, asap$parms$endyr),
                       SSB = asap$SSB,
                       F = asap$F.report,
                       R = asap$N.age[, 1])

ts_ssbGBYT <- ggplot(tsGBYT, aes(x=Year, y=SSB)) +
  geom_point() +
  geom_line() +
  geom_line(data=baseGBYT, aes(x=Year, y=SSB), color = "blue") +
  facet_grid(ramplab ~ case) +
  theme_bw()
print(ts_ssbGBYT)
ggsave(filename = "..\\figs\\time_series_ssbGBYT.png", ts_ssbGBYT)

# # plots for F and R not used in paper, just uncomment to see
# ts_fGBYT <- ggplot(tsGBYT, aes(x=Year, y=F)) +
#   geom_point() +
#   geom_line() +
#   geom_line(data=baseGBYT, aes(x=Year, y=F), color = "blue") +
#   facet_grid(ramplab ~ case) +
#   theme_bw()
# print(ts_fGBYT)
# ggsave(filename = "..\\figs\\time_series_fGBYT.png", ts_fGBYT)
# 
# # recruitment in millions
# ts_rGBYT <- ggplot(tsGBYT, aes(x=Year, y=R/1000)) +
#   geom_point() +
#   geom_line() +
#   geom_line(data=baseGBYT, aes(x=Year, y=R/1000), color = "blue") +
#   facet_grid(ramplab ~ case) +
#   ylab("Recruitment") +
#   theme_bw()
# print(ts_rGBYT)
# ggsave(filename = "..\\figs\\time_series_rGBYT.png", ts_rGBYT)
# 
# ts_rGBYT_limited <- ts_rGBYT +
#   coord_cartesian(ylim = c(0, 300))  # better than ylim
# print(ts_rGBYT_limited)
# ggsave(filename = "..\\figs\\time_series_r_limitedGBYT.png", ts_rGBYT_limited)

tsWH <- read.csv("..\\saved\\time_seriesWH.csv", header = TRUE)

tsWH <- tsWH %>%
  mutate(ramplab = factor(ramplab, levels = c("Sudden", "Ramp4", "Ramp9")))

asap <- dget("..\\saved\\whitehake_000.rdat")
baseWH <- data.frame(Year = seq(asap$parms$styr, asap$parms$endyr),
                     SSB = asap$SSB,
                     F = asap$F.report,
                     R = asap$N.age[, 1])

ts_ssbWH <- ggplot(tsWH, aes(x=Year, y=SSB, color=factor(change.year))) +
  geom_point() +
  geom_line() +
  geom_line(data=baseWH, aes(x=Year, y=SSB), color = "blue") +
  expand_limits(y = 0) +
  facet_grid(ramplab ~ case) +
  labs(color = "Change Year") +
  theme_bw()
print(ts_ssbWH)
ggsave(filename = "..\\figs\\time_series_ssbWH.png", ts_ssbWH)

# # plots for F and R not used in paper, just uncomment to see
# ts_fWH <- ggplot(tsWH, aes(x=Year, y=F, color=factor(change.year))) +
#   geom_point() +
#   geom_line() +
#   geom_line(data=baseWH, aes(x=Year, y=F), color = "blue") +
#   expand_limits(y = 0) +
#   facet_grid(ramplab ~ case) +
#   labs(color = "Change Year") +
#   theme_bw()
# print(ts_fWH)
# ggsave(filename = "..\\figs\\time_series_fWH.png", ts_fWH)
# 
# ts_RWH <- ggplot(tsWH, aes(x=Year, y=R, color=factor(change.year))) +
#   geom_point() +
#   geom_line() +
#   geom_line(data=baseWH, aes(x=Year, y=R), color = "blue") +
#   expand_limits(y = 0) +
#   facet_grid(ramplab ~ case) +
#   labs(color = "Change Year") +
#   theme_bw()
# print(ts_RWH)
# ggsave(filename = "..\\figs\\time_series_RWH.png", ts_RWH)


# status plots ------------------------------------------------------------
# need to read in csv file first
statusGBYT <- read.csv(file = "..\\saved\\GBYTstatus.csv")
statusGBYT <- statusGBYT %>%
  transform(Metric = factor(Metric, levels = c("Terminal", "Target", "Ratio")))
orig.statusGBYT <- filter(statusGBYT, substr(Scenario, 1, 4) == "orig")
roseGBYT <- filter(statusGBYT, Scenario == "Rose")

ratio_plotGBYT <- ggplot(filter(statusGBYT, Metric == "Ratio"), aes(x = SSB, y = F, color = Scenario)) +
  geom_point() +
  geom_segment(data=filter(orig.statusGBYT, Metric == "Ratio"), aes(x=SSB[1], y=F[1], xend=SSB[2], yend=F[2], linetype="rho adjustment"), arrow = arrow(), color="black") +
  scale_linetype_manual("rho adjustment", values = c("rho adjustment"=1), name="") +
  geom_point(data=filter(roseGBYT, Metric == "Ratio"), aes(x=SSB, y=F), shape=1, color="black", size=3) +
  xlab("SSB/SSBtarget") +
  ylab("F/Ftarget") +
  ggtitle("Status") +
  theme_bw()
print(ratio_plotGBYT)
ggsave(filename = "..\\figs\\GBYTstatus_ratio.png", ratio_plotGBYT)

# # terminal and target plots not shown in paper, just uncomment to see
# term_plotGBYT <- ggplot(filter(statusGBYT, Metric == "Terminal"), aes(x = SSB, y = F, color = Scenario)) +
#   geom_point() +
#   geom_segment(data=filter(orig.statusGBYT, Metric == "Terminal"), aes(x=SSB[1], y=F[1], xend=SSB[2], yend=F[2], linetype="rho adjustment"), arrow = arrow(), color="black") +
#   scale_linetype_manual("rho adjustment", values = c("rho adjustment"=1), name="") +
#   geom_point(data=filter(roseGBYT, Metric == "Terminal"), aes(x=SSB, y=F), shape=1, color="black", size=3) +
#   ggtitle("Terminal Year") +
#   theme_bw()
# print(term_plotGBYT)
# ggsave(filename = "..\\figs\\GBYTstatus_terminal.png", term_plotGBYT)
# 
# target_plotGBYT <- ggplot(filter(statusGBYT, Metric == "Target"), aes(x = SSB, y = F, color = Scenario)) +
#   geom_point() +
#   geom_segment(data=filter(orig.statusGBYT, Metric == "Target"), aes(x=SSB[1], y=F[1], xend=SSB[2], yend=F[2], linetype="rho adjustment"), arrow = arrow(), color="black") +
#   scale_linetype_manual("rho adjustment", values = c("rho adjustment"=1), name="") +
#   expand_limits(y=0) +
#   geom_point(data=filter(roseGBYT, Metric == "Target"), aes(x=SSB, y=F), shape=1, color="black", size=3) +
#   ggtitle("Targets") +
#   theme_bw()
# print(target_plotGBYT)
# ggsave(filename = "..\\figs\\GBYTstatus_target.png", target_plotGBYT)

f3way_plotGBYT <- ggplot(statusGBYT, aes(x=F, y=reorder(Scenario, desc(Scenario)))) +
  geom_point() +
  facet_wrap(~ Metric) +
  ylab("") +
  theme_bw()
print(f3way_plotGBYT)
ggsave(filename = "..\\figs\\GBYT_F3way.png", f3way_plotGBYT)

ssb3way_plotGBYT <- ggplot(statusGBYT, aes(x=SSB, y=reorder(Scenario, desc(Scenario)))) +
  geom_point() +
  facet_wrap(~ Metric, scales = "free_x") +
  ylab("") +
  theme_bw()
print(ssb3way_plotGBYT)
ggsave(filename = "..\\figs\\GBYT_SSB3way.png", ssb3way_plotGBYT)

statusWH <- read.csv(file = "..\\saved\\WHstatus.csv")
statusWH <- statusWH %>%
  transform(Metric = factor(Metric, levels = c("Terminal", "Target", "Ratio")))
orig.statusWH <- filter(statusWH, substr(Scenario, 1, 4) == "orig")
roseWH <- filter(statusWH, Scenario == "Rose")

ratio_plotWH <- ggplot(filter(statusWH, Metric == "Ratio"), aes(x = SSB, y = F, color = Scenario)) +
  geom_point() +
  geom_segment(data=filter(orig.statusWH, Metric == "Ratio"), aes(x=SSB[1], y=F[1], xend=SSB[2], yend=F[2], linetype="rho adjustment"), arrow = arrow(), color="black") +
  scale_linetype_manual("rho adjustment", values = c("rho adjustment"=1), name="") +
  geom_point(data=filter(roseWH, Metric == "Ratio"), aes(x=SSB, y=F), shape=1, color="black", size=3) +
  xlab("SSB/SSBtarget") +
  ylab("F/Ftarget") +
  ggtitle("Status") +
  theme_bw() + 
  theme(legend.position = "none")
print(ratio_plotWH)
ggsave(filename = "..\\figs\\WHstatus_ratio.png", ratio_plotWH)

# # terminal and target plots not shown in paper, just uncomment to see
# term_plotWH <- ggplot(filter(statusWH, Metric == "Terminal"), aes(x = SSB, y = F, color = Scenario)) +
#   geom_point() +
#   geom_segment(data=filter(orig.statusWH, Metric == "Terminal"), aes(x=SSB[1], y=F[1], xend=SSB[2], yend=F[2], linetype="rho adjustment"), arrow = arrow(), color="black") +
#   scale_linetype_manual("rho adjustment", values = c("rho adjustment"=1), name="") +
#   geom_point(data=filter(roseWH, Metric == "Terminal"), aes(x=SSB, y=F), shape=1, color="black", size=3) +
#   ggtitle("Terminal Year") +
#   theme_bw() + 
#   theme(legend.position = "none")
# print(term_plotWH)
# ggsave(filename = "..\\figs\\WHstatus_terminal.png", term_plotWH)
# 
# target_plotWH <- ggplot(filter(statusWH, Metric == "Target"), aes(x = SSB, y = F, color = Scenario)) +
#   geom_point() +
#   geom_segment(data=filter(orig.statusWH, Metric == "Target"), aes(x=SSB[1], y=F[1], xend=SSB[2], yend=F[2], linetype="rho adjustment"), arrow = arrow(), color="black") +
#   scale_linetype_manual("rho adjustment", values = c("rho adjustment"=1), name="") +
#   expand_limits(y=0) +
#   geom_point(data=filter(roseWH, Metric == "Target"), aes(x=SSB, y=F), shape=1, color="black", size=3) +
#   ggtitle("Targets") +
#   theme_bw() + 
#   theme(legend.position = "none")
# print(target_plotWH)
# ggsave(filename = "..\\figs\\WHstatus_target.png", target_plotWH)

f3way_plotWH <- ggplot(statusWH, aes(x=F, y=reorder(Scenario, desc(Scenario)))) +
  geom_point() +
  facet_wrap(~ Metric, scales = "free_x") +
  ylab("") +
  theme_bw()
print(f3way_plotWH)
ggsave(filename = "..\\figs\\WH_F3way.png", f3way_plotWH)

ssb3way_plotWH <- ggplot(statusWH, aes(x=SSB, y=reorder(Scenario, desc(Scenario)))) +
  geom_point() +
  facet_wrap(~ Metric, scales = "free_x") +
  ylab("") +
  theme_bw()
print(ssb3way_plotWH)
ggsave(filename = "..\\figs\\WH_SSB3way.png", ssb3way_plotWH)


# catch plots -------------------------------------------------------------
catchGBYT <- read.csv("..\\saved\\GBYTcatch_advice.csv")
catch_plotGBYT <- ggplot(catchGBYT, aes(x=Catch, y=reorder(Scenario, desc(Scenario)))) +
  geom_point() +
  facet_wrap(~Year) +
  ylab("") +
  theme_bw() +
  theme(legend.position = "none")
print(catch_plotGBYT)
ggsave(filename = "..\\figs\\GBYTcatch_advice.png", catch_plotGBYT)

catchWH <- read.csv("..\\saved\\WHcatch_advice.csv")
catch_plotWH <- ggplot(catchWH, aes(x=Catch, y=reorder(Scenario, desc(Scenario)))) +
  geom_point() +
  expand_limits(x = 0) +
  facet_wrap(~Year) +
  ylab("") +
  theme_bw() +
  theme(legend.position = "none")
print(catch_plotWH)
ggsave(filename = "..\\figs\\WHcatch_advice.png", catch_plotWH)


