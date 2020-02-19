# gbyt_step4.R
# compare Rose and rho-adjusted to expanded survey biomass estimates

# set working directory to source file location

# if needed
library(dplyr)
library(ggplot2)

# get the expanded survey data
surveyb <- read.csv("..\\data\\surveyB.csv", header = TRUE) %>%
  mutate(surveyB = (DFO + Spring + Fall_lag) / 3,
         surveyBcv = (CVDFO + CVSpring + CVFall_lag) / 3)
surveyb <- surveyb %>%
  mutate(surveyBlow = surveyB * (1.0 - 1.96 * surveyBcv),
         surveyBhigh = surveyB * (1.0 + 1.96 * surveyBcv)) %>%
  filter(Year <= 2018)

# get SSB time series
ts.df <- read.csv("..\\saved\\time_seriesGBYT.csv", header = TRUE) %>%
  transform(ramplab = factor(ramplab, levels = c("Sudden", "Ramp4", "Ramp9")))

# get orig run
orig <- dget("..\\saved\\GBYT_000.rdat")
orig.df <- data.frame(Year = seq(orig$parms$styr, orig$parms$endyr),
                      SSB = orig$SSB)

# rho-adjust orig run
orig.rho.table <- read.csv("..\\saved\\Retro.rho.values_GBYT_000.csv", header = TRUE)
orig.rho.SSB <- orig.rho.table$ssb.rho[orig.rho.table$X == "Mohn.rho"]

orig_SSB_2018 <- filter(orig.df, Year == 2018) %>%
  select(SSB)

rhoadj <- data.frame(Year = 2018,
                     SSB = orig_SSB_2018 / (1 + orig.rho.SSB))

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
  theme_bw()
print(ts_ssb_survey_plot)
ggsave(filename = "..\\figs\\GBYTcompare_survey12.png", ts_ssb_survey_plot)

# now just Rose SSB
rose_ssb <- ts.df %>%
  group_by(Year) %>%
  summarize(roseSSB = mean(SSB))

ts_rose_ssb_survey_plot <- ggplot(filter(rose_ssb, Year >= 2010), aes(x=Year)) +
  geom_point(aes(y=roseSSB)) +
  geom_line(aes(y=roseSSB)) +
  geom_line(data=surveyb, aes(x=Year, y=surveyB), color = "blue") +
  geom_ribbon(data=surveyb, aes(x=Year, ymin=surveyBlow, ymax=surveyBhigh), fill = "blue", alpha=0.3) +
  geom_line(data=filter(orig.df, Year >= 2010), aes(x=Year, y=SSB), color = "red") +
  geom_point(data=rhoadj, aes(x=Year, y=SSB), color = "red", alpha = 0.6) +
  ylab("SSB or Survey Biomass") +
  theme_bw()
print(ts_rose_ssb_survey_plot)
ggsave(filename = "..\\figs\\GBYTcompare_survey_rose.png", ts_rose_ssb_survey_plot)
