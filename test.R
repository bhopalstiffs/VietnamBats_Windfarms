
# setup -------------------------------------------------------------------

library(tidyverse) # data management
library(scales)
library(brms) # data analysis
library(cmdstanr) # interface for Stan
library(tidybayes) # generate data for visualization of results
library(modelr) # generate data for predictions
library(patchwork) # format figures for publication

# set wd and paths --------------------------------------------------------

setwd("~/VietnamBats_Windfarms")
options(mc.cores = parallel::detectCores())
set_cmdstan_path("C:/Users/AHitch/.cmdstan/cmdstan-2.35.0")

# load data ---------------------------------------------------------------

dt <- read_csv("data/act_data.csv",
               col_types = cols(Date_Time =
                                  col_datetime("%m/%d/%Y %H:%M")))

wind <- read_csv("data/wind_data.csv") 

# data prep ---------------------------------------------------------------

# wind data prep ----------------------------------------------------------

wind_no_header <- wind[-1, ]

first_three <- wind_no_header %>%
  select(1:3)

# Select columns 4-6, 7-9, 10-12
cols_4_6 <- wind_no_header %>% 
  select(4:6)
cols_7_9 <- wind_no_header %>% 
  select(7:9)
cols_10_12 <- wind_no_header %>%
  select(10:12)

wind1 <- bind_rows(first_three, 
                   setNames(cols_4_6, names(first_three)),
                   setNames(cols_7_9, names(first_three)),
                   setNames(cols_10_12, names(first_three)))

wind2 <- wind1 %>% 
  separate(`ST.Date.H`, into = c("Location", "Date_Time"), sep = "\\.") %>%
  separate(Date_Time, into = c("Date", "Time"), sep = " ") %>%
  mutate(Time = ifelse(is.na(Time), "00:00:00", Time)) %>%
  mutate(Date_Time = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S")) %>% 
  rename(act = `A_Occurrence`,
         Wind.sp = `A_Win.speed`) %>%
  select(Location, Date_Time, Wind.sp, act)

wind3 <- wind2 %>% 
  select(Location, Date_Time, Wind.sp)

# missing rows ------------------------------------------------------------

dt %>% 
  rename(Wind.sp = `Win.speed`, Wind.di = `Wind_Direction`, Temp = Temperature,
         Rain = `Rain_Actual`, act = `Bat_Occurrence`) %>%
  mutate(across(c(Wind.sp, Temp, Rain),  .names = "scale_{.col}", 
                ~scale(.) %>% as.vector)) %>%
  mutate(Date = date(Date_Time)) %>%
  mutate(Month = month(Date, label = TRUE, abbr = TRUE),
         MonthFull = month(Date, label = TRUE, abbr = FALSE),
         Year = year(Date), Hour = hour(Date_Time)) %>% 
  mutate(Location = factor(Location)) %>% 
  mutate(HourF = factor(Hour)) %>% 
  unite(MonthYear, MonthFull, Year, sep = " ") %>% 
  mutate(MonthYear = factor(MonthYear)) %>%
  mutate(jdate = yday(Date_Time)) %>% 
  mutate(hour.ord = case_match(Hour, 
                               16 ~ 1,
                               17 ~ 2,
                               18 ~ 3,
                               19 ~ 4,
                               20 ~ 5,
                               21 ~ 6,
                               22 ~ 7,
                               23 ~ 8,
                               0 ~ 9,
                               1 ~ 10,
                               2 ~ 11,
                               3 ~ 12,
                               4 ~ 13,
                               5 ~ 14,
                               6 ~ 15)) %>%
  mutate(LocationVN = case_when(
    Location == "SM4-A" ~ "WTG14",
    Location == "SM4-B" ~ "WTG20",
    Location == "SM4-C" ~ "WTG07",
    Location == "SM4-D" ~ "WTG10")) %>%
  mutate(LocationVN = factor(LocationVN)) %>%
  na.omit() -> dt1


# add missing rows --------------------------------------------------------

dt %>% 
  rename(Wind.sp = `Win.speed`, Wind.di = `Wind_Direction`, Temp = Temperature,
         Rain = `Rain_Actual`, act = `Bat_Occurrence`) %>%
  select(Location, Date_Time, act, Wind.sp, Temp, Rain) %>%
  mutate(across(c(Wind.sp, Temp, Rain),  .names = "scale_{.col}", 
                ~scale(.) %>% as.vector)) -> dat1

df_grid <- expand.grid(Location = unique(dat1$Location), 
                       Date_Time = unique(dat1$Date_Time))

dat2 <- df_grid %>% 
  left_join(dat1, by = c("Location", "Date_Time")) %>%
  mutate(act = ifelse(is.na(act), 0, act)) %>% 
  mutate(scale_Wind.sp = ifelse(is.na(scale_Wind.sp), NA, scale_Wind.sp)) %>%
  mutate(scale_Rain = ifelse(is.na(scale_Rain), NA, scale_Rain)) %>%
  mutate(scale_Temp = ifelse(is.na(scale_Temp), NA, scale_Temp)) %>% 
  mutate(Hour = hour(Date_Time)) %>% 
  mutate(hour.ord = case_match(Hour, 
                               16 ~ 1,
                               17 ~ 2,
                               18 ~ 3,
                               19 ~ 4,
                               20 ~ 5,
                               21 ~ 6,
                               22 ~ 7,
                               23 ~ 8,
                               0 ~ 9,
                               1 ~ 10,
                               2 ~ 11,
                               3 ~ 12,
                               4 ~ 13,
                               5 ~ 14,
                               6 ~ 15)) %>%
  mutate(Location = factor(Location)) %>%
  filter(hour.ord > 1 & hour.ord < 15)
    

# fill in missing wind values ---------------------------------------------

wind4 <- dat2 %>%
    select(Location, Date_Time, Wind.sp) %>%
    left_join(wind3, by = c("Location", "Date_Time"), suffix = c("_missing", "_filled")) %>%
    mutate(Wind.sp = ifelse(is.na(Wind.sp_missing), Wind.sp_filled, Wind.sp_missing)) %>%
    select(-Wind.sp_missing, -Wind.sp_filled)


# combine data sets -------------------------------------------------------

dat3 <- dat2 %>%
    left_join(wind4, by = c("Location", "Date_Time")) %>% 
    select(-ends_with(".x")) %>% 
    rename(Wind.sp = Wind.sp.y) %>%
    select(-scale_Wind.sp) %>% 
    mutate(across(c(Wind.sp),  .names = "scale_{.col}", 
                  ~scale(.) %>% as.vector)) %>% 
    mutate(Month = month(Date_Time, label = TRUE, abbr = TRUE),
           MonthFull = month(Date_Time, label = TRUE, abbr = FALSE),
           Year = year(Date_Time)) %>% 
    mutate(Date = factor(format(as.Date(Date_Time), "%m-%d"),
                              levels = format(seq(as.Date("2023-09-01"), 
                                                  as.Date("2024-08-31"), 
                                                  by = "1 day"), "%m-%d"))) %>% 
    unite(MonthYear, MonthFull, Year, sep = " ") %>% 
    mutate(MonthYear = factor(MonthYear))

# check wind data for first three hours -----------------------------------

data$MonthYear <- fct_relevel(dat3$MonthYear, "September 2023", "October 2023", "November 2023", 
                             "December 2023", "January 2024", "February 2024", "March 2024",
                             "April 2024", "May 2024", "June 2024", "July 2024", "August 2024")
data %>% 
  filter(hour.ord < 5) %>%
  group_by(MonthYear, Date) %>%
  summarise(mean_wind = mean(scale_Wind.sp),
            se_wind = sd(scale_Wind.sp, na.rm = TRUE) / sqrt(sum(!is.na(scale_Wind.sp))),
            .groups = "drop") %>%
  ggplot(aes(x = Date, y = mean_wind)) +
  geom_point(size = 0.75) +
  geom_errorbar(aes(ymin = mean_wind - se_wind, ymax = mean_wind + se_wind), 
                  width = 0.2) +
  facet_wrap(~MonthYear, scales = "free_x") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  labs(y = "Scaled Mean Wind Speed (m/s)")


# format data for wind activity prediction --------------------------------

dt1 %>% 
  filter(hour.ord >= 2, hour.ord <= 5) %>%
  group_by(Date) %>%
  summarise(mean_wind = mean(scale_Wind.sp),
            .groups = "drop") -> dusk_wind

dt1 %>%
  filter(hour.ord >= 6, hour.ord <= 9) %>%
  group_by(Date) %>%
  summarise(mean_act = mean(act),
            .groups = "drop") -> night_act

dt1 %>% 
  filter(hour.ord >= 2, hour.ord <= 5) %>%
  select(Date, Date_Time, MonthYear, LocationVN, hour.ord, 
         scale_Wind.sp, Wind.sp) -> dusk_wind

dt1 %>%
  filter(hour.ord >= 6, hour.ord <= 9) %>%
  select(Date, Date_Time, LocationVN, act) -> night_act

df <- left_join(dusk_wind, night_act, by = c("Date", "LocationVN"))

# set priors --------------------------------------------------------------
  
model.prior <- c(
  set_prior("normal(0, 1)", class = "Intercept"),
  set_prior("normal(0, 1)", class = "b"),
  # set_prior("student_t(3, 0, 1)", class = "sd"),
  set_prior("inv_gamma(0.4, 0.3)", class = "shape"))

# fit models --------------------------------------------------------------

m.wind <- brm(data = dat3,
              family = negbinomial(),
              act ~ 1 + scale_Wind.sp + (1 | LocationVN) + (1 | MonthYear),
              # prior = model.prior,
              iter = 1000, warmup = 100, cores = 4, chains = 2,
              control = list(adapt_delta = 0.99, max_treedepth = 15), 
              threads = threading(2))

m.rain <- brm(data = dat3,
            family = negbinomial(),
            act ~ 1 + scale_Rain + (scale_Rain | Location / Month),
            prior = model.prior,
            iter = 1000, warmup = 500, cores = 4, chains = 2, backend = "cmdstanr",
            threads = threading(2))

m.wind.rain.temp <- brm(data = dat3,
            family = negbinomial(),
            act ~ 1 + scale_Wind.sp + scale_Rain + scale_Temp + (1 | Location) + (1| Month) + 
              (1 | Hour),
            iter = 2000, warmup = 500, cores = 4, chains = 4, backend = "cmdstanr",
            control = list(adapt_delta = 0.99, max_treedepth = 15),
            threads = threading(2))

m.dusk.s <- brm(data = df,
          family = negbinomial(),
          act ~ 1 + s(scale_Wind.sp) + (1 | LocationVN) + (1 | MonthYear),
          iter = 1000, warmup = 250, cores = 4, chains = 2,
          control = list(adapt_delta = 0.99, max_treedepth = 15), 
          threads = threading(2))


# plot results ------------------------------------------------------------

windfig <- dt1 %>% 
  modelr::data_grid(scale_Wind.sp = seq_range(Wind.sp, n =100), LocationVN, MonthYear) %>%
  add_epred_draws(m.wind, dpar = TRUE, category = "act") %>%
  ggplot(aes(x = scale_Wind.sp, y = .epred)) +
  stat_lineribbon(mapping = aes(y = .epred), .width = 0.95, alpha = 0.5, color = "blue") +
  scale_fill_manual(values = c("gray80")) +
  geom_hline(yintercept = c(3,6,13,31), linetype = "dashed") +
  scale_x_continuous(
    name = "Wind Speed (m/s)",
    limits = c(0, 10),
    labels = scales::label_number(accuracy = 1)) +
  theme(legend.position = "none") +
  labs(y = "Hourly Bat Detections")

ggsave("~/VietBats/figs/windfig.PNG", windfig, width = 6.17, height = 3.34,
       units = "in", device = 'png', dpi = 300)

wind.gam <- df %>% 
  modelr::data_grid(scale_Wind.sp = seq_range(Wind.sp, n =100), LocationVN, MonthYear) %>%
  add_epred_draws(m.dusk.s, dpar = TRUE, category = "act") %>%
  ggplot(aes(x = scale_Wind.sp, y = .epred)) +
  stat_lineribbon(mapping = aes(y = .epred), .width = 0.95, alpha = 0.5, color = "blue") +
  scale_fill_manual(values = c("gray80")) +
  geom_hline(yintercept = c(3,6,13,31), linetype = "dashed") +
  scale_x_continuous(
    name = "Wind Speed (m/s)",
    limits = c(0, 10),
    labels = scales::label_number(accuracy = 1)) +
  ylim(c(0, 40)) +
  theme(legend.position = "none") +
  labs(y = "Hourly Bat Detections")

ggsave("~/VietBats/figs/windgam.PNG", wind.gam, width = 6.17, height = 3.34,
       units = "in", device = 'png', dpi = 300)

dt1 %>% 
  modelr::data_grid(scale_Rain = seq_range(Rain, n =100), Location,
                    Month) %>%
  add_epred_draws(m.rain, dpar = TRUE, category = "act") %>%
  ggplot(aes(x = scale_Rain, y = .epred)) +
  stat_lineribbon(mapping = aes(y = .epred), .width = 0.95, alpha = 0.5, color = "blue") +
  scale_fill_manual(values = c("gray80")) +
  geom_hline(yintercept = c(3,6,9,13,25,31), linetype = "dashed") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 40, by = 5)) +
  labs(x = "Total Hourly Rain (mm)", y = "Predicted Mean Hourly Bat Activity")


dt1 %>% 
  modelr::data_grid(Temp, Location, Month) %>%
  add_epred_draws(m.wind.rain.temp, dpar = TRUE, category = "act") %>%
  ggplot(aes(x = Temp, y = .epred)) +
  stat_lineribbon(mapping = aes(y = .epred), .width = 0.95, alpha = 0.5, color = "blue") +
  scale_fill_manual(values = c("gray80")) +
  geom_hline(yintercept = c(3,6,9,13,25,31), linetype = "dashed") +
  theme(legend.position = "none") +
  labs(x = "Hourly Temperature (\u00B0C)", y = "Predicted Mean Hourly Bat Activity")




# hour figure by month ----------------------------------------------------

dt1$HourF <- fct_relevel(dt1$HourF, "16", "17", "18", "19", "20", "21", "22", "23", "0", 
            "1", "2", "3", "4", "5", "6")

dt1$MonthYear <- fct_relevel(dt1$MonthYear, "September 2023", "October 2023", "November 2023", 
                             "December 2023", "January 2024", "February 2024", "March 2024",
                             "April 2024", "May 2024", "June 2024", "July 2024", "August 2024")
monthfig <- dt1 %>%
  group_by(LocationVN, HourF, MonthYear) %>% 
  summarise(mean_act = mean(act, na.rm = TRUE),
            sd_act = sd(act, na.rm = TRUE),
            n = n(),
            se_act = sd_act / sqrt(n), .groups = "drop") %>%
  ggplot(aes(x = HourF, y = mean_act, color = LocationVN, group = LocationVN)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_act - se_act, ymax = mean_act + se_act), width = 0.2) +
  geom_hline(yintercept = c(13,31), linetype = "dashed") +
  scale_color_manual(values = c("WTG14" = "#99CCFF", "WTG20" = "orange", "WTG07" = "red",
                                "WTG10" = "green")) +
  scale_x_discrete(
    labels = c("16:00-17:00", "17:00-18:00", "18:00-19:00", "19:00-20:00", "20:00-21:00",
               "21:00-22:00", "22:00-23:00", "23:00-0:00", "0:00-01:00", "01:00-02:00",
               "02:00-03:00", "03:00-04:00", "04:00-05:00", "05:00-06:00", "06:00-07:00")) +
  facet_wrap(~MonthYear, ncol = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.title.x = element_blank(),
        strip.background = element_rect(color = "black", linewidth = 1, linetype = "solid"),
        legend.position = "bottom",
        legend.title = element_blank()) +
  labs(x = "Hour", y = expression(paste("Mean Hourly Bat Detections ± SE")))

ggsave("~/VietBats/figs/monthhour.PNG", monthfig, width = 6.27, height = 5.96,
       units = "in", device = 'png', dpi = 300)
