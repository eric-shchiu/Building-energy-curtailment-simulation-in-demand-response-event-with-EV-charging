#### SETUP ####

# use pacman
require(pacman)

# load libraries
pacman::p_load(tidyverse, lubridate, here, scales, slider, patchwork, qpcR, # general
               broom, ggpmisc, ggpubr, # linear models
               RMV2.0, # TOWT model
               overlapping, gridExtra, # distribution overlapping
               sprtt, effsize) # sequential testing

# turn off scientific notation
options(scipen = 999)

# set directory
here::i_am("building_energy.Rproj")

# set default theme for ggplot
theme_set(theme_minimal())

# define base ggplot theme
theme_update(plot.title = element_text(size = 14, colour = "grey20", face = "bold", hjust = 0.5),
             plot.subtitle = element_text(size = 10, colour = "grey20", face = "italic", hjust = 0.5, margin = margin(b = 10)),
             plot.caption = element_text(size = 8, colour = "grey20", face = "italic", hjust = 0.5),
             plot.background = element_rect(fill = "white", colour = NA),
             panel.grid.minor = element_blank(),
             panel.grid.major = element_blank(),
             strip.text = element_text(size = 10, color = "grey20", face = "bold"),
             strip.background = element_blank())

#### DATA PREP ####

#load dataset
df_all <- read_rds(here("df_all.rds"))

# make 15-min averages
df_15 <- df_all %>%
  group_by(building, 
           datetime = floor_date(datetime, unit = "15 mins")) %>%
  summarise(strategy = median(strategy),
            power = mean(power, na.rm = TRUE),
            co2 = mean(co2, na.rm = TRUE),
            moer = mean(moer, na.rm = TRUE),
            mercury = mean(mercury, na.rm = TRUE),
            mmoer = mean(mmoer, na.rm = TRUE),
            t_out = mean(t_out, na.rm = TRUE)) %>%
  ungroup()

# make daily totals
df_daily <- df_15 %>%
  group_by(building, 
           datetime = floor_date(datetime, unit = "day")) %>%
  summarise(strategy = median(strategy),
            power_ave = mean(power, na.rm = TRUE),
            power_peak = max(power, na.rm = TRUE), # use 15-min data to get peak power
            co2 = mean(co2, na.rm = TRUE),
            moer = mean(moer, na.rm = TRUE),
            mercury = mean(mercury, na.rm = TRUE),
            mMoer = mean(mmoer, na.rm = TRUE),
            t_out = mean(t_out, na.rm = TRUE),
            t_out_min = min(t_out, na.rm = TRUE),
            t_out_max = max(t_out, na.rm = TRUE)) %>%
  ungroup()


#### OUTDOOR TEMPERATURE ####


# load tmy file and clean
df_tmy <- read_csv(here("../weather", "weather.csv")) %>%
  mutate(time = ymd_hms(datetime_UTC, tz = "UTC"),
         eload = NA,
         temp = t_out*(9/5) + 32) %>%
  dplyr::select(time, temp, eload)

#### TOWT MODEL ####

## prepare data Time Of Week Temperature Model
df_towt <- df_15 %>%
  mutate(date = date(datetime),
         time = format(datetime,format = "%Y%m%d %H:%M"),
         day = wday(datetime, week_start = 1),
         hour = hour(datetime),
         tow = ((day-1) * 24) + hour,
         strategy = factor(strategy),
         t_out = replace(t_out, is.nan(t_out), NA)) %>%
  # Convert NaN to NA
  dplyr::select(time, 
                strategy,
                # strategy = 2, 
                eload = power,
                temp = t_out)

# interpolate missing values
# Apply approximation for NA across multiple columns
df_towt <- df_towt %>%
  mutate(across(c(eload, temp), ~ zoo::na.approx(., na.rm = FALSE)),
         time = ymd_hm(time)) %>%
  # filter(time < ymd('2022-12-31')) %>%
  mutate(temp = temp*(9/5) + 32)


# make model training samples
model_train <- df_towt %>%
  filter(strategy != 0)

# re-sampling model train
source("resample.R")

post_model_train <- model_train %>%
  resample(., 1500)

# make baseline model training dataset
baseline_train <- post_model_train %>%
  filter(strategy == 1) %>%
  arrange(time) %>%
  dplyr::select(-strategy)


# Specifying model input information
model_input_options <- nmecr::assign_model_inputs(regression_type = "TOWT", occupancy_threshold = 1)

# do baseline model
base <- nmecr::model_with_TOWT(training_data = baseline_train,
                                    model_input_options = model_input_options)

source("model_pred.R")

baseline_results <- model_pred(model = base$model_unoccupied, 
                               time = df_tmy$time, 
                               temp = df_tmy$temp,
                               temp_knots = base$model_input_options$calculated_temp_knots, 
                               dataframe = df_tmy) %>%
  rename("time" = "dframe.time",
         "eload" = "dframe.eload")

baseline_results %>%
  ggplot(., aes(x = time)) +
  geom_line(aes(y = towt, color = "towt"), alpha = 1) +
  scale_y_continuous(breaks = seq(0, 150, by = 50), labels = c("0", "50", "100", "150kW")) +
  scale_color_brewer(palette = "Set3") +
  labs(x = NULL, 
       y = NULL, 
       color = NULL,
       title = "TOWT model prediction of annual building energy consumption") +
  theme(panel.grid.major.y = element_line())

ggsave(here("TOWT_estimation.png"), width = 2560, height = 1440, units = "px", dpi = 300)

write_csv(baseline_results, here("building_energy.csv"))