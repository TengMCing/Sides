library(bomrang)
library(tidyverse)
library(lubridate)

mel_air_rain <- get_historical_weather(stationid = "086282", type = "rain")
mel_air_maxt <- get_historical_weather(stationid = "086282", type = "max")
mel_air_mint <- get_historical_weather(stationid = "086282", type = "min")
mel_air_sol <- get_historical_weather(stationid = "086282", type = "solar")

mel_air_rain <- select(mel_air_rain, -quality, -product_code)
mel_air_maxt <- select(mel_air_maxt, -quality, -product_code)
mel_air_mint <- select(mel_air_mint, -quality, -product_code)
mel_air_sol <- select(mel_air_sol, -product_code)

full_join(mel_air_rain, mel_air_maxt, by = c("year",
                                             "month",
                                             "day",
                                             "station_number")) %>%
  full_join(mel_air_mint, by = c("year",
                                 "month",
                                 "day",
                                 "station_number")) %>%
  full_join(mel_air_sol, by = c("year",
                                "month",
                                "day",
                                "station_number")) -> mel_air

mel_air <- filter(mel_air, year >= 2000) %>%
  mutate(date = make_date(year = year, month = month, day = day)) %>%
  mutate(date2 = as.Date(yday(date), origin = "1970-01-01")) %>%
  mutate(midt = (max_temperature + min_temperature)/2) %>%
  filter((year != 2021) | (day != 18)) %>%
  group_by(month, day) %>%
  mutate(min_min = min(min_temperature),
         min_max = min(max_temperature),
         max_min = max(min_temperature),
         max_max = max(max_temperature),
         max_solar = max(solar_exposure, na.rm = T),
         min_solar = min(solar_exposure, na.rm = T),
         max_rain = max(rainfall, na.rm = T),
         min_rain = min(rainfall, na.rm = T)) %>%
  ungroup()


ggplot() +
  geom_ribbon(data = filter(mel_air, month == 1, year == 2020),
              aes(date2, ymin = min_solar, ymax = max_solar, fill = "Since 2000"),
              alpha = 0.4, col = "#0571b0") +
  geom_line(data = filter(mel_air, month == 1, year == 2021),
            aes(date2, solar_exposure, col = "2021")) +
  theme_minimal() +
  scale_x_date(limits = as.Date(c("1970-01-01", "1970-01-31")),
               date_labels = "%d") +
  scale_color_manual(values = "#ca0020") +
  scale_fill_manual(values = "#0571b0") +
  theme(legend.position = "bottom") +
  labs(col = "", fill = "", y = "Solar Exposure", x = "Jan", title = "Melbourne") -> p1


ggplot() +
  geom_ribbon(data = filter(mel_air, month == 1, year == 2020),
              aes(date2, ymin = min_rain, ymax = max_rain, fill = "Since 2000"),
              alpha = 0.4) +
  geom_line(data = filter(mel_air, month == 1, year == 2021),
            aes(date2, rainfall, col = "2021")) +
  theme_bw() +
  scale_x_date(limits = as.Date(c("1970-01-01", "1970-01-31")),
               date_labels = "%d") +
  scale_color_manual(values = "#ca0020") +
  scale_fill_manual(values = "#0571b0") +
  theme(legend.position = "bottom") +
  labs(col = "", fill = "", y = "Rainfall", x = "Jan", title = "Melbourne") -> p2


ggplot() +
  geom_ribbon(data = filter(mel_air, month == 1, year == 2020),
              aes(date2, ymin = min_min, ymax = max_min, fill = "Since 2000"),
              alpha = 0.4) +
  geom_line(data = filter(mel_air, month == 1, year == 2021),
            aes(date2, min_temperature, col = "2021")) +
  theme_bw() +
  scale_x_date(limits = as.Date(c("1970-01-01", "1970-01-31")),
               date_labels = "%d") +
  scale_color_manual(values = "#ca0020") +
  scale_fill_manual(values = "#0571b0") +
  theme(legend.position = "bottom") +
  labs(col = "", fill = "", y = "Min. Temperature", x = "Jan", title = "Melbourne") -> p3


ggplot() +
  geom_ribbon(data = filter(mel_air, month == 1, year == 2020),
              aes(date2, ymin = min_max, ymax = max_max, fill = "Since 2000"),
              alpha = 0.4) +
  geom_line(data = filter(mel_air, month == 1, year == 2021),
            aes(date2, max_temperature, col = "2021")) +
  theme_bw() +
  scale_x_date(limits = as.Date(c("1970-01-01", "1970-01-31")),
               date_labels = "%d") +
  scale_color_manual(values = "#ca0020") +
  scale_fill_manual(values = "#0571b0") +
  theme(legend.position = "bottom") +
  labs(col = "", fill = "", y = "Max. Temperature", x = "Jan", title = "Melbourne") -> p4

ggsave(plot = p1, filename = "solar.png", dpi = 900)
ggsave(plot = p2, filename = "rain.png", dpi = 900)
ggsave(plot = p3, filename = "min.png", dpi = 900)
ggsave(plot = p4, filename = "max.png", dpi = 900)
