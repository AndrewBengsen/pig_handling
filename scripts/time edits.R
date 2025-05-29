library(tidyverse)

x <- read.csv("Combined feral pig data_SD_20250328.csv", header = T) %>%
  filter(Capture.method == "Manual") %>%
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y %H:%M:%S"))

x$Date[which(x$Date == "2023-09-21 00:00:00 AEST")] <- "2023-09-21 06:10:00 AEST"
x$ymdhms <- ymd_hms(x$Date, tz = "Australia/Sydney")

x$Date
x$ymdhms

x$restraint <- hms(x$Time.of.restraint)
x$restraint

x$release <- hms(x$Time.of.release)

x$t_restraint <- x$ymdhms + x$restraint
x$t_release <- x$ymdhms + x$release

x$t_handling <- (x$t_release - x$t_restraint) / 60
x$t_total <- (x$t_release - x$Date)

x <- x %>%
  select(ID, Date, restraint, t_restraint, release, t_release, t_handling, t_total)

write.csv(x, "edited_times.csv")
