## EDA
library(tidyverse)

x <- read.csv("Combined feral pig data_SD_20250328 AB.csv", header = T) %>%
  janitor::clean_names()%>%
  mutate(date = as.POSIXct(date, format = "%d/%m/%Y %H:%M:%S"),
         ambient_temp = as.numeric(ambient_temp),
         temp_body = as.numeric(temp_body),
         hyperthermic = 0) 

x$hyperthermic[which(x$temp_body >=40)] <- 1
x$date[which(x$date == "2023-09-21 00:00:00 AEST")] <- "2023-09-21 06:10:00 AEST"

x$handling_duration_min <- as.numeric(x$handling_duration_min)
x$total_duration_min <- as.numeric(x$total_duration_min)

x <- x %>%
  select(-name, -state, -x, -notes_on_collar_recovery, -fate, -collar_data, 
         -collar_in_hand, -collar_end_date, -tags_in_trap, -last_usable_fix)

ggplot(data=x) +
  geom_boxplot(aes(x=capture_method, y=handling_duration_min))

ggplot(data=x) +
  geom_boxplot(aes(x=capture_method, y=total_duration_min))

x$rev_drugs <- NA
x$dose_yohimbine_mg <- as.numeric(x$dose_yohimbine_mg)
x$dose_atipamezole_mg <- as.numeric(x$dose_atipamezole_mg)
x$rev_drugs[which(is.na(x$dose_yohimbine_mg) == F)] <- "yohimbine"
x$rev_drugs[which(!is.na(x$dose_atipamezole_mg))] <- "atipamezole"

table(x$drug_s, x$capture_method)
table(x$rev_drugs)
length(which(x$drug_s == "Zoletil/Xylazine" & is.na(x$rev_drugs)))

ggplot(data=x) +
  geom_point(aes(x=ambient_temp, y=temp_body, colour=capture_method))# +
  facet_wrap(~capture_method)
  
ggplot(data=x) +
  geom_boxplot(aes(x=capture_method, y=temp_body))
  
summary(lm(temp_body ~ capture_method + ambient_temp, data=x))
summary(glm(hyperthermic ~ capture_method, family = "binomial", data=x))



