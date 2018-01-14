cd /data/common/raw_data/
#33752 unique people with a bmi measurement in the first two files
library(dplyr)
library(lubridate)
# Read in File
data <- read.table("bmi_test.txt", header=TRUE, sep="\t")
# Append Count data
data <- data %>% rename(ID = X2, BMI = X35.5)
logit_data <- data
logit_data$obese <- ifelse(logit_data$BMI < 35,c("healthy"), c("obese"))
data2 <- data %>% count(ID)
#33752 unique people with a bmi measurement in the first two files
data3 <- left_join(data, data2, by="ID")
# Subset count data
#data4 <- filter(data3, n > 4)
data4 <- filter(data3, n > 1)
# 5849 unique id's of with 5 or more measurements (49716 est sample)
# find range of dates
data4 <- data4 %>% rename(date = X2015.09.07.07.02.00.000)
data4$date <- ymd_hms(data4$date) # gives you date range in days
# find mean, range of bmi and date
data4$obese <- ifelse(data4$BMI > 35, 1,0)
data4 <- data4 %>% arrange(ID, date)
datab <- distinct(data4, ID, .keep_all = TRUE) 
datab <- datab %>% rename(PatientNo = ID, baseline_bmi = BMI)
datab <- dplyr::select(datab, PatientNo, baseline_bmi)
datac <- data4 %>% arrange(ID, desc(date))
datac <- datac %>% rename(PatientNo = ID, end_bmi = BMI)
data5 <- data4 %>% group_by(ID) %>% summarise(min = min(date), max = max(date), obese = mean(obese))
data5 <- data4 %>% group_by(ID) %>% summarise(n=n(),bmi_mean = mean(BMI), range = max(BMI)-min(BMI), date_range = max(date)-min(date), max_bmi = max(BMI))

data6 <- rename(data5, PatientNo = ID)
data6 <- left_join(data6, datab, by="PatientNo")
data6 <- left_join(data6, datac, by="PatientNo") # 17,412 people

lm <- lm()
# age, sex, baseline_bmi
# start_date, end_date
#median(data5$n)
#mean(data5$range)
#median(data5$date_range)
# median n = 7 (n=number of bmi observations), mean = 9.9
# mean range of bmi = 13.7
# mean date_range is 1026 days, median 855 days (2.34 years)
# setwd("../clean_summ")
# #pdf("bmi_test_densities.pdf")
# # find density of measurements 
# g <- ggplot(data5, aes(x=bmi_mean))+geom_density()
# g
# g <- ggplot(data5, aes(x=date_range))+geom_density()
# g
# #dev.off()