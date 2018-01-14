# bmi trajectories among the severely obese
# only keep these ids
data <- data4 %>% group_by(ID) %>% filter(max(BMI) >= 35) # 1617 unique
data <- arrange(data, ID, date)
data2 <- filter(data, BMI >= 35)
data2 <- distinct(data2, ID, .keep_all = TRUE) # filters for first obese weigh in
data2 <- data2 %>% select(ID, date, BMI)
data2 <- rename(data2, start_date = date, baseline_bmi = BMI)
data3 <- left_join(data, data2, by="ID")

# Time from initial obesity to end date
# find end date
data3 <- arrange(data3, ID, desc(date))
# keep end date and end bmi
data4 <- distinct(data3, ID, .keep_all = TRUE)
data4 <- rename(data4, end_date = date, end_bmi = BMI)
data4 <- select(data4, ID, end_date, end_bmi)
data.use <- left_join(data3, data4, by="ID") 
data.use <- data.use %>% rename(PatientNo = ID)


# What percentage of people are not obese at the end?
#z <- data3 %>% group_by(ID) %>% filter(end_bmi < 35)
#arrange/sort by date
#sort by id

lm_data <- distinct(data.use, id, .keep_all=TRUE)
lm <- lm(end_bmi ~ age + gender + bmi_baseline, data=lm_data)
summary(lm)

# logistic model
lm_data$obese <- ifelse(lm_data$end_bmi < 35,c("healthy"), c("obese"))

# What percentage are obese
# What percentage that become obese leave obesity
# What percentage that leave come back again