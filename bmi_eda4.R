# Logistic regression of obesity 
logit_data <- logit_data %>% rename(PatientNo =ID)
logit_data2 <- left_join(demo_data, logit_data, by="PatientNo")
# How do you predict whether someone will become obese, do you take their first bmi measurement
full_data <- left_join(data.use, demo_data, by="PatientNo")
# make age variable
full_data$DOB <- as.Date(full_data$DOB)
full_data$DOB <- ymd(full_data$DOB)
full_data$start_date <- ymd(full_data$start_date)
full_data <- full_data %>% mutate(age = year(start_date) - year(DOB)) # crude age variable
full_data <- full_data %>% filter(baseline_bmi < 75)

# Create binary variable for max bmi
# subset to top zipcodes
