
# download demographics data 
# demo_data <- read.table("Demographics_20170307.txt", header=FALSE,sep="\t",fill=NA)
# demo_data$V1 <- as.numeric(demo_data$V1) # 1,006,892 observations
demo_data <- read.table("Demographics_20171130.txt", header=TRUE, sep="\t",fill=NA) # 3,961,126 observations
# remove so only birthday and sex
#demo_data <- demo_data %>% select(PatientNo, DOB, Sex)
# merge demographics data with visits data
full_data <- left_join(data.use, demo_data, by="PatientNo")
# make age variable
full_data$DOB <- as.Date(full_data$DOB)
full_data$DOB <- ymd(full_data$DOB)
full_data <- full_data %>% mutate(age = year(start_date) - year(DOB)) # crude age variable
full_data <- full_data %>% filter(baseline_bmi < 75)

lm_data <- distinct(full_data, PatientNo, .keep_all=TRUE)
lm_data <- lm_data %>% filter(age >= 18)
lm <- lm(end_bmi ~ age + Sex + baseline_bmi, data=lm_data) # regress end bmi on baseline bmi age and sex
# Age variable consistent with findings even though literature doesn't generally include younger adults (grouphealth cutoff =35)
# baseline_bmi shouldn't have a positive coefficient, not consistent with findings
# 42.12% that are at one point obese end the study not obese. That makes no sense

lm_data$obese <- ifelse(lm_data$end_bmi < 35,0, 1)
lm_data$obese <- as.numeric(lm_data$obese)
model <- glm (obese ~ Sex + baseline_bmi + age, data = lm_data, family = binomial)
summary(model)
require(MASS)
exp(cbind(OR=coef(model), confint(model)))
# lower OR = less likely to stay obese
# 1 is obese at end of study, 0 is healthy
# Being Male Reduces your chance of being obese by 25%
# first_bmi measurement is significantly associated with obesity (may be because of data quality issues)
  # not significant when you filter out baseline_bmi > 75
# the older you are the less likely you are to stay obese



