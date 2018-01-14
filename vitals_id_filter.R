# Extract ID's for sample data set
files <- dir()
vitals_files <- files[grep("Vitals", files)]
for(i in vitals_files){
  data <- read.table(i,header=TRUE, sep="\t", fill=NA)
  data2 <- data[grep("Body Mass Index",data$EVENT_CD_DESCR),]
  data2 <- data %>% count(PatientNo)
  data2 <- full_join(data, data2, by="PatientNo")
  data2 <- data2 %>% filter(n > 5)
}

# find way to aggregate id's