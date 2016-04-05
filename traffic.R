library(dplyr)
library(ggplot2)

traffic.data <- read.csv("~/Desktop/DataScience/traffic-data.csv", stringsAsFactors = FALSE)
class(traffic.data)
dim(traffic.data)
names(traffic.data)
str(traffic.data)

glimpse(traffic.data)
summary(traffic.data)
head(traffic.data, 50)
tail(traffic.data, 100)
hist(traffic.data$V29)
summary(traffic.data)
traffic.data = traffic.data[-c(2:14)]
traffic.data$rd.img <- NULL
traffic.data$rd.cl <- NULL
traffic.data$rd.rp.rpImg <- NULL
traffic.data$rd.rp.img <- NULL
traffic.data$rd.rp.type <- NULL
summary(traffic.data)
dim(traffic.data)
names(traffic.data)
table(traffic.data$rd.new)
table(traffic.data$rd.stid)
table(traffic.data$rd.rp.stid)
table(traffic.data$rd.rp.cm)
traffic.frame <- data.frame(traffic.data)
traffic.data <- traffic.data[!duplicated(traffic.data$rd.rp.cmid),]
traffic.data %>% dim()
traffic.frame %>% dim()


