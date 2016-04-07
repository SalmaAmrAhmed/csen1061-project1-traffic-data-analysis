library(dplyr)
library(ggplot2)
library(chron)
library(stringi)

##set system to read arabic
Sys.setlocale("LC_ALL", "en_US.UTF-8")

traffic.data <- read.csv("~/Desktop/DataScience/traffic-data.csv", stringsAsFactors = FALSE, fileEncoding="UTF-8")
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

##group all roads
all_roads <- unique(traffic.data$rd.nm)

##check if hours are correlated with each other or not
traffic.data %>% select(crawl_date, rd.hr, rd.rp.hr) %>% head(100)

##check what is the class of time in the system to change the column time to it
class(Sys.time())

##break the crawl_date column and extract time
traffic.data <- traffic.data %>% mutate(crawl_time = gsub("[^0-9]+ \\d+", "\\1", crawl_date))

##break the crawl_date column and extract day
traffic.data$crawl_day <- stri_extract_first_regex(traffic.data$crawl_date, "[0-9]+")
traffic.data$crawl_day <- paste("2016-02-", traffic.data$crawl_day, sep = "")

##convert to date and adjust to EET time 
traffic.data$new_crawl_date <- paste(traffic.data$crawl_day, traffic.data$crawl_time)
traffic.data$new_crawl_date <- as.POSIXct(traffic.data$new_crawl_date,format="%Y-%m-%d %H:%M:%OS", tz = "UTC")
attr(traffic.data$new_crawl_date, "tzone") <- "Egypt"

##extract actual report datetime
traffic.data$actual_report_time <- traffic.data$new_crawl_date - (traffic.data$rd.rp.hr*60*60) - (traffic.data$rd.rp.mn * 60)

##indicate whether report is question(0) about road or not(1)
traffic.data$question <- stri_extract_first_regex(traffic.data$rd.rp.cm, "(\\?) | (\\؟)")
traffic.data$question <- ifelse(grepl("(?) | (؟)", traffic.data$question), 1, 0)
traffic.data$question %>% table()

##extract speed from data
traffic.data$speed <- stri_extract_first_regex(traffic.data$rd.rp.cm, "([0-9]+ كم/س)|([0-9]+ km/h)")
traffic.data$speed <- stri_extract_first_regex(traffic.data$speed, "[0-9]+")
traffic.data$speed <- as.numeric(as.character(traffic.data$speed))

##inspect the data ... etc.
traffic.data %>% filter(rd.rp.cm == "radar") %>% select(rd.stid, rd.rp.stid, rd.rp.cm)
traffic.data %>% filter(rd.rp.cm == "lagna") %>% select(rd.stid, rd.rp.stid, rd.rp.cm)
traffic.data %>% filter(rd.rp.cm == "حادثة") %>% select(rd.stid, rd.rp.stid, rd.rp.cm)
traffic.data %>% filter(rd.rp.cm == "لجنة") %>% select(rd.stid, rd.rp.stid, rd.rp.cm)
traffic.data %>% filter(rd.rp.cm == "accident") %>% select(rd.stid, rd.rp.stid, rd.rp.cm)
traffic.data %>% filter(rd.rp.cm == "رادار") %>% select(rd.stid, rd.rp.stid, rd.rp.cm)
traffic.data %>% filter(rd.rp.cm == "ردار") %>% select(rd.stid, rd.rp.stid, rd.rp.cm)
grepl("(za7ma)", traffic.data$rd.rp.cm)
grepl("(حادث)", traffic.data$rd.rp.cm)

## Split cairo and alex roads after downloading xml from website
cairo <- read.csv("~/Desktop/DataScience/cairo.csv", stringsAsFactors = FALSE, fileEncoding="UTF-8")
cairo %>% dim
cairo %>% summary()
cairo <- cairo[-c(3:13)]

alex <- read.csv("~/Desktop/DataScience/alex.csv", stringsAsFactors = FALSE, fileEncoding="UTF-8")
alex %>% dim()
alex %>% summary()
alex <- alex[-c(3:10)]

##after inspecting data there are some missing roads thus match with patterns
##thus their database has either changed i.e
traffic.data %>% filter(rd.ri == 520) %>% select(rd.nm)

##add new column for city
## [0] --> cairo , [1] --> alex
traffic.data %>% mutate(city = ifelse(rd.ri %in% cairo$ri, 0, ifelse(rd.ri %in% alex$ri, 1, NA))) %>% select(rd.nm, city) %>% unique()
traffic.data <- traffic.data %>% mutate(city = ifelse(rd.ri %in% cairo$ri, 0, ifelse(rd.ri %in% alex$ri, 1, NA))) 

## check for NA values then add them manually
traffic.data %>% mutate(city = ifelse(rd.ri %in% cairo$ri, 0, ifelse(rd.ri %in% alex$ri, 1, NA))) %>% select(rd.nm, rd.ri, city) %>% filter(is.na(city)) %>% unique()

##                                          rd.nm rd.ri city
##1    Makram 3ebeid;Autostrad To Mostafa ElNa7as   678   NA
##173        Gam3et ElDewal St;Sudan St To Sphinx   545   NA
##561                       Sa7rawy;Alex To Cairo   520   NA   ##this road id is changed from their data-base
##3182                                    Tagamo3   547   NA
##3317                          توتال (برج العرب)   485   NA
traffic.data[traffic.data$rd.ri == 547,]$city <- 0
traffic.data[traffic.data$rd.ri == 678,]$city <- 0
traffic.data[traffic.data$rd.ri == 545,]$city <- 0
traffic.data[traffic.data$rd.ri == 520,]$city <- 1
traffic.data[traffic.data$rd.ri == 485,]$city <- 1

##make sure no NA's
traffic.data %>% select(city, rd.ri) %>% filter(is.na(city)) %>% unique()



