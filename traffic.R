library(dplyr)
library(tidyr)
library(ggplot2)
library(chron)
library(stringi)
library(knitr)

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

############start our analysis::::
##First plot the number of reports along the day hours and analyze its results
traffic.data %>% separate(actual_report_time, into=c("report_date","report_time"), sep =" ") %>% group_by(report_time) %>% summarize(number=length(rd.rp.cmid)) %>% ggplot(aes(x=report_time, y=number)) + geom_bar(stat = 'identity')


## plot the average speed in top 10 roads where the number of reports are the highest
top_10 <- traffic.data %>% group_by(rd.ri) %>% summarize(count=length(rd.rp.cmid)) %>% arrange(desc(count)) %>% head(10)
traffic.data[is.na(traffic.data$speed),]$speed <- 0

## speeds table
speeds <- traffic.data %>% filter(speed != 0)

speeds %>% filter(rd.ri %in% top_10$rd.ri) %>% group_by(rd.ri) %>% summarize(speed=mean(speed))
speeds %>% filter(rd.ri %in% top_10$rd.ri) %>% group_by(rd.ri) %>% summarize(s=mean(speed)) %>% ggplot(aes(x=rd.ri, y=s)) + geom_bar(stat = 'identity')

##name of those roads are:
##rd.ri     speed
##(int)     (dbl)
##1     31  68.97272       Da2ery;Autostrad To Moneeb
##2     32  69.27468       Da2ery;Moneeb To Autostrad
##3    164  55.36564       Kobry 6 October;Mohandesin To Ta7rir
##4    165  55.31048       Kobry 6 October;Ta7rir To Madinet Nasr
##5    166  60.75797       Kobry 6 October;Madinet Nasr To Ta7rir
##6    167  51.53032       Kobry 6 October;Ta7rir To Mohandesin
##7    282  75.51525       Da2ery;Sokhna Exit To Autostrad
##8    307  131.68868      Sa7rawy;Cairo To Alex
##9    520  129.98000      Sa7rawy;Alex To Cairo
##10   553  80.12701       Me7war;Sa7rawy To Da2ery


##define the meaning of our 10 categories:

traffic.data %>% filter(rd.rp.stid == 10) %>% select(rd.rp.cm) %>% head(100)

##repeat it, replacing the rd.rp.stid from 1 to 10 and got the following
## 1 --> 7alawa
## 2 --> lazez
## 3 --> mashy
## 4 --> za7ma
## 5 --> mafeesh 2amal
## 6 --> so2al
## 7 --> lagna/radar
## 8 --> 7adsa
## 9 --> 3otl
## 10 --> gps Reporter



##inspect the road that I take everyday
## in the commentary compare to max legal speed
## maximum number of reports is at 09:17:30 --> 7
##
sokhna_to_autostrad <- traffic.data %>% filter(rd.ri == 282)
sokhna_to_autostrad %>% separate(actual_report_time, into=c("report_date","report_time"), sep =" ") %>% group_by(report_time) %>% summarize(number=length(rd.rp.cmid)) %>% ggplot(aes(x=report_time, y=number)) + geom_bar(stat = 'identity')
t <- sokhna_to_autostrad %>% separate(actual_report_time, into=c("report_date","report_time"), sep =" ") %>% group_by(report_time) %>% summarize(number=length(rd.rp.cmid))
t[ t$number == max( t$number ) , ]

##average speed is  75.51525
sokhna_to_autostrad %>% filter(rd.rp.cmid %in% speeds$rd.rp.cmid) %>% summarise(mean(speed))

# plot number of reports of each category
sokhna_to_autostrad %>% group_by(rd.rp.stid) %>% summarise(c = length(rd.rp.cmid)) %>%  ggplot(aes(x=rd.rp.stid, y=c)) + geom_bar(stat = 'identity')

# rd.rp.stid     c
# (int) (int)
# 1          1   135
# 2          2   523
# 3          3   129
# 4          4   137
# 5          5   187
# 6          6    88
# 7          7     7
# 8          8     7
# 9         10   834



##max number of reports
## 07:43:30      6
## 16:00:30      6
## 17:24:30      6
autostrad_to_sokhna <- traffic.data %>% filter(rd.ri == 281)
autostrad_to_sokhna %>% separate(actual_report_time, into=c("report_date","report_time"), sep =" ") %>% group_by(report_time) %>% summarize(number=length(rd.rp.cmid)) %>% ggplot(aes(x=report_time, y=number)) + geom_bar(stat = 'identity')
t <- autostrad_to_sokhna %>% separate(actual_report_time, into=c("report_date","report_time"), sep =" ") %>% group_by(report_time) %>% summarize(number=length(rd.rp.cmid))
t[ t$number == max( t$number ) , ]

##average speed is 74.39928
autostrad_to_sokhna %>% filter(rd.rp.cmid %in% speeds$rd.rp.cmid) %>% summarise(mean(speed))

## plot no. of reports on each category
autostrad_to_sokhna %>% group_by(rd.rp.stid) %>% summarise(c = length(rd.rp.cmid)) %>%  ggplot(aes(x=rd.rp.stid, y=c)) + geom_bar(stat = 'identity')

##        (int) (int)
# 1           1   121
# 2           2   622
# 3           3    57
# 4           4    38
# 5           5   150
# 6           6    56
# 7           7     1
# 8           8    12
# 9           9     1
# 10         10   893


##max number of reports
##1    16:27:35      6
##2    21:01:34      6
##3    21:37:34      6
##4    22:55:34      6
tagamo3_to_sokhna <- traffic.data %>% filter(rd.ri == 318)
tagamo3_to_sokhna %>% separate(actual_report_time, into=c("report_date","report_time"), sep =" ") %>% group_by(report_time) %>% summarize(number=length(rd.rp.cmid)) %>% ggplot(aes(x=report_time, y=number)) + geom_bar(stat = 'identity')
t <- tagamo3_to_sokhna %>% separate(actual_report_time, into=c("report_date","report_time"), sep =" ") %>% group_by(report_time) %>% summarize(number=length(rd.rp.cmid))
t[ t$number == max( t$number ) , ]

##average speed is 87.50582
tagamo3_to_sokhna %>% filter(rd.rp.cmid %in% speeds$rd.rp.cmid) %>% summarise(mean(speed))

##plot number of reports on each category
tagamo3_to_sokhna %>% group_by(rd.rp.stid) %>% summarise(c = length(rd.rp.cmid)) %>%  ggplot(aes(x=rd.rp.stid, y=c)) + geom_bar(stat = 'identity')
## rd.rp.stid     c
##(int) (int)
##1          1   280
##2          2   314
##3          3    15
##4          4    10
##5          5    90
##6          6    25
##7          8     5
##8         10   791



##max number of reports
##1    17:21:35      7
##2    23:10:33      7
sokhna_to_tagamo3 <- traffic.data %>% filter(rd.ri == 315)
sokhna_to_tagamo3 %>% separate(actual_report_time, into=c("report_date","report_time"), sep =" ") %>% group_by(report_time) %>% summarize(number=length(rd.rp.cmid)) %>% ggplot(aes(x=report_time, y=number)) + geom_bar(stat = 'identity')
t <- sokhna_to_tagamo3 %>% separate(actual_report_time, into=c("report_date","report_time"), sep =" ") %>% group_by(report_time) %>% summarize(number=length(rd.rp.cmid))
t[ t$number == max( t$number ) , ]

##avg speed is 85.96212
sokhna_to_tagamo3 %>% filter(rd.rp.cmid %in% speeds$rd.rp.cmid) %>% summarise(mean(speed))

##plot number of reports on each category
sokhna_to_tagamo3 %>% group_by(rd.rp.stid) %>% summarise(c = length(rd.rp.cmid)) %>%  ggplot(aes(x=rd.rp.stid, y=c)) + geom_bar(stat = 'identity')

##rd.rp.stid     c
##(int) (int)
##1          1   232
##2          2   413
##3          3    14
##4          4     7
##5          5     8
##6          6    13
##7         10   887



##Cairo Vs. Alex:
# quite clear that cairo reports more than alex
traffic.data %>% group_by(city) %>% summarise(c = length(rd.rp.cmid))
traffic.data %>% group_by(city) %>% summarise(c = length(rd.rp.cmid)) %>% ggplot(aes(x=city, y=c)) + geom_bar(stat = 'identity')
a <- traffic.data %>% filter(city == 1)
c <- traffic.data %>% filter(city == 0)

##check the number of reports for rcategories
## good Vs bad = 11396 Vs 597 with ratio approx 19:1
bad_alex_reports <- a %>% filter(rd.rp.stid == 4 | rd.rp.stid == 5 | rd.rp.stid == 8 | rd.rp.stid == 9 ) %>% summarise(c = length(rd.rp.cmid))
good_alex_reports <- a %>% filter(rd.rp.stid == 1 | rd.rp.stid == 2 | rd.rp.stid == 3 ) %>% summarise(c = length(rd.rp.cmid))

# good Vs bad = 62143 Vs 21404 with ratio approx 3:1
good_cairo_reports <- c %>% filter(rd.rp.stid == 1 | rd.rp.stid == 2 | rd.rp.stid == 3 ) %>% summarise(c = length(rd.rp.cmid))
bad_cairo_reports <- c %>% filter(rd.rp.stid == 4 | rd.rp.stid == 5 | rd.rp.stid == 8 | rd.rp.stid == 9 ) %>% summarise(c = length(rd.rp.cmid))

##get average speed of top 10 reported on roads in alex Vs cairo
top_10_cairo <- c %>% group_by(rd.ri) %>% summarize(count=length(rd.rp.cmid)) %>% arrange(desc(count)) %>% head(10)
cairo_speeds <- speeds %>% filter(rd.ri %in% top_10_cairo$rd.ri) %>% group_by(rd.ri) %>% summarize(speed=mean(speed))
left_join(top_10_cairo, cairo_speeds, "rd.ri")


# rd.ri     count     speed
# (int)     (int)     (dbl)
# 1    166  3218  60.75797
# 2     32  3075  69.27468
# 3     31  2998  68.97272
# 4    164  2862  55.36564
# 5    167  2318  51.53032
# 6    307  2297 131.68868
# 7    165  2249  55.31048
# 8    282  2047  75.51525
# 9    553  2009  80.12701
# 10   281  1951  74.39928

top_10_alex <- a %>% group_by(rd.ri) %>% summarize(count=length(rd.rp.cmid)) %>% arrange(desc(count)) %>% head(10)
alex_speeds <- speeds %>% filter(rd.ri %in% top_10_alex$rd.ri) %>% group_by(rd.ri) %>% summarize(speed=mean(speed))
left_join(top_10_alex, alex_speeds, "rd.ri")

# rd.ri count     speed
# (int) (int)     (dbl)
# 1    520  2621 129.98000
# 2     99   423        NA ---> speed not reported
# 3     56   345  43.06481
# 4     43   344  49.41414
# 5     55   265  43.39850
# 6     40   251  47.43299
# 7     42   248  50.13592
# 8     39   246  56.95122
# 9     38   228  53.55932
# 10    47   191  33.41667




