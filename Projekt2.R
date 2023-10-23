#install.packages("dynlm")
library(dynlm)
#install.packages("readr")
library(readr)
#setwd("H:/BAOVE/pr2")
#install.packages("lubridate")
library(lubridate)
library(dplyr)

# nacitani dat
dat_ads_2020 <- read.csv2("data_ads_2020.csv")
dat_visits_2020 <- read.csv2("data_visits_2020.csv")

#install.packages("stringr")
library(stringr)

#str_split_fixed(dat_visits_2020$time, " ", 2)
#visits <- data.frame(matrix(nrow = 0, ncol = 3))
#colnames(visits) <- c("date", "time", "number")
#visits$date <- str_split_fixed(dat_visits_2020$time, " ", 2)
dat_visits_2020[c("date", "time")] <- str_split_fixed(dat_visits_2020$time, " ", 2)
dat_visits_2020 <- dat_visits_2020[c("date", "time", "visits")]

dat_visits_2020["wday"] <- wday(ymd(dat_visits_2020$date))
dat_visits_2020["wdayname"]<- wday(ymd(dat_visits_2020$date), label = TRUE)
dat_visits_2020["hodina"] <- hour(hms(dat_visits_2020$time))
#

dat_visits_2020_ts <- as.ts(dat_visits_2020)
#options("scipen"=100, "digits"=4) #
regrese <- lm(visits ~ date + as.factor(wday) + as.factor(hodina), data = dat_visits_2020_ts)
summary(regrese)
koefs <- coefficients(regrese)
#####

dat_ads_2020[c("date", "time")] <- str_split_fixed(dat_ads_2020$time, " ", 2)
dat_ads_2020 <- dat_ads_2020[c("date", "time", "channel")]

n_dat_ads_2020 <- read.csv2("data_ads_2020.csv")
n_dat_visits_2020 <- read.csv2("data_visits_2020.csv")
n_dat_ads_2021 <- read.csv2("data_ads_2021.csv")

visitory <- filter(n_dat_visits_2020, time %in% n_dat_ads_2020$time)

kanaly <- unique(dat_ads_2020$channel)
watchers <- data.frame(matrix(nrow = 3, ncol = 5))
watchers[is.na(watchers)] <-0
colnames(watchers) <- kanaly
for(k in 1:length(kanaly)){ 
  for(i in 1:nrow(visitory)){ 
    if (n_dat_ads_2020$channel[i] == kanaly[k]){
      watchers[1,k] <- watchers[1,k] + visitory$visits[i]
      watchers[2, k] <- watchers[2,k] + 1
    }
  }
}
watchers[3, ] <- watchers[1, ]/watchers[2, ] ### prumer 

#####

dat_visits_2020["channel"] <- "none"
for(i in 1:nrow(dat_visits_2020)){
  for(j in 1:nrow(dat_ads_2020)){
    if(dat_visits_2020$date[i] == dat_ads_2020$date[j] & dat_visits_2020$time[i] == dat_ads_2020$time[j]){
      dat_visits_2020$channel[i] <- dat_ads_2020$channel[j]
    }
  }
}
dat_visits_2020_ts2 <- as.ts(dat_visits_2020)
dat_visits <- cbind(seq(1:nrow(dat_visits_2020)), dat_visits_2020)
colnames(dat_visits)[1] <- "day"
detrending <- lm(visits ~ day, data = dat_visits)

summary(detrending)

dat_visits["detrended"] <- detrending$residuals

regrese_grade <- lm(detrended ~ as.factor(wday) + as.factor(hodina) + relevel(as.factor(channel), ref = "none"), data = dat_visits)
summary(regrese_grade)
koefs_grade <- coefficients(regrese_grade)
unique(dat_visits_2020$channel)
filter(dat_visits_2020, channel == "Barevná hodinka" )
filter(dat_visits_2020, time == "04:08:00", date == "2020-08-23")

#####
dat_ads_2021 <- read.csv2("data_ads_2021.csv")
dat_ads_2021[c("date", "time")] <- str_split_fixed(dat_ads_2021$time, " ", 2)
dat_ads_2021 <- dat_ads_2021[c("date", "time", "channel", "cost")]
dat_ads_2021["wday"] <- wday(ymd(dat_ads_2021$date))
dat_ads_2021["wdayname"]<- wday(ymd(dat_ads_2021$date), label = TRUE)
dat_ads_2021["hodina"] <- hour(hms(dat_ads_2021$time))
dat_ads_2021["pr_wday"] <- 0
dat_ads_2021["pr_hodina"] <- 0
dat_ads_2021["pr_channel"] <- 0

nav_ocek <- c()
for(i in 1:nrow(dat_ads_2021)){
  if(dat_ads_2021$wday[i] != 1){
   dat_ads_2021$pr_wday[i]<-koefs_grade[dat_ads_2021$wday[i]+1] 
  }
  if(dat_ads_2021$hodina[i] != 0){
    dat_ads_2021$pr_hodina[i] <- koefs_grade[dat_ads_2021$hodina[i]+8]
  }
  if(dat_ads_2021$channel[i] == "Barevná hodinka"){
   dat_ads_2021$pr_channel[i] <- 6970.313
  } else if (dat_ads_2021$channel[i] == "Kanál"){
    dat_ads_2021$pr_channel[i] <- 2743.804
  } else if (dat_ads_2021$channel[i] == "none"){
    dat_ads_2021$pr_channel[i] <- 0
  } else if (dat_ads_2021$channel[i] == "TV Žumpa"){
    dat_ads_2021$pr_channel[i] <- 10641.146
  } else if (dat_ads_2021$channel[i] == "Tévévize"){
    dat_ads_2021$pr_channel[i] <- 5912.095
  } else if (dat_ads_2021$channel[i] == "Èum na mì!"){
    dat_ads_2021$pr_channel[i] <- 1130.561
  }
}
dat_ads_2021["predpoved"] <- 209.178+dat_ads_2021$pr_wday+dat_ads_2021$pr_hodina+dat_ads_2021$pr_channel

write.csv2(dat_ads_2021, file = "Predpoved.csv")

#install.packages("adagio")
w <- dat_ads_2021$cost
p <- dat_ads_2021$predpoved
cap <- 10000000

#### Greeedy ####

greedy_dat <- dat_ads_2021[order(-dat_ads_2021$predpoved), ]
cena <- 0
i <- 1
while(cena < cap){
  i <- i + 1
  cena<-sum(greedy_dat$cost[1:i])
}
i <- i-1
cena_greedy <- sum(greedy_dat$cost[1:i])
greedy_ocek <- sum(greedy_dat$predpoved[1:i])
volba_greedy <- greedy_dat[1:i, ]

#### Cena-Navstevy ####

second_dat <- dat_ads_2021
second_dat["podil"] <- second_dat$predpoved/second_dat$cost
second_dat <- second_dat[order(-second_dat$podil), ]
j <- 1
cena <- 0
while(cena < cap){
  j <- j + 1
  cena <- sum(second_dat$cost[1:j])
}
j <- j - 1
cena_second <- sum(second_dat$cost[1:j])
second_ocek <- sum(second_dat$predpoved[1:j])
volba_second <- second_dat[1:j, ]
nrow(volba_second)

### images ###
bar_data <- c(0,koefs_grade[8:30])
names(bar_data) <- seq(from = 0, to = 23)
barplot(bar_data, ylab = "Sezonnost hodiny", xlab = "hodina", col = "green", main = "Sezonnost hodin")

bar_data2 <- c(0, koefs_grade[31:35])
names(bar_data2) <- c("none", "Barevna hodinka", "Kanal", "TV Cumpa", "Televize", "Cum na me!")
barplot(bar_data2, col = "yellow", xlab = "kanal", ylab = "kanal navstevy", main = "Sezonnost kanalu")

volba_second[ ,1:4]

N = nrow(dat_ads_2021)
knap <- function(W = 10000000, wt = dat_ads_2021$cost, val = dat_ads_2021$predpoved, n = nrow(dat_ads_2021)){
  K <- matrix(0, nrow = n, ncol = W)
  
  for(i in 1:n){
    for(w in 1:W){
       if (wt[i-1] <= w){
        K[i,w] <- max(val[i-1]+K[i-1, w-wt[i-1]], K[i-1,w])
      } else {
        K[i,w] <- K[i-1, w]
      }
    }}
    return(K[n,W])
}
profit = c(60, 100, 120)
weight = c(10, 20, 30)
W = 50
n = length(profit)
print(knap(W, weight, profit, n))

write.csv2(volba_greedy,file = "greedy.csv")
write.csv2(volba_second, file  = "second.csv")



