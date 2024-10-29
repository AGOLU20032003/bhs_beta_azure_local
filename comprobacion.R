#Comentarios Mark

## Required Libraries

library(data.table)
library(ggplot2)
library(sysfonts)
library(showtext)
library(jsonlite)
library(curl)
library(extrafont)
library(lubridate)
library(readxl)


#### 0. PREPARACIÓN ####
ddfs <- as.data.table(ddfs)
ddfs_airline <- unique(ddfs$Airline)
checkin_airline <- unique(checkin_area$Airline)
makeup_airline <- unique(make_up_carousel$Airline)


##### 1. COMPROBACION DDFS - CHECK-IN ####

airline_ddfs_checkin <- unique(c(ddfs_airline, checkin_airline))

airline_ddfs_ci_dt <- data.table(RawData = (airline_ddfs_checkin))
airline_ddfs_ci_dt[, Comp_DDFS := 0]
airline_ddfs_ci_dt[, Comp_CI := 0]
airline_ddfs_ci_dt[RawData %in% ddfs_airline, Comp_DDFS := 1]
airline_ddfs_ci_dt[RawData %in% checkin_airline, Comp_CI := 1]

comp_airl_ddfs_ci <- airline_ddfs_ci_dt[Comp_DDFS == 1 & Comp_CI == 0]


##### 2. COMPROBACION DDFS - MAKE-UP ####

airline_ddfs_makeup <- unique(c(ddfs_airline, makeup_airline))

airline_ddfs_mu_dt <- data.table(RawData = (airline_ddfs_makeup))
airline_ddfs_mu_dt[, Comp_DDFS := 0]
airline_ddfs_mu_dt[, Comp_MU := 0]
airline_ddfs_mu_dt[RawData %in% ddfs_airline, Comp_DDFS := 1]
airline_ddfs_mu_dt[RawData %in% makeup_airline, Comp_MU := 1]

comp_airl_ddfs_mu <- airline_ddfs_mu_dt[Comp_DDFS == 1 & Comp_MU == 0]
