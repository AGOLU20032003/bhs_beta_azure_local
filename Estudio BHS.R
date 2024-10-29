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


#### 0. FUNCTIONS ####

function1_OD <- function(flightschedule, emptytable){ # This function fills the departures table with the pax in 5 minutes interval
  
  prueba <- cbind(flightschedule, emptytable)
  
  
  prueba_long <- melt(prueba, id.vars = c("FlightNo", "Bag_OD"), 
                      measure.vars = patterns("^Arriving Hour_", "^Percentage of PAX arriving_"), 
                      variable.name = "Llegada", value.name = c("Hora_llegada", "Porcentaje_pasajeros"))
  
  # Multiplicar los porcentajes por el número de bolsas
  prueba_long[, Valor := ceiling(as.numeric(Bag_OD) * as.numeric(Porcentaje_pasajeros)/100)]
  
  hours <- data.table(Hora_llegada = date_colnames)
  
  prueba_long <- merge(prueba_long, hours, by = "Hora_llegada", all = TRUE)
  
  # Reemplazar NA en las columnas 'Vuelo', 'Bag' y 'Valor' con 0
  prueba_long[is.na(FlightNo), FlightNo := ""]
  prueba_long[is.na(Bag_OD), Bag_OD := 0]
  prueba_long[is.na(Valor), Valor := 0]
  
  
  # Convertir la tabla larga de vuelta a una tabla ancha con las horas como columnas
  prueba_wide <- dcast(prueba_long, FlightNo + Bag_OD ~ Hora_llegada, value.var = "Valor", fill = 0)
  
  prueba_wide <- prueba_wide[FlightNo != ""]
  prueba_wide[, Bag_OD := NULL]
  flightschedule_result <- merge(flightschedule, prueba_wide, by = "FlightNo")
  
  return(flightschedule_result)
}

function1_Transf <- function(flightschedule, emptytable){ # This function fills the departures table with the pax in 5 minutes interval
  
  prueba <- cbind(flightschedule, emptytable)
  
  
  prueba_long <- melt(prueba, id.vars = c("FlightNo", "Bag_transf"), 
                      measure.vars = patterns("^Arriving Hour_", "^Percentage of PAX arriving_"), 
                      variable.name = "Llegada", value.name = c("Hora_llegada", "Porcentaje_pasajeros"))
  
  # Multiplicar los porcentajes por el número de bolsas
  prueba_long[, Valor := ceiling(as.numeric(Bag_transf) * as.numeric(Porcentaje_pasajeros)/100)]
  
  hours <- data.table(Hora_llegada = date_colnames)
  
  prueba_long <- merge(prueba_long, hours, by = "Hora_llegada", all = TRUE)
  
  # Reemplazar NA en las columnas 'Vuelo', 'Bag' y 'Valor' con 0
  prueba_long[is.na(FlightNo), FlightNo := ""]
  prueba_long[is.na(Bag_transf), Bag_transf := 0]
  prueba_long[is.na(Valor), Valor := 0]
  
  
  # Convertir la tabla larga de vuelta a una tabla ancha con las horas como columnas
  prueba_wide <- dcast(prueba_long, FlightNo + Bag_transf ~ Hora_llegada, value.var = "Valor", fill = 0)
  
  prueba_wide <- prueba_wide[FlightNo != ""]
  prueba_wide[, Bag_transf := NULL]
  flightschedule_result <- merge(flightschedule, prueba_wide, by = "FlightNo")
  
  return(flightschedule_result)
}

function2 <- function(output_ddfs, date_colnames, pax_aircraft_parameter){ # This function post processes the results obtained from the flight schedule
  
  passenger_aircraft_output <- output_ddfs[ ,colnames(output_ddfs) %in% as.character(date_colnames), with=FALSE]            # We only consider the hour columns
  passenger_aircraft_output <- melt(passenger_aircraft_output)                                                # We melt the data to process it better
  passenger_aircraft_output <- passenger_aircraft_output[, .(newcol= sum(value)), by = variable]              # We sum all the pax in the same hour
  setnames(passenger_aircraft_output, c("variable", "newcol"),  c("hour", pax_aircraft_parameter))            # We set new names to the columns
  
  return(passenger_aircraft_output)
}

function_rollinghour <- function(rolling_hour_input){ # This function compute the pax in each rolling hour assuming that the rolling hour is the previous one
  
  rolling_hour_input$hour <- as.ITime(rolling_hour_input$hour)
  
  rolling_hour_input_sum <- rolling_hour_input[, .(sum_pax = sum(pax)), by = hour]
  
  rolling_hour_input_sum[, subthour:= as.POSIXct(as.ITime(as.character(rolling_hour_input_sum$hour)),  format="%d/%m/%Y %H:%M:%S", tz="CET") + 3600 ]
  
  rolling_hour_input_sum$hour <- as.POSIXct(as.ITime(as.character(rolling_hour_input_sum$hour)),  format="%d/%m/%Y %H:%M:%S", tz="CET")
  
  rolling_hour_input_sum[, rolling_hour := 
                           rolling_hour_input_sum[
                             rolling_hour_input_sum,
                             on = c("hour<subthour", "hour>=hour"),
                             sum(sum_pax),
                             by =.EACHI
                           ]$V1
  ]
  
  rolling_hour_input_sum$subthour <- NULL
  
  rolling_hour_input_sum$hour <- as.ITime(rolling_hour_input_sum$hour)
  rolling_hour_input_sum <- rolling_hour_input_sum[order(hour, decreasing = FALSE),]
  
  
  return(rolling_hour_input_sum)
}

function_showupprofile <- function(ddfs_showupprofile, profilepresentation){ # This function applies the airport departure show-up profile
  
  ddfs_showupprofile$`Departure Time (hh:mm:ss)` <- as.POSIXct(paste("2022/12/18", as.character(ddfs_showupprofile$`Departure Time`)))
  ddfs_showupprofile$`Departure Time (hh:mm:ss)` <- as.POSIXct(ddfs_showupprofile$`Departure Time (hh:mm:ss)`, format="%d/%m/%Y %H:%M:%S") 
  
  for (i in 1:nrow(profilepresentation)){
    ddfs_showupprofile[, paste0("Arriving Hour","_",i):= `Departure Time (hh:mm:ss)` - as.difftime(as.numeric(profilepresentation[i,"Minutes before departure"]), units ="mins")]
    ddfs_showupprofile[, paste0("Percentage of PAX arriving","_",i):= profilepresentation[i,"% PAX"]]
  }
  
  ddfs_showupprofile <- ddfs_showupprofile[order(`Departure Time (hh:mm:ss)`, decreasing = FALSE),]
  
  return(ddfs_showupprofile)
}


#### 2. TABLE INITIALIZATION  ####

# Empty Table Creation
date_colnames_aux1 <- seq.POSIXt(as.POSIXct("18/12/2022 00:00:00",format="%d/%m/%Y %H:%M:%S", tz="CET"), as.POSIXct("18/12/2022 07:55:00",format="%d/%m/%Y %H:%M:%S", tz="CET"), by = "5 min")  # We create a vector with negative time (flights departing at midnight so the Check-In Counters will be open during the night before of the day of study). We set the maximum length of this vector to 4 hours. The date is supposed to be set manually
date_colnames_aux1 <- as.POSIXct(date_colnames_aux1) - as.difftime(8, units ="hours")  # We subtract 8 hours to get negative times
date_colnames_aux2 <- seq.POSIXt(as.POSIXct("18/12/2022 00:00:00",format="%d/%m/%Y %H:%M:%S", tz="CET"), as.POSIXct("19/12/2022 00:00:00",format="%d/%m/%Y %H:%M:%S", tz="CET"), by = "5 min")  # We create a vector dor the rest of the day. The date is supposed to be placed manually
date_colnames <- c(date_colnames_aux1,date_colnames_aux2)  # This line merges the two auxiliary vectors resulting in a vector going to -4:00:00 to 00:00:00 of the following day
row_zerovalue <-rep(0,length(date_colnames))  # We create a 0s vector that has the same length of the date_colnames vector
emptytable <- setDT(as.list(row_zerovalue))[]   # We create the empty table that will be filled
setnames(emptytable, colnames(emptytable),as.character(date_colnames))




#### 4. SHOW-UP OD LATAM & AVIANCA ####

ddfs_departures_LATAM_AVIANCA <- copy(ddfs_complete)   # We copy the original DDFS

ddfs_departures_LATAM_AVIANCA <- ddfs_departures_LATAM_AVIANCA[Airline == "LA" | Airline == "AV"]

##### 4.1 DOM #####
ddfs_departures_LATAM_AVIANCA_OD_DOM <- copy(ddfs_departures_LATAM_AVIANCA[DOM_INT == "DOM"])

# We add the Show Up Hour for each flight
ddfs_departures_LATAM_AVIANCA_OD_DOM <- function_showupprofile(ddfs_departures_LATAM_AVIANCA_OD_DOM, profilepresentation_LATAM_DOM)

## OD_Pax Calculation
# A function is used to run the loop and obtain the filled departures table with all passengers in 5 min intervals
ddfs_departures_LATAM_AVIANCA_OD_DOM <- function1_OD(ddfs_departures_LATAM_AVIANCA_OD_DOM,emptytable)

##### 4.2 INT #####
ddfs_departures_LATAM_AVIANCA_OD_INT <- copy(ddfs_departures_LATAM_AVIANCA[DOM_INT == "INT"])

# We add the Show Up Hour for each flight
ddfs_departures_LATAM_AVIANCA_OD_INT <- function_showupprofile(ddfs_departures_LATAM_AVIANCA_OD_INT, profilepresentation_LATAM_INT)

## Pax Calculation
# A function is used to run the loop and obtain the filled departures table with all passengers in 5 min intervals
ddfs_departures_LATAM_AVIANCA_OD_INT <- function1_OD(ddfs_departures_LATAM_AVIANCA_OD_INT,  emptytable)

##### 4.3 JOIN #####
ddfs_departures_LATAM_AVIANCA_OD_showup <- copy(ddfs_departures_LATAM_AVIANCA_OD_DOM)
ddfs_departures_LATAM_AVIANCA_OD_showup <- rbind(ddfs_departures_LATAM_AVIANCA_OD_showup, ddfs_departures_LATAM_AVIANCA_OD_INT, fill = TRUE)


#### 5. SHOW-UP OD NO LATAM & AVIANCA ####

ddfs_departures_rest <- copy(ddfs_complete)   # We copy the original DDFS
ddfs_departures_rest <- ddfs_departures_rest[Airline != "LA" & Airline != "AV"]

##### 5.1 DOM #####
ddfs_departures_rest_OD_DOM <- copy(ddfs_departures_rest[DOM_INT == "DOM"])

# We add the Show Up Hour for each flight
ddfs_departures_rest_OD_DOM <- function_showupprofile(ddfs_departures_rest_OD_DOM, profilepresentation_AvRest_DOM)

## OD_Pax Calculation
# A function is used to run the loop and obtain the filled departures table with all passengers in 5 min intervals
ddfs_departures_rest_OD_DOM <- function1_OD(ddfs_departures_rest_OD_DOM,  emptytable)

##### 5.2 INT #####
ddfs_departures_rest_OD_INT <- copy(ddfs_departures_rest[DOM_INT == "INT"])

# We add the Show Up Hour for each flight
ddfs_departures_rest_OD_INT <- function_showupprofile(ddfs_departures_rest_OD_INT, profilepresentation_AvRest_INT)

## Pax Calculation
# A function is used to run the loop and obtain the filled departures table with all passengers in 5 min intervals
ddfs_departures_rest_OD_INT <- function1_OD(ddfs_departures_rest_OD_INT,emptytable)

##### 5.3 JOIN #####
ddfs_departures_rest_OD_showup <- copy(ddfs_departures_rest_OD_DOM)
ddfs_departures_rest_OD_showup <- rbind(ddfs_departures_rest_OD_showup, ddfs_departures_rest_OD_INT, fill = TRUE)

#### 6. JOIN OD ####

ddfs_departures_OD <- rbind(ddfs_departures_rest_OD_showup, ddfs_departures_LATAM_AVIANCA_OD_showup, fill = TRUE)
ddfs_departures_OD[, OD_Transf := "OD"]


#### 7. SHOW-UP TRANSFER ####

# Assign TX at CI_Area
ddfs_departures_transf <- copy(ddfs_complete)   # We copy the original DDFS
ddfs_departures_transf[, CI_Area := "TX"]

##### 7.1 DOM #####
ddfs_departures_transf_DOM <- copy(ddfs_departures_transf[DOM_INT == "DOM"])

# We add the Show Up Hour for each flight
ddfs_departures_transf_DOM <- function_showupprofile(ddfs_departures_transf_DOM, profilepresentation_Transfer_DOM)

## OD_Pax Calculation
# A function is used to run the loop and obtain the filled departures table with all passengers in 5 min intervals
ddfs_departures_transf_DOM <- function1_Transf(ddfs_departures_transf_DOM,emptytable)

##### 7.2 INT #####
ddfs_departures_transf_INT <- copy(ddfs_departures_transf[DOM_INT == "INT"])

# We add the Show Up Hour for each flight
ddfs_departures_transf_INT <- function_showupprofile(ddfs_departures_transf_INT, profilepresentation_Transfer_INT)

## OD_Pax Calculation
# A function is used to run the loop and obtain the filled departures table with all passengers in 5 min intervals
ddfs_departures_transf_INT <- function1_Transf(ddfs_departures_transf_INT, emptytable)

##### 7.3 JOIN #####
ddfs_departures_transf_showup <- copy(ddfs_departures_transf_DOM)
ddfs_departures_transf_showup <- rbind(ddfs_departures_transf_showup, ddfs_departures_transf_INT, fill = TRUE)
ddfs_departures_transf_showup[, OD_Transf := "Transf"]

# TX line for Transf
ddfs_departures_transf_showup[Airline != "AV" & Airline != "LA", CI_Area := "TX04"]
ddfs_departures_transf_showup[Airline == "LA", CI_Area := "TX01"]
ddfs_departures_transf_showup[Airline == "AV", CI_Area := rep(c("TX02", "TX03"), length.out = .N)]


#### 8. BAGLIST OD ####

if(sum(ddfs_complete$Bag_OD)>0){
  for(i in 1:length(ddfs_departures_OD$Airline)){
    aux_bags_output_OD <- ddfs_departures_OD[i ,colnames(ddfs_departures_OD) %in% as.character(date_colnames), with=FALSE]
    aux_flight_data <- ddfs_departures_OD[i, c("FlightNo","Airline","Departure Time","Destination","DOM_INT","CI_Area","Carousel","Car_open","Car_close","Car_open_aux")]
    aux_flightno <- ddfs_departures_OD[i]$FlightNo
    aux_bags_output_OD[, FlightNo := aux_flightno]
    aux_baglist_OD <- melt(aux_bags_output_OD, id.vars = "FlightNo")
    aux_baglist_OD[variable == "2022-12-18"]$variable <- "2022-12-18 00:00:00"
    aux_baglist_OD[variable == "2022-12-19"]$variable <- "2022-12-19 00:00:00"
    aux_baglist_OD[, variable := as.ITime(variable)]
    if (length(aux_baglist_OD[value>0,]$value) != 0){
      aux_baglist_OD <- aux_baglist_OD[value>0,]
      aux_baglist_OD <- aux_baglist_OD[rep(1:.N,value)][,BagID:=1:.N]
      aux_baglist_OD[, value := NULL]
      setnames(aux_baglist_OD, c("variable"), c("Bag Entry Time"))
      aux_baglist_OD <- merge(aux_baglist_OD, aux_flight_data, by = "FlightNo")
      
      if(i == 1){
        baglist_OD <- copy(aux_baglist_OD)
      }
      if(i != 1){
        baglist_OD <- rbind(baglist_OD, aux_baglist_OD, fill = TRUE)
      }
    }
  }
}


#### 9. BAGLIST TRANSFER ####

if(sum(ddfs_complete$Bag_transf)>0){
  for(i in 1:length(ddfs_departures_transf_showup$Airline)){
    aux_bags_output_transf <- ddfs_departures_transf_showup[i ,colnames(ddfs_departures_transf_showup) %in% as.character(date_colnames), with=FALSE]
    aux_flight_data <- ddfs_departures_transf_showup[i, c("FlightNo","Airline","Departure Time","Destination","DOM_INT","CI_Area","Carousel","Car_open","Car_close","Car_open_aux")]
    aux_flightno <- ddfs_departures_transf_showup[i]$FlightNo
    aux_bags_output_transf[, FlightNo := aux_flightno]
    aux_baglist_transf <- melt(aux_bags_output_transf, id.vars = "FlightNo")
    aux_baglist_transf[variable == "2022-12-18"]$variable <- "2022-12-18 00:00:00"
    aux_baglist_transf[variable == "2022-12-19"]$variable <- "2022-12-19 00:00:00"
    aux_baglist_transf[, variable := as.ITime(variable)]
    if(length(aux_baglist_transf[value>0,]$value)!=0){
      aux_baglist_transf <- aux_baglist_transf[value>0,]
      aux_baglist_transf <- aux_baglist_transf[rep(1:.N,value)][,BagID:=1:.N]
      aux_baglist_transf[, value := NULL]
      setnames(aux_baglist_transf, c("variable"), c("Bag Entry Time"))
      aux_baglist_transf <- merge(aux_baglist_transf, aux_flight_data, by = "FlightNo")
      
      if(i == 1){
        baglist_transf <- copy(aux_baglist_transf)
      }
      if(i != 1){
        baglist_transf <- rbind(baglist_transf, aux_baglist_transf, fill = TRUE)
      } 
    }
  }
  
  ##### 9.1 TX ALLOCATION #####
  
  # AKEs
  TX <- "TX01"
  baglist_transf <- tx_akes_distribution(baglist_transf, TX)
  
  TX <- "TX02"
  baglist_transf <- tx_akes_distribution(baglist_transf, TX)
  
  TX <- "TX03"
  baglist_transf <- tx_akes_distribution(baglist_transf, TX)
  
  # Dollies
  TX <- "TX04"
  baglist_transf <- transfer_allocation(TX, baglist_transf, max_dollies_tx = 4)
}



#### 10. GLOBAL BAGLIST ####

if(sum(ddfs_complete$Bag_OD)>0 & sum(ddfs_complete$Bag_transf)>0){
  baglist_global <- rbind(baglist_OD, baglist_transf, fill = TRUE)
  baglist_global[, Car_open := as.ITime(Car_open)]
  setorder(baglist_global, `Bag Entry Time`)
}

if(sum(ddfs_complete$Bag_OD)>0 & sum(ddfs_complete$Bag_transf)==0){
  baglist_global <- copy(baglist_OD)
  baglist_global[, Car_open := as.ITime(Car_open)]
  setorder(baglist_global, `Bag Entry Time`)
}

if(sum(ddfs_complete$Bag_OD)==0 & sum(ddfs_complete$Bag_transf)>0){
  baglist_global <- copy(baglist_transf)
  baglist_global[, Car_open := as.ITime(Car_open)]
  setorder(baglist_global, `Bag Entry Time`)
}


#### 11. ROUTING ####

##### 11.1 LOOP #####

baglist_global[, Loop := "0"]

baglist_global[CI_Area == "A1", Loop := "MS01"]
baglist_global[CI_Area == "B1", Loop := "MS02"]
baglist_global[CI_Area == "B2", Loop := "MS01"]
baglist_global[CI_Area == "B3", Loop := "MS02"]

baglist_global[CI_Area == "C1", Loop := rep(c("MS01", "MS02"), length.out = .N)]
baglist_global[CI_Area == "C2", Loop := rep(c("MS01", "MS02"), length.out = .N)]

baglist_global[CI_Area == "TX01", Loop := rep(c("MS01", "MS02"), length.out = .N)]
baglist_global[CI_Area == "TX02", Loop := rep(c("MS01", "MS02"), length.out = .N)]
baglist_global[CI_Area == "TX03", Loop := rep(c("MS01", "MS02"), length.out = .N)]
baglist_global[CI_Area == "TX04", Loop := rep(c("MS01", "MS02"), length.out = .N)]

##### 11.2 EDS ALLOCATION #####

baglist_global[, EDS_machine := "0"]

baglist_global[(CI_Area == "A1") | (CI_Area == "B2") | (CI_Area == "TX02" & Loop == "MS01")  | (CI_Area == "TX03" & Loop == "MS01")| (CI_Area == "TX04" & Loop == "MS01"), EDS_machine := rep(c("IL5", "IL7", "IL5", "IL6"), length.out = .N)]
baglist_global[(CI_Area == "B1") | (CI_Area == "B3") | (CI_Area == "TX02" & Loop == "MS02")  | (CI_Area == "TX03" & Loop == "MS02")| (CI_Area == "TX04" & Loop == "MS02"), EDS_machine := rep(c("IL4", "IL7", "IL4", "IL6"), length.out = .N)]

baglist_global[(CI_Area == "TX01" & Loop == "MS01")  | (CI_Area == "C1" & Loop == "MS01") | (CI_Area == "C2" & Loop == "MS01"), EDS_machine := rep(c("IL1", "IL3", "IL1"), length.out = .N)]
baglist_global[(CI_Area == "TX01" & Loop == "MS02")  | (CI_Area == "C1" & Loop == "MS02") | (CI_Area == "C2" & Loop == "MS02"), EDS_machine := rep(c("IL2", "IL3","IL2"), length.out = .N)]

##### 11.3 INSPECTION LEVELS #####

# Parameters
# eds_limit <- parameters[PARAMETER == "L1_approved"]$VALUE
bags_to_L3_limit <- 100 - no_pic - rec_n2
no_pic_limit <- bags_to_L3_limit + no_pic
# lost_track_value <- parameters[PARAMETER == "Lost_Track"]$VALUE

# Random values
baglist_global[, aux_random := sample(1:100, .N, replace = TRUE)]

# Level 1
baglist_global[, L1_Status := "Clear"]
baglist_global[(aux_random>eds_limit), L1_Status := "Reject"]
baglist_global[(aux_random>bags_to_L3_limit & aux_random<=no_pic_limit), L1_Status := "No Pic"]

# Level 2
baglist_global[L1_Status == "Clear", L2 := "-"]
baglist_global[L1_Status == "Reject" | L1_Status=="No Pic", L2 := "Yes"]

baglist_global[, L2_Status := "-"]
baglist_global[(aux_random>eds_limit & aux_random<=bags_to_L3_limit), L2_Status := "Clear"]
baglist_global[(aux_random>bags_to_L3_limit), L2_Status := "Reject"]
baglist_global[(aux_random>bags_to_L3_limit & aux_random<=no_pic_limit), L2_Status := "No Pic"]

# Level 3
baglist_global[, L3 := "-"]
baglist_global[, L3_Reason := "-"]
baglist_global[L2_Status=="Reject", L3_Reason := "Reject L2"]
baglist_global[(aux_random>bags_to_L3_limit & aux_random<=no_pic_limit), L3_Reason := "No Pic"]
baglist_global[aux_random<=lost_track_value, L3_Reason := "Lost Tracking"]
baglist_global[L3_Reason!="-", L3 := "Yes"]

# ##### 11.4 MES #####

mes_limit <- 100-mes_val
mes_lost_eds <- 100-eds_lost

baglist_global[, aux_random_mes := sample(1:100, .N, replace = TRUE)]

baglist_global[, MES := "No"]
baglist_global[aux_random_mes>mes_limit, MES := "Yes"]

baglist_global[, aux_random_lost_EDS := 1]
baglist_global[MES == "No", aux_random_lost_EDS := sample(1:100, .N, replace = TRUE)]

baglist_global[, MES_Station := "-"]
baglist_global[MES=="Yes" & Loop=="MS01" & (CI_Area=="TX01" | CI_Area=="C1" | CI_Area=="C2"), MES_Station := "ME3"]
baglist_global[MES=="Yes" & Loop=="MS02" & (CI_Area=="TX01" | CI_Area=="C1" | CI_Area=="C2"), MES_Station := "ME4"]
baglist_global[MES=="Yes" & Loop=="MS01" & (CI_Area=="TX02" | CI_Area=="TX03" | CI_Area=="TX04" | CI_Area=="A1" | CI_Area=="B2"), MES_Station := "ME2"]
baglist_global[MES=="Yes" & Loop=="MS02" & (CI_Area=="TX02" | CI_Area=="TX03" | CI_Area=="TX04" | CI_Area=="B1" | CI_Area=="B3"), MES_Station := "ME1"]

baglist_global[aux_random_lost_EDS>mes_lost_eds, MES := "Yes"]
baglist_global[MES=="Yes" & Loop=="MS01" & MES_Station == "-", MES_Station := "ME3"]
baglist_global[MES=="Yes" & Loop=="MS02" & MES_Station == "-", MES_Station := "ME4"]

##### 11.5 EBS #####

baglist_global[, EBS := "No"]
baglist_global[, `Bag Entry Aux` := `Bag Entry Time` + as.ITime(12*3600) + as.ITime(12*3600)]
baglist_global[`Bag Entry Aux` < Car_open_aux, EBS := "Yes"]
baglist_global[, `Carousel Entry Time` := `Bag Entry Time`]
baglist_global[EBS == "Yes", `Carousel Entry Time` := Car_open]
baglist_global[, EBS_Type := "-"]
baglist_global[Airline %in% ebs_asignacion$Airline, EBS_Type := "Automatic"]
baglist_global[EBS == "Yes" & EBS_Type != "Automatic", EBS_Type := "Manual"]


#### 12 RESULTS ####

baglist_global[`Bag Entry Time` > as.ITime(23.99*3600), `Bag Entry Time` := as.ITime(0)]
baglist_global[`Carousel Entry Time` > as.ITime(23.99*3600), `Carousel Entry Time` := as.ITime(0)]

setnames(baglist_global, c("Bag Entry Time", "Carousel Entry Time"), c("Bag_Entry_Time","Carousel_Entry_Time"))
hours <- data.table(Bag_Entry_Time = as.ITime(seq(0,86100, by = 300)))
hours2 <- data.table(Carousel_Entry_Time = date_colnames)
hours3 <- data.table(Carousel_Entry_Time = as.ITime(seq(0,86100, by = 300)))
hours4 <- data.table(Bag_Entry_Time = date_colnames)

##### 12.1 BAG ENTRY (CHECK-IN + TRANSFER) #####

baglist_global[, CI_Area_aux := CI_Area]
baglist_global[CI_Area == "C1" | CI_Area == "C2", CI_Area_aux := paste0(CI_Area,"_",Loop)]

result_aux_bag_entry <- baglist_global[,.N, by = c("Bag_Entry_Time", "CI_Area_aux")]
result_aux_bag_entry <- merge(result_aux_bag_entry, hours, by = "Bag_Entry_Time", all = TRUE)


result_aux_bag_entry[is.na(CI_Area_aux), CI_Area_aux := ""]
result_aux_bag_entry[is.na(N), N := 0]

result_bag_entry <- dcast(result_aux_bag_entry, Bag_Entry_Time ~ CI_Area_aux, value.var = "N", fun.aggregate = sum, fill = 0)
result_bag_entry[, V1 := NULL]

columnas_a_verificar <- c("A1","B1","B2","B3","C1_MS01","C1_MS02","C2_MS01","C2_MS02","TX01","TX02","TX03","TX04")

lapply(columnas_a_verificar, function(col) {
  
  if (!col %in% colnames(result_bag_entry)) {
    
    result_bag_entry[, (col) := 0]  # Se agrega la columna con valor 0
    
  }
  
})

result_bag_entry[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = 2:ncol(result_bag_entry)]

##### 12.2 MANUAL ENCODING STATION #####

result_aux_mes <- baglist_global[MES == "Yes"][,.N, by = c("Bag_Entry_Time", "MES_Station")]
result_aux_mes <- merge(result_aux_mes, hours, by = "Bag_Entry_Time", all = TRUE)

result_aux_mes[is.na(MES_Station), MES_Station := ""]
result_aux_mes[is.na(N), N := 0]

result_mes <- dcast(result_aux_mes, Bag_Entry_Time ~ MES_Station, value.var = "N", fun.aggregate = sum, fill = 0)
result_mes[, V1 := NULL]

columnas_a_verificar <- c("ME1","ME2","ME3","ME4")

lapply(columnas_a_verificar, function(col) {
  
  if (!col %in% colnames(result_mes)) {
    
    result_mes[, (col) := 0]  # Se agrega la columna con valor 0
    
  }
  
})

result_mes[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = 2:ncol(result_mes)]

##### 12.3 LEVEL 3 #####

result_aux_L3 <- baglist_global[L3 == "Yes"][,.N, by = c("Bag_Entry_Time")]
result_aux_L3 <- merge(result_aux_L3, hours, by = "Bag_Entry_Time", all = TRUE)

result_aux_L3[is.na(N), N := 0]

result_L3 <- dcast(result_aux_L3, Bag_Entry_Time ~ ., value.var = "N", fun.aggregate = sum, fill = 0)
setnames(result_L3, c("."), c("L3"))

##### 12.4 EDS #####

result_aux_eds <- baglist_global[,.N, by = c("Bag_Entry_Time", "EDS_machine")]
result_aux_eds <- merge(result_aux_eds, hours, by = "Bag_Entry_Time", all = TRUE)

result_aux_eds[is.na(EDS_machine), EDS_machine := ""]
result_aux_eds[is.na(N), N := 0]

result_eds <- dcast(result_aux_eds, Bag_Entry_Time ~ EDS_machine, value.var = "N", fun.aggregate = sum, fill = 0)
result_eds[, V1 := NULL]

columnas_a_verificar <- c("IL1","IL2","IL3","IL4","IL5","IL6","IL7")

lapply(columnas_a_verificar, function(col) {
  
  if (!col %in% colnames(result_eds)) {
    
    result_eds[, (col) := 0]  # Se agrega la columna con valor 0
    
  }
  
})

result_eds[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = 2:ncol(result_eds)]

##### 12.5 LOOP #####
result_aux_loop <- baglist_global[,.N, by = c("Bag_Entry_Time", "Loop")]
result_aux_loop <- merge(result_aux_loop, hours, by = "Bag_Entry_Time", all = TRUE)

result_aux_loop[is.na(Loop), Loop := ""]
result_aux_loop[is.na(N), N :=0]

result_loop <- dcast(result_aux_loop, Bag_Entry_Time ~ Loop, value.var = "N", fun.aggregate = sum, fill = 0)
result_loop[, V1 := NULL]

columnas_a_verificar <- c("MS01","MS02")

lapply(columnas_a_verificar, function(col) {
  
  if (!col %in% colnames(result_loop)) {
    
    result_loop[, (col) := 0]  # Se agrega la columna con valor 0
    
  }
  
})

result_loop[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = 2:ncol(result_loop)]

##### 12.6 MAKE-UP #####
hours3$Carousel_Entry_Time <- as.ITime(hours3$Carousel_Entry_Time)

baglist_global[, Loop_aux := 2]
baglist_global[Loop == "MS01", Loop_aux := 1]
baglist_global<-baglist_global[, makeup_aux := paste0("MU0",Carousel,"_", Loop_aux)]
result_aux_makeup <- baglist_global[,.N, by = c("Carousel_Entry_Time", "makeup_aux")]
result_aux_makeup <- merge(result_aux_makeup, hours3, by = "Carousel_Entry_Time", all = TRUE)

result_aux_makeup[is.na(makeup_aux), makeup_aux := ""]
result_aux_makeup[is.na(N), N :=0]

result_makeup<- dcast(result_aux_makeup, Carousel_Entry_Time ~ makeup_aux, value.var = "N", fun.aggregate = sum, fill = 0)

columnas_a_verificar <- c(paste0("MU0",carousel_list,"_1"),paste0("MU0",carousel_list,"_2"))

lapply(columnas_a_verificar, function(col) {
  
  if (!col %in% colnames(result_makeup)) {
    
    result_makeup[, (col) := 0]  # Se agrega la columna con valor 0
    
  }
  
})

result_makeup[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = 2:ncol(result_makeup)]

##### 12.7 EBS #####

# Poner hours2 y carousel_entry_time
#Adaptamos la tabla hours2
hours4$Bag_Entry_Time<-as.ITime(hours4$Bag_Entry_Time)

result_aux_ebs <- baglist_global[EBS == "Yes"][,.N, by = c("Bag_Entry_Time", "EBS_Type")]
result_aux_ebs <- merge(result_aux_ebs, hours4, by = "Bag_Entry_Time", all = TRUE)

result_aux_ebs[is.na(EBS_Type), EBS_Type := ""]
result_aux_ebs[is.na(N), N := 0]

result_ebs <- dcast(result_aux_ebs, Bag_Entry_Time ~ EBS_Type, value.var = "N", fun.aggregate = sum, fill = 0)
result_ebs[, V1 := NULL]

columnas_a_verificar <- c("Automatic","Manual")

lapply(columnas_a_verificar, function(col) {
  
  if (!col %in% colnames(result_ebs)) {
    
    result_ebs[, (col) := 0]  # Se agrega la columna con valor 0
    
  }
  
})

result_ebs[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = 2:ncol(result_ebs)]

##### 13. ROLLING HOUR #####

##### 13.1 BAG ENTRY #####

# rh_bag_entry <- data.table(hour = copy(result_bag_entry$hour))
setnames(result_bag_entry, c(1), "hour")
rh_bag_entry <- data.table(hour = copy(result_bag_entry$hour))

for(i in 2:length(result_bag_entry)){
  aux_rh <- copy(result_bag_entry[, .SD, .SDcols = c(1, i)])
  aux_colnames <- colnames(aux_rh)
  colname_2 <- aux_colnames[2]
  setnames(aux_rh,c(2),c("pax"))
  aux_rh_2 <- function_rollinghour(aux_rh)
  setnames(aux_rh_2, c("rolling_hour"), c(colname_2))
  rh_bag_entry <- cbind(rh_bag_entry, aux_rh_2[, c(3)])
}

##### 13.2 MANUAL ENCODING STATION #####

setnames(result_mes, c(1), "hour")
rh_mes <- data.table(hour = copy(result_mes$hour))
for(i in 2:length(result_mes)){
  aux_rh <- copy(result_mes[, .SD, .SDcols = c(1, i)])
  aux_colnames <- colnames(aux_rh)
  colname_2 <- aux_colnames[2]
  setnames(aux_rh,c(2),c("pax"))
  aux_rh_2 <- function_rollinghour(aux_rh)
  setnames(aux_rh_2, c("rolling_hour"), c(colname_2))
  rh_mes <- cbind(rh_mes, aux_rh_2[, c(3)])
}

##### 13.3 LEVEL 3 #####
setnames(result_L3, c(1), "hour")
rh_L3 <- data.table(hour = copy(result_L3$hour))
for(i in 2:length(result_L3)){
  aux_rh <- copy(result_L3[, .SD, .SDcols = c(1, i)])
  aux_colnames <- colnames(aux_rh)
  colname_2 <- aux_colnames[2]
  setnames(aux_rh,c(2),c("pax"))
  aux_rh_2 <- function_rollinghour(aux_rh)
  setnames(aux_rh_2, c("rolling_hour"), c(colname_2))
  rh_L3 <- cbind(rh_L3, aux_rh_2[, c(3)])
}

##### 13.4 EDS #####
setnames(result_eds, c(1), "hour")
rh_eds <- data.table(hour = copy(result_eds$hour))
for(i in 2:length(result_eds)){
  aux_rh <- copy(result_eds[, .SD, .SDcols = c(1, i)])
  aux_colnames <- colnames(aux_rh)
  colname_2 <- aux_colnames[2]
  setnames(aux_rh,c(2),c("pax"))
  aux_rh_2 <- function_rollinghour(aux_rh)
  setnames(aux_rh_2, c("rolling_hour"), c(colname_2))
  rh_eds <- cbind(rh_eds, aux_rh_2[, c(3)])
}

##### 13.5 LOOP #####
setnames(result_loop, c(1), "hour")
rh_loop <- data.table(hour = copy(result_loop$hour))
# recirc <- as.numeric(parameters[PARAMETER == "Recirc"]$VALUE)/100 + 1
recirc <- recirc/100 + 1
for(i in 2:length(result_loop)){
  aux_rh <- copy(result_loop[, .SD, .SDcols = c(1, i)])
  aux_colnames <- colnames(aux_rh)
  colname_2 <- aux_colnames[2]
  setnames(aux_rh,c(2),c("pax"))
  aux_rh_2 <- function_rollinghour(aux_rh)
  aux_rh_2[, rolling_hour := ceiling(rolling_hour*recirc)]
  setnames(aux_rh_2, c("rolling_hour"), c(colname_2))
  rh_loop <- cbind(rh_loop, aux_rh_2[, c(3)])
}


##### 13.6 MAKE-UP #####
setnames(result_makeup, c(1), "hour")
rh_make_up <- data.table(hour = copy(result_makeup$hour))
for(i in 2:length(result_makeup)){
  aux_rh <- copy(result_makeup[, .SD, .SDcols = c(1, i)])
  aux_colnames <- colnames(aux_rh)
  colname_2 <- aux_colnames[2]
  setnames(aux_rh,c(2),c("pax"))
  aux_rh_2 <- function_rollinghour(aux_rh)
  setnames(aux_rh_2, c("rolling_hour"), c(colname_2))
  rh_make_up <- cbind(rh_make_up, aux_rh_2[, c(3)])
}

##### 13.7 EBS #####
setnames(result_ebs, c(1), "hour")
rh_ebs <- data.table(hour = copy(result_ebs$hour))
for(i in 2:length(result_ebs)){
  aux_rh <- copy(result_ebs[, .SD, .SDcols = c(1, i)])
  aux_colnames <- colnames(aux_rh)
  colname_2 <- aux_colnames[2]
  setnames(aux_rh,c(2),c("pax"))
  aux_rh_2 <- function_rollinghour(aux_rh)
  setnames(aux_rh_2, c("rolling_hour"), c(colname_2))
  rh_ebs <- cbind(rh_ebs, aux_rh_2[, c(3)])
}


#### 14. OPERATORS CALCULATION ####

##### 14.1 L2 #####
# Equipajes a Nivel 2
result_aux_N2 <- baglist_global[L2 == "Yes"][,.N, by = c("Bag_Entry_Time", "EDS_machine")]
result_aux_N2 <- merge(result_aux_N2, hours, by = "Bag_Entry_Time", all = TRUE)

result_aux_N2[is.na(EDS_machine), EDS_machine := ""]
result_aux_N2[is.na(N), N := 0]

result_N2 <- dcast(result_aux_N2, Bag_Entry_Time ~ EDS_machine, value.var = "N", fun.aggregate = sum, fill = 0)
result_N2[, V1 := NULL]
result_N2[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = 2:ncol(result_N2)]

# Rolling Hour
setnames(result_N2, "Bag_Entry_Time", "hour")
rh_N2 <- data.table(hour = copy(result_N2$hour))
for(i in 2:length(result_N2)){
  aux_rh <- copy(result_N2[, .SD, .SDcols = c(1, i)])
  aux_colnames <- colnames(aux_rh)
  colname_2 <- aux_colnames[2]
  setnames(aux_rh,c(2),c("pax"))
  aux_rh_2 <- function_rollinghour(aux_rh)
  setnames(aux_rh_2, c("rolling_hour"), c(colname_2))
  rh_N2 <- cbind(rh_N2, aux_rh_2[, c(3)])
}

# Operadores
ops_L2 <- copy(rh_N2)
ops_L2[, operadores_L2 := ceiling(tasa_N2*Total/capacidad_N2)]

##### 14.2 L3 ####
ops_L3 <- copy(rh_L3)
ops_L3[, operadores_L3 := ceiling(tasa_N3*L3/capacidad_N3)]


#### 15. PLOTS ####

# Definición de la función create_plot
create_plot <- function(data, x_var, y_var, y_label, hline_value) {
  max_y <- max(data[[y_var]])
  tope <- max(c(max_y, hline_value))
  y_limit <- tope * 1.1  # Añadir un 10% de margen superior
  y_breaks <- pretty(c(0, y_limit), n = 10)  # Calcular los breaks de manera automática
  percentage <- (max_y / hline_value) * 100  # Calcular el porcentaje respecto a la línea horizontal
  
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_line(size = 1.5, color = "#1f78b4") +
    geom_hline(yintercept = hline_value, linetype = "dashed", color = "red", size = 1) +  # Añadir la línea horizontal
    annotate(
      "text",
      x = max(data[[x_var]]) * 0.5,  # Ajustar la posición del texto en el eje x
      #x = 1,  # Ajustar la posición del texto en el eje x
      y = hline_value + (y_limit * 0.05),  # Colocar el texto un poco por encima de la línea horizontal
      label = paste0("Capacidad: ", hline_value, "   ","Porcentaje: ", round(percentage, 2), "%"),
      size = 6,
      color = "red"
    ) +
    scale_x_continuous(
      breaks = seq(0, 86100, by = 3600),
      labels = function(x) sprintf("%02d:00", x %/% 3600),
      limits = c(0, 86100),  # Establecer los límites de 0 a 23 horas
      expand = c(0, 0)  # Eliminar espacio adicional
    ) +
    scale_y_continuous(
      limits = c(0, max(y_breaks)),
      breaks = y_breaks,  # Ajustar los intervalos de las líneas de la cuadrícula
      expand = c(0, 0)
    ) +
    labs(y = y_label, x = NULL) +
    theme_minimal(base_family = "Lato") +
    theme(
      text = element_text(family = "Lato"),
      axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 1, size = 14, color = "gray20"),
      axis.text.y = element_text(size = 14, color = "gray20", hjust = 1),
      axis.title.y = element_text(size = 18, color = "gray20"),
      axis.ticks.length = unit(-0.25, "cm"),  # Longitud de las marcas pequeñas, negativas para que sean internas
      axis.ticks = element_line(size = 0.5, color = "gray20"),  # Apariencia de las marcas pequeñas
      axis.line = element_line(size = 0.5, color = "gray20"),  # Línea del eje
      panel.border = element_rect(color = "gray30", fill = NA),
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      panel.grid.minor = element_line(color = "gray90", size = 0.3),
      plot.margin = margin(10, 20, 10, 10),
      panel.background = element_rect(fill = alpha("white", 0.3)),
      plot.background = element_rect(fill = alpha("white", 0.3), color = NA)
    )
}

create_plot_ops <- function(data, x_var, y_var, y_label) {
  max_y <- max(data[[y_var]])
  y_limit <- max_y * 1.1  # Añadir un 10% de margen superior
  y_breaks <- pretty(c(0, y_limit), n = 10)  # Calcular los breaks de manera automática

  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_line(size = 1.5, color = "#1f78b4") +
    scale_x_continuous(
      breaks = seq(0, 86100, by = 3600),
      labels = function(x) sprintf("%02d:00", x %/% 3600),
      limits = c(0, 86100),  # Establecer los límites de 0 a 23 horas
      expand = c(0, 0)  # Eliminar espacio adicional
    ) +
    scale_y_continuous(
      limits = c(0, max(y_breaks)),
      breaks = y_breaks,  # Ajustar los intervalos de las líneas de la cuadrícula
      expand = c(0, 0)
    ) +
    labs(y = y_label, x = NULL) +
    theme_minimal(base_family = "Lato") +
    theme(
      text = element_text(family = "Lato"),
      axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 1, size = 14, color = "gray20"),
      axis.text.y = element_text(size = 14, color = "gray20", hjust = 1),
      axis.title.y = element_text(size = 18, color = "gray20"),
      # plot.title = element_text(size = 18, hjust = 0.5, color = "gray20", face = "bold"),
      axis.ticks.length = unit(-0.25, "cm"),  # Longitud de las marcas pequeñas, negativas para que sean internas
      axis.ticks = element_line(size = 0.5, color = "gray20"),  # Apariencia de las marcas pequeñas
      axis.line = element_line(size = 0.5, color = "gray20"),  # Línea del eje
      panel.border = element_rect(color = "gray30", fill = NA),
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      panel.grid.minor = element_line(color = "gray90", size = 0.3),
      plot.margin = margin(10, 20, 10, 10),
      panel.background = element_rect(fill = alpha("white", 0.3)),
      plot.background = element_rect(fill = alpha("white", 0.3), color = NA)
    )
}


bag_entry_tx_cap <- line_capacity[Line == "TX01"]$Capacity + line_capacity[Line == "TX02"]$Capacity + line_capacity[Line == "TX03"]$Capacity + line_capacity[Line == "TX04"]$Capacity
bag_entry_tr_cap <- line_capacity[Line == "TR07"]$Capacity + line_capacity[Line == "TR08"]$Capacity + line_capacity[Line == "TR09"]$Capacity + line_capacity[Line == "TR10"]$Capacity + line_capacity[Line == "TR06"]$Capacity + line_capacity[Line == "TR05"]$Capacity + line_capacity[Line == "TR04"]$Capacity + line_capacity[Line == "TR03"]$Capacity + line_capacity[Line == "TR02"]$Capacity + line_capacity[Line == "TR01"]$Capacity
 

bag_mes_total_cap <- line_capacity[Line == "ME1"]$Capacity + line_capacity[Line == "ME2"]$Capacity + line_capacity[Line == "ME3"]$Capacity + line_capacity[Line == "ME4"]$Capacity

bag_eds_total_cap <- line_capacity[Line == "1L1"]$Capacity + line_capacity[Line == "1L2"]$Capacity + line_capacity[Line == "1L3"]$Capacity + line_capacity[Line == "1L4"]$Capacity + line_capacity[Line == "1L5"]$Capacity + line_capacity[Line == "1L6"]$Capacity + line_capacity[Line == "1L7"]$Capacity

bag_entry_total_cap <- copy(bag_eds_total_cap)

plots <- list(
  
  list(data = rh_bag_entry, y_var = "Total", file = "Bag_Entry_Total.png", hline_value = bag_entry_total_cap),
  list(data = rh_bag_entry, y_var = "A1", file = "Bag_Entry_A1.png", hline_value = line_capacity[Line == "TR07"]$Capacity),
  list(data = rh_bag_entry, y_var = "B1", file = "Bag_Entry_B1.png", hline_value = line_capacity[Line == "TR08"]$Capacity),
  list(data = rh_bag_entry, y_var = "B2", file = "Bag_Entry_B2.png", hline_value = line_capacity[Line == "TR09"]$Capacity),
  list(data = rh_bag_entry, y_var = "B3", file = "Bag_Entry_B3.png", hline_value = line_capacity[Line == "TR10"]$Capacity),
  list(data = rh_bag_entry, y_var = "C1_MS01", file = "Bag_Entry_C1_MS01.png", hline_value = line_capacity[Line == "TR04"]$Capacity),
  list(data = rh_bag_entry, y_var = "C1_MS02", file = "Bag_Entry_C1_MS02.png", hline_value = line_capacity[Line == "TR03"]$Capacity),
  list(data = rh_bag_entry, y_var = "C2_MS01", file = "Bag_Entry_C2_MS01.png", hline_value = line_capacity[Line == "TR02"]$Capacity),
  list(data = rh_bag_entry, y_var = "C2_MS02", file = "Bag_Entry_C2_MS02.png", hline_value = line_capacity[Line == "TR01"]$Capacity),
  list(data = rh_bag_entry, y_var = "TX01", file = "Bag_Entry_TX01.png", hline_value = line_capacity[Line == "TX01"]$Capacity),
  list(data = rh_bag_entry, y_var = "TX02", file = "Bag_Entry_TX02.png", hline_value = line_capacity[Line == "TX02"]$Capacity),
  list(data = rh_bag_entry, y_var = "TX03", file = "Bag_Entry_TX03.png", hline_value = line_capacity[Line == "TX03"]$Capacity),
  list(data = rh_bag_entry, y_var = "TX04", file = "Bag_Entry_TX04.png", hline_value = line_capacity[Line == "TX04"]$Capacity),
  list(data = rh_mes, y_var = "Total", file = "MES_Total.png", hline_value = bag_mes_total_cap),
  list(data = rh_mes, y_var = "ME1", file = "MES_ME1.png", hline_value = line_capacity[Line == "ME1"]$Capacity),
  list(data = rh_mes, y_var = "ME2", file = "MES_ME2.png", hline_value = line_capacity[Line == "ME2"]$Capacity),
  list(data = rh_mes, y_var = "ME3", file = "MES_ME3.png", hline_value = line_capacity[Line == "ME3"]$Capacity),
  list(data = rh_mes, y_var = "ME4", file = "MES_ME4.png", hline_value = line_capacity[Line == "ME4"]$Capacity),
  list(data = rh_L3, y_var = "L3", file = "L3_Total.png", hline_value = line_capacity[Line == "3L1"]$Capacity),
  list(data = rh_eds, y_var = "Total", file = "EDS_Total.png", hline_value = bag_eds_total_cap),
  list(data = rh_eds, y_var = "IL1", file = "EDS_IL1.png", hline_value = line_capacity[Line == "1L1"]$Capacity),
  list(data = rh_eds, y_var = "IL2", file = "EDS_IL2.png", hline_value = line_capacity[Line == "1L2"]$Capacity),
  list(data = rh_eds, y_var = "IL3", file = "EDS_IL3.png", hline_value = line_capacity[Line == "1L3"]$Capacity),
  list(data = rh_eds, y_var = "IL4", file = "EDS_IL4.png", hline_value = line_capacity[Line == "1L4"]$Capacity),
  list(data = rh_eds, y_var = "IL5", file = "EDS_IL5.png", hline_value = line_capacity[Line == "1L5"]$Capacity),
  list(data = rh_eds, y_var = "IL6", file = "EDS_IL6.png", hline_value = line_capacity[Line == "1L6"]$Capacity),
  list(data = rh_eds, y_var = "IL7", file = "EDS_IL7.png", hline_value = line_capacity[Line == "1L7"]$Capacity),
  list(data = rh_loop, y_var = "MS01", file = "Loop_MS01.png", hline_value = line_capacity[Line == "MS01"]$Capacity),
  list(data = rh_loop, y_var = "MS02", file = "Loop_MS02.png", hline_value = line_capacity[Line == "MS02"]$Capacity),
  list(data = rh_make_up, y_var = "MU01_1", file = "make_up_MU01_1.png", hline_value = line_capacity[Line == "MU01_1"]$Capacity),
  list(data = rh_make_up, y_var = "MU01_2", file = "make_up_MU01_2.png", hline_value = line_capacity[Line == "MU01_2"]$Capacity),
  list(data = rh_make_up, y_var = "MU02_1", file = "make_up_MU02_1.png", hline_value = line_capacity[Line == "MU02_1"]$Capacity),
  list(data = rh_make_up, y_var = "MU02_2", file = "make_up_MU02_2.png", hline_value = line_capacity[Line == "MU02_2"]$Capacity),
  list(data = rh_make_up, y_var = "MU03_1", file = "make_up_MU03_1.png", hline_value = line_capacity[Line == "MU03_1"]$Capacity),
  list(data = rh_make_up, y_var = "MU03_2", file = "make_up_MU03_2.png", hline_value = line_capacity[Line == "MU03_2"]$Capacity),
  list(data = rh_make_up, y_var = "MU05_1", file = "make_up_MU05_1.png", hline_value = line_capacity[Line == "MU05_1"]$Capacity),
  list(data = rh_make_up, y_var = "MU05_2", file = "make_up_MU05_2.png", hline_value = line_capacity[Line == "MU05_2"]$Capacity),
  list(data = rh_make_up, y_var = "MU06_1", file = "make_up_MU06_1.png", hline_value = line_capacity[Line == "MU06_1"]$Capacity),
  list(data = rh_make_up, y_var = "MU06_2", file = "make_up_MU06_2.png", hline_value = line_capacity[Line == "MU06_2"]$Capacity),
  list(data = rh_make_up, y_var = "MU07_1", file = "make_up_MU07_1.png", hline_value = line_capacity[Line == "MU07_1"]$Capacity),
  list(data = rh_make_up, y_var = "MU07_2", file = "make_up_MU07_2.png", hline_value = line_capacity[Line == "MU07_2"]$Capacity),
  list(data = rh_make_up, y_var = "MU08_1", file = "make_up_MU08_1.png", hline_value = line_capacity[Line == "MU08_1"]$Capacity),
  list(data = rh_make_up, y_var = "MU08_2", file = "make_up_MU08_2.png", hline_value = line_capacity[Line == "MU08_2"]$Capacity),
  list(data = rh_make_up, y_var = "MU09_1", file = "make_up_MU09_1.png", hline_value = line_capacity[Line == "MU09_1"]$Capacity),
  list(data = rh_make_up, y_var = "MU09_2", file = "make_up_MU09_2.png", hline_value = line_capacity[Line == "MU09_2"]$Capacity),
  list(data = rh_make_up, y_var = "MU010_1", file = "make_up_MU10_1.png", hline_value = line_capacity[Line == "MU10_1"]$Capacity),
  list(data = rh_make_up, y_var = "MU010_2", file = "make_up_MU10_2.png", hline_value = line_capacity[Line == "MU10_2"]$Capacity),
  list(data = rh_make_up, y_var = "MU011_1", file = "make_up_MU11_1.png", hline_value = line_capacity[Line == "MU11_1"]$Capacity),
  list(data = rh_make_up, y_var = "MU011_2", file = "make_up_MU11_2.png", hline_value = line_capacity[Line == "MU11_2"]$Capacity),
  list(data = rh_ebs, y_var = "Automatic", file = "EBS_Automatic.png", hline_value = line_capacity[Line == "EBS_AUTO"]$Capacity),
  list(data = rh_ebs, y_var = "Manual", file = "EBS_Manual.png", hline_value = line_capacity[Line == "EBS_MANUAL"]$Capacity)
)
  
return(plots)

# Generar y guardar todos los gráficos
for (i in 1:length(plots)) {
  plot <- create_plot(
    data = plots[[i]]$data,
    x_var = "hour",
    y_var = plots[[i]]$y_var,
    y_label = "Equipajes",
    hline_value = plots[[i]]$hline_value
  )
  theme(
    axis.text = element_text(size = 1),
    axis.title = element_text(size = 4),
    plot.title = element_text(size = 3, hjust = 0.5)  # Reducir y centrar el título
  )  # Reducir el tamaño del texto de los ejes
  #ggsave(filename = paste0("01 Resultados/Plots/", plots[[i]]$file), plot = plot, width = 14, height = 7)
  print(plot)
  
}

plots_ops <- list(
  
  list(data = ops_L3, y_var = "operadores_L3", file = "OpsL3_L3.png"),
  list(data = ops_L2, y_var = "operadores_L2", file = "OpsL2_Total.png")
)

return(plots_ops)

# Generar y guardar todos los gráficos
for (i in 1:length(plots_ops)) {
  plot <- create_plot_ops(
    data = plots_ops[[i]]$data,
    x_var = "hour",
    y_var = plots_ops[[i]]$y_var,
    y_label = "Operadores"
  )
  #ggsave(filename = paste0("01 Resultados/Plots/", plots_ops[[i]]$file), plot = plot, width = 14, height = 7)
  print(plot)
}



#### 16. TABLE WITH MAXIMUM DEMAND ####

##### 16.1 BAG ENTRY #####
table_bag_entry <- data.table(`Línea de Entrada` = c("Total", "TR07", "TR08", "TR09", "TR10", "TR04", "TR03", "TR02", "TR01", "TX01", "TX02", "TX03", "TX04"))
table_bag_entry[, `Máxima demanda (equipajes/hora)` := 0]
table_bag_entry$`Máxima demanda (equipajes/hora)`[1] <- max(rh_bag_entry$Total)
table_bag_entry$`Máxima demanda (equipajes/hora)`[2] <- max(rh_bag_entry$A1)
table_bag_entry$`Máxima demanda (equipajes/hora)`[3] <- max(rh_bag_entry$B1)
table_bag_entry$`Máxima demanda (equipajes/hora)`[4] <- max(rh_bag_entry$B2)
table_bag_entry$`Máxima demanda (equipajes/hora)`[5] <- max(rh_bag_entry$B3)
table_bag_entry$`Máxima demanda (equipajes/hora)`[6] <- max(rh_bag_entry$C1_MS01)
table_bag_entry$`Máxima demanda (equipajes/hora)`[7] <- max(rh_bag_entry$C1_MS02)
table_bag_entry$`Máxima demanda (equipajes/hora)`[8] <- max(rh_bag_entry$C2_MS01)
table_bag_entry$`Máxima demanda (equipajes/hora)`[9] <- max(rh_bag_entry$C2_MS02)
table_bag_entry$`Máxima demanda (equipajes/hora)`[10] <- max(rh_bag_entry$TX01)
table_bag_entry$`Máxima demanda (equipajes/hora)`[11] <- max(rh_bag_entry$TX02)
table_bag_entry$`Máxima demanda (equipajes/hora)`[12] <- max(rh_bag_entry$TX03)
table_bag_entry$`Máxima demanda (equipajes/hora)`[13] <- max(rh_bag_entry$TX04)


##### 16.2 MANUAL ENCONDING STATION #####
table_mes <- data.table(`Estación de Codificación Manual` = c("Total", "ME1", "ME2", "ME3", "ME4"))
table_mes[, `Máxima demanda (equipajes/hora)` := 0]
table_mes$`Máxima demanda (equipajes/hora)`[1] <- max(rh_mes$Total)
table_mes$`Máxima demanda (equipajes/hora)`[2] <- max(rh_mes$ME1)
table_mes$`Máxima demanda (equipajes/hora)`[3] <- max(rh_mes$ME2)
table_mes$`Máxima demanda (equipajes/hora)`[4] <- max(rh_mes$ME3)
table_mes$`Máxima demanda (equipajes/hora)`[5] <- max(rh_mes$ME4)


##### 16.3 LEVEL 3 #####
table_L3 <- data.table(`Nivel 3` = c("L3"))
table_L3[, `Máxima demanda (equipajes/hora)` := 0]
table_L3[, `Máximo número de operadores (operadores/hora)` := 0]
table_L3$`Máxima demanda (equipajes/hora)`[1] <- max(rh_L3$L3)
table_L3$`Máximo número de operadores (operadores/hora)`[1] <- max(ops_L3$operadores_L3)


##### 16.4 EDS #####
table_eds <- data.table(`Línea de Inspección` = c("Total", "1L1", "1L2", "1L3", "1L4", "1L5", "1L6", "1L7"))
table_eds[, `Máxima demanda (equipajes/hora)` := 0]
table_eds$`Máxima demanda (equipajes/hora)`[1] <- max(rh_eds$Total)
table_eds$`Máxima demanda (equipajes/hora)`[2] <- max(rh_eds$IL1)
table_eds$`Máxima demanda (equipajes/hora)`[3] <- max(rh_eds$IL2)
table_eds$`Máxima demanda (equipajes/hora)`[4] <- max(rh_eds$IL3)
table_eds$`Máxima demanda (equipajes/hora)`[5] <- max(rh_eds$IL4)
table_eds$`Máxima demanda (equipajes/hora)`[6] <- max(rh_eds$IL5)
table_eds$`Máxima demanda (equipajes/hora)`[7] <- max(rh_eds$IL6)
table_eds$`Máxima demanda (equipajes/hora)`[8] <- max(rh_eds$IL7)


##### 16.5 SORTER #####
table_sorter <- data.table(`Sorter` = c("Total", "MS01", "MS02"))
table_sorter[, `Máxima demanda (equipajes/hora)` := 0]
table_sorter$`Máxima demanda (equipajes/hora)`[1] <- max(rh_loop$Total)
table_sorter$`Máxima demanda (equipajes/hora)`[2] <- max(rh_loop$MS01)
table_sorter$`Máxima demanda (equipajes/hora)`[3] <- max(rh_loop$MS02)


##### 16.6 MAKE-UP #####
table_make_up <- data.table(`Entrada a Make-Up` = c("Total", "MU01-1xx", "MU01-2xx", "MU02-1xx", "MU02-2xx", "MU03-1xx", "MU03-2xx", "MU01-4xx", "MU01-4xx", "MU05-1xx", "MU05-2xx", "MU06-1xx", "MU06-2xx", "MU07-1xx", "MU07-2xx", "MU08-1xx", "MU08-2xx", "MU09-1xx", "MU09-2xx", "MU10-1xx", "MU10-2xx", "MU11-1xx", "MU11-2xx"))
table_make_up[, `Máxima demanda (equipajes/hora)` := 0]
table_make_up$`Máxima demanda (equipajes/hora)`[1] <- max(rh_make_up$Total)
table_make_up$`Máxima demanda (equipajes/hora)`[2] <- max(rh_make_up$MU01_1)
table_make_up$`Máxima demanda (equipajes/hora)`[3] <- max(rh_make_up$MU01_2)
table_make_up$`Máxima demanda (equipajes/hora)`[4] <- max(rh_make_up$MU02_1)
table_make_up$`Máxima demanda (equipajes/hora)`[5] <- max(rh_make_up$MU02_2)
table_make_up$`Máxima demanda (equipajes/hora)`[6] <- max(rh_make_up$MU03_1)
table_make_up$`Máxima demanda (equipajes/hora)`[7] <- max(rh_make_up$MU03_2)
table_make_up$`Máxima demanda (equipajes/hora)`[10] <- max(rh_make_up$MU05_1)
table_make_up$`Máxima demanda (equipajes/hora)`[11] <- max(rh_make_up$MU05_2)
table_make_up$`Máxima demanda (equipajes/hora)`[12] <- max(rh_make_up$MU06_1)
table_make_up$`Máxima demanda (equipajes/hora)`[13] <- max(rh_make_up$MU06_2)
table_make_up$`Máxima demanda (equipajes/hora)`[14] <- max(rh_make_up$MU07_1)
table_make_up$`Máxima demanda (equipajes/hora)`[15] <- max(rh_make_up$MU07_2)
table_make_up$`Máxima demanda (equipajes/hora)`[16] <- max(rh_make_up$MU08_1)
table_make_up$`Máxima demanda (equipajes/hora)`[17] <- max(rh_make_up$MU08_2)
table_make_up$`Máxima demanda (equipajes/hora)`[18] <- max(rh_make_up$MU09_1)
table_make_up$`Máxima demanda (equipajes/hora)`[19] <- max(rh_make_up$MU09_2)
table_make_up$`Máxima demanda (equipajes/hora)`[20] <- max(rh_make_up$MU010_1)
table_make_up$`Máxima demanda (equipajes/hora)`[21] <- max(rh_make_up$MU010_2)
table_make_up$`Máxima demanda (equipajes/hora)`[22] <- max(rh_make_up$MU011_1)
table_make_up$`Máxima demanda (equipajes/hora)`[23] <- max(rh_make_up$MU011_2)


##### 16.7 EBS #####
table_ebs <- data.table(`EBS` = c("Automático", "Manual"))
table_ebs[, `Máxima demanda (equipajes/hora)` := 0]
# table_ebs$`Máxima demanda (equipajes/hora)`[1] <- max(rh_ebs$Automatic)
table_ebs[EBS == "Automático", `Máxima demanda (equipajes/hora)` := max(rh_ebs$Automatic)]
# table_ebs$`Máxima demanda (equipajes/hora)`[1] <- max(rh_ebs$Manual)
table_ebs[EBS == "Manual", `Máxima demanda (equipajes/hora)` := max(rh_ebs$Manual)]


