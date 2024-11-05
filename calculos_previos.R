
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


make_up_allocation <- function(ddfs_func, list_car, int_dom, dollies_schedule, capacity_carousels, car_allocation){
  ddfs_func[, Carousel := NA_integer_]
  
  # Loop sobre cada vuelo en el plan de vuelos
  for (i in 1:nrow(ddfs_func)) {
    
    # Obtener la aerolínea y el número de carritos necesarios para el vuelo actual
    airline <- ddfs_func[i, Airline]
    n_dollies <- ddfs_func[i, N_dollies]
    car_open_aux <- ddfs_func$Car_open_aux[i]
    car_close_aux <- ddfs_func$Car_close_aux[i]
    
    # Obtener los carruseles asignados a la aerolínea
    assigned_carousels <- car_allocation[Airline == airline & DOM_INT == int_dom, Carousel]
    
    if (airline %in% list_car) {
      # Aerolíneas con más de un carrusel asignado
      for (car in assigned_carousels) {
        
        current_carousel <- paste0("Car_",car)
        
        # Revisar cuántos carritos hay actualmente en el carrusel en el horario correspondiente
        current_car_load <- dollies_schedule[hour == as.ITime(car_open_aux), .(hour, get(current_carousel))]$V2
        
        # Revisar la capacidad del carrusel
        car_capacity <- capacity_carousels[Carousel == car, Capacity]
        
        # Si cabe el número de carritos en este carrusel
        if ((current_car_load + n_dollies) <= car_capacity) {
          # Asignar este carrusel al vuelo
          ddfs_func[i, Carousel := car]
          
          # Actualizar el número de carritos ocupados en el dollies_schedule
          dollies_schedule[(hour >= as.ITime(car_open_aux) & hour <= as.ITime(car_close_aux)), (current_carousel) := get(current_carousel) + n_dollies]
          
          # Salir del loop si ya se asignó un carrusel
          break
        }
        if(car == assigned_carousels[length(assigned_carousels)]){ break}
      }
      
    } 
  }
  for(air in list_car){
    assigned_carousels <- car_allocation[Airline == airline & DOM_INT == int_dom, Carousel]
    ddfs_func[Airline == air & is.na(Carousel)==TRUE, Carousel := rep(assigned_carousels, length.out = .N)]
  }
  
  to_return <- list(ddfs_func, dollies_schedule)
  
  return(to_return)
}


transfer_allocation <- function(TX, baglist_transf, max_dollies_tx){
  
  time_tx <- 15
  next_tx <- 5
  
  prueba_tx <- baglist_transf[CI_Area == TX][,.N, by = c("Bag Entry Time", "FlightNo")]
  setorder(prueba_tx, `Bag Entry Time`)
  prueba_tx[, Bag_Entry_Time_aux := `Bag Entry Time`]
  prueba_tx[, timetx_start := `Bag Entry Time`]
  prueba_tx[, timetx_stop := `Bag Entry Time` + as.ITime(time_tx*60)]
  
  
  tx_schedule <- data.table( hour = seq(0, 86100, by = 300))
  tx_schedule[, hour := as.ITime(hour)]
  for (i in 1:max_dollies_tx) {
    tx_schedule[, paste0("MUP_",i) := 0]
  }
  
  # Primera entrada equipajes
  start_aux <- prueba_tx$timetx_start[1]
  close_aux <- prueba_tx$timetx_stop[1]
  
  tx_schedule[hour >= start_aux & hour < close_aux, 2 := 1]
  
  
  # Resto entrada equipajes
  
  for(i in 2:length(prueba_tx$`Bag Entry Time`)){
    #for(i in 2:3){
    start_aux <- prueba_tx$timetx_start[i]
    close_aux <- prueba_tx$timetx_stop[i]
    
    if(sum(tx_schedule[hour >= start_aux & hour < close_aux,2]) == 0){
      tx_schedule[hour >= start_aux & hour < close_aux, 2 := 1]
    }else if(sum(tx_schedule[hour >= start_aux & hour < close_aux,2]) != 0){
      if(sum(tx_schedule[hour >= start_aux & hour < close_aux,3]) == 0){
        tx_schedule[hour >= start_aux & hour < close_aux, 3 := 1]
      }else if(sum(tx_schedule[hour >= start_aux & hour < close_aux,3]) != 0){
        start_aux <- start_aux + as.ITime(next_tx*60)
        close_aux <- close_aux + as.ITime(next_tx*60)
        if(sum(tx_schedule[hour >= start_aux & hour < close_aux,2]) == 0){
          tx_schedule[hour >= start_aux & hour < close_aux, 2 := 1]
        }else if(sum(tx_schedule[hour >= start_aux & hour < close_aux,2]) != 0){
          if(sum(tx_schedule[hour >= start_aux & hour < close_aux,3]) == 0){
            tx_schedule[hour >= start_aux & hour < close_aux, 3 := 1]
          }else if(sum(tx_schedule[hour >= start_aux & hour < close_aux,3]) != 0){
            start_aux <- start_aux + as.ITime(next_tx*60)
            close_aux <- close_aux + as.ITime(next_tx*60)
            if(sum(tx_schedule[hour >= start_aux & hour < close_aux,2]) == 0){
              tx_schedule[hour >= start_aux & hour < close_aux, 2 := 1]
            }else if(sum(tx_schedule[hour >= start_aux & hour < close_aux,2]) != 0){
              if(sum(tx_schedule[hour >= start_aux & hour < close_aux,3]) == 0){
                tx_schedule[hour >= start_aux & hour < close_aux, 3 := 1]
              }else if(sum(tx_schedule[hour >= start_aux & hour < close_aux,3]) != 0){
                start_aux <- start_aux + as.ITime(next_tx*60)
                close_aux <- close_aux + as.ITime(next_tx*60)
                if(sum(tx_schedule[hour >= start_aux & hour < close_aux,2]) != 0){
                  if(sum(tx_schedule[hour >= start_aux & hour < close_aux,2]) == 0){
                    tx_schedule[hour >= start_aux & hour < close_aux, 2 := 1]
                  }else if(sum(tx_schedule[hour >= start_aux & hour < close_aux,3]) == 0){
                    tx_schedule[hour >= start_aux & hour < close_aux, 3 := 1]
                  }
                }
              }
            }
          }
        }
      }
    }
    
    prueba_tx$Bag_Entry_Time_aux[i] <- start_aux
  }
  prueba_tx <- prueba_tx[,c("Bag Entry Time", "FlightNo", "Bag_Entry_Time_aux")]
  
  baglist_transf <- merge(baglist_transf, prueba_tx, by = c("Bag Entry Time", "FlightNo"), all = TRUE)
  baglist_transf[CI_Area == TX, `Bag Entry Time` := Bag_Entry_Time_aux]
  baglist_transf[, Bag_Entry_Time_aux := NULL]
  
  return(baglist_transf)
}

tx_akes_distribution <- function(akes_transf, TX){
  
  akes_transf[CI_Area == TX, `Bag Entry Time 2` := as.POSIXct(`Bag Entry Time`, format = "%H:%M", tz = "UTC")]
  akes_transf[CI_Area == TX, `Bag Entry Time Aux` := {
    minutes <- as.numeric(format(`Bag Entry Time 2`, "%M"))
    hours <- as.numeric(format(`Bag Entry Time 2`, "%H"))
    
    total_minutes <- hours*60 + minutes
    
    rounded_minutes <- ((total_minutes %/% 15)+1)*15
    
    rounded_hours <- rounded_minutes %/% 60
    rounded_minutes <- rounded_minutes %% 60
    
    sprintf("%02d:%02d", rounded_hours, rounded_minutes)
  }]
  akes_transf[, `Bag Entry Time Aux` := as.ITime(`Bag Entry Time Aux`)]
  akes_transf[CI_Area == TX, `Bag Entry Time` := `Bag Entry Time Aux`]
  
  akes_transf[, `Bag Entry Time 2` := NULL]
  akes_transf[, `Bag Entry Time Aux` := NULL]
  
  return(akes_transf)
}



#### 1. INPUT DATA  ####
#Se comprueba que los archivos hayan llegado correctamente

#Preparación de datos
ddfs <- as.data.table(ddfs)

setnames(ddfs, c("AEROLINEA", "Numero vuelo", "Proyeccion PAX  Saliendo", "Código IATA"), c("Airline", "FlightNo", "Seats (Total)","Destination"))
ddfs <- as.data.table(ddfs)

ddfs[, FlightNo := paste0(Airline,"-",FlightNo)]

ddfs[, `Seats (Total)` := round(`Seats (Total)`, digits = 0)]

ddfs[, DOM_INT := `Tipo Vuelo Generico`]
ddfs[DOM_INT == "I", DOM_INT := "INT"]
ddfs[DOM_INT == "D", DOM_INT := "DOM"]

ddfs[, Hora_AMPM := ifelse(grepl("p", substr(Hora, nchar(Hora) - 4, nchar(Hora))), 
                           sub(".{5}$", "PM", Hora), 
                           sub(".{5}$", "AM", Hora))]

ddfs[, Departure := as.ITime(strptime(Hora_AMPM, format = "%I:%M:%S %p"))]


ddfs$`Departure Time` <- as.ITime(format(ddfs$Departure, "%H:%M:%S"))
ddfs[, Departure:=NULL]
setnames(ddfs,"Departure Time","Departure")
ddfs$`Departure Time` <- as.ITime(floor(as.numeric(as.ITime(ddfs$`Departure`))/(5*60))*(5*60)) # We round up the Departure Time to the previous 5 minutes interval

make_up_capacity <- data.table(
  Carousel = c(1, 2, 3, 5, 6, 7, 8, 9, 10, 11),
  Capacity = c(48, 50, 50, 30, 32, 32, 22, 28, 22, 22),
  Unit = rep("MUPs", 10)
) 


# Carousel times value

DOM_cierre_AV <- as.numeric(carousel_times[Airline == "Avianca" & Type == "DOM"]$`Close Time`)
DOM_duracion_AV <- as.numeric(carousel_times[Airline == "Avianca" & Type == "DOM"]$`Duration`)
INT_cierre_AV <- as.numeric(carousel_times[Airline == "Avianca" & Type == "INT"]$`Close Time`)
INT_duracion_AV <- as.numeric(carousel_times[Airline == "Avianca" & Type == "INT"]$`Duration`)
DOM_cierre_LA <- as.numeric(carousel_times[Airline == "Latam" & Type == "DOM"]$`Close Time`)
DOM_duracion_LA <- as.numeric(carousel_times[Airline == "Latam" & Type == "DOM"]$`Duration`)
INT_cierre_LA <- as.numeric(carousel_times[Airline == "Latam" & Type == "INT"]$`Close Time`)
INT_duracion_LA <- as.numeric(carousel_times[Airline == "Latam" & Type == "INT"]$`Duration`)
DOM_cierre_rest <- as.numeric(carousel_times[Airline == "Others" & Type == "DOM"]$`Close Time`)
DOM_duracion_rest <- as.numeric(carousel_times[Airline == "Others" & Type == "DOM"]$`Duration`)
INT_cierre_rest <- as.numeric(carousel_times[Airline == "Others" & Type == "INT"]$`Close Time`)
INT_duracion_rest <- as.numeric(carousel_times[Airline == "Others" & Type == "INT"]$`Duration`)


#### 3. PREVIOUS OPERATIONS ####

# Parameters
OD_pax <- OD_pax/100
LF <- LF/100

# # Ops Data
tasa_N2 <- 1.25
tasa_N3 <- 1.25
inspeccion_canina <- "No"
reduccion_car <- 0

if(inspeccion_canina == "Sí" | inspeccion_canina == "sí" | inspeccion_canina == "Si" | inspeccion_canina == "si" | inspeccion_canina == "Yes" | inspeccion_canina == "yes" | inspeccion_canina == "SI" | inspeccion_canina == "SÍ" | inspeccion_canina == "YES"){
  make_up_capacity[, Capacity := Capacity - reduccion_car]
}

# DDFS
ddfs[, `Departure Time` := as.ITime(`Departure Time`)]

make_up_carousel <- as.data.table(make_up_carousel)
checkin_area <- as.data.table(checkin_area)

make_up_carousel[DOM_INT != "DOM" & DOM_INT != "INT", DOM_INT := NA]
checkin_area[DOM_INT != "DOM" & DOM_INT != "INT", DOM_INT := NA]

ddfs[, Pax := ceiling(`Seats (Total)`*LF)]
ddfs[, Pax_OD := ceiling(Pax*OD_pax)]
ddfs[, Pax_transf := Pax - Pax_OD]

ddfs[DOM_INT=="DOM", Bag_OD := ceiling(bag_pax_DOM*Pax_OD)]
ddfs[DOM_INT=="INT", Bag_OD := ceiling(bag_pax_INT*Pax_OD)]
ddfs[, Bag_transf := ceiling(bag_pax_Transf*Pax_transf)]

# Aircraft Types
ddfs2 <- copy(ddfs)

# Number of Dollies
cap_dollies <- 35

ddfs2[, N_dollies := ceiling((Bag_OD + Bag_transf)/cap_dollies)]

# Carousel Times AVIANCA

ddfs2[(Airline == "Avianca") & DOM_INT == "DOM", Car_close := `Departure Time` - as.ITime(DOM_cierre_AV*60)]
ddfs2[(Airline == "Avianca") & DOM_INT == "DOM", Car_open := Car_close - as.ITime(DOM_duracion_AV*60)]
ddfs2[(Airline == "Avianca") & DOM_INT == "DOM", Car_close_aux := Car_close + as.ITime(12*3600) + as.ITime(12*3600)]
ddfs2[(Airline == "Avianca") & DOM_INT == "DOM", Car_open_aux := Car_open + as.ITime(12*3600) + as.ITime(12*3600)]

ddfs2[(Airline == "Avianca" & Airline != "Latam") & DOM_INT == "INT", Car_close := `Departure Time` - as.ITime(INT_cierre_AV*60)]
ddfs2[(Airline == "Avianca" & Airline != "Latam") & DOM_INT == "INT", Car_open := Car_close - as.ITime(INT_duracion_AV*60)]
ddfs2[(Airline == "Avianca" & Airline != "Latam") & DOM_INT == "INT", Car_close_aux := Car_close + as.ITime(12*3600) + as.ITime(12*3600 )]
ddfs2[(Airline == "Avianca" & Airline != "Latam") & DOM_INT == "INT", Car_open_aux := Car_open + as.ITime(12*3600) + as.ITime(12*3600)]

# Carousel Times LA

ddfs2[(Airline == "Latam") & DOM_INT == "DOM", Car_close := `Departure Time` - as.ITime(DOM_cierre_LA*60)]
ddfs2[(Airline == "Latam") & DOM_INT == "DOM", Car_open := Car_close - as.ITime(DOM_duracion_LA*60)]
ddfs2[(Airline == "Latam") & DOM_INT == "DOM", Car_close_aux := Car_close + as.ITime(12*3600) + as.ITime(12*3600)]
ddfs2[(Airline == "Latam") & DOM_INT == "DOM", Car_open_aux := Car_open + as.ITime(12*3600) + as.ITime(12*3600)]

ddfs2[(Airline == "Latam") & DOM_INT == "INT", Car_close := `Departure Time` - as.ITime(INT_cierre_LA*60)]
ddfs2[(Airline == "Latam") & DOM_INT == "INT", Car_open := Car_close - as.ITime(INT_duracion_LA*60)]
ddfs2[(Airline == "Latam") & DOM_INT == "INT", Car_close_aux := Car_close + as.ITime(12*3600) + as.ITime(12*3600 )]
ddfs2[(Airline == "Latam") & DOM_INT == "INT", Car_open_aux := Car_open + as.ITime(12*3600) + as.ITime(12*3600)]


# Carousel Times Rest

ddfs2[(Airline != "Avianca" & Airline != "Latam") & DOM_INT == "DOM", Car_close := `Departure Time` - as.ITime(DOM_cierre_rest*60)]
ddfs2[(Airline != "Avianca" & Airline != "Latam") & DOM_INT == "DOM", Car_open := Car_close - as.ITime(DOM_duracion_rest*60)]
ddfs2[(Airline != "Avianca" & Airline != "Latam") & DOM_INT == "DOM", Car_close_aux := Car_close + as.ITime(12*3600) + as.ITime(12*3600)]
ddfs2[(Airline != "Avianca" & Airline != "Latam") & DOM_INT == "DOM", Car_open_aux := Car_open + as.ITime(12*3600) + as.ITime(12*3600)]

ddfs2[(Airline != "Avianca" & Airline != "Latam") & DOM_INT == "INT", Car_close := `Departure Time` - as.ITime(INT_cierre_rest*60)]
ddfs2[(Airline != "Avianca" & Airline != "Latam") & DOM_INT == "INT", Car_open := Car_close - as.ITime(INT_duracion_rest*60)]
ddfs2[(Airline != "Avianca" & Airline != "Latam") & DOM_INT == "INT", Car_close_aux := Car_close + as.ITime(12*3600) + as.ITime(12*3600 )]
ddfs2[(Airline != "Avianca" & Airline != "Latam") & DOM_INT == "INT", Car_open_aux := Car_open + as.ITime(12*3600) + as.ITime(12*3600)]


#Ordenamos por orden de salida
setorder(ddfs2, `Departure Time`)

# Make-up Carousel - Not Avianca
# Single Carousel and DOM_INT = NA
list_single_car_complete <- unique(make_up_carousel[is.na(DOM_INT)==TRUE, .N, by = Airline][N == 1]$Airline)

ddfs_single_car <- copy(ddfs2)
ddfs_single_car <- ddfs_single_car[Airline %in% list_single_car_complete,]
ddfs_single_car <- merge(ddfs_single_car, make_up_carousel[Airline %in% list_single_car_complete, c(1,2)], by = "Airline")

# Single Carousel and DOM_INT = DOM
list_single_car_DOM <- unique(make_up_carousel[DOM_INT=="DOM", .N, by = Airline][N == 1]$Airline)

ddfs_single_car_DOM <- copy(ddfs2)
ddfs_single_car_DOM <- ddfs_single_car_DOM[Airline %in% list_single_car_DOM & DOM_INT == "DOM",]
ddfs_single_car_DOM <- merge(ddfs_single_car_DOM, make_up_carousel[Airline %in% list_single_car_DOM & DOM_INT == "DOM", c(1,2)], by = "Airline")

# Single Carousel and DOM_INT = INT
list_single_car_INT <- unique(make_up_carousel[DOM_INT=="INT", .N, by = Airline][N == 1]$Airline)

ddfs_single_car_INT <- copy(ddfs2)
ddfs_single_car_INT <- ddfs_single_car_INT[Airline %in% list_single_car_INT & DOM_INT == "INT",]
ddfs_single_car_INT <- merge(ddfs_single_car_INT, make_up_carousel[Airline %in% list_single_car_INT & DOM_INT == "INT", c(1,2)], by = "Airline")

# Join single carousel
ddfs_make_up_single <- rbind(ddfs_single_car, ddfs_single_car_DOM, ddfs_single_car_INT, fill = TRUE)

# Create schedule for single
dollies_schedule_single <- data.table(hour = as.ITime(seq(0,86100, by = 300)))
dollies_schedule_single[, Car_1 := 0]
dollies_schedule_single[, Car_2 := 0]
dollies_schedule_single[, Car_3 := 0]
dollies_schedule_single[, Car_5 := 0]
dollies_schedule_single[, Car_6 := 0]
dollies_schedule_single[, Car_7 := 0]
dollies_schedule_single[, Car_8 := 0]
dollies_schedule_single[, Car_9 := 0]
dollies_schedule_single[, Car_10 := 0]
dollies_schedule_single[, Car_11 := 0]

carousel_list <- unique(make_up_carousel$Carousel)

for(i in 1:length(carousel_list)){
  for(j in 1:length(ddfs_make_up_single[Carousel == carousel_list[i]]$Carousel)){
    if(length(ddfs_make_up_single[Carousel == carousel_list[i]]$Carousel) > 0){
      open_t <- ddfs_make_up_single[Carousel == carousel_list[i]]$Car_open[j]
      close_t <- ddfs_make_up_single[Carousel == carousel_list[i]]$Car_close[j]
      dollies_t <- ddfs_make_up_single[Carousel == carousel_list[i]]$N_dollies[j]
      dollies_schedule_single[hour >= open_t & hour <= close_t, paste0("Car_",carousel_list[i]) := get(paste0("Car_",carousel_list[i])) + dollies_t] 
    }
  }
}


# Various Carousels and DOM_INT = DOM
list_various_car_DOM <- unique(make_up_carousel[DOM_INT=="DOM", .N, by = Airline][N > 1]$Airline)

ddfs_various_car_DOM <- copy(ddfs2)
ddfs_various_car_DOM <- ddfs_various_car_DOM[Airline %in% list_various_car_DOM & DOM_INT == "DOM",]

if(length(ddfs_various_car_DOM$Date)>0){
  make_up_allocation_various_DOM <- make_up_allocation(ddfs_various_car_DOM, list_various_car_DOM, "DOM", dollies_schedule_single, make_up_capacity, make_up_carousel)
  
  ddfs_various_car_DOM_2 <- make_up_allocation_various_DOM[[1]]
  dollies_schedule_various_DOM <- make_up_allocation_various_DOM[[2]]
} else{
  ddfs_various_car_DOM_2 <- copy(ddfs_various_car_DOM)
  ddfs_various_car_DOM_2[, Carousel := NA]
  dollies_schedule_various_DOM <- copy(dollies_schedule_single)
}

# Various Carousels and DOM_INT = INT
list_various_car_INT <- unique(make_up_carousel[DOM_INT=="INT", .N, by = Airline][N > 1]$Airline)

ddfs_various_car_INT <- copy(ddfs2)
ddfs_various_car_INT <- ddfs_various_car_INT[Airline %in% list_various_car_INT & DOM_INT == "INT",]

if(length(ddfs_various_car_INT$Date)>0){
  make_up_allocation_various_INT <- make_up_allocation(ddfs_various_car_INT, list_various_car_INT, "INT", dollies_schedule_various_DOM, make_up_capacity, make_up_carousel)
  
  ddfs_various_car_INT_2 <- make_up_allocation_various_INT[[1]]
  dollies_schedule_various_INT <- make_up_allocation_various_INT[[2]]
} else{
  ddfs_various_car_INT_2 <- copy(ddfs_various_car_INT)
  ddfs_various_car_INT_2[, Carousel := NA]
  dollies_schedule_various_INT <- copy(dollies_schedule_various_DOM)
}

# Various Carousels and DOM_INT = NA
list_various_car_complete <- unique(make_up_carousel[is.na(DOM_INT)==TRUE, .N, by = Airline][N > 1]$Airline)

ddfs_various_car <- copy(ddfs2)
ddfs_various_car <- ddfs_various_car[Airline %in% list_various_car_complete,]

if(length(ddfs_various_car$Date)>0){
  make_up_allocation_various <- make_up_allocation(ddfs_various_car, list_various_car_complete, NA, dollies_schedule_various_INT, make_up_capacity, make_up_carousel)
  
  ddfs_various_car_2 <- make_up_allocation_various[[1]]
  dollies_schedule_various_complete <- make_up_allocation_various[[2]]
} else{
  ddfs_various_car_2 <- copy(ddfs_various_car)
  ddfs_various_car_2[, Carousel := NA]
  dollies_schedule_various_complete <- copy(dollies_schedule_various_INT)
}

# Join
ddfs_make_up_asignado <- rbind(ddfs_make_up_single, ddfs_various_car_DOM_2, ddfs_various_car_INT_2, ddfs_various_car_2, fill = TRUE)


# Check-in Area
# Single Check-in and DOM_INT = NA
list_single_ci_complete <- unique(checkin_area[is.na(DOM_INT)==TRUE, .N, by = Airline][N == 1]$Airline)

ddfs_single_ci <- copy(ddfs_make_up_asignado)
ddfs_single_ci <- ddfs_single_ci[Airline %in% list_single_ci_complete,]
ddfs_single_ci <- merge(ddfs_single_ci, checkin_area[Airline %in% list_single_car_complete, c(1,2)], by = "Airline")

# Single Check-in and DOM_INT = DOM
list_single_ci_DOM <- unique(checkin_area[DOM_INT=="DOM", .N, by = Airline][N == 1]$Airline)

ddfs_single_ci_DOM <- copy(ddfs_make_up_asignado)
ddfs_single_ci_DOM <- ddfs_single_ci_DOM[Airline %in% list_single_ci_DOM & DOM_INT == "DOM",]
ddfs_single_ci_DOM <- merge(ddfs_single_ci_DOM, checkin_area[Airline %in% list_single_ci_DOM & DOM_INT == "DOM", c(1,2)], by = "Airline")

# Single Check-in and DOM_INT = INT
list_single_ci_INT <- unique(checkin_area[DOM_INT=="INT", .N, by = Airline][N == 1]$Airline)

ddfs_single_ci_INT <- copy(ddfs_make_up_asignado)
ddfs_single_ci_INT <- ddfs_single_ci_INT[Airline %in% list_single_ci_INT & DOM_INT == "INT",]
ddfs_single_ci_INT <- merge(ddfs_single_ci_INT, checkin_area[Airline %in% list_single_ci_INT & DOM_INT == "INT", c(1,2)], by = "Airline")

# Various Check-in and DOM_INT = DOM
list_various_ci_DOM <- unique(checkin_area[DOM_INT=="DOM", .N, by = Airline][N > 1]$Airline)

ddfs_various_ci_DOM <- copy(ddfs_make_up_asignado)
ddfs_various_ci_DOM <- ddfs_various_ci_DOM[Airline %in% list_various_ci_DOM & DOM_INT == "DOM",]

for(i in 1:length(list_various_ci_DOM)){
  ddfs_various_ci_DOM[Airline == list_various_ci_DOM[i] & DOM_INT == "DOM", CI_Area := rep(checkin_area[Airline == list_various_ci_DOM[i] & DOM_INT == "DOM"]$CI_Area, length.out = .N)]
}

# Various Check-in and DOM_INT = INT
list_various_ci_INT <- unique(checkin_area[DOM_INT=="INT", .N, by = Airline][N > 1]$Airline)

ddfs_various_ci_INT <- copy(ddfs_make_up_asignado)
ddfs_various_ci_INT <- ddfs_various_ci_INT[Airline %in% list_various_ci_INT & DOM_INT == "INT",]

for(i in 1:length(list_various_ci_INT)){
  ddfs_various_ci_INT[Airline == list_various_ci_INT[i] & DOM_INT == "INT", CI_Area := rep(checkin_area[Airline == list_various_ci_INT[i] & DOM_INT == "INT"]$CI_Area, length.out = .N)]
}

# Various Check-in and DOM_INT = NA
list_various_ci_complete <- unique(checkin_area[is.na(DOM_INT) == TRUE, .N, by = Airline][N > 1]$Airline)

ddfs_various_ci_complete <- copy(ddfs_make_up_asignado)
ddfs_various_ci_complete <- ddfs_various_ci_complete[Airline %in% list_various_ci_INT & is.na(DOM_INT) == TRUE]

for(i in 1:length(list_various_ci_complete)){
  ddfs_various_ci_complete[Airline == list_various_ci_complete[i] & is.na(DOM_INT) == TRUE, CI_Area := rep(checkin_area[Airline == list_various_ci_INT[i] & is.na(DOM_INT) == TRUE]$CI_Area, length.out = .N)]
}

ddfs_ci_asignado <- rbind(ddfs_single_ci, ddfs_single_ci_DOM, ddfs_single_ci_INT, ddfs_various_ci_complete, ddfs_various_ci_DOM, ddfs_various_ci_INT, fill = TRUE)

setorder(ddfs_ci_asignado, `Departure Time`)

ddfs_complete <- copy(ddfs_ci_asignado)


