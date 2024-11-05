#Mensajes de depuracion para la imagen de docker
#options(shiny.trace = TRUE)
#options(shiny.sanitize.errors = TRUE)

#Cargar los paquetes necesarios
library(shiny)
library(shinydashboard)
library(readxl)
library(data.table)
library(dplyr)
library(shinythemes)
library(readr)
library(DT)
library(webshot2)
library(htmlwidgets)
library(htmltools)
library(pagedown)
library(shinycssloaders)
library(writexl)
library(plotly)
library(rsconnect)
library(showtext)
library(shinybusy)
library(shinyscreenshot)
library(bsicons)
library(knitr)

#Joan
library(bslib)

wd <- getwd()

# Cargar y habilitar el uso de fuentes personalizadas
font_add_google("Lato", "lato")
showtext_auto()


gantt_calc <- function(ddfs_base, car_number){
  ddfs_aux <- copy(ddfs_base[Carousel == car_number, c("FlightNo","Departure","Car_close_aux","Car_open_aux")])
  ddfs_aux[, Car_open_1 := as.ITime(Car_open_aux)]
  ddfs_aux[, opentime_aux1 := as.numeric(Car_open_1)]
  ddfs_aux[, Car_close_1 := as.ITime(Car_close_aux)]
  ddfs_aux[, closetime_aux1 := as.numeric(Car_close_1)]
  setorder(ddfs_aux, -Departure, -opentime_aux1, -closetime_aux1)
  
  ddfs_aux[, FlightNo := factor(FlightNo, levels = unique(FlightNo))]
  
  j <- 1
  ddfs_result <- copy(ddfs_aux[0,])
  for(i in 1:length(ddfs_aux$FlightNo)){
    
    open_hour <- ddfs_aux$opentime_aux1[i]
    close_hour <- ddfs_aux$closetime_aux1[i]
    
    if(close_hour > open_hour){
      ddfs_result <- rbind(ddfs_result, ddfs_aux[i,])
    }
    
    if(close_hour < open_hour){
      ddfs_result <- rbind(ddfs_result, ddfs_aux[i,])
      ddfs_result$opentime_aux1[j] <- 0
      ddfs_result$Car_open_1[j] <- as.ITime(0)
      j <- j + 1
      ddfs_result <- rbind(ddfs_result, ddfs_aux[i,])
      ddfs_result$closetime_aux1[j] <- 86400
      ddfs_result$Car_close_1[j] <- as.ITime(12*3600) + as.ITime(12*3600)
    }
    j <- j +1 
  }
  return(ddfs_result)
}

#### 1.Definir la interfaz de usuario del dashboard ####
ui <- navbarPage(
  
  ##### 1.1. Definir el dashboard header #####
  
  title = tags$span(
    tags$img(
      src = "BOGagePlannerv2.svg",
      width = "120px",
      height = "auto",
      class = "me-3",
      align = "right",
      alt = "Shiny hex logo"
    ),),
  
  theme = bs_theme(version = 5, bootswatch = "cosmo",),  # Cambia la versión y el tema según prefieras
  
  tags$head(tags$style(
    HTML('

         @import url("https://fonts.googleapis.com/css?family=Lato&display=swap");

         #sidebar {
         background-color: white;
         }

         * {
         font-family: "Lato";
         }
         
         .navbar {
        position: fixed;
        top: 0;
        width: 100%;
        z-index: 1000;
      }
      
      /* Agregar un margen superior para evitar que el contenido se superponga con el menú */
      body {
        padding-top: 60px;
      }
         
         
         
         /* Estilos para hacer que el eje X esté fijo */
         .fixed-x-axis {
           position: sticky;
           top: 0;
           background-color: white;
           z-index: 10000;
         }

         /* Contenedor que permitirá el scroll */
         .scroll-container {
           max-height: 500px;  /* Ajusta según sea necesario */
           overflow-y: scroll;
         }
         
         .disclaimer-background {
              background-image: url("Background.png");
              background-size: cover;
              background-position: center;
              background-repeat: no-repeat;
              background-attachment: fixed;
              min-height: 100vh; /* Asegura que ocupe todo el alto del viewport */
              width: 100vw;
              margin: 0;
              padding: 0;
              position: absolute; /* Asegura que cubra todo el contenedor */
              top: 70px;
              left: 0;
         }
         
         .navbar-text {
          position: absolute;
          right: 20px;
          top: 12px;
          font-size: 14px;
          color: #ffffff;
        }
         
         ')
  )),
            
           
  
  tabPanel("Parámetros de Entrada",
           div(style = "height:50px"),
           
           use_busy_spinner(spin = "circles-to-rhombuses", height = "100px", width = "100px", color = "#FFF", margins = c(25, 20),),
           
           layout_columns(
             fluidRow(),
             card(
               navset_underline( 
                 nav_panel("Parámetros de pasajeros",    
                           div(style = "height:15px"),
                           fluidRow(fileInput("archivo1", "Plan de Vuelos:", accept = c(".xlsx"), width = "100%")),
                           fluidRow(fileInput("archivo2", "Perfiles de Presentación. Máx. 8 horas antes del STD:", accept = c(".xlsx"), width = "100%"), uiOutput("info_hojas_error")),
                           fluidRow(fileInput("archivo_fixed_parameters", "Otros Parámetros:", accept = c(".xlsx"), width = "100%"), uiOutput("info_hojas_fixed_error")),
                           tags$div(
                             style = "position: absolute; top: -1px; left: 370px;",  # Ajusta las posiciones según sea necesario
                           ),),),style = "background-color: #f0f0f0; border-radius: 8px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",
               
             ),
             card(    
               navset_underline( 
                 nav_panel("Parámetros pasajeros",
                           div(style = "height:15px"),
                           layout_columns(
                             fluidRow(
                               numericInput("valor1", "Pasajeros O/D (%):", value = 60, min = 0.0, max = 100, step = 0.01, width = "100%")
                             ),
                             fluidRow(
                               numericInput("valor2", "Load Factor (%):", value = 88, min = 0.0, max = 100, step = 0.01, width = "100%")
                             ),
                             fluidRow(
                               numericInput("valor3", "Equipajes/pasajero (doméstico):", value = 0.23, min = 0.0, max = 1000, step = 0.01, width = "100%")
                             ),
                             fluidRow(
                               numericInput("valor4", "Equipajes/pasajero (internacional):", value = 0.72, min = 0.0, max = 1000, step = 0.01, width = "100%")
                             ),
                             fluidRow(
                               numericInput("valor5", "Equipajes/pasajero (transferencias):", value = 0.4, min = 0.0, max = 1000, step = 0.01, width = "100%")
                             ),
                             col_widths = c(6,6)
                           ),), 
                 nav_panel("Parámetros inspección", 
                           div(style = "height:15px"),
                           layout_columns(
                             fluidRow(numericInput("valor7", "Equipajes aprobados en nivel 1 (%):", value = 80, min = 0.0, max = 100, step = 0.01, width = "90%")),
                             fluidRow(numericInput("valor100", "Equipajes aprobados en nivel 2 (%):", value = 17, min = 0.0, max = 100, step = 0.01, width = "90%")),
                             fluidRow(numericInput("valor10", "Equipajes rechazados en nivel 2 (%):", value = 2, min = 0.0, max = 100, step = 0.01, width = "90%")),
                             fluidRow(numericInput("valor9", "Equipajes No Pic (%):", value = 1, min = 0.0, max = 100, step = 0.01, width = "90%")),
                             col_widths = c(6,6)
                           ),
                           fluidRow(uiOutput("warning_message")),  # Espacio para el mensaje de advertencia
                           ), 
                 nav_panel("Parámetros operadores", 
                           
                           div(style = "height:15px"),
                           layout_columns(
                             fluidRow(class = "flex-item", sliderInput("valor13", "Capacidad de un operador de nivel 2 (equipajes/hora):", value = 120, min = 1.0, max = 600, step = 1, width = "100%")),
                             fluidRow(class = "flex-item", sliderInput("valor14", "Capacidad de un operador de nivel 3 (equipajes/hora):", value = 24, min = 1.0, max = 100, step = 1, width = "100%")),
                             
                             col_widths = c(6,6)
                           ),
                 ), 
                 nav_panel("Parámetros operativos", 
                           div(style = "height:15px"),
                           layout_columns(
                             fluidRow(numericInput("valor6", "Recirculación de equipajes en el sorter (%):", value = 8, min = 0.0, max = 100, step = 0.01, width = "100%")),
                             fluidRow(numericInput("valor11", "Equipajes no leídos por ATR (%):", value = 2, min = 0.0, max = 100, step = 0.01, width = "100%")),
                             fluidRow(numericInput("valor12", "Equipajes lost-tracking previo a EDS (%):", value = 1, min = 0.0, max = 100, step = 0.01, width = "100%")),
                             fluidRow(numericInput("valor8", "Equipajes lost-tracking tras EDS (%):", value = 1, min = 0.0, max = 100, step = 0.01, width = "100%")),
                             
                             col_widths = c(6,6)
                           ),
                 ),
               ),      style = "background-color: #f0f0f0; border-radius: 8px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",
             ),
             fluidRow(),
             col_widths = c(1,3,7,1)
           ),
           layout_columns(
             fluidRow(),
             fluidRow(
                box(
                  title = NULL, status = "primary", solidheader = TRUE, width = NULL, actionButton("load_data", "Cargar Datos", icon = icon("upload"), width = "100%", disabled = FALSE, style = "border-radius: 8px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1)"),
                ),
                
                fluidRow(
                  box(
                    title = NULL, 
                    status = "primary", 
                    solidHeader = TRUE, 
                    
                    uiOutput("loading_message1"),  # Loading message here
                    uiOutput("error_hojas_ddfs"),
                    uiOutput("error_hojas"),
                    uiOutput("error_hojas_fixed"),
                  ),
                  
                ),
                
                div(style = "height:20px"),
                p("Consola:"),
                box(title = NULL, status = "primary", solidHeader = TRUE, width = NULL,     style = "background-color: #fafafa ; border-radius: 8px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); font-family: 'Lucida Console', Monaco, monospace; height: 200px; overflow-y: auto;font-size: 12px; line-height: 0.5;",  # Custom height and scroll
                                                                                               p(""),
                                                                                               uiOutput("message_verificacion"),
                                                                                               uiOutput("message_make_up_dist"),
                                                                                               uiOutput("message_ci_dist"),
                                                                                               uiOutput("message_cap_make_up"),
                                                                                               uiOutput("message_cap_line"),
                                                                                               uiOutput("message_carousel_times"),
                                                                                               uiOutput("message_ebs"),
                                                                                               uiOutput("message_archivo1"),
                                                                                               uiOutput("message_archivo2"),
                                                                                               uiOutput("message_archivo3"),
                                                                                               uiOutput("message_archivo4"),
                                                                                               uiOutput("message_archivo5"),
                                                                                               uiOutput("message_archivo6"),
                                                                                               uiOutput("message_archivo7"),
                ),
             ),
             fluidRow(),
             col_widths = c(1,10,1)
           ),
  ),
  
  tabPanel("Datos de entrada",
           
           div(style = "height:50px"),
           

           
           layout_columns(
           fluidRow(),
           fluidRow(
             
             nav_panel(
               "Paneles",
               use_busy_spinner(spin = "circles-to-rhombuses", height = "100px", width = "100px", color = "#FFF", margins = c(25, 20),),
               splitLayout(

                 p(uiOutput("value_box1")),
                 p(uiOutput("value_box2")),
                 p(uiOutput("value_box3")),
                       
                 
                 cellWidths = "33.33%"
                 ),
             ),
             
             box(
               title = NULL, status = "primary", solidheader = TRUE, width = NULL, actionButton("run_calculations", "Ejecutar Cálculos", icon = icon("play"), width = "100%", disabled = FALSE, style = "border-radius: 8px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); margin-bottom: 15px;"), uiOutput("loading_message2"), # Mensaje de carga aquí
               ),
             
             card(
               navset_underline( 
               nav_panel("Plan de vuelos",
                         div(style = "height:15px"),
                         DTOutput("table_ddfs")),
               nav_panel("Asignación Check-In",
                         
                         tags$div(
                           tags$img(src = "Check_in_app.svg", style = "display: block; margin: 0 auto; width: 60%; height: auto;"),  # Ajusta 'src' con la ruta de tu imagen
                           style = "text-align: center; padding-bottom: 10px;"
                         ),
                         
                         tags$div(style = "height: 15px;"),
                         
                         splitLayout(
                           
                           p(uiOutput("value_box4")),
                           p(uiOutput("value_box5")),
                           p(uiOutput("value_box6")),
                           
                           cellWidths = "33.33%"
                         ),
                         
                         splitLayout(
                           
                           p(uiOutput("value_box7")),
                           p(uiOutput("value_box8")),
                           p(uiOutput("value_box9")),
                           
                           cellWidths = "33.33%"
                         ),
                         
                         ),
               nav_panel("Asignación Carruseles Make-Up",
                         
                         navset_underline(  # Crea pestañas internas
                           nav_panel("Carrusel 1",
                                     style = "margin-top: 20px;",
                                     uiOutput("dynamic_plot_gant_car1")  # El gráfico de Gantt con altura dinámica
                                     
                           ),
                           nav_panel("Carrusel 2", 
                                     style = "margin-top: 20px;",
                                     uiOutput("dynamic_plot_gant_car2")
                                     
                           ),
                           nav_panel("Carrusel 3", 
                                     style = "margin-top: 20px;",
                                     uiOutput("dynamic_plot_gant_car3")
                                     
                           ),
                           nav_panel("Carrusel 5", 
                                     style = "margin-top: 20px;",
                                     uiOutput("dynamic_plot_gant_car5")
                                     
                           ),
                           nav_panel("Carrusel 6", 
                                     style = "margin-top: 20px;",
                                     uiOutput("dynamic_plot_gant_car6")
                                     
                           ),
                           nav_panel("Carrusel 7", 
                                     style = "margin-top: 20px;",
                                     uiOutput("dynamic_plot_gant_car7")
                                     
                           ),
                           nav_panel("Carrusel 8", 
                                     style = "margin-top: 20px;",
                                     uiOutput("dynamic_plot_gant_car8")
                                     
                           ),
                           nav_panel("Carrusel 9", 
                                     style = "margin-top: 20px;",
                                     uiOutput("dynamic_plot_gant_car9")
                                     
                           ),
                           nav_panel("Carrusel 10", 
                                     style = "margin-top: 20px;",
                                     uiOutput("dynamic_plot_gant_car10")
                                     
                           ),
                           nav_panel("Carrusel 11", 
                                     style = "margin-top: 20px;",
                                     uiOutput("dynamic_plot_gant_car11")
                                     
                           ),
                           
                         ),
               ),
                         
               nav_panel("Perfiles de presentación",

                         navset_underline(  # Crea pestañas internas
                           nav_panel("Aerolíneas HUB",
                                     style = "margin-top: 20px;",
                                     fluidRow(
                                       # Primera columna para la figura (gráfico)
                                       column(width = 8, 
                                              plotOutput("showup_HUB")),
                                       
                                       # Segunda columna para la tabla
                                       column(width = 4, 
                                              dataTableOutput("table_showup_HUB"))
                                     )
                                     
                           ),
                           nav_panel("Aerolíneas No HUB", 
                                     style = "margin-top: 20px;",
                                     fluidRow(
                                       # Primera columna para la figura (gráfico)
                                       column(width = 8, 
                                              plotOutput("showup_NoHUB")),
                                       
                                       # Segunda columna para la tabla
                                       column(width = 4, 
                                              dataTableOutput("table_showup_NoHUB"))
                                     )
                                     
                                     
                           ),
                           nav_panel("Transferencias", 
                                     style = "margin-top: 20px;",
                                     fluidRow(
                                       # Primera columna para la figura (gráfico)
                                       column(width = 8, 
                                              plotOutput("showup_Transfer")),
                                       
                                       # Segunda columna para la tabla
                                       column(width = 4, 
                                              dataTableOutput("table_showup_Transfer"))
                                     )
                                     
                           ),
                           
                         ),
                         
                         ),
               
             ),    width = "100%",  style = "background-color: #f0f0f0; border-radius: 8px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",),

             # box(
             #   title = NULL, status = "primary", solidheader = TRUE, width = NULL, actionButton("run_calculations", "Ejecutar Cálculos", icon = icon("play"), width = NULL, disabled = FALSE), uiOutput("loading_message2"), add_busy_spinner(spin = "fading-circle"), # Mensaje de carga aquí
             # )
           ),
           
           fluidRow(),
           
  
           col_widths = c(1,10,1)
           ),
  ),
  
 
  tabPanel("Resultados",
           fluidRow(
             column(1),
             column(10,
                    downloadButton("download_html", "Descargar Resultados en Informe", style = "border-radius: 8px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
                                margin-right: 40px; margin-left: 40px; width: 100%; margin: 0 auto;")
             ),
             column(1),
           ),
           
           tags$div(style = "height: 10px;"),
           
           layout_columns(
             
             fluidRow(),
             
             fluidRow(
               
               column(12,
                      div(
                        style = "background-color: #f0f0f0; border-radius: 8px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); width: 100%; padding: 20px;",
                        
                        # Título de la sección (simulando la cabecera de una box)
                        tags$h4("Selección de Resultados", style = "margin-bottom: 15px;"),
                        
                        # SelectInput con 100% de ancho
                        selectInput("select_option", "Selecciona un área del sistema para ver los resultados:", 
                                    choices = list("Entrada de equipajes al sistema" = "bag_entry",
                                                   "Sorter" = "sorter",
                                                   "Líneas EDS" = "eds",
                                                   "Nivel 3" = "nivel3",
                                                   "Estación de Codificación Manual" = "mes",
                                                   "Carruseles de make-up" = "make_up",
                                                   "Early Baggage Storage (EBS)" = "ebs"),
                                    width = "100%")
                      )
               )
             ),
             
             fluidRow(),
             
             col_widths = c(1,10,1)
           ),
           
           
           tags$div(style = "height: 10px;"),
           
           tags$div(style = "height: 10px;"),
             
           # Contenido condicional basado en el valor del desplegable
           uiOutput("dynamic_content_resultados"),
             
          
  ),
  
  tabPanel("Manual de Usuario",
           
           div(style = "height:50px"),
           
           layout_columns(
             fluidRow(),
             
             box(title = "Manual de Usuario", 
                 status = "primary",
                 solidHeader = TRUE,
                 width = NULL,
                 
                 div(style = "display: flex; justify-content: center; align-items: center;",
                     tags$iframe(src = "manual_usuario.pdf", style = "width: 100%; height: 100vh; border: none; background-color: transparent; margin-bottom: 50px;")
                 )    
             ),
             
             fluidRow(),
             
             col_widths = c(1,10,1)
           )
           
          
  ),
  
  tabPanel("Disclaimer",
           
           div(class = "disclaimer-background",
               div(style = "height:50px"),
               
               #setBackgroundImage(
               #src = "aeropuerto-el-dorado.png",
               #shinydashboard = FALSE
               #),
              
               layout_columns(
                 
                 fluidRow(),
                 
                 
                 fluidRow(div(style = "margin-right: 0px; margin-left: 0px; justify-content: center; text-align: justify; background-color: rgba(50, 50, 50, 0.9); padding: 20px; border-radius: 20px; color: white;",
                              box(status = "primary",
                                  solidHeader = TRUE,
                                  width = NULL,
                                  div(style = "display: flex; justify-content: center; align-items: center;",
                                      tags$img(src = "LogosDisclaimer.svg", height = "200px", width = "800px", style = "vertical-align: middle; margin-right: 10px;")
                                  )  
                              ),
                              tags$br(),
                              tags$br(),
                              
                                  p("La aplicación", strong("BOGage Planner"), ", desarrollada por ", strong("LATAM LOGISTICS"), "para
                    el", strong("cálculo estático del Sistema de Manejo de Equipajes (BHS) del Aeropuerto Internacional
                    El Dorado de Bogotá"), ", ha sido creada exclusivamente para uso de", strong("OPAIN S.A."), "concesionario y operador del aeropuerto.
                    Esta aplicación tiene como objetivo facilitar la simulación y cálculo de cargas estáticas del sistema BHS, con base en datos
                    y parámetros específicos proporcionados por OPAIN y otras entidades autorizadas. Para asistencia técnica, dudas o problemas con la aplicación, por favor contacte a", 
                                    strong("LATAM LOGISTICS"), " en el correo ", tags$a(href="mailto:BOGagePlanner-helpdesk@latamlogistics.es", "BOGagePlanner-helpdesk@latamlogistics.es"), 
                                    ". Estaremos encantados de ayudarle en caso de cualquier inconveniente."),
                                  h6("Alcance y Limitaciones"),
                                  tags$ul(
                                    tags$li("La aplicación está diseñada para uso técnico e interno y no debe
                            ser utilizada como base única para la toma de decisiones operativas, de mantenimiento
                            o de inversión sin la validación correspondiente por parte de expertos en el sistema BHS."),
                                    tags$li("LATAM LOGISTICS no se hace responsable por el uso indebido o fuera del alcance definido de esta aplicación.")
                                  ),
                                  h6("Exclusión de Responsabilidad"),
                                  p("LATAM LOGISTICS no garantiza la ", strong("exactitud, integridad o adecuación"), " de los cálculos y simulaciones generados por
                  la aplicación para todos los escenarios posibles, ya que estos dependen de las entradas de datos y los parámetros definidos por
                  los usuarios finales. El uso de los resultados debe estar acompañado de una revisión y verificación adicional por parte del personal
                  técnico de OPAIN y/o consultores externos especializados."),
                                  h6("Propiedad Intelectual"),
                                  p("La aplicación y su contenido son propiedad exclusiva de LATAM LOGISTICS. Está prohibida cualquier reproducción, distribución
                  o modificación del software sin el consentimiento expreso y por escrito de LATAM LOGISTICS.", strong("OPAIN S.A."), " es el único autorizado para operar y utilizar
                  esta aplicación en el contexto del proyecto del BHS en el Aeropuerto Internacional El Dorado. Cualquier uso no autorizado o su distribución
                  fuera de este alcance estará sujeto a acciones legales. Para cualquier duda o aclaración adicional
                  sobre el uso o alcance de la aplicación, por favor contacte a ", strong("LATAM LOGISTICS"), " directamente."),
                                  
                 )),
                 
                 fluidRow(),
                 
                 col_widths = c(2,8,2)
                 
               ),
           )
           
  ),
  
  # tags$div(class = "navbar-text", "Texto adicional aquí"),

) #End dashboard page

#### 2. Definir Funciones ####
### Se definen las funciones que se vayan a utilizar en el servidor de la aplicación ###

### 2a. Función para verificar columnas en el archivo .csv ###
verificar_columnas <- function(datos, num_columnas, nombre_archivo) {
  if (ncol(datos) == num_columnas) {
    return(list(valido = TRUE, mensaje = sprintf("El archivo %s se ha cargado correctamente.", nombre_archivo)))
    #return("El archivo .csv tiene el número correcto de columnas.")
  } else {
    return(list(valido = FALSE, mensaje = sprintf("Error: El archivo %s tiene %d columnas. Se esperaban %d columnas.", nombre_archivo, ncol(datos), num_columnas)))
  }
}

### 2b. Función para leer los archivos ###
leer<-function(ruta_archivo, numero_hoja){
  ext <- tools::file_ext(ruta_archivo)
  switch(ext,
         xlsx = read_excel(ruta_archivo, sheet = numero_hoja),
         stop("Formato de archivo inválido; por favor suba un archivo .xlsx"))
}


#### 3. Definir el servidor para el dashboard ####
server <- function(input, output) {
  #### 3.1 Data ####
  #Se definen los archivos que serán predeterminados
  data <- reactiveValues(
    archivo1 = NULL,
    archivo2 = NULL,
    archivo3 = NULL,
    archivo4 = NULL,
    archivo5 = NULL,
    archivo6 = NULL,
    archivo7 = NULL,
    archivo1_mostrar = NULL,
    
    
    verificacion_make_up_dist= NULL,
    verificacion_ci_dist = NULL,
    verificacion_cap_make_up = NULL,
    verificacion_cap_line = NULL,
    verificacion_ebs = NULL,
    verificacion_dom= NULL,
    verificacion_carousel_times= NULL
  )
  
  #### 3.2 Carga de archivos predeterminados ####
  # Cargar archivo predeterminado comprobando su existencia
  
  #TABLAS INPUT

  output$value_box1 <- renderUI({
value_box(
      title = "UN TOTAL DE",
      value = paste(length(data$archivo1_mostrar$Hora), "vuelos"),
      tags$p(paste(
        "A", length(unique(data$archivo1_mostrar$`Código IATA`)), "destinos distintos"
      )),
      tags$p(paste(
        "Con", length(unique(data$archivo1_mostrar$AEROLINEA)), "aerolíneas distintas"
      )),
      showcase = bsicons::bs_icon("airplane"), # Ensure the bsicons library is loaded
      theme = value_box_theme(bg = "#373a3cff", fg = "#fafafa"),   
      height = 150,
      style = "border-radius: 8px;",
      
    )
})

    output$value_box2 <- renderUI({
    value_box(
      title = "UN TOTAL DE",
      value = paste(sum(data$archivo1_mostrar$`Proyeccion PAX  Saliendo`), "asientos"),
      tags$p(paste(
        "Media de", round(mean(data$archivo1_mostrar$`Proyeccion PAX  Saliendo`), digits = 1), "asientos por aeronave"
      )),
      showcase = bsicons::bs_icon("person"), # Ensure the bsicons library is loaded)
      theme = value_box_theme(bg = "#373a3cff", fg = "#fafafa"),   
      height = 150,
      style = "border-radius: 8px;",
    )
  })

    output$value_box3 <- renderUI({
      value_box(
        title = "UN TOTAL DE",
        value = HTML(paste(
          "<span style='font-size: 22px;'>",  # Ajusta el tamaño de la fuente aquí
          length(ddfs_complete[DOM_INT == "DOM"]$DOM_INT), "vuelos domésticos", "<br>",
          length(ddfs_complete[DOM_INT == "INT"]$DOM_INT), "vuelos internacionales", 
          "</span>"
        )),
        
        
        showcase = bsicons::bs_icon("pin-map"), # Ensure the bsicons library is loaded)
        theme = value_box_theme(bg = "#373a3cff", fg = "#fafafa"),   
        height = 150,
        style = "border-radius: 8px;",
      )
    })
    
    output$value_box4 <- renderUI({
      
      copy_ci_dist <- copy(as.data.table(data$ci_dist)[CI_Area == "A1"])
      copy_ci_dist <- copy_ci_dist[Airline %in% unique(data$archivo1$`AEROLINEA`),]
      copy_ci_dist[DOM_INT == "DOM", Airline := paste0(Airline," - Doméstico")]
      copy_ci_dist[DOM_INT == "INT", Airline := paste0(Airline," - Internacional")]
      
      unique_values <- unique(unlist(copy_ci_dist$Airline))
      if(length(unique_values)>0){value <- paste(unique_values, collapse = "<br>")}
      if(length(unique_values)==0){value <- paste("No hay aerolíneas", " asignadas")}
      
      value_box(
        # title = tags$div("Área A1", style = "font-size: 20px; font-weight: bold; margin-top: 10px;"),
        title = NULL,
        value = HTML(paste0('<div style="font-size: 18px; height: 150px; overflow-y: auto;">', value, '</div>')),
        showcase = tags$img(src = "A1.svg", width = "40px", height = "40px"), 
        # showcase = bsicons::bs_icon("suitcase"), # Ensure the bsicons library is loaded)
        theme = value_box_theme(bg = "#373a3cff", fg = "#fafafa"),   
        height = 200,
        style = "border-radius: 8px; overflow-y: auto;",
      )
    })
    
    output$value_box5 <- renderUI({

      copy_ci_dist <- copy(as.data.table(data$ci_dist)[CI_Area == "B1"])
      copy_ci_dist <- copy_ci_dist[Airline %in% unique(data$archivo1$`AEROLINEA`),]
      copy_ci_dist[DOM_INT == "DOM", Airline := paste0(Airline," - Doméstico")]
      copy_ci_dist[DOM_INT == "INT", Airline := paste0(Airline," - Internacional")]

      unique_values <- unique(unlist(copy_ci_dist$Airline))
      if(length(unique_values)>0){value <- paste(unique_values, collapse = "<br>")}
      if(length(unique_values)==0){value <- paste("No hay aerolíneas", " asignadas")}

      value_box(
        # title = tags$div("Área B1", style = "font-size: 20px; font-weight: bold;margin-top: 10px;"),
        title = NULL,
        value = HTML(paste0('<div style="font-size: 18px; height: 150px; overflow-y: auto;">', value, '</div>')),
        showcase = tags$img(src = "B1.svg", width = "40px", height = "40px"), 
        # showcase = bsicons::bs_icon("suitcase"), # Ensure the bsicons library is loaded)
        theme = value_box_theme(bg = "#373a3cff", fg = "#fafafa"),
        height = 200,
        style = "border-radius: 8px; overflow-y: auto;",
      )
    })
    
    
    output$value_box6 <- renderUI({
      
      copy_ci_dist <- copy(as.data.table(data$ci_dist)[CI_Area == "B2"])
      copy_ci_dist <- copy_ci_dist[Airline %in% unique(data$archivo1$`AEROLINEA`),]
      copy_ci_dist[DOM_INT == "DOM", Airline := paste0(Airline," - Doméstico")]
      copy_ci_dist[DOM_INT == "INT", Airline := paste0(Airline," - Internacional")]
      
      unique_values <- unique(unlist(copy_ci_dist$Airline))
      if(length(unique_values)>0){value <- paste(unique_values, collapse = "<br>")}
      if(length(unique_values)==0){value <- paste("No hay aerolíneas", " asignadas")}
      
      value_box(
        # title = tags$div("Área B2", style = "font-size: 20px; font-weight: bold;margin-top: 10px;"),
        title = NULL,
        value = HTML(paste0('<div style="font-size: 18px; height: 150px; overflow-y: auto;">', value, '</div>')),
        showcase = tags$img(src = "B2.svg", width = "40px", height = "40px"), 
        # showcase = bsicons::bs_icon("suitcase"), # Ensure the bsicons library is loaded)
        theme = value_box_theme(bg = "#373a3cff", fg = "#fafafa"),   
        height = 200,
        style = "border-radius: 8px; overflow-y: auto;",
      )
    })
    
    output$value_box7 <- renderUI({
      
      copy_ci_dist <- copy(as.data.table(data$ci_dist)[CI_Area == "B3"])
      copy_ci_dist <- copy_ci_dist[Airline %in% unique(data$archivo1$`AEROLINEA`),]
      copy_ci_dist[DOM_INT == "DOM", Airline := paste0(Airline," - Doméstico")]
      copy_ci_dist[DOM_INT == "INT", Airline := paste0(Airline," - Internacional")]
      
      unique_values <- unique(unlist(copy_ci_dist$Airline))
      if(length(unique_values)>0){value <- paste(unique_values, collapse = "<br>")}
      if(length(unique_values)==0){value <- paste("No hay aerolíneas", " asignadas")}
      
      value_box(
        # title = tags$div("Área B3", style = "font-size: 20px; font-weight: bold;margin-top: 10px;"),
        title = NULL,
        value = HTML(paste0('<div style="font-size: 18px; height: 150px; overflow-y: auto;">', value, '</div>')),
        showcase = tags$img(src = "B3.svg", width = "40px", height = "40px"), 
        # showcase = bsicons::bs_icon("suitcase"), # Ensure the bsicons library is loaded)
        theme = value_box_theme(bg = "#373a3cff", fg = "#fafafa"),   
        height = 200,
        style = "border-radius: 8px; overflow-y: auto;",
      )
    })
    
    output$value_box8 <- renderUI({
      
      copy_ci_dist <- copy(as.data.table(data$ci_dist)[CI_Area == "C1"])
      copy_ci_dist <- copy_ci_dist[Airline %in% unique(data$archivo1$`AEROLINEA`),]
      copy_ci_dist[DOM_INT == "DOM", Airline := paste0(Airline," - Doméstico")]
      copy_ci_dist[DOM_INT == "INT", Airline := paste0(Airline," - Internacional")]
      
      unique_values <- unique(unlist(copy_ci_dist$Airline))
      if(length(unique_values)>0){value <- paste(unique_values, collapse = "<br>")}
      if(length(unique_values)==0){value <- paste("No hay aerolíneas", " asignadas")}
      
      value_box(
        # title = tags$div("Área C1", style = "font-size: 20px; font-weight: bold;margin-top: 10px;"),
        title = NULL,
        value = HTML(paste0('<div style="font-size: 18px; height: 150px; overflow-y: auto;">', value, '</div>')),
        showcase = tags$img(src = "C1.svg", width = "40px", height = "40px"), 
        # showcase = bsicons::bs_icon("suitcase"), # Ensure the bsicons library is loaded)
        theme = value_box_theme(bg = "#373a3cff", fg = "#fafafa"),   
        height = 200,
        style = "border-radius: 8px; overflow-y: auto;",
      )
    })
    
    output$value_box9 <- renderUI({
      
      copy_ci_dist <- copy(as.data.table(data$ci_dist)[CI_Area == "C2"])
      copy_ci_dist <- copy_ci_dist[Airline %in% unique(data$archivo1$`AEROLINEA`),]
      copy_ci_dist[DOM_INT == "DOM", Airline := paste0(Airline," - Doméstico")]
      copy_ci_dist[DOM_INT == "INT", Airline := paste0(Airline," - Internacional")]
      
      unique_values <- unique(unlist(copy_ci_dist$Airline))
      if(length(unique_values)>0){value <- paste(unique_values, collapse = "<br>")}
      if(length(unique_values)==0){value <- paste("No hay aerolíneas", " asignadas")}
      
      value_box(
        # title = tags$div("Área C2", style = "font-size: 20px; font-weight: bold;margin-top: 10px;"),
        title = NULL,
        value = HTML(paste0('<div style="font-size: 18px; height: 150px; overflow-y: auto;">', value, '</div>')),
        showcase = tags$img(src = "A1.svg", width = "40px", height = "40px"), 
        # showcase = bsicons::bs_icon("suitcase"), # Ensure the bsicons library is loaded)
        theme = value_box_theme(bg = "#373a3cff", fg = "#fafafa"),   
        height = 200,
        style = "border-radius: 8px; overflow-y: auto;",
      )
    })

    
    output$gantt_car1 <- renderPlot({
       
      ddfs_gantt_car1 <- gantt_calc(ddfs_complete, 1)

       ggplot(ddfs_gantt_car1, aes(x = opentime_aux1, xend = closetime_aux1, y = FlightNo, yend = FlightNo)) +
         geom_segment(linewidth = 3, color = "blue") +
         scale_x_continuous(breaks = seq(0, 24*3600, by = 3600), labels = function(x) sprintf("%02d:%02d", x %/% 3600, (x %% 3600) %/% 60), position = "top") +
         labs(y = NULL, x = NULL) +
         theme_minimal() +
         theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1), axis.ticks.length.x = unit(40, "pt"), axis.text.y = element_text(size = 14)) +
         guides(color = "none")
       
    })
    
    output$dynamic_plot_gant_car1 <- renderUI({
      ddfs_gant_car1 <- copy(ddfs_complete[Carousel == 1, c("FlightNo","Departure","Car_close_aux","Car_open_aux")])
      
      if (nrow(ddfs_gant_car1) > 0) {
        # Si hay vuelos, calcular la altura y mostrar el gráfico
        height_px <- paste0(60 + nrow(ddfs_gant_car1) * 25, "px")
        plotOutput("gantt_car1", height = height_px)
      } else {
        # Si no hay vuelos, mostrar un mensaje
        tags$p("No hay vuelos asignados a este carrusel", style = "color: black; font-weight: bold;")
      }
    })
       
    output$gantt_car2 <- renderPlot({
      
      ddfs_gantt_car2 <- gantt_calc(ddfs_complete, 2)
      
      ggplot(ddfs_gantt_car2, aes(x = opentime_aux1, xend = closetime_aux1, y = FlightNo, yend = FlightNo)) +
        geom_segment(linewidth = 3, color = "blue") +
        scale_x_continuous(breaks = seq(0, 24*3600, by = 3600), labels = function(x) sprintf("%02d:%02d", x %/% 3600, (x %% 3600) %/% 60), position = "top") +
        labs(y = NULL, x = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1), axis.ticks.length.x = unit(40, "pt"), axis.text.y = element_text(size = 14)) +
        guides(color = "none")
      
    })
    
    output$dynamic_plot_gant_car2 <- renderUI({
      ddfs_gant_car2 <- copy(ddfs_complete[Carousel == 2, c("FlightNo","Departure","Car_close_aux","Car_open_aux")])
      
      if (nrow(ddfs_gant_car2) > 0) {
        # Si hay vuelos, calcular la altura y mostrar el gráfico
        height_px <- paste0(60 + nrow(ddfs_gant_car2) * 25, "px")
        plotOutput("gantt_car2", height = height_px)
      } else {
        # Si no hay vuelos, mostrar un mensaje
        tags$p("No hay vuelos asignados a este carrusel", style = "color: black; font-weight: bold;")
      }
    })  
    
    
    output$gantt_car3 <- renderPlot({
      
      ddfs_gantt_car3 <- gantt_calc(ddfs_complete, 3)
      
      ggplot(ddfs_gantt_car3, aes(x = opentime_aux1, xend = closetime_aux1, y = FlightNo, yend = FlightNo)) +
        geom_segment(linewidth = 3, color = "blue") +
        scale_x_continuous(breaks = seq(0, 24*3600, by = 3600), labels = function(x) sprintf("%02d:%02d", x %/% 3600, (x %% 3600) %/% 60), position = "top") +
        labs(y = NULL, x = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1), axis.ticks.length.x = unit(40, "pt"), axis.text.y = element_text(size = 14)) +
        guides(color = "none")
      
    })
    
    output$dynamic_plot_gant_car3 <- renderUI({
      ddfs_gant_car3 <- copy(ddfs_complete[Carousel == 3, c("FlightNo","Departure","Car_close_aux","Car_open_aux")])
      
      if (nrow(ddfs_gant_car3) > 0) {
        # Si hay vuelos, calcular la altura y mostrar el gráfico
        height_px <- paste0(60 + nrow(ddfs_gant_car3) * 25, "px")
        plotOutput("gantt_car3", height = height_px)
      } else {
        # Si no hay vuelos, mostrar un mensaje
        tags$p("No hay vuelos asignados a este carrusel", style = "color: black; font-weight: bold;")
      }
    })
    
    
    output$gantt_car5 <- renderPlot({

      ddfs_gantt_car5 <- gantt_calc(ddfs_complete, 5)

      ggplot(ddfs_gantt_car5, aes(x = opentime_aux1, xend = closetime_aux1, y = FlightNo, yend = FlightNo)) +
        geom_segment(linewidth = 3, color = "blue") +
        scale_x_continuous(breaks = seq(0, 24*3600, by = 3600), labels = function(x) sprintf("%02d:%02d", x %/% 3600, (x %% 3600) %/% 60), position = "top") +
        labs(y = NULL, x = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1), axis.ticks.length.x = unit(40, "pt"), axis.text.y = element_text(size = 14)) +
        guides(color = "none")

    })

    output$dynamic_plot_gant_car5 <- renderUI({
      ddfs_gant_car5 <- copy(ddfs_complete[Carousel == 5, c("FlightNo","Departure","Car_close_aux","Car_open_aux")])

      if (nrow(ddfs_gant_car5) > 0) {
        # Si hay vuelos, calcular la altura y mostrar el gráfico
        height_px <- paste0(60 + nrow(ddfs_gant_car5) * 25, "px")
        plotOutput("gantt_car5", height = height_px)
      } else {
        # Si no hay vuelos, mostrar un mensaje
        tags$p("No hay vuelos asignados a este carrusel", style = "color: black; font-weight: bold;")
      }
    })
    
    # output$gantt_car5 <- renderPlotly({
    #   
    #   ddfs_gantt_car5 <- gantt_calc(ddfs_complete, 5)
    #   
    #   # Crear un gráfico con plotly, dibujando cada segmento individualmente
    #   p <- plot_ly()
    #   for (i in 1:nrow(ddfs_gantt_car5)) {
    #     p <- add_trace(
    #       p,
    #       x = c(ddfs_gantt_car5$opentime_aux1[i], ddfs_gantt_car5$closetime_aux1[i]),
    #       y = c(ddfs_gantt_car5$FlightNo[i], ddfs_gantt_car5$FlightNo[i]),
    #       type = "scatter",
    #       mode = "lines",
    #       line = list(color = "blue", width = 6),
    #       showlegend = FALSE
    #     )
    #   }
    #   
    #   p <- layout(
    #     p,
    #     xaxis = list(
    #       title = NULL,
    #       tickvals = seq(0, 24*3600, by = 3600),
    #       ticktext = sapply(seq(0, 24*3600, by = 3600), function(x) sprintf("%02d:%02d", x %/% 3600, (x %% 3600) %/% 60)),
    #       tickangle = 45,
    #       side = "top"
    #     ),
    #     yaxis = list(title = NULL, tickfont = list(size = 14)),
    #     margin = list(t = 50),
    #     showlegend = FALSE
    #   )
    #   
    #   p
    # })
    # 
    # output$dynamic_plot_gant_car5 <- renderUI({
    #   ddfs_gant_car5 <- copy(ddfs_complete[Carousel == 5, c("FlightNo","Departure","Car_close_aux","Car_open_aux")])
    #   
    #   if (nrow(ddfs_gant_car5) > 0) {
    #     height_px <- paste0(60 + nrow(ddfs_gant_car5) * 25, "px")
    #     plotlyOutput("gantt_car5", height = height_px)
    #   } else {
    #     tags$p("No hay vuelos asignados a este carrusel", style = "color: black; font-weight: bold;")
    #   }
    # })
    
    output$gantt_car6 <- renderPlot({
      
      ddfs_gantt_car6 <- gantt_calc(ddfs_complete, 6)
      
      ggplot(ddfs_gantt_car6, aes(x = opentime_aux1, xend = closetime_aux1, y = FlightNo, yend = FlightNo)) +
        geom_segment(linewidth = 3, color = "blue") +
        scale_x_continuous(breaks = seq(0, 24*3600, by = 3600), labels = function(x) sprintf("%02d:%02d", x %/% 3600, (x %% 3600) %/% 60), position = "top") +
        labs(y = NULL, x = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1), axis.ticks.length.x = unit(40, "pt"), axis.text.y = element_text(size = 14)) +
        guides(color = "none")
      
    })
    
    output$dynamic_plot_gant_car6 <- renderUI({
      ddfs_gant_car6 <- copy(ddfs_complete[Carousel == 6, c("FlightNo","Departure","Car_close_aux","Car_open_aux")])
      
      if (nrow(ddfs_gant_car6) > 0) {
        # Si hay vuelos, calcular la altura y mostrar el gráfico
        height_px <- paste0(60 + nrow(ddfs_gant_car6) * 25, "px")
        plotOutput("gantt_car6", height = height_px)
      } else {
        # Si no hay vuelos, mostrar un mensaje
        tags$p("No hay vuelos asignados a este carrusel", style = "color: black; font-weight: bold;")
      }
    })
    
    
    output$gantt_car7 <- renderPlot({
      
      ddfs_gantt_car7 <- gantt_calc(ddfs_complete, 7)
      
      ggplot(ddfs_gantt_car7, aes(x = opentime_aux1, xend = closetime_aux1, y = FlightNo, yend = FlightNo)) +
        geom_segment(linewidth = 3, color = "blue") +
        scale_x_continuous(breaks = seq(0, 24*3600, by = 3600), labels = function(x) sprintf("%02d:%02d", x %/% 3600, (x %% 3600) %/% 60), position = "top") +
        labs(y = NULL, x = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1), axis.ticks.length.x = unit(40, "pt"), axis.text.y = element_text(size = 14)) +
        guides(color = "none")
      
    })
    
    output$dynamic_plot_gant_car7 <- renderUI({
      ddfs_gant_car7 <- copy(ddfs_complete[Carousel == 7, c("FlightNo","Departure","Car_close_aux","Car_open_aux")])
      
      if (nrow(ddfs_gant_car7) > 0) {
        # Si hay vuelos, calcular la altura y mostrar el gráfico
        height_px <- paste0(60 + nrow(ddfs_gant_car7) * 25, "px")
        plotOutput("gantt_car7", height = height_px)
      } else {
        # Si no hay vuelos, mostrar un mensaje
        tags$p("No hay vuelos asignados a este carrusel", style = "color: black; font-weight: bold;")
      }
    })
    
    output$gantt_car8 <- renderPlot({
      
      ddfs_gantt_car8 <- gantt_calc(ddfs_complete, 8)
      
      ggplot(ddfs_gantt_car8, aes(x = opentime_aux1, xend = closetime_aux1, y = FlightNo, yend = FlightNo)) +
        geom_segment(linewidth = 3, color = "blue") +
        scale_x_continuous(breaks = seq(0, 24*3600, by = 3600), labels = function(x) sprintf("%02d:%02d", x %/% 3600, (x %% 3600) %/% 60), position = "top") +
        labs(y = NULL, x = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1), axis.ticks.length.x = unit(40, "pt"), axis.text.y = element_text(size = 14)) +
        guides(color = "none")
      
    })
    
    output$dynamic_plot_gant_car8 <- renderUI({
      ddfs_gant_car8 <- copy(ddfs_complete[Carousel == 8, c("FlightNo","Departure","Car_close_aux","Car_open_aux")])
      
      if (nrow(ddfs_gant_car8) > 0) {
        # Si hay vuelos, calcular la altura y mostrar el gráfico
        height_px <- paste0(60 + nrow(ddfs_gant_car8) * 25, "px")
        plotOutput("gantt_car8", height = height_px)
      } else {
        # Si no hay vuelos, mostrar un mensaje
        tags$p("No hay vuelos asignados a este carrusel", style = "color: black; font-weight: bold;")
      }
    })
    
    output$gantt_car9 <- renderPlot({
      
      ddfs_gantt_car9 <- gantt_calc(ddfs_complete, 9)
      
      ggplot(ddfs_gantt_car9, aes(x = opentime_aux1, xend = closetime_aux1, y = FlightNo, yend = FlightNo)) +
        geom_segment(linewidth = 3, color = "blue") +
        scale_x_continuous(breaks = seq(0, 24*3600, by = 3600), labels = function(x) sprintf("%02d:%02d", x %/% 3600, (x %% 3600) %/% 60), position = "top") +
        labs(y = NULL, x = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1), axis.ticks.length.x = unit(40, "pt"), axis.text.y = element_text(size = 14)) +
        guides(color = "none")
      
    })
    
    output$dynamic_plot_gant_car9 <- renderUI({
      ddfs_gant_car9 <- copy(ddfs_complete[Carousel == 9, c("FlightNo","Departure","Car_close_aux","Car_open_aux")])
      
      if (nrow(ddfs_gant_car9) > 0) {
        # Si hay vuelos, calcular la altura y mostrar el gráfico
        height_px <- paste0(60 + nrow(ddfs_gant_car9) * 25, "px")
        plotOutput("gantt_car9", height = height_px)
      } else {
        # Si no hay vuelos, mostrar un mensaje
        tags$p("No hay vuelos asignados a este carrusel", style = "color: black; font-weight: bold;")
      }
    })
    
    
    output$gantt_car10 <- renderPlot({
      
      ddfs_gantt_car10 <- gantt_calc(ddfs_complete, 10)
      
      ggplot(ddfs_gantt_car10, aes(x = opentime_aux1, xend = closetime_aux1, y = FlightNo, yend = FlightNo)) +
        geom_segment(linewidth = 3, color = "blue") +
        scale_x_continuous(breaks = seq(0, 24*3600, by = 3600), labels = function(x) sprintf("%02d:%02d", x %/% 3600, (x %% 3600) %/% 60), position = "top") +
        labs(y = NULL, x = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1), axis.ticks.length.x = unit(40, "pt"), axis.text.y = element_text(size = 14)) +
        guides(color = "none")
      
    })
    
    output$dynamic_plot_gant_car10 <- renderUI({
      ddfs_gant_car10 <- copy(ddfs_complete[Carousel == 10, c("FlightNo","Departure","Car_close_aux","Car_open_aux")])
      
      if (nrow(ddfs_gant_car10) > 0) {
        # Si hay vuelos, calcular la altura y mostrar el gráfico
        height_px <- paste0(60 + nrow(ddfs_gant_car10) * 25, "px")
        plotOutput("gantt_car10", height = height_px)
      } else {
        # Si no hay vuelos, mostrar un mensaje
        tags$p("No hay vuelos asignados a este carrusel", style = "color: black; font-weight: bold;")
      }
    })
    
    
    output$gantt_car11 <- renderPlot({
      
      ddfs_gantt_car11 <- gantt_calc(ddfs_complete, 11)
      
      ggplot(ddfs_gantt_car11, aes(x = opentime_aux1, xend = closetime_aux1, y = FlightNo, yend = FlightNo)) +
        geom_segment(linewidth = 3, color = "blue") +
        scale_x_continuous(breaks = seq(0, 24*3600, by = 3600), labels = function(x) sprintf("%02d:%02d", x %/% 3600, (x %% 3600) %/% 60), position = "top") +
        labs(y = NULL, x = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1), axis.ticks.length.x = unit(40, "pt"), axis.text.y = element_text(size = 14)) +
        guides(color = "none")
      
    })
    
    output$dynamic_plot_gant_car11 <- renderUI({
      ddfs_gant_car11 <- copy(ddfs_complete[Carousel == 11, c("FlightNo","Departure","Car_close_aux","Car_open_aux")])
      
      if (nrow(ddfs_gant_car11) > 0) {
        # Si hay vuelos, calcular la altura y mostrar el gráfico
        height_px <- paste0(60 + nrow(ddfs_gant_car11) * 25, "px")
        plotOutput("gantt_car11", height = height_px)
      } else {
        # Si no hay vuelos, mostrar un mensaje
        tags$p("No hay vuelos asignados a este carrusel", style = "color: black; font-weight: bold;")
      }
    })
    

    output$showup_HUB <- renderPlot({
      
      profile_HUB <- data.table(`Minutes before departure` = profilepresentation_LATAM_DOM$`Minutes before departure`, DOM = profilepresentation_LATAM_DOM$`% PAX`, INT = profilepresentation_LATAM_INT$`% PAX`)
      
      # Encontrar el pico más alto entre DOM e INT
      max_non_cumulative <- ceiling(max(profile_HUB$DOM, profile_HUB$INT) / 10) * 10
      
      # Calcular el ancho proporcional según el número de barras
      num_barras <- nrow(profile_HUB)
      width_proporcional <- 90 / num_barras  # El valor 0.8 es un ejemplo de proporción del espacio
      
      profile_HUB <- profile_HUB %>%
        mutate(DOM_acumulado = cumsum(DOM), 
               INT_acumulado = cumsum(INT),
               # Normalizar las barras para que estén en la escala de 0 a 100
               DOM_normalizado = DOM / max_non_cumulative * 100,
               INT_normalizado = INT / max_non_cumulative * 100)
      
      
      # Graficar con ggplot
      
      ggplot(profile_HUB, aes(x = `Minutes before departure`)) +  
        # Invertir el eje X para mostrar de mayor a menor
        scale_x_reverse() +
        
        # Barras lado a lado (normalizadas) usando position_dodge con un ancho definido
        geom_bar(aes(y = DOM_normalizado, fill = "DOM"), stat = "identity", 
                 position = position_dodge(width = 1), width = width_proporcional) +  # Barras de DOM
        geom_bar(aes(y = INT_normalizado, fill = "INT"), stat = "identity", 
                 position = position_dodge(width = 1), width = width_proporcional) +  # Barras de INT
        
        # Líneas acumuladas
        geom_line(aes(y = DOM_acumulado, color = "DOM Acumulado"), linewidth = 1.25, linetype = "dashed") +
        geom_line(aes(y = INT_acumulado, color = "INT Acumulado"), linewidth = 1.25, linetype = "dotted") +
        
        # Escala secundaria para las líneas acumuladas y mantener hasta 100
        scale_y_continuous(
          name = "Acumulado Pasajeros (%)",
          limits = c(0, 100),  # Mantener el eje acumulado hasta 100
          sec.axis = sec_axis(~ . * (max_non_cumulative / 100),  # Ajustar el eje Y secundario proporcionalmente
                              name = "Llegada de Pasajeros (%)")
        ) +
        
        # Etiquetas y estilo
        labs(x = "Minutos antes de STD") +
        
        # Especificar los colores personalizados
        scale_fill_manual(values = c("DOM" = "#5B9BD5", "INT" = "#70AD47")) +
        scale_color_manual(values = c("DOM Acumulado" = "#1F4E79", "INT Acumulado" = "#385723")) +
        
        theme_minimal() +
        theme(
          legend.title = element_blank(),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 14, margin = margin(r = 8)),  # Offset entre eje Y izquierdo y su título
          axis.title.y.right = element_text(size = 14, margin = margin(l = 8)),  # Offset entre eje Y derecho (secundario) y su título
          axis.title.x = element_text(size = 14, margin = margin(t = 15)),
          plot.margin = margin(t = 20, r = 5, b = 5, l = 5)
        )
      
    })
    
    
    
    output$table_showup_HUB <- renderDT({
      
      profile_HUB <- data.table(`Minutes before departure` = profilepresentation_LATAM_DOM$`Minutes before departure`, 
                                DOM = profilepresentation_LATAM_DOM$`% PAX`, 
                                INT = profilepresentation_LATAM_INT$`% PAX`)
      setnames(profile_HUB, c("Minutes before departure", "DOM", "INT"), 
               c("Minutos antes de STD", "% Pasajeros DOM", "% Pasajeros INT"))
      
      datatable(profile_HUB, options = list(dom = 't', pageLength = nrow(profile_HUB)))

    })
    

    output$showup_NoHUB <- renderPlot({
      
      profile_NoHUB <- data.table(`Minutes before departure` = profilepresentation_AvRest_DOM$`Minutes before departure`, DOM = profilepresentation_AvRest_DOM$`% PAX`, INT = profilepresentation_AvRest_INT$`% PAX`)
      
      # Encontrar el pico más alto entre DOM e INT
      max_non_cumulative <- ceiling(max(profile_NoHUB$DOM, profile_NoHUB$INT) / 10) * 10
      
      # Calcular el ancho proporcional según el número de barras
      num_barras <- nrow(profile_NoHUB)
      width_proporcional <- 90 / num_barras  # El valor 0.8 es un ejemplo de proporción del espacio
      
      profile_NoHUB <- profile_NoHUB %>%
        mutate(DOM_acumulado = cumsum(DOM), 
               INT_acumulado = cumsum(INT),
               # Normalizar las barras para que estén en la escala de 0 a 100
               DOM_normalizado = DOM / max_non_cumulative * 100,
               INT_normalizado = INT / max_non_cumulative * 100)
      
      
      # Graficar con ggplot
      
      ggplot(profile_NoHUB, aes(x = `Minutes before departure`)) +  
        # Invertir el eje X para mostrar de mayor a menor
        scale_x_reverse() +
        
        # Barras lado a lado (normalizadas) usando position_dodge con un ancho definido
        geom_bar(aes(y = DOM_normalizado, fill = "DOM"), stat = "identity", 
                 position = position_dodge(width = 1), width = width_proporcional) +  # Barras de DOM
        geom_bar(aes(y = INT_normalizado, fill = "INT"), stat = "identity", 
                 position = position_dodge(width = 1), width = width_proporcional) +  # Barras de INT
        
        # Líneas acumuladas
        geom_line(aes(y = DOM_acumulado, color = "DOM Acumulado"), linewidth = 1.25, linetype = "dashed") +
        geom_line(aes(y = INT_acumulado, color = "INT Acumulado"), linewidth = 1.25, linetype = "dotted") +
        
        # Escala secundaria para las líneas acumuladas y mantener hasta 100
        scale_y_continuous(
          name = "Acumulado Pasajeros (%)",
          limits = c(0, 100),  # Mantener el eje acumulado hasta 100
          sec.axis = sec_axis(~ . * (max_non_cumulative / 100),  # Ajustar el eje Y secundario proporcionalmente
                              name = "Llegada de Pasajeros (%)")
        ) +
        
        # Etiquetas y estilo
        labs(x = "Minutos antes de STD") +
        
        # Especificar los colores personalizados
        scale_fill_manual(values = c("DOM" = "#5B9BD5", "INT" = "#70AD47")) +
        scale_color_manual(values = c("DOM Acumulado" = "#1F4E79", "INT Acumulado" = "#385723")) +
        
        theme_minimal() +
        theme(
          legend.title = element_blank(),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 14, margin = margin(r = 8)),  # Offset entre eje Y izquierdo y su título
          axis.title.y.right = element_text(size = 14, margin = margin(l = 8)),  # Offset entre eje Y derecho (secundario) y su título
          axis.title.x = element_text(size = 14, margin = margin(t = 15)),
          plot.margin = margin(t = 20, r = 5, b = 5, l = 5)
        )
      
    })
    
    
    output$table_showup_NoHUB <-renderDT({
      
      profile_NoHUB <- data.table(`Minutes before departure` = profilepresentation_AvRest_DOM$`Minutes before departure`, DOM = profilepresentation_AvRest_DOM$`% PAX`, INT = profilepresentation_AvRest_INT$`% PAX`)
      setnames(profile_NoHUB, c("Minutes before departure", "DOM", "INT"), c("Minutos antes de STD", "% Pasajeros DOM", "% Pasajeros INT"))
      datatable(profile_NoHUB, options = list(dom = 't', pageLength = nrow(profile_NoHUB)))
      
    })
    
    
    output$showup_Transfer <- renderPlot({
      
      profile_Transfer <- data.table(`Minutes before departure` = profilepresentation_Transfer_DOM$`Minutes before departure`, DOM = profilepresentation_Transfer_DOM$`% PAX`, INT = profilepresentation_Transfer_INT$`% PAX`)
      
      # Encontrar el pico más alto entre DOM e INT
      max_non_cumulative <- ceiling(max(profile_Transfer$DOM, profile_Transfer$INT) / 10) * 10
      
      # Calcular el ancho proporcional según el número de barras
      num_barras <- nrow(profile_Transfer)
      width_proporcional <- 90 / num_barras  # El valor 0.8 es un ejemplo de proporción del espacio
      
      profile_Transfer <- profile_Transfer %>%
        mutate(DOM_acumulado = cumsum(DOM), 
               INT_acumulado = cumsum(INT),
               # Normalizar las barras para que estén en la escala de 0 a 100
               DOM_normalizado = DOM / max_non_cumulative * 100,
               INT_normalizado = INT / max_non_cumulative * 100)
      
      
      # Graficar con ggplot
      
      ggplot(profile_Transfer, aes(x = `Minutes before departure`)) +  
        # Invertir el eje X para mostrar de mayor a menor
        scale_x_reverse() +
        
        # Barras lado a lado (normalizadas) usando position_dodge con un ancho definido
        geom_bar(aes(y = DOM_normalizado, fill = "DOM"), stat = "identity", 
                 position = position_dodge(width = 1), width = width_proporcional) +  # Barras de DOM
        geom_bar(aes(y = INT_normalizado, fill = "INT"), stat = "identity", 
                 position = position_dodge(width = 1), width = width_proporcional) +  # Barras de INT
        
        # Líneas acumuladas
        geom_line(aes(y = DOM_acumulado, color = "DOM Acumulado"), linewidth = 1.25, linetype = "dashed") +
        geom_line(aes(y = INT_acumulado, color = "INT Acumulado"), linewidth = 1.25, linetype = "dotted") +
        
        # Escala secundaria para las líneas acumuladas y mantener hasta 100
        scale_y_continuous(
          name = "Acumulado Pasajeros (%)",
          limits = c(0, 100),  # Mantener el eje acumulado hasta 100
          sec.axis = sec_axis(~ . * (max_non_cumulative / 100),  # Ajustar el eje Y secundario proporcionalmente
                              name = "Llegada de Pasajeros (%)")
        ) +
        
        # Etiquetas y estilo
        labs(x = "Minutos antes de STD") +
        
        # Especificar los colores personalizados
        scale_fill_manual(values = c("DOM" = "#5B9BD5", "INT" = "#70AD47")) +
        scale_color_manual(values = c("DOM Acumulado" = "#1F4E79", "INT Acumulado" = "#385723")) +
        
        theme_minimal() +
        theme(
          legend.title = element_blank(),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 14, margin = margin(r = 8)),  # Offset entre eje Y izquierdo y su título
          axis.title.y.right = element_text(size = 14, margin = margin(l = 8)),  # Offset entre eje Y derecho (secundario) y su título
          axis.title.x = element_text(size = 14, margin = margin(t = 15)),
          plot.margin = margin(t = 20, r = 5, b = 5, l = 5)
        )
      
    })
    
    
    output$table_showup_Transfer <-renderDT({
      
      profile_Transfer <- data.table(`Minutes before departure` = profilepresentation_Transfer_DOM$`Minutes before departure`, DOM = profilepresentation_Transfer_DOM$`% PAX`, INT = profilepresentation_Transfer_INT$`% PAX`)
      setnames(profile_Transfer, c("Minutes before departure", "DOM", "INT"), c("Minutos antes de STD", "% Pasajeros DOM", "% Pasajeros INT"))
      datatable(profile_Transfer, options = list(dom = 't', pageLength = nrow(profile_Transfer)))
      
    })
    
    
    
    
  output$table_ddfs <-renderDT({
    datatable(data$archivo1_mostrar, 
              options = list(
                dom = 'rtip',  # Remove length control ('l'), search bar ('f'), keep table, info, and pagination
                pageLength = 15,  # Set default number of rows to 20
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#373a3cff', 'color': '#fff', 'font-size': '14px'});",
                  "$(this.api().table().body()).css({'font-size': '12px'});",
                  "$('.dataTables_paginate').css({'font-size': '14px', 'color': '#373a3cff'});",
                  "$('.paginate_button').css({'font-size': '14px', 'color': '#373a3cff'});",
                  "$('.paginate_button.previous, .paginate_button.next').css({'font-size': '14px', 'color': '#373a3cff'});",
                  "}"
                )
              ))
  })
  

  


  
  
  #### 3.4. Observe destinado a verificar que la suma de los parámetros de los equipos de inspección suman 100% ####
  observe({
    
    if (is.finite(input$valor7) && is.finite(input$valor9) && 
        is.finite(input$valor10) && is.finite(input$valor100)) {
      
      valor <- input$valor7 + input$valor9 + input$valor10 + input$valor100

      output$warning_message <- renderUI({
        if (valor != 100) {
          tags$p("El valor de la suma de los parámetros debe ser igual a 100", 
                 style = "color: red; font-weight: bold;")
        } else {
          NULL  # No mostrar nada si la condición no se cumple
        }
      })
    }
  })
  
  #PNG INPUT
  output$imagen_box <- renderUI({
    tags$img(src = "LATAMLOGISTICSLOGO.png", height = "200px", width = "200px")  # Inserta la imagen
  })
  #### 3.5. Observe de Carga de Parámetros de Entrada ####
  #Observe Event destinado a cargar los parámetros de entrada
  observeEvent(input$load_data, {
    
    show_spinner() # Muestra el spinner, es el icono de carga
    Sys.sleep(5) # Simula un cálculo pesado
    
    num_hojas_fixed <- 0
    
    ##### 3.5.1 Lectura y verificación de los archivos referentes a Parámetros Comunmente Fijos #####
    if (!is.null(input$archivo_fixed_parameters)) {
      hojas_fixed <- excel_sheets(input$archivo_fixed_parameters$datapath)
      num_hojas_fixed <- as.numeric(length(hojas_fixed))
      if(num_hojas_fixed != 5) {
        
        output$error_hojas_fixed<- renderUI({tags$p("El número de hojas del archivo 'Otros Parámetros' no es el correcto", style = "color: red;")})
      }
      
      else{
        output$error_hojas_fixed<- renderUI({ NULL })
      }
    }
    
    if (num_hojas_fixed == 5){
      hojas_fixed_verificar <- TRUE
    }
    
    else {
      hojas_fixed_verificar <- FALSE
    }
    
    data$make_up_dist <- NULL
    data$ci_dist <- NULL
    data$cap_line <- NULL
    data$ebs <- NULL
    data$carousel_times <- NULL
    
    if(!is.null(input$archivo_fixed_parameters) && num_hojas_fixed == 5){
      
      
      if (!is.null(input$archivo_fixed_parameters) && num_hojas_fixed >= 4) {
        data$make_up_dist <- read_excel(input$archivo_fixed_parameters$datapath, 4)
      }
      
      if (!is.null(input$archivo_fixed_parameters) && num_hojas_fixed >= 2) {
        data$ci_dist <- read_excel(input$archivo_fixed_parameters$datapath, 2)
      }
      
      if (!is.null(input$archivo_fixed_parameters) && num_hojas_fixed >= 1) {
        data$cap_line <- read_excel(input$archivo_fixed_parameters$datapath, 1)
      }
      
      if (!is.null(input$archivo_fixed_parameters) && num_hojas_fixed >= 3) {
        data$ebs <- read_excel(input$archivo_fixed_parameters$datapath, 3)
      }

      if (!is.null(input$archivo_fixed_parameters) && num_hojas_fixed == 5) {
        data$carousel_times <- read_excel(input$archivo_fixed_parameters$datapath, 5)
      }
      
    }
    
    if (!is.null(data$make_up_dist)) {
      tryCatch({
        # Make-up Distribution
        data$verificacion_make_up_dist <- verificar_columnas(data$make_up_dist, 3, "Make-up Distribution")
        output$message_make_up_dist <- renderUI({
          tags$p(data$verificacion_make_up_dist$mensaje, style = ifelse(data$verificacion_make_up_dist$valido, "color: green;", "color: red;"))
        })
      }, error = function(e) {
        output$message_make_up_dist <- renderUI({
          tags$p(paste("Error al cargar el archivo 'Make-up Distribution'", conditionMessage(e)), style = "color: red;")
        })
      })
    }else {
      output$message_make_up_dist <- renderUI({
        tags$p("Archivo 'Make-up Distribution' no encontrado.", style = "color: red;")
      })
    }
    if (!is.null(data$ci_dist)) {
      tryCatch({
        # Check-in Distribution
        data$verificacion_ci_dist <- verificar_columnas(data$ci_dist, 3, "Check-in Distribution")
        output$message_ci_dist <- renderUI({
          tags$p(data$verificacion_ci_dist$mensaje, style = ifelse(data$verificacion_ci_dist$valido, "color: green;", "color: red;"))
        })
      }, error = function(e) {
        output$message_ci_dist <- renderUI({
          tags$p(paste("Error al cargar el archivo 'Check-in Distribution'", conditionMessage(e)), style = "color: red;")
        })
      })
    }else {
      output$message_ci_dist <- renderUI({
        tags$p("Archivo 'Check-in Distribution' no encontrado.", style = "color: red;")
      })
    }
    if (!is.null(data$cap_line)) {
      tryCatch({
        # Capacidad de Líneas
        data$verificacion_cap_line <- verificar_columnas(data$cap_line, 3, "Lines Capacity")
        output$message_cap_line <- renderUI({
          tags$p(data$verificacion_cap_line$mensaje, style = ifelse(data$verificacion_cap_line$valido, "color: green;", "color: red;"))
        })
      }, error = function(e) {
        output$message_cap_line <- renderUI({
          tags$p(paste("Error al cargar el archivo 'Lines Capacity'", conditionMessage(e)), style = "color: red;")
        })
      })
    }else {
      output$message_cap_line <- renderUI({
        tags$p("Archivo 'Lines Capacity' no encontrado.", style = "color: red;")
      })
    }
    if (!is.null(data$ebs)) {
      tryCatch({
        # EBS Automático
        data$verificacion_ebs <- verificar_columnas(data$ebs, 1, "Automatic EBS")
        output$message_ebs <- renderUI({
          tags$p(data$verificacion_ebs$mensaje, style = ifelse(data$verificacion_ebs$valido, "color: green;", "color: red;"))
        })
      }, error = function(e) {
        output$message_ebs <- renderUI({
          tags$p(paste("Error al cargar el archivo 'Automatic EBS'", conditionMessage(e)), style = "color: red;")
        })
      })
    }else {
      output$message_ebs <- renderUI({
        tags$p("Archivo 'Automatic EBS' no encontrado.", style = "color: red;")
      })
    }
    if (!is.null(data$carousel_times)) {
      tryCatch({
        # DOM Airport
        data$verificacion_carousel_times <- verificar_columnas(data$carousel_times, 4, "Carousel Times")
        output$message_carousel_times <- renderUI({
          tags$p(data$verificacion_carousel_times$mensaje, style = ifelse(data$verificacion_carousel_times$valido, "color: green;", "color: red;"))
        })
      }, error = function(e) {
        output$message_carousel_times <- renderUI({
          tags$p(paste("Error al cargar el archivo 'Carousel Times'", conditionMessage(e)), style = "color: red;")
        })
      })
    }else {
      output$message_carousel_times <- renderUI({
        tags$p("Archivo 'Carousel Times' no encontrado.", style = "color: red;")
      })
    }
    
    ##### 3.5.2. Carga y verificación de Parámetros de Entrada Comunmente Modificados #####
    
    num_hojas_ddfs <- 0
    
    # Leer archivo 1: Plan de Vuelos
    if (!is.null(input$archivo1)) {
      data$archivo1 <- read_excel(input$archivo1$datapath)
      data$verificacion_archivo1 <- verificar_columnas(data$archivo1, 8, "Plan de Vuelos")
      if (data$verificacion_archivo1$valido) {
        data$archivo1_mostrar <<- data$archivo1
        # data$archivo1_mostrar$Date <<- as.character(as.IDate(data$archivo1_mostrar$Date))
        data$archivo1_mostrar$`Proyeccion PAX  Saliendo` <<- round(as.numeric(data$archivo1_mostrar$`Proyeccion PAX  Saliendo`), digits = 0)
        
      }
      
      hojas_ddfs <- excel_sheets(input$archivo1$datapath)
      num_hojas_ddfs <- as.numeric(length(hojas_ddfs))
      
      if(num_hojas_ddfs != 1) {
        
        output$error_hojas_ddfs <- renderUI({tags$p("El número de hojas del archivo correspondiente a Plan de Vuelo no es el correcto", style = "color: red;")})
      }
      
      else{
        output$error_hojas_ddfs<- renderUI({ NULL })
      }
      
    }
    
    # Leer archivo 2: Perfil de presentación de vuelo doméstico de aerolínea HUB
    
    num_hojas <- 0
    
    verificar_hojas = FALSE
    if (!is.null(input$archivo2)) {
      hojas <- excel_sheets(input$archivo2$datapath)
      num_hojas <- as.numeric(length(hojas))
      verificar_hojas = TRUE
      if(num_hojas != 6) {
        
        output$error_hojas<- renderUI({tags$p("El número de hojas del archivo correspondiente a Perfiles de Presentación no es el correcto", style = "color: red;")})
      }
      
      else{
        output$error_hojas<- renderUI({ NULL })
      }
    }
    
    if (num_hojas == 6){
      hojas_verificar <- TRUE
    }
    
    else {
      hojas_verificar <- FALSE
    }
    
    data$archivo2 <- NULL
    data$archivo3 <- NULL
    data$archivo4 <- NULL
    data$archivo5 <- NULL
    data$archivo6 <- NULL
    data$archivo7 <- NULL
    
    #Apartado destinado a leer el excel .xlsx de Perfiles de Presentación, cada hoja corresponde un archivo
    if(!is.null(input$archivo2)){
      
      if (!is.null(input$archivo2) && num_hojas >= 1) {
        data$archivo2 <- read_excel(input$archivo2$datapath, 1)
      }
      
      # Leer archivo 3: Perfil de presentación de vuelo internacional de aerolínea HUB
      if (!is.null(input$archivo2) && num_hojas >= 2) {
        data$archivo3 <- read_excel(input$archivo2$datapath, 2)
      }
      
      # Leer archivo 4, 5, 6, 7 de manera similar
      if (!is.null(input$archivo2) && num_hojas >= 3) {
        data$archivo4 <- read_excel(input$archivo2$datapath, 3)
      }
      
      if (!is.null(input$archivo2) && num_hojas >= 4) {
        data$archivo5 <- read_excel(input$archivo2$datapath, 4)
      }
      
      if (!is.null(input$archivo2) && num_hojas >= 5) {
        data$archivo6 <- read_excel(input$archivo2$datapath, 5)
      }
      
      if (!is.null(input$archivo2) && num_hojas == 6) {
        data$archivo7 <- read_excel(input$archivo2$datapath, 6)
      }
      
    }
    #Una vez se leen los arhivos y se guardan, se verifica si cumplen con el formato (con el número de columnas)
    if (!is.null(data$archivo1)) {
      tryCatch({
        # ARCHIVO 1
        data$verificacion_archivo1 <- verificar_columnas(data$archivo1, 8, "Plan de Vuelos")
        output$message_archivo1 <- renderUI({
          tags$p(data$verificacion_archivo1$mensaje, style = ifelse(data$verificacion_archivo1$valido, "color: green;", "color: red;"))
        })
      }, error = function(e) {
        output$message_archivo1 <- renderUI({
          tags$p(paste("Error al cargar el archivo 'Plan de Vuelos'", conditionMessage(e)), style = "color: red;")
        })
      })
    }else {
      output$message_archivo1 <- renderUI({
        tags$p("Plan de Vuelos' no encontrado.", style = "color: red;")
      })
    }
    
    if (!is.null(data$archivo2)) {
      tryCatch({
        # ARCHIVO 2
        data$verificacion_archivo2 <- verificar_columnas(data$archivo2, 3, "Perfil de presentación de vuelo doméstico de aerolínea HUB (LATAM y AVIANCA)")
        output$message_archivo2 <- renderUI({
          tags$p(data$verificacion_archivo2$mensaje, style = ifelse(data$verificacion_archivo2$valido, "color: green;", "color: red;"))
        })
      }, error = function(e) {
        output$message_archivo2 <- renderUI({
          tags$p(paste("Error al cargar el archivo 'Perfil de presentación de vuelo doméstico de aerolínea HUB (LATAM y AVIANCA)'", conditionMessage(e)), style = "color: red;")
        })
      })
    }else {
      output$message_archivo2 <- renderUI({
        tags$p("Perfil de presentación de vuelo doméstico de aerolínea HUB (LATAM y AVIANCA)", style = "color: red;")
      })
    }
    
    if (!is.null(data$archivo3)) {
      tryCatch({
        # ARCHIVO 3
        data$verificacion_archivo3 <- verificar_columnas(data$archivo3, 3, "Perfil de presentación de vuelo internacional de aerolínea HUB (LATAM y AVIANCA)")
        output$message_archivo3 <- renderUI({
          tags$p(data$verificacion_archivo3$mensaje, style = ifelse(data$verificacion_archivo3$valido, "color: green;", "color: red;"))
        })
      }, error = function(e) {
        output$message_archivo3 <- renderUI({
          tags$p(paste("Error al cargar el archivo 'Perfil de presentación de vuelo internacional de aerolínea HUB (LATAM y AVIANCA)'", conditionMessage(e)), style = "color: red;")
        })
      })
    }else {
      output$message_archivo3 <- renderUI({
        tags$p("Perfil de presentación de vuelo internacional de aerolínea HUB (LATAM y AVIANCA)", style = "color: red;")
      })
    }
    
    if (!is.null(data$archivo4)) {
      tryCatch({
        # ARCHIVO 4
        data$verificacion_archivo4 <- verificar_columnas(data$archivo4, 3, "Perfil de presentación de vuelo doméstico de otras aerolíneas")
        output$message_archivo4 <- renderUI({
          tags$p(data$verificacion_archivo4$mensaje, style = ifelse(data$verificacion_archivo4$valido, "color: green;", "color: red;"))
        })
      }, error = function(e) {
        output$message_archivo4 <- renderUI({
          tags$p(paste("Error al cargar el archivo 'Perfil de presentación de vuelo doméstico de otras aerolíneas'", conditionMessage(e)), style = "color: red;")
        })
      })
    }else {
      output$message_archivo4 <- renderUI({
        tags$p("Perfil de presentación de vuelo doméstico de otras aerolíneas' no encontrado.", style = "color: red;")
      })
    }
    
    if (!is.null(data$archivo5)) {
      tryCatch({
        # ARCHIVO 5
        data$verificacion_archivo5 <- verificar_columnas(data$archivo5, 3, "Perfil de presentación de vuelo internacional de otras aerolíneas")
        output$message_archivo5 <- renderUI({
          tags$p(data$verificacion_archivo5$mensaje, style = ifelse(data$verificacion_archivo5$valido, "color: green;", "color: red;"))
        })
      }, error = function(e) {
        output$message_archivo5 <- renderUI({
          tags$p(paste("Error al cargar el archivo 'Perfil de presentación de vuelo internacional de otras aerolíneas'", conditionMessage(e)), style = "color: red;")
        })
      })
    }else {
      output$message_archivo5 <- renderUI({
        tags$p("Perfil de presentación de vuelo internacional de otras aerolíneas' no encontrado.", style = "color: red;")
      })
    }
    
    if (!is.null(data$archivo6)) {
      tryCatch({
        # ARCHIVO 6
        data$verificacion_archivo6 <- verificar_columnas(data$archivo6, 3, "Perfil de presentación de vuelo en transferencia doméstica")
        output$message_archivo6 <- renderUI({
          tags$p(data$verificacion_archivo6$mensaje, style = ifelse(data$verificacion_archivo6$valido, "color: green;", "color: red;"))
        })
      }, error = function(e) {
        output$message_archivo6 <- renderUI({
          tags$p(paste("Error al cargar el archivo 'Perfil de presentación de vuelo en transferencia doméstica'", conditionMessage(e)), style = "color: red;")
        })
      })
    }else {
      output$message_archivo6 <- renderUI({
        tags$p("Perfil de presentación de vuelo en transferencia doméstica' no encontrado.", style = "color: red;")
      })
    }
    
    if (!is.null(data$archivo7)) {
      tryCatch({
        # ARCHIVO 7
        data$verificacion_archivo7 <- verificar_columnas(data$archivo7, 3, "Perfil de presentación de vuelo en transferencia internacional")
        output$message_archivo7 <- renderUI({
          tags$p(data$verificacion_archivo7$mensaje, style = ifelse(data$verificacion_archivo7$valido, "color: green;", "color: red;"))
        })
      }, error = function(e) {
        output$message_archivo7 <- renderUI({
          tags$p(paste("Error al cargar el archivo 'Perfil de presentación de vuelo en transferencia internacional'", conditionMessage(e)), style = "color: red;")
        })
      })
    }else {
      output$message_archivo7 <- renderUI({
        tags$p("Perfil de presentación de vuelo en transferencia internacional' no encontrado.", style = "color: red;")
      })
    }
    
    #### hola2 ####
    verificador_archivos <- FALSE
    if (!is.null(data$archivo1) && !is.null(data$archivo2) && !is.null(data$archivo3) && !is.null(data$archivo4) && !is.null(data$archivo5) && !is.null(data$archivo6) && !is.null(data$archivo7) && !is.null(data$make_up_dist) && !is.null(data$ci_dist) && !is.null(data$cap_line) && !is.null(data$ebs) && !is.null(data$carousel_times)){
      
      if (data$verificacion_archivo1$valido && data$verificacion_archivo2$valido && data$verificacion_archivo3$valido && data$verificacion_archivo4$valido && data$verificacion_archivo5$valido && data$verificacion_archivo6$valido && data$verificacion_archivo7$valido && data$verificacion_make_up_dist$valido && data$verificacion_ci_dist$valido
          && data$verificacion_cap_line$valido && data$verificacion_ebs$valido && data$verificacion_carousel_times$valido && hojas_verificar == TRUE && hojas_fixed_verificar == TRUE){
        output$loading_message1 <- renderUI({tags$p("Carga de datos completa", style = "color: green;")})
        verificador_archivos <- TRUE
        
      }
      
      else{
        output$loading_message1 <- renderUI({tags$p("Carga de datos no completa, revise los archivos adjuntados", style = "color: red;")})
      }
      
    }
    
    else{
      output$loading_message1 <- renderUI({tags$p("No se pueden cargar los datos debido a que faltan archivos por adjuntar o se han adjuntado archivos que presentan menos hojas que las requeridas", style = "color: red;")})
    }
    
    if (verificador_archivos == FALSE) {
      
      output$message_verificacion <- renderUI({
        tags$p("Error en la verificación debido a errores al adjuntar los archivos", style = "color: red;")
      })
      
      hide_spinner()
      
      return()
    }
    
    ##### 3.5.3. Transferencia de los datos almacenados al segundo script #####
    
    # Aquí puedes pasar los datos verificados al segundo script
    ddfs <<- as.data.table(data$archivo1)
    profilepresentation_LATAM_DOM <<- as.data.table(data$archivo2)
    profilepresentation_LATAM_INT <<- as.data.table(data$archivo3)
    profilepresentation_AvRest_DOM <<- as.data.table(data$archivo4)
    profilepresentation_AvRest_INT <<- as.data.table(data$archivo5)
    profilepresentation_Transfer_DOM <<- as.data.table(data$archivo6)
    profilepresentation_Transfer_INT <<- as.data.table(data$archivo7)
    
    #No modif
    checkin_area <<-as.data.table(data$ci_dist)
    make_up_carousel <<-as.data.table(data$make_up_dist)
    ebs_asignacion <<-as.data.table(data$ebs)
    line_capacity <<-as.data.table(data$cap_line)
    carousel_times <<-as.data.table(data$carousel_times)
    
    #Se guardan las variables modificadas en la app
    OD_pax <<- as.numeric(input$valor1)
    LF <<- as.numeric(input$valor2)
    bag_pax_DOM <<- as.numeric(input$valor3)
    bag_pax_INT <<- as.numeric(input$valor4)
    bag_pax_Transf <<- as.numeric(input$valor5)
    recirc <<- as.numeric(input$valor6)
    eds_limit <<- as.numeric(input$valor7)
    lost_track_value <<- as.numeric(input$valor8)
    no_pic <<- as.numeric(input$valor9)
    apr_n2 <<- as.numeric(input$valor100)
    rec_n2 <<- as.numeric(input$valor10)
    mes_val <<- as.numeric(input$valor11)
    eds_lost <<- as.numeric(input$valor12)
    capacidad_N2 <<- as.numeric(input$valor13)
    capacidad_N3 <<- as.numeric(input$valor14)
    cap_dollies <<- as.numeric(input$valor19)
    DOM_cierre_AV <<- as.numeric(input$valor20)
    DOM_duracion_AV <<- as.numeric(input$valor21)
    INT_cierre_AV <<- as.numeric(input$valor22)
    INT_duracion_AV <<- as.numeric(input$valor23)
    DOM_cierre_LA <<- as.numeric(input$valor24)
    DOM_duracion_LA <<- as.numeric(input$valor25)
    INT_cierre_LA <<- as.numeric(input$valor26)
    INT_duracion_LA <<- as.numeric(input$valor27)
    DOM_cierre_rest <<- as.numeric(input$valor28)
    DOM_duracion_rest <<- as.numeric(input$valor29)
    INT_cierre_rest <<- as.numeric(input$valor30)
    INT_duracion_rest <<- as.numeric(input$valor31)
    
    source("comprobacion.R")
    
    mensaje <- list()
    
    if(nrow(comp_airl_ddfs_ci) == 0 && nrow(comp_airl_ddfs_mu) == 0){
      output$message_verificacion <- renderUI({
        tags$p("Verificación exitosa", style = "color: green;")
      })
    } 
    
    else {
      # Empieza a acumular los mensajes de error
      mensaje <- append(mensaje, list(tags$p("Error en la verificación.", style = "color: red;")))
      
      if(nrow(comp_airl_ddfs_mu) != 0){
        # Obtener los nombres de las aerolíneas de la columna 'RawData'
        aerolineas_mu <- paste(comp_airl_ddfs_mu$RawData, collapse = ", ")
        mensaje <- append(mensaje, list(tags$p(paste("Las siguientes aerolíneas se deben añadir al archivo de make-up:", aerolineas_mu), style = "color: red; margin-left: 20px;")))
      }
      
      if(nrow(comp_airl_ddfs_ci) != 0){
        # Obtener los nombres de las aerolíneas de la columna 'RawData'
        aerolineas_ci <- paste(comp_airl_ddfs_ci$RawData, collapse = ", ")
        mensaje <- append(mensaje, list(tags$p(paste("Las siguientes aerolíneas se deben añadir al archivo de check-in:", aerolineas_ci), style = "color: red; margin-left: 20px;")))
      }
      
      # Finalmente, renderiza todos los mensajes acumulados como una lista de elementos
      output$message_verificacion <- renderUI({
        do.call(tagList, mensaje)
      })
    }
    
    source("calculos_previos.R")
    
    hide_spinner()
    
  })#Fin del observe
  
  #### 3.6. Observe de Ejecución de Cálculos ####
  #Inicio Observe Event que se usa para ejecutar los cálculos
  
  observeEvent(input$run_calculations, {
    
    ##### 3.6.1. Generación del gif de carga #####
        
    show_spinner() # Muestra el spinner, es el icono de carga
    Sys.sleep(5) # Simula un cálculo pesado
    #hide_spinner() # Oculta el spinner
    #Fin de Generación del gif de carga

    plots_list<-reactiveVal(NULL)
    plots_ops_list<-reactiveVal(NULL)
    
    source(paste0(wd,"/Estudio BHS.R"))
    
    ##### 3.6.2. Redirigir la salida del segundo script a una variable #####
    tryCatch({
      data$script_output <- capture.output(source(paste0(wd,"/Estudio BHS.R")))
      output$output_print <- renderPrint({ data$script_output })
    }, error = function(e) {
      output$output_print <- renderPrint({
        paste("Error al ejecutar el segundo script:", conditionMessage(e))
      })
    })
    # } else {
    #   output$contents <- renderUI({
    #     tags$p("Error: No todos los archivos tienen la estructura correcta.", style = "color: red;")
    #   })
    # }
    
    ##### 3.6.3. DEVOLUCIÓN DE TABLAS DE MÁXIMA DEMANDA #####

    
    output$tab_bag_entry <- renderDT({
      datatable(
        as.data.table(table_bag_entry),  # Convertir a data.table si es necesario
        options = list(
          pageLength = 13,                          # Número de filas por página
          dom = 'trB',                           # Incluir los botones y otros elementos
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Botones de exportación
          initComplete = JS(                        # Añadir un poco de espacio sobre los botones con CSS
            "function(settings, json) {",
            "$('.dt-buttons').css({'display': 'flex', 'justify-content': 'center', 'margin-top': '20px'});",
            "$('.dt-buttons button').css({'border-radius': '8px', 'font-size': '10px', 'padding': '4px 8px', 'width': '18%', 'margin': '0 2px'});",
            "}"
          )
        ),
        extensions = 'Buttons'                      # Extensión para los botones
      )
    })
    

    
    output$tab_mes<- renderDT({
      table_mes <- as.data.table(table_mes[2:5,])
      datatable(
        as.data.table(table_mes),  # Convertir a data.table si es necesario
        options = list(
          pageLength = 4,                          # Número de filas por página
          dom = 'trB',                           # Incluir los botones y otros elementos
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Botones de exportación
          initComplete = JS(                        # Añadir un poco de espacio sobre los botones con CSS
            "function(settings, json) {",
            "$('.dt-buttons').css({'display': 'flex', 'justify-content': 'center', 'margin-top': '20px'});",
            "$('.dt-buttons button').css({'border-radius': '8px', 'font-size': '10px', 'padding': '4px 8px', 'width': '18%', 'margin': '0 2px'});",
            "}"
          )
        ),
        extensions = 'Buttons'                      # Extensión para los botones
      )
    })
    

    output$tab_makeup<- renderDT({
      table_make_up <- as.data.table(table_make_up)
      table_make_up <- rbind(table_make_up[2:7,],table_make_up[10:23,] )
      datatable(
        as.data.table(table_make_up),  # Convertir a data.table si es necesario
        options = list(
          pageLength = 22,                          # Número de filas por página
          dom = 'trB',                           # Incluir los botones y otros elementos
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Botones de exportación
          initComplete = JS(                        # Añadir un poco de espacio sobre los botones con CSS
            "function(settings, json) {",
            "$('.dt-buttons').css({'display': 'flex', 'justify-content': 'center', 'margin-top': '20px'});",
            "$('.dt-buttons button').css({'border-radius': '8px', 'font-size': '10px', 'padding': '4px 8px', 'width': '18%', 'margin': '0 2px'});",
            "}"
          )
        ),
        extensions = 'Buttons'                      # Extensión para los botones
      )
      
    })
    

    
    output$tab_L3<- renderDT({
      datatable(
        as.data.table(table_L3),  # Convertir a data.table si es necesario
        options = list(
          pageLength = 1,                          # Número de filas por página
          dom = 'trB',                           # Incluir los botones y otros elementos
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Botones de exportación
          initComplete = JS(                        # Añadir un poco de espacio sobre los botones con CSS
            "function(settings, json) {",
            "$('.dt-buttons').css({'display': 'flex', 'justify-content': 'center', 'margin-top': '20px'});",
            "$('.dt-buttons button').css({'border-radius': '8px', 'font-size': '10px', 'padding': '4px 8px', 'width': '18%', 'margin': '0 2px'});",
            "}"
          )
        ),
        extensions = 'Buttons'                      # Extensión para los botones
      )
    })

    
    output$tab_eds<- renderDT({
      datatable(
        as.data.table(table_eds),  # Convertir a data.table si es necesario
        options = list(
          pageLength = 8,                          # Número de filas por página
          dom = 'trB',                           # Incluir los botones y otros elementos
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Botones de exportación
          initComplete = JS(                        # Añadir un poco de espacio sobre los botones con CSS
            "function(settings, json) {",
            "$('.dt-buttons').css({'display': 'flex', 'justify-content': 'center', 'margin-top': '20px'});",
            "$('.dt-buttons button').css({'border-radius': '8px', 'font-size': '10px', 'padding': '4px 8px', 'width': '18%', 'margin': '0 2px'});",
            "}"
          )
        ),
        extensions = 'Buttons'                      # Extensión para los botones
      )
    })
    

    output$tab_sorter<- renderDT({
      table_sorter <- as.data.table(table_sorter[2:3,])
      datatable(
        as.data.table(table_sorter),  # Convertir a data.table si es necesario
        options = list(
          pageLength = 2,                          # Número de filas por página
          dom = 'trB',                           # Incluir los botones y otros elementos
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Botones de exportación
          initComplete = JS(                        # Añadir un poco de espacio sobre los botones con CSS
            "function(settings, json) {",
            "$('.dt-buttons').css({'display': 'flex', 'justify-content': 'center', 'margin-top': '20px'});",
            "$('.dt-buttons button').css({'border-radius': '8px', 'font-size': '10px', 'padding': '4px 8px', 'width': '18%', 'margin': '0 2px'});",
            "}"
          )
        ),
        extensions = 'Buttons'                      # Extensión para los botones
      )
    })
    

    
    output$tab_ebs <- renderDT({
      datatable(
        as.data.table(table_ebs),  # Convertir a data.table si es necesario
        options = list(
          pageLength = 2,                          # Número de filas por página
          dom = 'trB',                           # Incluir los botones y otros elementos
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),  # Botones de exportación
          initComplete = JS(                        # Añadir un poco de espacio sobre los botones con CSS
            "function(settings, json) {",
            "$('.dt-buttons').css({'display': 'flex', 'justify-content': 'center', 'margin-top': '20px'});",
            "$('.dt-buttons button').css({'border-radius': '8px', 'font-size': '10px', 'padding': '4px 8px', 'width': '18%', 'margin': '0 2px'});",
            "}"
          )
        ),
        extensions = 'Buttons'                      # Extensión para los botones
      )
    })
    
    ##### 3.6.4. GENERACIÓN DE GRÁFICOS DE DEMANDA Y OPERADORES #####
    ### DEMANDA ###
    # Generar los gráficos en un bucle
    generated_plots <- lapply(seq_along(plots), function(i) {
      create_plot(
        data = plots[[i]]$data,
        x_var = "hour",
        y_var = plots[[i]]$y_var,
        y_label = "Equipajes",
        hline_value = plots[[i]]$hline_value
      )
    })
    
    
    plots_list(generated_plots)  # Actualiza la variable reactiva con los gráficos creados
    
    # Genera dinámicamente las salidas de los gráficos
    output$plots_ui <- renderUI({
      plot_output_list <- lapply(seq_along(plots_list()), function(i) {
        plotname <- paste("plot", i, sep = "")
        plotlyOutput(plotname)
      })
      do.call(tagList, plot_output_list)
    })
    
    # Renderiza cada gráfico en la lista plots_list
    lapply(seq_along(plots_list()), function(i) {
      output[[paste("plot", i, sep = "")]] <- renderPlotly({
        plots_list()[[i]]  # Aquí renderizas cada gráfico
      })
    })
    
    ### OPERADORES ###
    # Generar los gráficos en un bucle
    generated_operators_plots <- lapply(seq_along(plots_ops), function(i) {
      create_plot_ops(
        data = plots_ops[[i]]$data,
        x_var = "hour",
        y_var = plots_ops[[i]]$y_var,
        y_label = "Operadores"
      )
    })
    
    plots_ops_list(generated_operators_plots)  # Actualiza la variable reactiva con los gráficos creados
    
    # Genera dinámicamente las salidas de los gráficos
    output$plots_ui <- renderUI({
      plot_output_list_operators <- lapply(seq_along(plots_ops_list()), function(i) {
        plotname <- paste("plot", i, sep = "")
        plotlyOutput(plotname)
      })
      do.call(tagList, plot_output_list_operators)
    })
    
    # Renderiza cada gráfico
    lapply(seq_along(plots_list()), function(i) {
      output[[paste("plot_op", i, sep = "")]] <- renderPlotly({
        plots_ops_list()[[i]]
      })
    })
    
   
    output$loading_message2 <- renderUI({tags$p("Ejecución completa", style = "color: green;")})
    
    output$download_result_bag_entry <- downloadHandler(
      filename = function() {
        paste("result_bag_entry", ".xlsx", sep = "")
      },
      content = function(file) {
        if (exists("result_bag_entry")) {
          write_xlsx(result_bag_entry, path = file)
          # También puedes usar otros formatos como .xlsx si prefieres
          # write_xlsx(result_bag_entry, path = file)
        }
      }
    )
    

    output$dynamic_content_resultados <- renderUI({
      if (input$select_option == "bag_entry") {
        
        # Contenido para la opción "Entrada de equipajes al sistema"
        fluidRow(
          # Primer card con un único navPanel "Máxima Demanda"
          column(1),
          column(3,
                 card(
                   navset_underline(
                     nav_panel("Máxima demanda",    
                               dataTableOutput("tab_bag_entry")  # Tabla dentro del card
                     )
                   ),
                   style = "background-color: #ffffff; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);"
                 )
          ),
          
          # Segundo card con múltiples navPanels para gráficos
          column(7,
                 card(
                   navset_underline(
                     nav_panel("Total", 
                               tags$p("Entrada de equipajes total", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot1", width = "100%")  
                     ),
                     nav_panel("TR07", 
                               tags$p("Línea TR07", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot2", width = "100%")
                     ),
                     nav_panel("TR08", 
                               tags$p("Línea TR08", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot3", width = "100%")
                     ),
                     nav_panel("TR09", 
                               tags$p("Línea TR09", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot4", width = "100%")
                     ),
                     nav_panel("TR10", 
                               tags$p("Línea TR10", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot5", width = "100%")
                     ),
                     nav_panel("TR04", 
                               tags$p("Línea TR04", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot6", width = "100%")
                     ),
                     nav_panel("TR03", 
                               tags$p("Línea TR03", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot7", width = "100%")
                     ),
                     nav_panel("TR02", 
                               tags$p("Línea TR02", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot8", width = "100%")
                     ),
                     nav_panel("TR01", 
                               tags$p("Línea TR01", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot9", width = "100%")
                     ),
                     nav_panel("TX01", 
                               tags$p("Línea TX01", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot10", width = "100%")
                     ),
                     nav_panel("TX02", 
                               tags$p("Línea TX02", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot11", width = "100%")
                     ),
                     nav_panel("TX03", 
                               tags$p("Línea TX03", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot12", width = "100%")
                     ),
                     nav_panel("TX04", 
                               tags$p("Línea TX04", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot13", width = "100%")
                     ),
                   ),
                   style = "background-color: #ffffff; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);"
                 )
          ),
          column(1)
        )
      }
      
      else if (input$select_option == "sorter") {
        
        # Contenido para la opción "Entrada de equipajes al sistema"
        fluidRow(
          column(1),
          # Primer card con un único navPanel "Máxima Demanda"
          column(3,
                 card(
                   navset_underline(
                     nav_panel("Máxima demanda",    
                               dataTableOutput("tab_sorter")  # Tabla dentro del card
                     )
                   ),
                   style = "background-color: #ffffff; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);"
                 )
          ),
          
          # Segundo card con múltiples navPanels para gráficos
          column(7,
                 card(
                   navset_underline(
                     nav_panel("MS01", 
                               tags$p("Sorter MS01", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot28", width = "100%")  
                     ),
                     nav_panel("MS02", 
                               tags$p("Sorter MS02", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot29", width = "100%")
                     ),
                     
                   ),
                   style = "background-color: #ffffff; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);"
                 )
          ),
          column(1)
        )
      }
      
      else if (input$select_option == "mes") {
        
        # Contenido para la opción "Entrada de equipajes al sistema"
        fluidRow(
          column(1),
          # Primer card con un único navPanel "Máxima Demanda"
          column(3,
                 card(
                   navset_underline(
                     nav_panel("Máxima demanda",    
                               dataTableOutput("tab_mes")  # Tabla dentro del card
                     )
                   ),
                   style = "background-color: #ffffff; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);"
                 )
          ),
          
          # Segundo card con múltiples navPanels para gráficos
          column(7,
                 card(
                   navset_underline(
                     nav_panel("ME1", 
                               tags$p("Estación ME1", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot15", width = "100%")  
                     ),
                     nav_panel("ME2", 
                               tags$p("Estación ME2", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot16", width = "100%")
                     ),
                     nav_panel("ME3", 
                               tags$p("Estación ME3", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot17", width = "100%")  
                     ),
                     nav_panel("ME4", 
                               tags$p("Estación ME4", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot18", width = "100%")
                     ),
                     
                   ),
                   style = "background-color: #ffffff; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);"
                 )
          ),
          column(1)
        )
      }
      
      else if (input$select_option == "make_up") {

        # Contenido para la opción "Entrada de equipajes al sistema"
        fluidRow(
          column(1),
          # Primer card con un único navPanel "Máxima Demanda"
          column(3,
                 card(
                   navset_underline(
                     nav_panel("Máxima demanda",    
                               dataTableOutput("tab_makeup")  # Tabla dentro del card
                     )
                   ),
                   style = "background-color: #ffffff; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);"
                 )
          ),
          
          # Segundo card con múltiples navPanels para gráficos
          column(7,
                 card(
                   navset_underline(
                     nav_panel("MU01", 
                               tags$p("Línea MU01-1xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot30", width = "100%"),
                               tags$p("Línea MU01-2xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot31", width = "100%")
                     ),
                     nav_panel("MU02", 
                               tags$p("Línea MU02-1xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot32", width = "100%"),
                               tags$p("Línea MU02-2xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot33", width = "100%")
                     ),
                     nav_panel("MU03", 
                               tags$p("Línea MU03-1xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot34", width = "100%"),
                               tags$p("Línea MU03-2xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot35", width = "100%")
                     ),
                     nav_panel("MU05", 
                               tags$p("Línea MU05-1xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot36", width = "100%"),
                               tags$p("Línea MU05-2xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot37", width = "100%")
                     ),
                     nav_panel("MU06", 
                               tags$p("Línea MU06-1xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot38", width = "100%"),
                               tags$p("Línea MU06-2xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot39", width = "100%")
                     ),
                     nav_panel("MU07", 
                               tags$p("Línea MU07-1xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot40", width = "100%"),
                               tags$p("Línea MU07-2xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot41", width = "100%")
                     ),
                     nav_panel("MU08", 
                               tags$p("Línea MU08-1xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot42", width = "100%"),
                               tags$p("Línea MU08-2xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot43", width = "100%")
                     ),
                     nav_panel("MU09", 
                               tags$p("Línea MU09-1xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot44", width = "100%"),
                               tags$p("Línea MU09-2xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot45", width = "100%")
                     ),
                     nav_panel("MU10", 
                               tags$p("Línea MU10-1xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot46", width = "100%"),
                               tags$p("Línea MU10-2xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot47", width = "100%")
                     ),
                     nav_panel("MU11", 
                               tags$p("Línea MU11-1xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot48", width = "100%"),
                               tags$p("Línea MU11-2xx", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot49", width = "100%")
                     ),
                     
                   ),
                   style = "background-color: #ffffff; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);"
                 )
          ),
          column(1)
        )
      }
      
      else if (input$select_option == "nivel3") {
        
        # Contenido para la opción "Entrada de equipajes al sistema"
        fluidRow(
          column(1),
          # Primer card con un único navPanel "Máxima Demanda"
          column(3,
                 card(
                   navset_underline(
                     nav_panel("Máxima demanda",    
                               dataTableOutput("tab_L3")  # Tabla dentro del card
                     )
                   ),
                   style = "background-color: #ffffff; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);"
                 )
          ),
          
          # Segundo card con múltiples navPanels para gráficos
          column(7,
                 card(
                   navset_underline(
                     nav_panel("Nivel 3", 
                               tags$p("Demanda en Nivel 3", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot19", width = "100%"),
                               tags$p("Operadores necesarios en Nivel 3", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot_op1", width = "100%")
                     ),
                     
                   ),
                   style = "background-color: #ffffff; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);"
                 )
          ),
          column(1)
        )
      }
      
      else if (input$select_option == "eds") {
        
        # Contenido para la opción "Entrada de equipajes al sistema"
        fluidRow(
          column(1),
          # Primer card con un único navPanel "Máxima Demanda"
          column(3,
                 card(
                   navset_underline(
                     nav_panel("Máxima demanda",    
                               dataTableOutput("tab_eds")  # Tabla dentro del card
                     )
                   ),
                   style = "background-color: #ffffff; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);"
                 )
          ),
          
          # Segundo card con múltiples navPanels para gráficos
          column(7,
                 card(
                   navset_underline(
                     nav_panel("Total", 
                               tags$p("Demanda total en las líneas de inspección", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot20", width = "100%"),
                               tags$p("Operadores necesarios en Nivel 2", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot_op2", width = "100%")
                     ),
                     nav_panel("1L1", 
                               tags$p("Línea 1L1", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot21", width = "100%")
                     ),
                     nav_panel("1L2", 
                               tags$p("Línea 1L2", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot22", width = "100%")
                     ),
                     nav_panel("1L3", 
                               tags$p("Línea 1L3", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot23", width = "100%")
                     ),
                     nav_panel("1L4", 
                               tags$p("Línea 1L4", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot24", width = "100%")
                     ),
                     nav_panel("1L5", 
                               tags$p("Línea 1L5", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot25", width = "100%")
                     ),
                     nav_panel("1L6", 
                               tags$p("Línea 1L6", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot26", width = "100%")
                     ),
                     nav_panel("1L7", 
                               tags$p("Línea 1L7", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot27", width = "100%")
                     ),
                     
                   ),
                   style = "background-color: #ffffff; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);"
                 )
          ),
          column(1)
        )
      }
      
      else if (input$select_option == "ebs") {
        
        # Contenido para la opción "Entrada de equipajes al sistema"
        fluidRow(
          column(1),
          # Primer card con un único navPanel "Máxima Demanda"
          column(3,
                 card(
                   navset_underline(
                     nav_panel("Máxima demanda",    
                               dataTableOutput("tab_ebs")  # Tabla dentro del card
                     )
                   ),
                   style = "background-color: #ffffff; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);"
                 )
          ),
          
          # Segundo card con múltiples navPanels para gráficos
          column(7,
                 card(
                   navset_underline(
                     nav_panel("Automático", 
                               tags$p("EBS Automático", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot50", width = "100%")
                     ),
                     nav_panel("Manual", 
                               tags$p("EBS Manual", style = "font-weight: bold; font-size: 16px; margin-bottom: 10px;"),  # Texto antes del gráfico
                               plotlyOutput("plot51", width = "100%")
                     ),
                     
                   ),
                   style = "background-color: #ffffff; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);"
                 )
          ),
          column(1)
        )
      }
      
    })
    
    
    hide_spinner()
    
    
  })#Fin del observe
  
  #### 3.7. Observe para Capturas de Pantalla ####

  
  
  output$download_html <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Informe.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Informe.Rmd")
      file.copy("Informe.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
}

#### 4. Ejecutar Aplicación ####
# Ejecutar la aplicación shiny con shinydashboard
shinyApp(ui = ui, server = server)


