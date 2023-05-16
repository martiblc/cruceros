# Setup
library(tidyverse)
library(lubridate)
library(rvest)

#---------------------------------------------------------------#

# DATOS PORT DE BARCELONA

# Descargar archivo

llegadas <- read_csv("https://opendata.portdebarcelona.cat/dataset/342fe09b-017b-4019-a743-ee773f09befd/resource/72f0fc9e-b4b4-4a61-a0fb-e7b65b601b4d/download/arribadesavui.csv", col_types = cols(IMO = col_character())) %>%
# Filtrar los buques de pasaje
  filter(VAIXELLTIPUS == "Passatge") %>%
# Seleccionar columnas 
  select(ETA, ETAUTC, ETADIA, ETAHORA, ETD, ETDUTC, ETDDIA, ETDHORA, MMSI, IMO, VAIXELLNOM, VAIXELLTIPUS, CALAT_METRES, MANEGA_METRES, ESLORA_METRES, VAIXELLBANDERANOM, VAIXELLBANDERACODI, PORTORIGENNOM, PORTORIGENCODI, PORTDESTINOM, PORTDESTICODI, TERMINALNOM, TERMINALCODI, MOLLCODI, MOLLMODULS, CONSIGNATARI, ESTOPERATIUID, ESCALAESTAT, ESCALANUM, MESINFO)

# Join con Thetis

# Abrir Thetis
thetis18_21 <-  read_csv("data/thetis/thetis18_21.csv", col_types = cols(imo_number = col_character(), annual_average_fuel_consumption_per_distance_kg_n_mile = col_number())) %>%
  select(imo_number, name, annual_average_fuel_consumption_per_distance_kg_n_mile)

# Join con el numero IMO
llegadas_final <- llegadas %>%
  left_join(thetis18_21, by = c("IMO" = "imo_number"))

# Guardar
# Definir la fecha
hoy <- today() %>%
  stringr::str_remove_all("-")

# Guardar el archivo del dia con la fecha
write_csv(llegadas_final, paste0("data/llegadas/", "llegadas_", hoy, ".csv"))

#---------------------------------------------------#

# SCRAPING DATOS ADICIONALES

# Definir funcion
viaje_activo <- function(enlace_barco){
  # Leer pagina
  content_barco <- read_html(enlace_barco)
  # Extraer tipo barco
  tipo_barco <- html_node(content_barco, "h2.mb-0") %>% html_text()
  # Extraer nombre barco
  nombre_barco <- html_node(content_barco, "h1.mb-0") %>% html_text()
  # Fecha y hora salida
  salida <- html_node(content_barco, xpath = "///*[@id='vpage-current-trip']/div[2]/div/div[1]/div/div[5]") %>% html_text()
  # Fecha y hora llegada prevista
  llegada <- html_node(content_barco, xpath = "//*[@id='vpage-current-trip']/div[2]/div/div[2]/div/div[5]") %>%
    html_text()
  # Puerto de origen
  puerto_origen <- html_node(content_barco, xpath = "//*[@id='vpage-current-trip']/div[2]/div/div[1]/div/div[1]/h3/a") %>%
    html_text()
  # Codigo puerto origen
  puerto_origen_locode <- html_node(content_barco, xpath = "//*[@id='vpage-current-trip']/div[2]/div/div[1]/div/div[2]/div") %>%
    html_text() %>%
    str_trim()
  # Puerto de destino
  puerto_destino <- html_node(content_barco, xpath = "//*[@id='vpage-current-trip']/div[2]/div/div[2]/div/div[1]/h3/a") %>%
    html_text()
  # Codigo puerto destino 
  puerto_destino_locode <- html_node(content_barco, xpath = "//*[@id='vpage-current-trip']/div[2]/div/div[2]/div/div[2]/div") %>%
    html_text() %>%
    str_trim()
  # Extraer info primera tabla, la de identidad del barco
  info_barco <- html_node(content_barco, xpath = "//*[@id='vsl-info-card']") %>%
    html_table() %>%
    mutate(X2 = str_remove_all(X2, " Tons")) %>%
     mutate(X2 = str_replace_all(X2, ",", ".")) %>%
    pivot_wider(names_from = X1, values_from = X2) %>%
    select(-1) %>%
    mutate(GT = as.numeric(GT),
           DWT = as.numeric(DWT),
           Build = str_sub(Build, 1, 4),
           type = tipo_barco,
           nombre = nombre_barco,
           departure = salida,
           arrival = llegada,
           origen = puerto_origen,
           origen_code = puerto_origen_locode,
           destino = puerto_destino,
           destino_code = puerto_destino_locode)
  # Limpiar nombres columnas primera tabla
  colnames(info_barco) <- colnames(info_barco) %>%
    tolower() %>%
    str_replace_all(" ", "_")
  # Extraer la info del viaje activo 
  viaje_activo <- html_node(content_barco, ".p-sm-2") %>%
    html_table() %>%
    pivot_wider(names_from = X1, values_from = X2)
    # Se reune toda la info en una tabla
  viaje_barco <- info_barco %>%
    cbind(viaje_activo) %>%
    mutate(url = enlace_barco)
  return(viaje_barco)
  Sys.sleep(2)
}

# Definir enlaces y scrapear
urls_barcos <- paste0("https://www.myshiptracking.com/vessels/mmsi-", llegadas_final$MMSI, "-imo-", llegadas_final$IMO)
llegadas_mst <- map_df(urls_barcos, viaje_activo)

# Guardar el archivo del dia con la fecha
write_csv(llegadas_mst, paste0("data/llegadas/", "llegadas_mst_", hoy, ".csv"))

