################# Extraccion viaje MyShipTracking ########################

# Setup
library(tidyverse)
library(lubridate)
library(rvest)

### Funcion

info_mst <- function(enlace_barco){
  
  # Leer pagina
  content_barco <- read_html(enlace_barco)
  
  ### INFO BARCO 1
  # Extraer tipo barco
  tipo_barco <- html_node(content_barco, "h2.mb-0") %>% html_text()
  # Extraer nombre barco
  nombre_barco <- html_node(content_barco, "h1.mb-0") %>% html_text()
  # Fecha y hora salida
  salida <- html_node(content_barco, xpath = "///*[@id='vpage-current-trip']/div[2]/div/div[1]/div/div[5]") %>%         html_text()
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
  
  ### INFO BARCO 2
  info_barco <- html_node(content_barco, xpath = "//*[@id='vsl-info-card']") %>%
    html_table() %>%
    mutate(X2 = str_remove_all(X2, " Tons")) %>%
    mutate(X2 = str_replace_all(X2, ",", ".")) %>%
    pivot_wider(names_from = X1, values_from = X2) %>%
    select(-1) 
  # Limpiar nombres columnas primera tabla
  colnames(info_barco) <- colnames(info_barco) %>%
    tolower() %>%
    str_replace_all(" ", "_")
  
  # Extraer la info del viaje
  viaje <- html_node(content_barco, ".p-sm-2") %>%
    html_table() %>%
    pivot_wider(names_from = X1, values_from = X2) %>%
    janitor::clean_names()
  
  # Construccion del dataframe final 
  df <- tibble(nombre_barco,
               imo = info_barco$imo,
               mmsi = info_barco$mmsi,
               salida,
               llegada,
               puerto_origen,
               puerto_destino,
               puerto_origen_locode,
               puerto_destino_locode) %>%
    mutate(trip_time = ifelse(is_empty(viaje$trip_time), NA, viaje$trip_time),
           time_travelled = ifelse(is_empty(viaje$time_travelled), NA, viaje$time_travelled),
           remaining_time = ifelse(is_empty(viaje$remaining_time), NA, viaje$remaining_time),
           trip_distance = ifelse(is_empty(viaje$trip_distance), NA, viaje$trip_distance),
           distance_travelled = ifelse(is_empty(viaje$distance_travelled), NA, viaje$distance_travelled),
           remaining_distance = ifelse(is_empty(viaje$remaining_distance), NA, viaje$remaining_distance))
  
  # Limpieza
  df_final <- df %>%
    mutate(across(where(is.character), str_remove_all, " nm")) %>%
    mutate(across(c(trip_distance, distance_travelled, remaining_distance), as.numeric)) %>%
    mutate(distance_total = ifelse(is.na(trip_distance), distance_travelled + remaining_distance, trip_distance))
  
  return(df_final)
  
  Sys.sleep(2)
}



# Urls y scraping

# Definir fecha hoy
hoy <- today() %>%
  stringr::str_remove_all("-")

# Recuperar archivo del dia
llegadas_final <- readr::read_csv(paste0("data/llegadas/llegadas_", hoy, ".csv"), col_types = cols(MMSI = col_character(), 
                                                                                            IMO = col_character()))

# Definir urls barcos a partir del MMSI e IMO de llegadas previstas Port BCN
urls_barcos <- paste0("https://www.myshiptracking.com/vessels/mmsi-", llegadas_final$MMSI, "-imo-", llegadas_final$IMO)
# Iterar con map_df y possibly para evitar que error detenga todo
llegadas_mst <- map_df(urls_barcos, possibly(info_mst))


# Guardar
# Guardar el archivo del dia con la fecha
write_csv(llegadas_mst, paste0("data/llegadas/", "llegadas_mst_", hoy, ".csv"))


############## Join MST con Port BCN -> QUEDA POR HACER ################

# # Seleccionar solo lo util de MST 
# llegadas_mst_util <- llegadas_mst %>%
#   select(imo, nombre_barco, salida, llegada, trip_distance, distance_travelled, remaining_distance, distance_total)
# 
# # Join 
# llegadas_join <- llegadas_final %>%
#   left_join(llegadas_mst_util, by = c("IMO" = "imo"))

