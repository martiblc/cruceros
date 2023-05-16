# Setup
library(tidyverse)
library(lubridate)

# Descargar archivo

llegadas <- read_csv("https://opendata.portdebarcelona.cat/dataset/342fe09b-017b-4019-a743-ee773f09befd/resource/72f0fc9e-b4b4-4a61-a0fb-e7b65b601b4d/download/arribadesavui.csv", col_types = cols(IMO = col_character())) %>%
# Filtrar los buques de pasaje
  filter(VAIXELLTIPUS == "Passatge") %>%
# Seleccionar columnas 
  select(ETA, ETAUTC, ETADIA, ETAHORA, ETD, ETDUTC, ETDDIA, ETDHORA, MMSI, IMO, VAIXELLNOM, VAIXELLTIPUS, CALAT_METRES, MANEGA_METRES, ESLORA_METRES, VAIXELLBANDERANOM, VAIXELLBANDERACODI, PORTORIGENNOM, PORTORIGENCODI, PORTDESTINOM, PORTDESTICODI, TERMINALNOM, TERMINALCODI, MOLLCODI, MOLLMODULS, CONSIGNATARI, ESTOPERATIUID, ESCALAESTAT, ESCALANUM, MESINFO)

# Join con Thetis

# Abrir Thetis
thetis18_21 <-  read_csv("data/thatis/thetis18_21.csv", col_types = cols(imo_number = col_character(), annual_average_fuel_consumption_per_distance_kg_n_mile = col_number())) %>%
  select(imo_number, name, annual_average_fuel_consumption_per_distance_kg_n_mile)

# Join con el numero IMO
llegadas_final <- llegadas %>%
  left_join(thetis18_21, by = c("IMO" = "imo_number"))

# Guardar
# Definir la fecha
hoy <- today() %>%
  stringr::str_remove_all("-")

# Guardar el archivo del dia con la fecha
write_csv(llegadas_final, paste0("data/llegadas", "llegadas_", hoy, ".csv"))

