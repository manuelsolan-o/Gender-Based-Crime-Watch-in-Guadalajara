# Load Libraries -------------------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(leaflet)
library(readr)
library(xtable)

# Read Data --------------------------------------------------------------------------------------------------

data <- read_csv('datos-2023-12-19.csv')
data <- na.omit(data)

data <- rename(data, Bien_Afectado = bien_afectado,
               Fecha = fecha,
               Delito = delito)

# User Input --------------------------------------------------------------------------------------------------

crimen <- 'Violacion'

data <- filter(data, Delito == crimen)

# Define Colors -------------------------------------------------------------------------------------------

color_dict <- c('Robo a negocio' = '#1f77b4', 
                'Robo a int de vehiculos' = '#aec7e8', 
                'Lesiones dolosas' = '#ff7f0e',
                'Robo a vehiculos particulares' = '#ffbb78', 
                'Homicidio doloso' = '#2ca02c',
                'Violencia familiar' = '#98df8a', 
                'Robo casa habitacion' = '#d62728', 
                'Robo a persona' = '#ff9896',
                'Abuso sexual infantil' = '#f7b6d2', 
                'Robo de motocicleta' = '#c7c7c7',
                'Robo a cuentahabientes' = '#8c564b', 
                'Robo a carga pesada' = '#c49c94',
                'Robo de autopartes' = '#e377c2', 
                'Violacion' = '#9467bd', 
                'Robo a bancos' = '#7f7f7f', 
                'Feminicidio' = '#c5b0d5')


# Map Visualization -------------------------------------------------------------------------------------------

createPopupHTML <- function(row) {
  # Replace 3, 4, 5 with the actual indices of 'Bien Afectado', 'Fecha', 'Delito' in your data
  if(any(is.na(row[c(9, 1, 2)]))) {
    return(NA)
  }
  info <- data.frame(Información = row[c(9, 1, 2)], stringsAsFactors = FALSE)
  paste(capture.output(print(xtable(info), type = "html", include.rownames = TRUE)), collapse = "\n")
}

# Precompute HTML strings for popups
info_htmls <- apply(data, 1, createPopupHTML)

# Create a map and add all circle markers at once
m <- leaflet(data) %>% 
  addTiles() %>%
  setView(lng = -103.39182, lat = 20.66682, zoom = 12) %>%
  addCircleMarkers(
    lng = data$x, lat = data$y,
    radius = 3,
    color = color_dict[[crimen]],
    fillOpacity = 1,  # Establecer un valor fijo para la opacidad del relleno
    opacity = 1,
    popup = info_htmls
  )

m