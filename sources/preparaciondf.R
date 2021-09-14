library(tidyverse)
library(readxl)
## preparacion del df para reporte interactivo 2020

# poner como workingdirectory la misma carpeta donde estan los archivos
setwd("sources")
# tener los reportes de actividad de la provincia en el WD

# crear lista de archivos
reportes <- list.files()
# filtrar solo los xls
reportes <- reportes[str_detect(reportes, ".xls")]
# lista vacia para el for
tablas <- list()
# levanto cada archivo y quito la extension ".xls" del nombre
for (i in seq_along(reportes)) {
    report <- str_remove(reportes[i], pattern = ".xls")
    tablas[[report]] <- read_xls(reportes[i])
}

# para cada tabla creo una columna "fecha" que toma el valor del nombre de la tabla
for (i in seq_along(tablas)) {
    tablas[[i]]["fecha"] <- names(tablas[i])
}

# creo un tibble vacio para el for
df <- tibble()

# hago un for y voy uniendo las tablas una por una con rbind
for (i in seq_along(tablas)) {
    df <- rbind(df, tablas[[i]])
}

print(df)

# limpieza de nombres y columnas
columnas <- names(df)
columnas[7:10] <- c("Menores de 12","De 12 a 20","De 21 a 65","Mayores de 65")
colnames(df) <- columnas
df$Provincia <- df$Provincia %>% 
    chartr("áéíóú", "aeiou",x = .)
df <- df %>% 
    filter(is.na(Provincia) != T)

df$fecha <- paste0(df$fecha,"01")
df$fecha <- lubridate::ymd(df$fecha)

df$Provincia <- toupper(df$Provincia)

write_csv(df, path = "df.csv")

## lista de vcs generales
videoconferenciasgenerales <- df %>% 
    filter(Eje == "Videoconferencias") %>% 
    group_by(fecha, Actividad) %>% 
    summarise(N = n()) %>% 
    ungroup() %>% 
    filter(N >= 30) %>% 
    select(Actividad) %>% 
    unique()