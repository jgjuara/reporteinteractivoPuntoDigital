## preparacion del df para reporte interactivo 2020

# tener los reportes de actividad de la provincia en el WD
reportes <- list.files()
# filtrar solo los xls de reporte de actividad de total

tablas <- list()

for (i in seq_along(reportes)) {
    report <- str_extract(reportes[i], pattern = "[0-9]{6}")
    tablas[[report]] <- read_xls(reportes[i])
}


for (i in seq_along(tablas)) {
    tablas[[i]]["fecha"] <- names(tablas[i])
}

df <- rbind(tablas[[1]], tablas[[2]], tablas[[3]], tablas[[4]],
            tablas[[5]], tablas[[6]],tablas[[7]], tablas[[8]], tablas[[9]], tablas[[10]],
            tablas[[11]], tablas[[12]])

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