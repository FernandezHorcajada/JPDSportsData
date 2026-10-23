#EDA en R
data_1<-read.csv("datos_con_pos_fin.csv",sep=";")
data_2<-read.csv("eurocup_teams.csv")
data_1.columns
library(dplyr)

data_1 <- data_1 %>%mutate(nombre_equipo = "")
summary(data_1)
data_1$nombre_equipo[data_1$team_id == "MAD"] <- "Real Madrid"

media_triples <- data_1 %>%
  group_by(season_code) %>%
  summarise(media_intentos_3p = mean(three_points_attempted_per_game, na.rm = TRUE))

# Para ver el resultado completo:
print(media_triples) #sube un 27% los tiros

26.6/20.8


media_dobles <- data_1 %>%
  group_by(season_code) %>%
  summarise(media_intentos_2p = mean(two_points_attempted_per_game, na.rm = TRUE))

# Para ver el resultado completo:
print(media_dobles) #no hay cambio

media_puntos <- data_1 %>%
  group_by(season_code) %>%
  summarise(media_puntos = mean(points_per_game, na.rm = TRUE))

# Para ver el resultado completo:
print(media_puntos) #aumenta


library(ggplot2)

ggplot(media_triples, aes(x = season_code, y = media_intentos_3p, group = 1)) +
  geom_line(color = "#2b5c8f", linewidth = 1.2) +
  geom_point(color = "#1f4068", size = 2.5) +
  theme_minimal() +
  labs(
      title = "Evolución histórica de la media de triples intentados",
      x = "Temporada",
      y = "Media de triples"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #gráfico líneas triples 






library(ggplot2)
library(ggrepel) # Muy útil para que los nombres de los equipos no se superpongan

# Filtrar los datos exclusivamente para la temporada E2007
data_2007 <- subset(data_1, season_code == "E2007")

# Crear el gráfico de dispersión con etiquetas
ggplot(data_2007, aes(x = steals_per_game, y = turnovers_per_game, label = team_id)) +
  geom_point(aes(color = pos_fin), size = 2.5) +
  geom_text_repel(size = 3.5, fontface = "bold", color = "#1f4068") +
  theme_minimal() +
  labs(
    title = "Relación Robos vs. Pérdidas por partido (Euroliga 2007)",
    x = "Robos por partido (Steals per game)",
    y = "Pérdidas por partido (Turnovers per game)",
    color = "Posición Final" # Título opcional para la leyenda
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 10)
  )

# 1. Creamos la columna vacía
data_1$pos_fin <- ""

# 2. Rellenamos únicamente las filas que cumplen la condición de forma vectorizada
data_1$pos_fin[data_1$season_code == "E2025" & data_1$games_played == 38] <- "Fase Regular"


# 2. Rellenamos únicamente las filas que cumplen la condición de forma vectorizada
data_1$pos_fin[data_1$season_code == "E2025" & (data_1$games_played==35 |data_1$games_played==36) ] <- "Play-in"


# 2. Rellenamos únicamente las filas que cumplen la condición de forma vectorizada
data_1$pos_fin[data_1$season_code == "E2022" & (data_1$pos_fin=="") ] <- "Playoffs"
data_1$pos_fin[data_1$season_code == "E2022" & data_1$team_id=="BAR"] <- "Final Four"


data_1$nombre_equipo[data_1$team_id == "CSK"] <- "CSKA Moscú"


# 1. Asegúrate de que la columna 'pos_fin' existe en tu dataframe 'data_2007'
# (puedes verificarlo ejecutando: head(data_2007))

# 2. Exporta el dataframe a un archivo CSV
write.csv(data_1, "datos_con_pos_fin.csv", row.names = FALSE)


data_1$nombre_equipo[data_1$team_id == "HTA"] <-"Hapoel Telavivv"

df2=read.csv("euroleague_play_by_play (demasiado para excel).csv")
summary(df)

library(dplyr)
library(tidyr)

df_unificado <- df_unificado %>%
  group_by(game_id) %>%
  # Rellenamos hacia abajo los puntos vacíos dentro de cada partido
  fill(points_a, points_b, .direction = "down") %>%
  # Opcional pero recomendado: los huecos del inicio (antes de la primera canasta) ponlos a 0
  mutate(
    points_a = coalesce(points_a, 0),
    points_b = coalesce(points_b, 0)
  ) %>%
  ungroup()



library(dplyr)

# 1. Ver qué tipos de jugada hay exactamente cuando team es NA
na_team_summary <- df %>%
  filter(is.na(team)) %>%
  count(play_type, sort = TRUE)

print("Tipos de jugada cuando el equipo es NA:")
print(na_team_summary)

# 2. Comprobar si los play_type de fin de periodo/partido tienen siempre team en NA
end_plays_summary <- df %>%
  filter(play_type %in% c("EP", "EG")) %>%
  count(is.na(team))

print("¿Los EP/EG tienen siempre team como NA?: (TRUE = es NA, FALSE = tiene equipo)")
print(end_plays_summary)



library(dplyr)

# Suponiendo que tu dataset principal de jugadas se llama 'df' 
# y el otro se llama 'datos_con_pos_fin' (y ambos tienen la columna 'team_id')

df_unificado <- df_clean %>%
  left_join(
    datos_con_pos_fin %>% select(season_code, team_id, nombre_equipo),
    by = c("season_code", "team_id")
  )

datos_con_pos_fin<-read.csv("datos_con_pos_fin.csv",sep=";")

library(dplyr)

# Eliminamos las columnas 'comment' (vacía) y 'type' (constante a 0)
df <- df_unificado %>%
  select(-any_of(c("comment", "type","team")))


library(readr)

# Guardar el dataframe limpio como CSV
write_csv(df, "euroleague_play_by_play_limpio.csv")