df<- read.csv("euroleague_play_by_play_limpio.csv")
#summary(df)

library(dplyr)
library(tidyr)
library(stringr)

for (anio in 2007:2025) {
  # Genera el código de temporada (ej: "E2007", "E2008", ..., "E2025")
  season_str <- paste0("E", anio)
  
  message("Procesando temporada: ", season_str)
  
  # Filtrar datos de la temporada
  data_temp <- subset(df, season_code == season_str)

  # 1. Definir el vector con las acciones deseadas
  acciones_deseadas <- c(
    "IN", "OUT", "D", "RV", "2FGM", "CM", "FTM", "2FGA", "TO", "AS", 
    "3FGA", "O", "ST", "3FGM", "FTA", "FV", "2FGAB", "AG", "OF", "TOUT", "CMT", "CMU"
  )

  # 2. Crear la tabla cruzada con las acciones
  tabla_partidos_acciones <- data_temp %>%
    filter(play_type %in% acciones_deseadas) %>%
    count(game_id, team_id,season_code, game, phase, round, play_type) %>%
    pivot_wider(
      names_from = play_type, 
      values_from = n, 
      values_fill = 0
    )

  # 3. Renombrar las columnas de acciones
  tabla_partidos_acciones <- tabla_partidos_acciones %>%
    rename(
      Cambios = "IN",
      Salidas = "OUT",
      Reb_def = "D",
      Falta_Rec = "RV",
      T2Anotado = "2FGM",
      Falta_Com = "CM",
      TLAnotado = "FTM",
      T2Fallado = "2FGA",
      Pérdidas = "TO",
      Asistencias = "AS",
      Tiros3F = "3FGA",
      Reb_Of = "O",
      Robos = "ST",
      Tiros3Anotado = "3FGM",
      TLFallado = "FTA",
      TaponAF = "FV",
      TaponC = "AG",
      FaltaAtaque = "OF",
      TiemposMuertos = "TOUT",
      Técnica = "CMT",
      Antideportivas = "CMU"
    )

  # 4. Calcular porcentajes, extraer equipos, puntos y si se ganó o perdió
  temporada <- tabla_partidos_acciones %>%
    mutate(
      pct_tiros_dos = T2Anotado / (T2Fallado + T2Anotado),
      pct_tiros_tres = Tiros3Anotado / (Tiros3F + Tiros3Anotado),
      pct_tiros_libres = TLAnotado / (TLFallado + TLAnotado),
      es_eliminatoria = !grepl("REGULAR", phase, ignore.case = TRUE),
      
      # Separar equipos local y visitante de forma segura
      home_team = str_split_fixed(game, "-", 2)[, 1],
      away_team = str_split_fixed(game, "-", 2)[, 2],
      
      # Variables de partido y puntos
      es_local = as.integer(team_id == home_team),
      puntos = (T2Anotado * 2) + (Tiros3Anotado * 3) + TLAnotado
    ) %>%
    # Agrupar por partido para calcular los puntos del rival y el resultado final
    group_by(game_id) %>%
    mutate(
      puntos_rival = ifelse(es_local == 1, puntos[es_local == 0], puntos[es_local == 1]),
      ganado = as.integer(puntos > puntos_rival)
    ) %>%
    ungroup() %>%
    # 5. Limpiar columnas temporales que ya no necesitas
    select(-any_of(c("Salidas", "no_regular_season", "column00", "2FGAB", "home_team", "away_team")))

  # 6. Guardar el archivo CSV de la temporada
  write.csv(temporada, paste0("temporada_", anio, ".csv"), row.names = FALSE)
}


library(dplyr)

# Lista vacía para acumular los dataframes de cada temporada
lista_temporadas <- list()

for (anio in 2007:2025) {
  nombre_archivo <- paste0("temporada_", anio, ".csv")
  
  # Verificamos si el archivo existe antes de leerlo
  if (file.exists(nombre_archivo)) {
    message("Leyendo: ", nombre_archivo)
    lista_temporadas[[as.character(anio)]] <- read.csv(nombre_archivo)
  }
}

# Unir todos los dataframes de la lista en uno solo
dataset_total <- bind_rows(lista_temporadas)

# (Opcional) Guardarlo en un archivo CSV consolidado
write.csv(dataset_total, "euroliga_stats_por_partido.csv", row.names = FALSE)

# Ver un resumen rápido
glimpse(dataset_total)