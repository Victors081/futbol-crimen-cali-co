# HEADER --------------------------------------------
#
# Author:     Victor Sarmiento
# Copyright     Copyright 2024 - Victor Sarmiento
# Email:      yourmail@example.com
#
# Date:     2024-12-04
#
# Script Name:    Tesis/analisis/scripts/01_wrangling_homicides_thefts.R
#
# Script Description:
#
#
# LIBRARIES ------------------------------------
library(tidyverse)
library(sf)
library(lubridate)
library(geosphere)
library(fastDummies)

# ANALYSIS ------------------------------------

## step 01: load data

homicidios <- read_rds(file = "output/homicidios.rds")

hurtos <- read_rds(file = "output/hurtos.rds")

partidos <- read_rds(file = "output/game_characteristics.rds")


# EDA

# step 02: the América de Cali matches are
# extracted (especifically those who where played
# in Estadio Olimpico Pascual Guerrero)

partidos_america <- partidos |>
  filter(Local %in% c("CD América")) |>
  filter(Sedes == "Estadio Olímpico Pascual Guerrero") |>
  mutate(
    Asistencia = replace_na(Asistencia, 0),
    Temporada = year(Fecha),
    Win = case_when(
      Home_marcador > Away_marcador ~ 1,
      TRUE ~ 0
    ),
    Tie = case_when(
      Home_marcador == Away_marcador ~ 1,
      TRUE ~ 0
    ),
    Loss = case_when(
      Home_marcador < Away_marcador ~ 1,
      TRUE ~ 0
    )
  ) |>
  mutate(
    Fecha_inicio = ymd_hms(paste0(Fecha, " ", Hora)),
    Id = seq_len(n())
  )

sf_estadios <- tibble(
  NOMBRE = "Estadio Olímpico Pascual Guerrero",
  X = -76.5436085,
  Y = 3.4298687
) |>
  st_as_sf(
    coords = c("X", "Y"),
    crs = 4326,
    remove = FALSE
  )

pt_estadios <- sf_estadios |>
  select(X, Y) |>
  st_drop_geometry() |>
  as.matrix()


# geosphere functions return meters. If you want miles:
meters_to_miles <- (1 / 1609.344)

# step 03: the América de Cali matches are summarized
# with metrics like n (number of matches), attendance
# (number people who attend the event), win (number
# of games won), tie (number of tied games), and
# loss (number of games lost)

df_partidos_stats <- partidos_america |>
  summarise(
    n = n(),
    Attendance = mean(Asistencia),
    Win = sum(Win),
    Tie = sum(Tie),
    Loss = sum(Loss)
    # TODO: calculate the spread and the upset loss metrics
  )

# step 04: the number of homicides before and after
# the beginning of the game are calculated

fechas_partidos <- partidos_america |>
  #mutate(Fecha = format(x = Fecha_inicio, format = "%Y-%m-%d")) |>
  pull(Fecha)

horario_partidos <- partidos_america |>
  pull(Fecha_inicio)

df_results <- data.frame()

for (i in seq_along(fechas_partidos)) {

  df_day <- homicidios |>
    filter(FECHA == fechas_partidos[i])


  df_day_b <- df_day |>
    filter(HORARIO < horario_partidos[i]) |>
    mutate(
      OCURRENCIA = "ANTES"
    )

  df_day_a <- df_day |>
    filter(HORARIO > horario_partidos[i]) |>
    mutate(
      OCURRENCIA = "DESPUES"
    )

  df_temp <- bind_rows(df_day_b, df_day_a)

  df_results <- bind_rows(df_results, df_temp)

}

# TODO: improve the logic here, maybe use case_when() instead of a for-loop

df_homicidios_filtrado <- df_results

table(df_homicidios_filtrado$OCURRENCIA)


# step 04: the number of thefts before and after
# the beginning of the game are calculated

fechas_partidos <- partidos_america |>
  #mutate(Fecha = format(Fecha_inicio,, format = "%Y-%m-%d")) |>
  pull(Fecha)

horario_partidos <- partidos_america |>
  pull(Fecha_inicio)

df_results <- data.frame()

for (i in seq_along(fechas_partidos)) {

  df_day <- hurtos |>
    filter(FECHA == fechas_partidos[i])


  df_day_b <- df_day |>
    filter(HORARIO < horario_partidos[i]) |>
    mutate(
      OCURRENCIA = "ANTES"
    )

  df_day_a <- df_day |>
    filter(HORARIO > horario_partidos[i]) |>
    mutate(
      OCURRENCIA = "DESPUES"
    )

  df_temp <- bind_rows(df_day_b, df_day_a)

  df_results <- bind_rows(df_results, df_temp)

}

# TODO: improve the logic here, maybe use case_when() instead of a for-loop


df_hurtos_filtrado <- df_results

table(df_hurtos_filtrado$OCURRENCIA)


# step 05: calculate the distance between the stadium and
# the homicidies and thefts


pt_hurtos <- df_hurtos_filtrado |>
  select(X, Y) |>
  as.matrix()


pt_homicidios <- df_homicidios_filtrado |>
  select(X, Y) |>
  as.matrix()

df_hurtos_filtrado_2 <- df_hurtos_filtrado |>
  mutate(
    dist_num = distGeo(pt_estadios, pt_hurtos) * meters_to_miles
  ) |>
  mutate(
    dist_cat = case_when(
      dist_num <= 1 ~ "0-1",
      dist_num > 1 & dist_num <= 2 ~ "1-2",
      dist_num > 2 & dist_num <= 3 ~ "2-3",
      TRUE ~  NA_character_
    )
  )

table(df_hurtos_filtrado_2$OCURRENCIA, df_hurtos_filtrado_2$dist_cat)


df_homicidios_filtrado_2 <- df_homicidios_filtrado |>
  mutate(
    dist_num = distGeo(pt_estadios, pt_homicidios) * meters_to_miles
  ) |>
  mutate(
    dist_cat = case_when(
      dist_num <= 1 ~ "0-1",
      dist_num > 1 & dist_num <= 2 ~ "1-2",
      dist_num > 2 & dist_num <= 3 ~ "2-3",
      TRUE ~  NA_character_
    )
  )

table(df_homicidios_filtrado_2$OCURRENCIA,df_homicidios_filtrado_2$dist_cat)


df_difftime <- data.frame()

for (i in seq_along(partidos_america$Id)) {

  fecha_inicio_p <- partidos_america$Fecha_inicio[i]

  df_temp  <- df_hurtos_filtrado_2 |>
    mutate(
      hour_relative_gs = as.numeric(
        difftime(
          time1 = HORARIO,
          time2 = partidos_america$Fecha_inicio[i],
          units = "hours"
        )
      ),
      id_partido = partidos_america$Id[i]
    )


  df_difftime <- bind_rows(df_difftime, df_temp)
}

end_game <- ((1/60) * 90)

plt <- df_difftime |>
  filter(hour_relative_gs >= -24 & hour_relative_gs <= 24) |>
  as_tibble() |>
  ggplot(aes(hour_relative_gs, fill = dist_cat, colour = dist_cat)) +
  geom_density(alpha = 0.1)+
  geom_vline(xintercept = end_game)

plt + annotate(x = end_game, y = +Inf, label = "Game end time", vjust = 2, geom = "label")
