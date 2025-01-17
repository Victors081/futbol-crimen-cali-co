# HEADER --------------------------------------------
#
# Author:     Victor Sarmiento
# Copyright     Copyright 2024 - Victor Sarmiento
# Email:      victorsarmiento8@gmail.com
#
# Date:     2024-12-04
#
# Script Name:    Tesis/analisis/scripts/02_econometric_analysis.R
#
# Script Description:
#
#
# LIBRARIES ------------------------------------
library(tidyverse)
library(sf)
library(lubridate)

## step 01: load data

homicidios <- read_rds(file = "output/homicidios.rds")

hurtos <- read_rds(file = "output/hurtos.rds")

partidos <- read_rds(file = "output/game_characteristics.rds")


# ECONOMETRIC ANALYSIS

library(MASS)

# ESTRATEGIA 1

## DID - ALL CRIMES

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

fechas_partidos <- partidos_america |>
  mutate(Year = as.numeric(format(Fecha_inicio, format = "%Y"))) |>
  filter(Year %in% c(2019:2022)) |> 
  pull(Fecha)

df_crimes <-
  homicidios |>
  filter(YEAR %in% c(2019:2022)) |>
  mutate(
    TIPO_CRIMEN = "HOMICIDIOS",
    COMUNA = as.character(COMUNA)
  ) |>
  bind_rows(
    hurtos |>
    mutate(TIPO_CRIMEN = "HURTOS")
  ) |>
  mutate(
    dist_num = distGeo(pt_estadios, cbind(X, Y)) * meters_to_miles
  ) |>
  mutate(
    ring_1 = case_when(
      dist_num <= 1 ~ 1,
      TRUE ~  0
    )
  ) |> 
  group_by(FECHA, ring_1) |>
  summarise(
    n = n()
  ) |>
  ungroup() |>
  group_by(ring_1)  |>
  complete(
    FECHA = as.character(
      seq.Date(
        min(as.Date(FECHA)),
        max(as.Date(FECHA)),
        by="day"
      )
    )
  ) |>
  mutate(n = replace_na(n, 0)) |>
  #mutate(
  #  FECHA =as.Date(FECHA)
  #)
  mutate(
    event = case_when(
      FECHA %in% fechas_partidos ~ 1,
      TRUE ~  0
    ),
    YEAR = format(as.Date(FECHA), "%Y"),
    MONTH = format(as.Date(FECHA), "%m"),
    DOW = as.character(wday(as.Date(FECHA), week_start=7))
  )



data_reg <-
  df_crimes |>
  mutate(
    ring_1 = as.character(ring_1),
    event = as.character(event)
  )

m1 <- glm.nb(n ~ ring_1 + event + YEAR + MONTH + DOW, data = data_reg)


summary(m1)

m2 <- glm.nb(n ~ ring_1 + event + ring_1 * event + YEAR + MONTH + DOW, data = data_reg)


summary(m2)


## DID - HURTOS

fechas_partidos <- partidos_america |>
  mutate(Year = as.numeric(format(Fecha_inicio,, format = "%Y"))) |>
  filter(Year %in% c(2019:2022)) |> 
  pull(Fecha)

df_crimes_hurtos <- 
  hurtos |> 
  mutate(
    dist_num = distGeo(pt_estadios, cbind(X, Y)) * meters_to_miles
  )|>
  mutate(
    ring_1 = case_when(
      dist_num <= 1 ~ 1,
      TRUE ~  0
    )
  ) |> 
  group_by(FECHA, ring_1) |> 
  summarise(
    n = n()
  ) |> 
  ungroup() |> 
  group_by(ring_1)  |>
  complete(FECHA = as.character(seq.Date(min(as.Date(FECHA)), max(as.Date(FECHA)), by="day"))) |> 
  mutate(n = replace_na(n, 0)) |>
  #mutate(
  #  FECHA =as.Date(FECHA)
  #)
  mutate(
    event = case_when(
      FECHA %in% fechas_partidos ~ 1,
      TRUE ~  0
    ),
    YEAR = format(as.Date(FECHA),"%Y"),
    MONTH = format(as.Date(FECHA),"%m"),
    DOW = as.character(wday(as.Date(FECHA), week_start=7))
  )



data_reg <- 
  df_crimes_hurtos |> 
  mutate(
    ring_1 = as.character(ring_1),
    event = as.character(event)
  )


m3 <- glm.nb(n ~ ring_1 + event + ring_1 * event + YEAR + MONTH + DOW, data = data_reg)


summary(m3)


## DID - HOMICIDIOS

fechas_partidos <- partidos_america |>
  mutate(Year = as.numeric(format(Fecha_inicio, format = "%Y"))) |>
  #filter(Year %in% c(2019:2022)) |> 
  pull(Fecha)

df_crimes_homicidios <-
  homicidios |>
  mutate(
    dist_num = distGeo(pt_estadios, cbind(X, Y)) * meters_to_miles
  )|>
  mutate(
    ring_1 = case_when(
      dist_num <= 1 ~ 1,
      TRUE ~  0
    )
  ) |>
  group_by(FECHA, ring_1) |> 
  summarise(
    n = n()
  ) |>
  ungroup() |>
  group_by(ring_1)  |>
  complete(FECHA = as.character(seq.Date(min(as.Date(FECHA)), max(as.Date(FECHA)), by="day"))) |>
  mutate(n = replace_na(n, 0)) |>
  #mutate(
  #  FECHA =as.Date(FECHA)
  #)
  mutate(
    event = case_when(
      FECHA %in% fechas_partidos ~ 1,
      TRUE ~  0
    ),
    YEAR = format(as.Date(FECHA),"%Y"),
    MONTH = format(as.Date(FECHA),"%m"),
    DOW = as.character(wday(as.Date(FECHA), week_start=7))
  )



data_reg <- 
  df_crimes_homicidios |> 
  mutate(
    ring_1 = as.character(ring_1),
    event = as.character(event)
  )


m4 <- glm.nb(n ~ ring_1 + event + ring_1 * event + YEAR + MONTH + DOW, data = data_reg)


summary(m4)


# ESTRATEGIA 2

fechas_partidos <- partidos_america |>
  mutate(Year = as.numeric(format(Fecha_inicio, format = "%Y"))) |>
  filter(Year %in% c(2019:2022)) |>
  pull(Fecha)

df_crimes_example <-
  hurtos |>
  filter(FECHA %in% fechas_partidos) |>
  mutate(
    TIPO_CRIMEN = "HURTOS",
    COMUNA = as.character(COMUNA)
  ) |>
  mutate(
    dist_num = distGeo(pt_estadios, cbind(X, Y)) * meters_to_miles
  ) |>
  mutate(
    rings = case_when(
      dist_num <= 1 ~ 1,
      dist_num > 1 & dist_num <= 2 ~ 2,
      dist_num > 2 & dist_num <= 3 ~ 3,
      # dist_num > 3 & dist_num <= 4 ~ 4,
      # dist_num > 4 & dist_num <= 5 ~ 5,
      TRUE ~  0
    )
  )

horario_partidos <- partidos_america |>
  mutate(Year = as.numeric(format(Fecha_inicio, format = "%Y"))) |>
  filter(Year %in% c(2019:2022)) |>
  pull(Fecha_inicio)


df_results <- data.frame()

for (i in seq_along(horario_partidos)) {

  df_temp <- df_crimes_example |>
    filter(
      HORARIO <= (horario_partidos[i] + hours(3)) &
        HORARIO >= (horario_partidos[i] - hours(3))
    ) |>
    mutate(Fecha_inicio = horario_partidos[i])|>
    mutate(
      min_diff = as.numeric(
      difftime(
        time1 = HORARIO,
        time2 = Fecha_inicio,
        units = "mins"
      )
    )/ 30
    )|>
    mutate(
    min_diff_cat = cut(
    min_diff,
    breaks = c(-7:6),             # Corrected breaks
    labels =  as.character(-6:6),             # Corrected labels
    #include.lowest = TRUE,       # Include the lowest boundary
    right = FALSE                # Left-closed intervals
  )
    ) |>
    filter(!is.na(min_diff_cat))

  #df_temp_2 <- df_crimes_example |>
  #filter(
  #  HORARIO <= (horario_partidos[i] + hours(3)) &
  #    HORARIO >= (horario_partidos[i] - hours(3))
  #)

  df_results <- bind_rows(df_results,df_temp)

}

df_crime_e2 <- df_results|>
  mutate_if(is.factor, as.character) |>
  group_by(Fecha_inicio, rings,min_diff_cat) |>
  summarise(
    n = n()
  ) |>
  ungroup() |>
  group_by(Fecha_inicio, rings) |>
  complete(
    min_diff_cat = as.character(seq(from = -6, to = 6, by = 1))
  )|>
  mutate(n = replace_na(n, 0)) |>
  mutate(
    min_diff_cat = case_when(
      as.numeric(min_diff_cat) < 0 ~ paste0("minus",gsub(pattern = "-", replacement = "", x = min_diff_cat)),
      as.numeric(min_diff_cat) >= 0 ~ min_diff_cat,
      TRUE ~ NA_character_
    )
  )


data_reg <- df_crime_e2 |>
  rename("Time" = "min_diff_cat")  |>
  dummy_cols(select_columns = "Time") |>
  select(n, rings, starts_with("Time_"))

# formula <- as.formula(
#   paste(
#     "n ~ ", 
#     paste0("Time_", c(paste0("minus", abs(-6:-1)), 0, 1:6), collapse = " + ")
#   )
# )

formula <- as.formula(
  paste(
    "n ~ ", 
    paste0(c(
      "Time_minus6",
      "Time_minus5",
      "Time_minus4",
      "Time_minus3",
      "Time_minus2",
      "Time_minus1",
      "Time_0",
      "Time_1",
      "Time_2",
      "Time_3",
      "Time_4",
      "Time_5"
      #"Time_6"
    ), collapse = " + ")

  )
)

mf <- lm(formula, data = data_reg)

summary(mf)

mr_0 <- lm(formula, data = data_reg |> filter(rings == 0))

summary(mr_0)

mr_1 <- lm(formula, data = data_reg |> filter(rings == 1))

summary(mr_1)

mr_2 <- lm(formula, data = data_reg |> filter(rings == 2))

summary(mr_2)

mr_3 <- lm(formula, data = data_reg |> filter(rings == 3))

summary(mr_3)



# ESTRATEGIA 3

fechas_partidos <- partidos_america |>
  mutate(Year = as.numeric(format(Fecha_inicio, format = "%Y"))) |>
  filter(Year %in% c(2019:2022)) |>
  pull(Fecha)

df_crimes_example <-
  hurtos |>
  filter(FECHA %in% fechas_partidos) |>
  mutate(
    TIPO_CRIMEN = "HURTOS",
    COMUNA = as.character(COMUNA)
  ) |>
  mutate(
    dist_num = distGeo(pt_estadios, cbind(X, Y)) * meters_to_miles
  ) |>
  mutate(
    rings = case_when(
      dist_num <= 1 ~ 1,
      dist_num > 1 & dist_num <= 2 ~ 2,
      dist_num > 2 & dist_num <= 3 ~ 3,
      # dist_num > 3 & dist_num <= 4 ~ 4,
      # dist_num > 4 & dist_num <= 5 ~ 5,
      TRUE ~  0
    )
  )

horario_partidos <- partidos_america |>
  mutate(Year = as.numeric(format(Fecha_inicio, format = "%Y"))) |>
  filter(Year %in% c(2019:2022)) |>
  pull(Fecha_inicio)


df_results <- data.frame()

for (i in seq_along(horario_partidos)) {

  df_temp <- df_crimes_example |>
    filter(
      HORARIO <= (horario_partidos[i] + hours(10)) &
        HORARIO >= (horario_partidos[i] - hours(10))
    ) |>
    mutate(Fecha_inicio = horario_partidos[i])|>
    mutate(
      min_diff = as.numeric(
      difftime(
        time1 = HORARIO,
        time2 = Fecha_inicio,
        units = "mins"
      )
    )/ 30
    )|>
    mutate(
    min_diff_cat = cut(
    min_diff,
    breaks = c(-21:20),             # Corrected breaks
    labels =  as.character(-20:20),             # Corrected labels
    #include.lowest = TRUE,       # Include the lowest boundary
    right = FALSE                # Left-closed intervals
  )
    ) |>
    filter(!is.na(min_diff_cat))

  #df_temp_2 <- df_crimes_example |>
  #filter(
  #  HORARIO <= (horario_partidos[i] + hours(3)) &
  #    HORARIO >= (horario_partidos[i] - hours(3))
  #)

  df_results <- bind_rows(df_results,df_temp)

}

df_crime_e2 <- df_results|>
  mutate_if(is.factor, as.character) |>
  group_by(Fecha_inicio, rings,min_diff_cat) |>
  summarise(
    n = n()
  ) |>
  ungroup() |>
  group_by(Fecha_inicio, rings) |>
  complete(
    min_diff_cat = as.character(seq(from = -20, to = 20, by = 1))
  )|>
  ungroup() |>
  mutate(
    n = replace_na(n, 0),
    Fecha_crimen = Fecha_inicio + hours(as.numeric(min_diff_cat))
  )|>
  mutate(
    min_diff_num = as.numeric(
      difftime(
        time1 = Fecha_crimen,
        time2 = Fecha_inicio,
        units = "mins"
      )
    ),
    end_game = case_when(
      Fecha_inicio == Fecha_crimen ~ 1,
      Fecha_inicio != Fecha_crimen ~ 0,
      TRUE ~ NA_integer_
    )
  )



  # mutate(
  #   min_diff_cat = case_when(
  #     as.numeric(min_diff_cat) < 0 ~ paste0("minus",gsub(pattern = "-", replacement = "", x = min_diff_cat)),
  #     as.numeric(min_diff_cat) >= 0 ~ min_diff_cat,
  #     TRUE ~ NA_character_
  #   )
  # )

library(rddtools)


# TOTAL

data_reg <- df_crime_e2 |>
  #rename("Time" = "min_diff_cat")  |>
  #dummy_cols(select_columns = "Time") |>
  select(n, rings, end_game, min_diff_num) |>
  mutate(
    min_diff_num_2 = min_diff_num^2,
    min_diff_num_3 = min_diff_num^3,
    min_diff_num_4 = min_diff_num^4,
    end_game = as.character(end_game)
  )

xs_data_reg <-
  data_reg |>
  #select(starts_with("min_diff"),-min_diff_num,end_game)|>
  select(end_game)

rdd_df <- rdd_data(
  y = n,
  x = min_diff_num,
  covar = xs_data_reg,
  cutpoint = 0,
  data = data_reg
)

rdd_reg <- rdd_reg_lm(
  rdd_object = rdd_df,
  covariates = TRUE,
  slope = c("same")
)

summary(rdd_reg)

rdd_reg <- rdd_reg_lm(
  rdd_object = rdd_df,
  covariates = TRUE,
  slope = c("separate"),
  order = 4
)

summary(rdd_reg)

rdd_df |>
  select(y, x) |>
  group_by(x) |>
  summarise(y = sum(y)) |>
  ungroup() |>
  mutate(
    threshold = as.character(case_when(
      x > 0 ~ 1,
      x < 0 ~ 0,
      TRUE ~ NA_integer_
    ))
  ) |>
  ggplot(aes(x = x, y = y, color = threshold)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ x + I(x ^2) + I(x ^3) + I(x ^4),
    level = 0.95
  ) +
  scale_color_brewer(palette = "Accent")  +
  guides(color = FALSE) +
  geom_vline(
    xintercept = 0,
    color = "red",
    size = 1,
    linetype = "dashed"
  ) +
  labs(
    y = "Total crimes",
    x = "Minutes"
  )

library(rddensity)
library(rdd)

DCdensity(rdd_df$x, cutpoint = 0)

density <- rddensity(rdd_df$x, c = 0)

rdplotdensity(density, rdd_df$x)

# RING 0

data_reg <- df_crime_e2 |>
  #rename("Time" = "min_diff_cat")  |>
  #dummy_cols(select_columns = "Time") |>
  select(n, rings, end_game, min_diff_num) |>
  mutate(
    min_diff_num_2 = min_diff_num^2,
    min_diff_num_3 = min_diff_num^3,
    min_diff_num_4 = min_diff_num^4,
    end_game = as.character(end_game)
  ) |>
  filter(
    rings == 0
  )


xs_data_reg <-
  data_reg |>
  #select(starts_with("min_diff"),-min_diff_num,end_game)|>
  select(end_game)

rdd_df <- rdd_data(
  y = n,
  x = min_diff_num,
  covar = xs_data_reg,
  cutpoint = 0,
  data = data_reg
)

rdd_reg <- rdd_reg_lm(
  rdd_object = rdd_df,
  covariates = TRUE,
  slope = c("same")
)

summary(rdd_reg)

rdd_reg <- rdd_reg_lm(
  rdd_object = rdd_df,
  covariates = TRUE,
  slope = c("separate"),
  order = 4
)

summary(rdd_reg)



rdd_df |>
  select(y, x) |>
  group_by(x) |>
  summarise(y = sum(y)) |>
  ungroup() |>
  mutate(
    threshold = as.character(case_when(
      x > 0 ~ 1,
      x < 0 ~ 0,
      TRUE ~ NA_integer_
    ))
  ) |>
  ggplot(aes(x = x, y = y, color = threshold)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ x + I(x ^2) + I(x ^3) + I(x ^4),
    level = 0.95
  ) +
  scale_color_brewer(palette = "Accent")  +
  guides(color = FALSE) +
  geom_vline(
    xintercept = 0,
    color = "red",
    size = 1,
    linetype = "dashed"
  ) +
  labs(
    y = "Total crimes",
    x = "Minutes"
  )


# RING 1

data_reg <- df_crime_e2 |>
  #rename("Time" = "min_diff_cat")  |>
  #dummy_cols(select_columns = "Time") |>
  select(n, rings, end_game, min_diff_num) |>
  mutate(
    min_diff_num_2 = min_diff_num^2,
    min_diff_num_3 = min_diff_num^3,
    min_diff_num_4 = min_diff_num^4,
    end_game = as.character(end_game)
  ) |>
  filter(
    rings == 1
  )


xs_data_reg <-
  data_reg |>
  #select(starts_with("min_diff"),-min_diff_num,end_game)|>
  select(end_game)

rdd_df <- rdd_data(
  y = n,
  x = min_diff_num,
  covar = xs_data_reg,
  cutpoint = 0,
  data = data_reg
)

rdd_reg <- rdd_reg_lm(
  rdd_object = rdd_df,
  covariates = TRUE,
  slope = c("same")
)

summary(rdd_reg)

rdd_reg <- rdd_reg_lm(
  rdd_object = rdd_df,
  covariates = TRUE,
  slope = c("separate"),
  order = 4
)

summary(rdd_reg)


rdd_df |>
  select(y, x) |>
  group_by(x) |>
  summarise(y = sum(y)) |>
  ungroup() |>
  mutate(
    threshold = as.character(case_when(
      x > 0 ~ 1,
      x < 0 ~ 0,
      TRUE ~ NA_integer_
    ))
  ) |>
  ggplot(aes(x = x, y = y, color = threshold)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ x + I(x ^2) + I(x ^3) + I(x ^4),
    level = 0.95
  ) +
  scale_color_brewer(palette = "Accent")  +
  guides(color = FALSE) +
  geom_vline(
    xintercept = 0,
    color = "red",
    size = 1,
    linetype = "dashed"
  ) +
  labs(
    y = "Total crimes",
    x = "Minutes"
  )


# RING 2

data_reg <- df_crime_e2 |>
  #rename("Time" = "min_diff_cat")  |>
  #dummy_cols(select_columns = "Time") |>
  select(n, rings, end_game, min_diff_num) |>
  mutate(
    min_diff_num_2 = min_diff_num^2,
    min_diff_num_3 = min_diff_num^3,
    min_diff_num_4 = min_diff_num^4,
    end_game = as.character(end_game)
  ) |>
  filter(
    rings == 2
  )


xs_data_reg <-
  data_reg |>
  #select(starts_with("min_diff"),-min_diff_num,end_game)|>
  select(end_game)

rdd_df <- rdd_data(
  y = n,
  x = min_diff_num,
  covar = xs_data_reg,
  cutpoint = 0,
  data = data_reg
)

rdd_reg <- rdd_reg_lm(
  rdd_object = rdd_df,
  covariates = TRUE,
  slope = c("same")
)

summary(rdd_reg)

rdd_reg <- rdd_reg_lm(
  rdd_object = rdd_df,
  covariates = TRUE,
  slope = c("separate"),
  order = 4
)

summary(rdd_reg)


rdd_df |>
  select(y, x) |>
  group_by(x) |>
  summarise(y = sum(y)) |>
  ungroup() |>
  mutate(
    threshold = as.character(case_when(
      x > 0 ~ 1,
      x < 0 ~ 0,
      TRUE ~ NA_integer_
    ))
  ) |>
  ggplot(aes(x = x, y = y, color = threshold)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ x + I(x ^2) + I(x ^3) + I(x ^4),
    #se = FALSE,
    level = 0.95
  )+
  scale_color_brewer(palette = "Accent")  +
  guides(color = FALSE) +
  geom_vline(
    xintercept = 0,
    color = "red",
    size = 1,
    linetype = "dashed"
  ) +
  labs(
    y = "Total crimes",
    x = "Minutes"
  )




# RING 3

data_reg <- df_crime_e2 |>
  #rename("Time" = "min_diff_cat")  |>
  #dummy_cols(select_columns = "Time") |>
  select(n, rings, end_game, min_diff_num) |>
  mutate(
    min_diff_num_2 = min_diff_num^2,
    min_diff_num_3 = min_diff_num^3,
    min_diff_num_4 = min_diff_num^4,
    end_game = as.character(end_game)
  ) |>
  filter(
    rings == 3
  )


xs_data_reg <-
  data_reg |>
  #select(starts_with("min_diff"),-min_diff_num,end_game)|>
  select(end_game)

rdd_df <- rdd_data(
  y = n,
  x = min_diff_num,
  covar = xs_data_reg,
  cutpoint = 0,
  data = data_reg
)

rdd_reg <- rdd_reg_lm(
  rdd_object = rdd_df,
  covariates = TRUE,
  slope = c("same")
)

summary(rdd_reg)

rdd_reg <- rdd_reg_lm(
  rdd_object = rdd_df,
  covariates = TRUE,
  slope = c("separate"),
  order = 4
)

summary(rdd_reg)


rdd_df |>
  select(y, x) |>
  group_by(x) |>
  summarise(y = sum(y)) |>
  ungroup() |>
  mutate(
    threshold = as.character(case_when(
      x > 0 ~ 1,
      x < 0 ~ 0,
      TRUE ~ NA_integer_
    ))
  ) |>
  ggplot(aes(x = x, y = y, color = threshold)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ x + I(x ^2) + I(x ^3) + I(x ^4),
    level = 0.95
  )+
  scale_color_brewer(palette = "Accent")  +
  guides(color = FALSE) +
  geom_vline(
    xintercept = 0,
    color = "red",
    size = 1,
    linetype = "dashed"
  ) +
  labs(
    y = "Total crimes",
    x = "Minutes"
  )





# rdd_data(y = carpenter_dobkin_2009$all, 
#          x = carpenter_dobkin_2009$agecell, 
#          cutpoint = 21) %>% 
#   rdd_reg_lm(slope = "separate") %>% 
#   summary()



# mutate(F2 = Fecha_inicio) |>
# mutate(fecha = format(horario_2, "%Y-%m-%d")) |>
# group_by(Fecha_inicio, rings) |>
# complete(
#   horario_2 =
#     seq(
#       min(F2) - hours(3),
#       min(F2) + hours(3),
#       by = "30 min"
#     )
# )|>
# ungroup()|>
# select(-F2)|>
# mutate(n = replace_na(n, 0))|>
# mutate(
#   hour_relative_gs = as.numeric(
#     difftime(
#       time1 = horario_2,
#       time2 = Fecha_inicio,
#       units = "hours"
#     )
#   )*2
# )|>
# mutate(
#   hour_relative_cat = case_when(
#     hour_relative_gs < 0 ~ paste0("l",gsub("-","",hour_relative_gs)),
#     hour_relative_gs >= 0 ~ paste0("u",hour_relative_gs),
#     TRUE ~ NA_character_
#   )
# )



# df_reg <- df_crime_e2 %>%
#  pivot_longer(cols = c(hour_relative_cat),
#               names_to = "dummy_names",
#               values_to = "dummy_levels") %>%
#  mutate(dummy_value = 1) %>%
#  pivot_wider(names_from = c(dummy_names, dummy_levels),
#              values_from = dummy_value, 
#              values_fill = 0)


# df_reg <- df_crime_e2 |>
# dummy_cols(select_columns = "hour_relative_cat")

# str(cl)



















# df_crime_e2|>
# mutate(horario_2=horario_2+minutes(29))

# data <- data.frame(
#   id= rep( (1:length(horario_partidos)), each = length(0:3)),
#   fecha_inicio = rep(horario_partidos, each = length(0:3))
# ) |>
# group_by(id) |>
# mutate(ring = c(0:3)) |>
# ungroup() |>
# rename("fecha_hecho" = "fecha_inicio") |>
# group_by(id,ring) |>
# complete(
#   fecha_hecho =
#     seq(
#       fecha_hecho - hours(3),
#       fecha_hecho + hours(3),
#       by = "30 min"
#     )
# )

# data$ring <- rep(0:3, length.out = nrow(data))


# df_crime_e2

#   df_crime_e2|>
#   #mutate(fecha = format(horario_2, "%Y-%m-%d")) |>
#   group_by(Fecha_inicio, rings) |>
#   complete(
#     horario_2 =
#       seq(
#         cur_group()$group[1] - hours(3),
#         cur_group()$group[1] + hours(3),
#         by = "30 min"
#       )
#   ) |>
#   mutate(n = replace_na(n, 0))

#   # group_by(Fecha_inicio)|>
#   # mutate(di =  HORARIO <= (Fecha_inicio + hours(6)) & HORARIO >= (Fecha_inicio - hours(6)))


# ymd_hms(
#       paste0(
#         format(FECHA, "%Y-%m-%d"),
#         " ",
#         HORA
#       )
#     )

# df_crimes_example |>
#   mutate(
#     horario_2 = lubridate::floor_date(HORARIO, "30 minutes")
#   ) |>
#   group_by(horario_2)|>
#   #group_by(DateTime = cut(HORARIO, breaks="30 min")) %>%
#   summarize(n = n())

# lubridate::as_datetime()

#   group_by(HORARIO, rings) |>
#   summarise(
#     n = n()
#   ) |>
#   ungroup() |>
#   mutate(
#     FECHA = as.Date(format(as.Date(HORARIO), "%Y-%m-%d"))
#   ) |>
#   left_join(
#     partidos_america |>
#     mutate(Year = as.numeric(format(Fecha_inicio, format = "%Y"))) |>
#     filter(Year %in% c(2019:2022)) |>
#     select(Fecha,Fecha_inicio),by = c("FECHA" = "Fecha")
#   ) |>
#   mutate(
#     hour_relative_gs = as.numeric(
#       difftime(
#         time1 = HORARIO,
#         time2 = Fecha_inicio,
#         units = "hours"
#       )
#     )
#   ) |>
#   filter(hour_relative_gs >= -3 & hour_relative_gs <= 3)

#   # complete(
#   #   FECHA = as.character(
#   #     seq.Date(
#   #       min(as.Date(FECHA)),
#   #       max(as.Date(FECHA)),
#   #       by="day"
#   #     )
#   #   )
#   # ) |>
#   # mutate(n = replace_na(n, 0)) |>
#   #mutate(
#   #  FECHA =as.Date(FECHA)
#   #)
#   # mutate(
#   #   event = case_when(
#   #     FECHA %in% fechas_partidos ~ 1,
#   #     TRUE ~  0
#   #   ),
#   #   YEAR = format(as.Date(FECHA), "%Y"),
#   #   MONTH = format(as.Date(FECHA), "%m"),
#   #   DOW = as.character(wday(as.Date(FECHA), week_start=7))
#   # )
























# # anova(m1, m2)






# # diamonds



# # fecha_inicio_p <- partidos_america$Fecha_inicio[44]

# # df_hurtos_filtrado_2 |>
# #   mutate(
# #     hour_relative_gs = as.numeric(difftime(HORARIO, fecha_inicio_p, 
# # units="hours"))
# #   )|> 
# #   slice_head(n = 100)



# # head(difftime(df_hurtos_filtrado_2$HORARIO, partidos_america$Fecha_inicio[44], 
# # units="hours"),100)











# # # BORRADOR ----

# # sapply(dist_hurtos_estadio, function(x) min(x) <= 1)


# # sapply(dist_hurtos_estadio, function(x) min(x) <= 1)


# # sapply(dist_hurtos_estadio, function(x) min(x) <= 1)


# # #risk_distances <- lapply(
# # #  X = 1:nrow(incidents),
# # #  FUN = function(x) distGeo(incidents[x,], risk_events)*meters_to_miles
# # #)







# # # Some fake latitude/longitude data. Make sure longitude is column 1 and latitude column 2
# # incidents = matrix(c(-70.1, -70.2, -73.3,  45 ,43, 46), nrow = 3)

# # # More fake data
# # risk_events = matrix(c(-70.05, -70.3, -71.6, -74.5,  44 ,40.2, 45.3, 44.3), nrow = 4)

# # # Distances between the different incident locations, in miles
# # # To get what we want we need to loop through the different incidences, to find 
# # # the distances between each. I'll do that looping here with lapply
# # incident_distances = lapply(1:nrow(incidents),
# #                             function(x) distGeo(incidents[x,], incidents[-x,])*meters_to_miles)

# # # For each location, are any of the distances to other locations below 1/4 mile?
# # # Again, loop through the different locations, this time with sapply to produce a vector
# # below_fourth = sapply(incident_distances, function(x) min(x) < 1/4)
# # # How many have another incident within a fourth of a mile?
# # sum(below_fourth)

# # # Similar code applies in checking one dataset of locations against another
# # # Except this time we don't have to drop our current location from the other data
# # risk_distances = lapply(1:nrow(incidents),
# #                             function(x) distGeo(incidents[x,], risk_events)*meters_to_miles)


# # distGeo()

# # below_fourth = sapply(risk_distances, function(x) min(x) < 1/4)
# # sum(below_fourth)




# # # partidos_cali <- partidos_eventos  |>
# # #   filter(Sedes == "Estadio Olímpico Pascual Guerrero")

# # partidos_america <-
# #   partidos |>
# #   filter(Local %in% c("CD América") | Visitante %in% c("CD América")) |>
# #   filter(Sedes == "Estadio Olímpico Pascual Guerrero") |>
# #   mutate(Asistencia=replace_na(Asistencia,0),
# #          Temporada = year(Fecha))|>
# #   filter(Temporada=="2017")

# # hist_odds <-
# #   historical_odds_colombia |>
# #   #filter(h_team=="America De Cali") |>
# #   filter(Temporada=="2017") |>
# #   filter(h_team=="America De Cali")





















# # sf_barrios_cali <- read_sf("input/barrios-cali/Barrios.shp")%>%
# #   st_transform(4326)

# # sf_estadios <- tibble(
# #   NOMBRE = "Estadio Olímpico Pascual Guerrero",
# #   X=-76.5436085,
# #   Y =3.4298687) %>%
# #   st_as_sf(coords = c("X", "Y"), crs=4326,remove = FALSE)



# # df_2 <- df_homicidios %>%
# #   filter(!is.na(X))%>%
# #   st_as_sf(coords = c("X", "Y"), crs=4326,remove = FALSE) |>
# #   filter(X != 0)




# # ggplot()+
# #   geom_sf(data = sf_barrios_cali)+
# #   geom_sf(data = df_2)+
# #   geom_sf(data = sf_estadios,color = "red")





# # sf_barrios_cali <- read_sf("input/barrios-cali/Barrios.shp")%>%
# #   st_transform(4326)

# # sf_estadios <- tibble(
# #   NOMBRE = "Estadio Olímpico Pascual Guerrero",
# #   X=-76.5436085,
# #   Y =3.4298687) %>%
# #   st_as_sf(coords = c("X", "Y"), crs=4326,remove = FALSE)



# # df_2 <- df_homicidios %>%
# #   filter(!is.na(X))%>%
# #   st_as_sf(coords = c("X", "Y"), crs=4326,remove = FALSE) |>
# #   filter(X != 0)




# # ggplot()+
# #   geom_sf(data = sf_barrios_cali)+
# #   geom_sf(data = df_2)+
# #   geom_sf(data = sf_estadios,color = "red")
