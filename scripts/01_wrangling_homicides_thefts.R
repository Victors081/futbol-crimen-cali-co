# HEADER --------------------------------------------
#
# Author:     Name Surname
# Copyright     Copyright 2024 - Name Surname
# Email:      yourmail@example.com
#
# Date:     2024-12-04
#
# Script Name:    D:/VICTOR/01_educacion/Posgrado/Maestria_ECO/Tesis/analisis/scripts/01_wrangling_homicides_thefts.R
#
# Script Description:
#
#
# SETUP ------------------------------------
cat("\014")                 # Clears the console
rm(list = ls())             # Remove all variables of the work space

# PACKAGES ------------------------------------
library(tidyverse)
library(readxl)

# FUNCTIONS ------------------------------------

source(file = "setup/functions.R")

# DATA ------------------------------------

df_homicidios <- read_excel(path = "input/B.D.HOMICIDIOS 2014 2024.xlsx")

df_homicidios <- df_homicidios |> 
  mutate(
    HORARIO = ymd_hms(
      paste0(
        format(`FECHA DEL HECHO`, "%Y-%m-%d"),
        " ",
        format(`HORA HECHO`, "%H:%M:%S")
      )
    ),
    FECHA = format(`FECHA DEL HECHO`, "%Y-%m-%d")
  ) |> 
  mutate(
    YEAR = format(as.Date(FECHA),"%Y"),
    MONTH = format(as.Date(FECHA),"%m"),
    DOW = as.character(wday(as.Date(FECHA), week_start=1))
  )

saveRDS(object = df_homicidios, file = "output/homicidios.rds")

list_hurtos <- read_excel_allsheets(
  filename = "input/B.D HURTOS 2019 2024.xls",
  tibble = TRUE
)


for (i in seq_along(list_hurtos)) {

  x <- names(list_hurtos)[i]

  if (any(x %in% c("Hurtos_2023","Hurtos_2024"))) {

    names(list_hurtos[[i]]) <- c(
      "TIPO_HURTO",
      "MODALIDAD","MODO",
      "CIUDAD","X","Y","AÑO",
      "FECHA","BARRIO","COMUNA"
    )

  } else{

    names(list_hurtos[[i]]) <- c(
      "TIPO_HURTO",
      "MODALIDAD",
      "MODO",
      "CIUDAD","X","Y",
      "AÑO","FECHA","HORA",
      "BARRIO","COMUNA"
    )
  }
}

df_hurtos <- bind_rows(list_hurtos)|>
  filter(!is.na(HORA), AÑO %in% c(2019:2022)) |>
  mutate(
    HORA = case_when(
      nchar(HORA) == 5 ~ paste0(HORA, ":00"),
      TRUE ~ HORA
    )
  ) |>
  mutate(
    HORARIO = ymd_hms(
      paste0(
        format(FECHA, "%Y-%m-%d"),
        " ",
        HORA
      )
    ),
    FECHA = format(FECHA, "%Y-%m-%d")
  ) |> 
    mutate(
      YEAR = format(as.Date(FECHA),"%Y"),
      MONTH = format(as.Date(FECHA),"%m"),
      DOW = as.character(wday(as.Date(FECHA), week_start=1))
    )

saveRDS(object = df_hurtos, file = "output/hurtos.rds")
