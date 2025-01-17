# HEADER --------------------------------------------
#
# Author:     Name Surname
# Copyright     Copyright 2024 - Name Surname
# Email:      yourmail@example.com
#
# Date:     2024-12-04
#
# Script Name:    D:/VICTOR/01_educacion/Posgrado/Maestria_ECO/Tesis/analisis/scripts/01_wrangling_game_characteristics.R
#
# Script Description:
#
#
# SETUP ------------------------------------
cat("\014")                 # Clears the console
rm(list = ls())             # Remove all variables of the work space

# PACKAGES ------------------------------------

library(tidyverse)

# FUNCTIONS ------------------------------------

historical_odds_colombia <- read_delim(file = "input/historical_odds_colombia_primera_a.csv",delim = ';')

partidos_eventos <- read_csv("output/partidos_eventos.csv") |>
separate(
  Marcador, 
  into = c("Home_marcador", "Away_marcador"), 
  sep = "–",
  remove = FALSE
)

# left_join(partidos_eventos,historical_odds_colombia)

saveRDS(partidos_eventos, file = "output/game_characteristics.rds")


