# HEADER --------------------------------------------
#
# Author:     Victor Sarmiento
# Copyright     Copyright 2024 - Victor Sarmiento
# Email:      victorsarmiento8@example.com
#
# Date:     2024-11-02
#
# Script Name:    D:/VICTOR/Educacion/Tesis/analisis/scripts/00_analisis.R
#
# Script Description:
#
#
# SETUP ------------------------------------
cat("\014")                 # Clears the console
rm(list = ls())             # Remove all variables of the work space

# PACKAGES ------------------------------------
library(tidyverse)
library(httr2)
library(rvest)

# DATA ------------------------------------

#https://fbref.com/es/comps/41/2023/horario/Marcadores-y-partidos-de-2023-Primera-A

anios <- 2014:2023

partidos_eventos <- data.frame()

for (i in seq_along(anios)) {


  html_page <- read_html(x = paste0("https://fbref.com/es/comps/41/",anios[i],"/horario/Marcadores-y-partidos-de-",anios[i],"-Primera-A"))

  results <- html_page %>%
    html_elements("#sched_all") %>%
    html_table()

  table <- results[[1]] |>
    filter(Ronda != "")

  partidos_eventos <- bind_rows(partidos_eventos,table)

}


write_csv(x = partidos_eventos,file = "output/partidos_eventos.csv")
#
# save(partidos_eventos,file = "output/partidos_eventos.RData")
#
#
# df_partidos <- partidos_eventos %>%
#   mutate(across(everything(), ~ replace(.x, .x=="", NA_character_)))
#
#
#
#
#
# html_page <- read_html(x = 'https://www.oddsportal.com/football/colombia/liga-postobon-2014/results/#/page/2/')
#
# html_page %>%
#   html_elements(css="div.min-h-[1000px]")
#
#
# html_page %>%
#   html_nodes(xpath='//*[@id="app"]/div[1]/div[1]/div/main/div[3]/div[4]/div[1]')
#
#
# html_page%>%
#   html_nodes('a') %>%
#   html_attr('href')
#
# html_page |>
#   html_elements('#app > div.relative.flex.flex-col.w-full.max-w-\[1350px\].font-main > div.w-full.flex-center.bg-gray-med_light > div > main > div.relative.w-full.flex-grow-1.min-w-\[320px\].bg-white-main > div.flex.flex-col.px-3.text-sm.max-mm\:px-0 > div.min-h-\[206px\] > div:nth-child(1)')
#
#  html_page |>
#   html_elements(xpath= '//*[@id="app"]/div[1]/div[1]/div/main/div[3]/div[4]')
#
# '#app > div.relative.flex.flex-col.w-full.max-w-\[1350px\].font-main > div.w-full.flex-center.bg-gray-med_light > div > main > div.relative.w-full.flex-grow-1.min-w-\[320px\].bg-white-main > div.flex.flex-col.px-3.text-sm.max-mm\:px-0 > div.min-h-\[206px\] > div:nth-child(1)'
#
# html_page %>%
#   html_nodes(xpath='//*[@id="app"]/div[1]/div[1]/div/main/div[3]/div[4]/div[1]') |>
#   html_node('href')
#
# html_page %>%
#   html_nodes(xpath='//*[@id="app"]/div/div[1]/div/main/div[2]/div[4]/div[1]/div')
#
#   html_nodes(xpath='//*[@id="app"]/div[1]/div[1]/div/main/div[3]/div[4]/div[1]/div[1]')
#
#
#
#   '//*[@id="app"]/div/div[1]/div/main/div[2]/div[4]/div[1]/div'
#
#
