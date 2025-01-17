
library(tidyverse)
library(sf)


df_homi <- read_sf(file.choose())


ggplot()+
  geom_sf(data = df_homi)
