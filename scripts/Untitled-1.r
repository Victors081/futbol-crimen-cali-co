df <- data_frame(
  group = c(1:2, 1),
  item_id = c(1:2, 2),
  item_name = c("a", "b", "b"),
  value1 = 1:3,
  value2 = 4:6
)
df %>% complete(group, nesting(item_id, item_name))



# Crear variables temporales de ejemplo
set.seed(123)
data <- data.frame(
  Crimes = rnorm(100),  # Variable dependiente
  Home = sample(0:1, 100, replace = TRUE),  # Variable de tratamiento
  ID = rep(1:10, each = 10),  # Identificadores de unidades
  Time = rep(-6:6, length.out = 100)  # Variables de tiempo
)

# Crear variables dummies para Time_t
for (t in -6:6) {
  col_name <- paste0("Time_", ifelse(t < 0, paste0("minus", abs(t)), t))  # Nombres válidos
  data[[col_name]] <- ifelse(data$Time == t, 1, 0)
}

# Interacciones Time_t * Home
for (t in -6:6) {
  time_col <- paste0("Time_", ifelse(t < 0, paste0("minus", abs(t)), t))
  interaction_col <- paste0("Interaction_", ifelse(t < 0, paste0("minus", abs(t)), t))
  data[[interaction_col]] <- data[[time_col]] * data$Home
}

# Especificar el modelo
formula <- as.formula(
  paste(
    "Crimes ~ Home +", 
    paste0("Time_", c(paste0("minus", abs(-6:-1)), 0, 1:6), collapse = " + "), " + ",
    paste0("Interaction_", c(paste0("minus", abs(-6:-1)), 0, 1:6), collapse = " + ")
  )
)

# Ajustar el modelo
model <- lm(formula, data = data)

# Resumen del modelo
summary(model)



# Generate 20 example timestamps
set.seed(123)
timestamps <- as.POSIXct("2024-01-01 12:00:00") + runif(100, min = -6.5*60*60, max = 6.5*60*60)

# Reference time
reference_time <- as.POSIXct("2024-01-01 12:00:00")  # Reference for t = 0

# Calculate differences in 30-minute units
minutes_difference <- as.numeric(difftime(timestamps, reference_time, units = "mins"))
interval_difference <- minutes_difference / 30  # Convert to 30-minute intervals

# Correct definition of breaks and labels
breaks <- c(-7:6)  # 14 boundaries
labels <- as.character(-6:6)  # 13 labels (must match the number of intervals)

# Classify into [-6, ..., 0, ..., 6]
intervals <- cut(
  interval_difference,
  breaks = breaks,             # Corrected breaks
  labels = labels,             # Corrected labels
  #include.lowest = TRUE,       # Include the lowest boundary
  right = FALSE                # Left-closed intervals
)

# Combine into a data frame
result <- data.frame(
  Timestamp = timestamps,
  Minutes_Difference = minutes_difference,
  Interval = intervals
)


cut()
# Print the result
print(result)
