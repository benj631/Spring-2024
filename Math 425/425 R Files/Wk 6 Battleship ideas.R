# Battleship Ideas

# Define the linear dimension
x <- seq(0, 10, length.out = 1000)

# Define the sine function to determine point presence
sin_intensity <- sin(x)  # Adjust amplitude and frequency as needed

# Scale the intensity to range between 0 and 1
scaled_intensity <- (sin_intensity - min(sin_intensity)) / (max(sin_intensity) - min(sin_intensity))

# Create a data frame
data <- data.frame(x = x, scaled_intensity = scaled_intensity)

# Plot the intensity
ggplot(data, aes(x = x, y = scaled_intensity)) +
  geom_line(color = "blue") +
  labs(x = "Linear Dimension", y = "Intensity", title = "Presence of Points Determined by Sine Function")

