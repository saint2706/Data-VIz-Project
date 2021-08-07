# Read the Dataset
data <- read.csv(r"(Datasets\forestfires.csv)")

# Store value of wind speeds
winds <- as.double(table(data$wind))

# Create intervals
intervals <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, Inf)

# Sort data into intervals
data$intervaled <- cut(data$wind, intervals, labels = FALSE)

# Save the intervaled data
values <- as.double(unname(table(data$intervaled)))

# Create labels
labels <- c("0 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 5", "5 - 6"
            , "6 - 7", "7 - 8", "8 - 9", "9 - 10")

# Assign colors
colors <- c("#e6194B", "#3cb44b", "#ffe119", "#4363d8", "#f58231",
            "#42d4f4", "#f032e6", "#fabed4", "#469990", "#dcbeff")

# Start PNG device driver
png(filename = r"(InitialProject\PieWind.png)", height = 720, width = 720)

# Calculate percentage of intervals
percents <- paste(round(values / sum(values) * 100, 1), "%", sep = "")

# Plot pie chart
pie(values, main = "Wind Speeds in Km/H", col = colors
    , labels = percents, cex = 1.3, radius = 1)

# Make Legend
legend("topright", labels, fill = colors, cex = 0.95)

# Close device driver to flush output to PNG
dev.off()


## Conclusion
# Most forest fires have a wind speed between 3 to 4 Km/H