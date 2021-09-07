# Read Data Values from CSV file
data <- read.csv(r"(Datasets\forestfires.csv)")

# Function to Ciel to nearest 10
round_up_10 <- function(x) {
    round(x + 5, -1)
}

# Intialize Vectors for Axis Labelling
months <- c("jan", "feb", "mar", "apr", "may", "jun",
            "jul", "aug", "sep", "oct", "nov", "dec")
days <- c("fri", "mon", "sat", "sun", "thu", "tue", "wed")

# Calculate and Store Final Data
to_count_frequency <- data$month
frequencies <- as.double(unname(table(to_count_frequency)))
max_y <- round_up_10(max(frequencies))

# Calculate and Store Final Data
to_count_frequency_day <- data$day
frequencies_day <- as.double(unname(table(to_count_frequency_day)))
max_y_day <- round_up_10(max(frequencies_day))

# Start PNG device driver to save output
png(filename = r"(Datasets\forestfires.csv)", bg = "white")

# Plot frequencies with y limits from 0 to max_y
# turn off default axes labels
plot(frequencies, type = "o", col = "blue", ylim = c(0, max_y),
    axes = FALSE, ann = FALSE)

# Set Custom Labels for x axis
axis(1, at = 1:12, lab = months)

# Make y axis with horizontal labels that display ticks at
# every 5 marks.
axis(2, las = 1, at = 10 * 0:max_y)

# Create Box around plot
box()

# Create a title with a red, bold/italic font
title(main = "Month Wise Frequency of Forest Fires",
        col.main = "red", font.main = 4)

# Label the x and y axes with dark green text
title(xlab = "Months", col.lab = rgb(0, 0.5, 0))
title(ylab = "Frequency of Forest Fires", col.lab = rgb(0, 0.5, 0))

# Turn off device driver (to flush output to png)
dev.off()

# Start PNG device driver to save output
png(filename = r"(InitialProject\line_day.png)", bg = "white")

# Plot frequencies with y limits from 0 to max_y
# turn off default axes labels
plot(frequencies_day, type = "o", col = "blue", ylim = c(0, max_y_day),
    axes = FALSE, ann = FALSE)

# Set Custom Labels for x axis
axis(1, at = 1:7, lab = days)

# Make y axis with horizontal labels that display ticks at
# every 5 marks.
axis(2, las = 1, at = 5 * 0:max_y_day)

# Create Box around plot
box()

# Create a title with a red, bold/italic font
title(main = "Day Wise Frequency of Forest Fires",
        col.main = "red", font.main = 4)

# Label the x and y axes with dark green text
title(xlab = "Days", col.lab = rgb(0, 0.5, 0))
title(ylab = "Frequency of Forest Fires", col.lab = rgb(0, 0.5, 0))

# Turn off device driver (to flush output to png)
dev.off()

## Conclusions
# Most forest fires happen in the month of February
# Most forest fires happen on a Sunday
# Statistically, Forest fires have a higher chance of occuring on a
# Sunday in February