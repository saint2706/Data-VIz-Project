data <- read.csv("forestfires.csv")
months <- c("jan", "feb", "mar", "apr", "may", "jun",
            "jul", "aug", "sep", "oct", "nov", "dec")
to_count_frequency <- data$month
frequencies <- as.double(unname(table(to_count_frequency)))
max_y <- max(frequencies)
png(filename = "line.png", bg = "white")
plot(frequencies, type = "o", col = "blue", ylim = c(0, max_y),
    axes = FALSE, ann = FALSE)
axis(1, at = 1:12, lab = months)
axis(2, las = 1, at = 5 * 0:max_y)
box()
title(main = "Month Wise Frequency of Forest Fires",
        col.main = "red", font.main = 4)
title(xlab = "Months", col.lab = rgb(0, 0.5, 0))
title(ylab = "Frequency of Forest Fires", col.lab = rgb(0, 0.5, 0))
dev.off()
png(filename = "line_day.png", bg = "white")
to_count_frequency <- data$day
frequencies <- as.double(unname(table(to_count_frequency)))
max_y <- max(frequencies)
days <- c("fri", "mon", "sat", "sun", "thu", "tue", "wed")
plot(frequencies, type = "o", col = "blue", ylim = c(0, max_y),
    axes = FALSE, ann = FALSE)
axis(1, at = 1:7, lab = days)
axis(2, las = 1, at = 5 * 0:max_y)
box()
title(main = "Day Wise Frequency of Forest Fires",
        col.main = "red", font.main = 4)
title(xlab = "Days", col.lab = rgb(0, 0.5, 0))
title(ylab = "Frequency of Forest Fires", col.lab = rgb(0, 0.5, 0))
dev.off()