data <- read.csv(r"(Datasets\forestfires.csv)")
winds <- as.double(table(data$wind))
max(data$wind)
intervals <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, Inf)
data$intervaled <- cut(data$wind, intervals, labels = FALSE)
table(data$intervaled)