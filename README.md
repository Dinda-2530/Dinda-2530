- 👋 Hi, I’m Dinda Kusuma Sari

#Read in your Data 
DataUTS = read.csv("C:/Users/risia/Downloads/Dinda/Employee Attrition.csv", sep = ";", header = TRUE)
DataUTS = data.frame(SatisfactionLevel = DataUTS$satisfaction_level, 
                            AverageMonthlyHours = DataUTS$average_montly_hours)
print(DataUTS)

#Check the Packaging
nrow(DataUTS)
ncol(DataUTS)
str(DataUTS)

#Look at the top and the bottom of your data
head(DataUTS[, c(1, 2)])
tail(DataUTS[, c(1, 2)])

#Check your “n”s
head = table(DataUTS$SatisfactionLevel)
head(head, n = 5)
head = table(DataUTS$AverageMonthlyHours)
head(head, n = 5)

library(dplyr)
DataUTS_filtered <- DataUTS %>%
  filter(SatisfactionLevel > 0.5 & AverageMonthlyHours == 150) %>%
  select(SatisfactionLevel, AverageMonthlyHours)
as.data.frame(DataUTS_filtered)

#Validate with at least one external data source
summary(DataUTS)
quantile(DataUTS$SatisfactionLevel, seq(0, 1, 0.1), na.rm = TRUE)
quantile(DataUTS$AverageMonthlyHours, seq(0, 1, 0.1), na.rm = TRUE)

#Make a plot (gunakan package ggplot2)
library(ggplot2)
ggplot(DataUTS, aes(x = factor(AverageMonthlyHours), y = SatisfactionLevel)) +
  geom_boxplot(fill = "red", color = "pink", outlier.color = "skyblue") +
  labs(
    title = "Boxplot of Satisfaction Level by Average Monthly Hours",
    x = "Average Monthly Hours (Grouped)",
    y = "Satisfaction Level"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

  #Try the easy solution first
correlation <- cor(DataUTS$SatisfactionLevel, DataUTS$AverageMonthlyHours, use = "complete.obs")
correlation
model <- lm(DataUTS$SatisfactionLevel ~ DataUTS$AverageMonthlyHours, data = DataUTS)
summary(model)

library(ggplot2)
ggplot(DataUTS, aes(x = AverageMonthlyHours, y = SatisfactionLevel)) +
  geom_point(color = "lightblue") +  
  geom_smooth(method = "lm", color = "red", se = FALSE) +  
  labs(
    title = "Hubungan antara Rata-rata Jam Kerja per Bulan dan Tingkat Kepuasan",
    x = "Rata-rata Jam Kerja per Bulan",
    y = "Tingkat Kepuasan"
  )

  #Model as Expectations
#Satisfication Level
sd_satisfaction <- sd(DataUTS$SatisfactionLevel, na.rm = TRUE)
cat("Standar deviasi dari SatisfactionLevel adalah:", sd_satisfaction, "\n")
prob <- pnorm(30, mean = mean(0.61), sd = sd(0.24), lower.tail = FALSE)
cat("Probabilitas nilai 30 berada di atas rata-rata distribusi:", prob, "\n")

#Average Monthly Hours
sd_average <- sd(DataUTS$AverageMonthlyHours, na.rm = TRUE)
cat("Standar deviasi dari AverageMonthlyHours adalah:", sd_average, "\n")
prob <- pnorm(30, mean = mean(201), sd = sd(0.24), lower.tail = FALSE)
cat("Probabilitas nilai 30 berada di atas rata-rata distribusi:", prob, "\n")

#Comparing Model Expectations to Reality
library(ggplot2)

# Histogram variabel dependen (SatisfactionLevel)
ggplot(DataUTS, aes(x = SatisfactionLevel)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black") +
  labs(
    title = "Histogram Tingkat Kepuasan Karyawan",
    x = "Tingkat Kepuasan",
    y = "Frekuensi"
  )

# Membuat distribusi normal dengan parameter mean dan sd yang sama dengan data asli
mean_value <- mean(DataUTS$SatisfactionLevel, na.rm = TRUE)
sd_value <- sd(DataUTS$SatisfactionLevel, na.rm = TRUE)
normal_data <- rnorm(1000, mean = mean_value, sd = sd_value)

# Histogram distribusi normal
ggplot(data.frame(normal_data), aes(x = normal_data)) +
  geom_histogram(binwidth = 0.05, fill = "lightgreen", color = "black") +
  labs(
    title = "Histogram Distribusi Normal",
    x = "Tingkat Kepuasan (Distribusi Normal)",
    y = "Frekuensi"
  )
