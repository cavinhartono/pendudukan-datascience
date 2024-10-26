library(ggplot2)
library(magrittr)
library(dplyr)
Samples = read.table("kependudukan_desa.csv", header = TRUE, sep = ",")

# Jenis Kelamin
ggplot(Samples, aes(x = Jenis.Kelamin)) +
  geom_bar(fill = "steelblue") + 
  labs(title = "Gender Distribution", x = "Jenis Kelamin", y = "Jumlah") +
  theme_minimal()

# Umur
ggplot(Samples, aes(x = Usia)) + 
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  labs(title = "Age Distribution", x = "Umur", y = "Jumlah") + 
  theme_minimal()

ggplot(Samples, aes(x = Usia)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 5, fill = "orange", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Age Distribution", x = "Age", y = "Percentage") +
  theme_minimal()

# Status Pernikahan
Samples$Status.Perkawinan <- as.factor(Samples$Status.Perkawinan)
status_count <- Samples %>% 
  group_by(Status.Perkawinan) %>% 
  summarise(count = n()) %>% 
  mutate(percentage = count / sum(count) * 100)

ggplot(status_count, aes(x = "", y = percentage, fill = Status.Perkawinan)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Rata-rata Pernikahan", x = "", y = "") +
  theme_void() +
  scale_fill_brewer(palette = "Set2")

# Tingkat Pendidikan
ggplot(Samples, aes(x = Pendidikan)) +
  geom_bar(fill = "purple") +
  geom_text(stat = 'count', aes(label = paste(round((..count../sum(..count..))*100, 1), "%", sep = "")), vjust = 1.5, color = "white") + 
  labs(title = "Education Level Distribution", x = "Tingkat Pendidikan", y = "Jumlah") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pekerjaan

# Rata-rata detail
ggplot(Samples, aes(x = Pekerjaan)) +
  geom_bar(fill = "green") +
  labs(title = "Rata-rata Pekerjaan", x = "Pekerjaan", y = "Jumlah") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Rata-rata singkat
occupation_summary <- Samples %>%
  group_by(Pekerjaan) %>%
  summarise(count = n()) %>%
  mutate(percentage = round((count / sum(count)) * 100, 1)) %>%
  arrange(desc(count))

occupation_summary_grouped <- occupation_summary %>%
  group_by(count, percentage) %>%
  summarise(occupations = paste(Pekerjaan, collapse = ", ")) %>%
  ungroup()

ggplot(occupation_summary_grouped, aes(x = reorder(occupations, count), y = count)) +
  geom_bar(stat = "identity", fill = "green") +
  geom_text(aes(label = paste(count, "(", percentage, "%)", sep = "")), vjust = -0.5) +
  labs(title = "Rata-rata Pekerjaan", x = "Pekerjaan", y = "Jumlah") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(occupation_summary_grouped, aes(x = reorder(occupations, count), y = count)) +
  geom_bar(stat = "identity", fill = "green") +
  geom_text(aes(label = paste(percentage, "%", sep = "")), vjust = 1.5, color = "white", size = 4) +
  labs(title = "Rata-rata Pekerjaan", x = "Pekerjaan", y = "Jumlah") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Rasio Penduduk Produktif
productive_population <- Samples %>%
  filter(Usia >= 15 & Usia <= 64) %>%
  summarise(count_productive = n())

non_productive_population <- Samples %>%
  filter(Usia < 15 | Usia > 64) %>%
  summarise(count_non_productive = n())

total_population <- nrow(Samples)

productive_percentage <- (productive_population$count_productive / total_population) * 100
non_productive_percentage <- (non_productive_population$count_non_productive / total_population) * 100

population_data <- data.frame(
  Group = c("Usia Produktif (15-64 tahun)", "Usia Non-Produktif (< 15 atau > 64 tahun)"),
  Count = c(productive_population$count_productive, non_productive_population$count_non_productive),
  Percentage = c(round(productive_percentage, 1), round(non_productive_percentage, 1))
)

ggplot(population_data, aes(x = "", y = Count, fill = Group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste(Count, " (", Percentage, "%)", sep = "")), 
            position = position_stack(vjust = 0.5), size = 5, color = "white") +  # Display count and percentage
  labs(title = "Distribusi Penduduk Usia Produktif dan Non-Produktif") +
  theme_void()
