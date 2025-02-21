install.packages("dplyr")
install.packages("moments")

library(dplyr)
library(moments)
library(ggplot2)

getwd()
setwd("/Users/feliziano/RWorkspace")
data <- read.csv("realestate_texas.csv",sep=",")
# Carica il dataset
# Statistiche descrittive per variabili numeriche
library(dplyr)
library(moments)


# Calcolo delle statistiche descrittive
summary_stats <- data %>%
  summarise(across(where(is.numeric), list(
    mean = ~ mean(., na.rm = TRUE),
    median = ~ median(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    skewness = ~ skewness(., na.rm = TRUE),
    kurtosis = ~ kurtosis(., na.rm = TRUE)
  )))

# Visualizza il risultato
print(summary_stats)
  
  # Distribuzione di frequenza per variabili categoriche
  table(data$city)
  table(data$year)
  table(data$month)
  # Creazione di classi per median_price
  

  breaks <- round(seq(0,max(data$median_price,na.rm = TRUE),length.out=6))
  
  
data <- data %>%
  mutate(
  median_price_class = cut(median_price, breaks = breaks,dig.lab = 10)
)

#distribuzione di frequenze median_price
freq_price_class <- table(data$median_price_class)

ggplot(data,aes(x = median_price_class)) + 
  geom_bar()

install.packages("DescTools")
library(DescTools)

gini_index <- Gini(freq_price_class)
print(gini_index)

# Probabilità che una riga sia "Beaumont"
prob_beaumont <- mean(data$city == "Beaumont")

# Probabilità che una riga sia "Luglio"
prob_july <- mean(data$month == 7)

# Probabilità che una riga sia "Dicembre 2012"
prob_dec_2012 <- mean(data$month == 12 & data$year == 2012)

print(c(prob_beaumont, prob_july, prob_dec_2012))

data <- data %>%
  mutate(
    avg_price = volume / sales, # Prezzo medio
    listing_effectiveness = sales / listings # Efficacia degli annunci
  )

# Statistiche condizionate per città
city_stats <- data %>%
  group_by(city) %>%
  summarise(
    mean_sales = mean(sales, na.rm = TRUE),
    sd_sales = sd(sales, na.rm = TRUE)
  )

print(city_stats)

# Grafico delle vendite medie per città
ggplot(city_stats, aes(x = city, y = mean_sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Vendite medie per città", x = "Città", y = "Vendite medie")
