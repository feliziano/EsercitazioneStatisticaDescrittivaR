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

head(data)
# Descrizione delle variabili
cat("Il dataset contiene variabili numeriche (sales, volume, median_price, listings, months_inventory) e categoriche (city, year, month).\n")

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

head(data)

# Distribuzione di frequenza
freq_table <- table(data$median_price_class)
print(freq_table)

# Grafico a barre
ggplot(data, aes(x = median_price_class)) +
  geom_bar() +
  labs(title = "Distribuzione del prezzo mediano", x = "Classi di prezzo", y = "Frequenza")


install.packages("DescTools")
library(DescTools)

# Indice di Gini
gini_index <- Gini(freq_table)
cat("L'indice di Gini per le classi di prezzo mediano è:", gini_index, "\n")

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

head(data)

# Dividere il dataset in liste di matrici per ogni città
matrici_per_citta <- split(data[, c("listing_effectiveness","month","year")], data$city)



# Visualizzare la matrice della città "Beaumont" come esempio
beaumont<-matrici_per_citta[["Beaumont"]]

bryan_cllg_stat<-matrici_per_citta[["Bryan-College Station"]]

tyler<-matrici_per_citta[["Tyler"]]

wichita_falls<-matrici_per_citta[["Wichita Falls"]]

ggplot(beaumont,aes(x=year,y=listing_effectiveness))+
  geom_bar(stat = "identity",fill="red")+
  labs(title = "Efficacia degli Annunci negli anni, Beaumont",x="Anno",y="Rapporto Annunci/Vendite")
  
ggplot(beaumont,aes(x=month,y=listing_effectiveness))+
  geom_bar(stat = "identity",fill="red")+
  labs(title = "Efficacia degli Annunci nei mesi, Bryan",x="Anno",y="Rapporto Annunci/Vendite")

ggplot(bryan_cllg_stat,aes(x=year,y=listing_effectiveness))+
  geom_bar(stat = "identity",fill="green")+
  labs(title = "Efficacia degli Annunci negli anni, Bryan College State",x="Anno",y="Rapporto Annunci/Vendite")

ggplot(bryan_cllg_stat,aes(x=month,y=listing_effectiveness))+
  geom_bar(stat = "identity",fill="green")+
  labs(title = "Efficacia degli Annunci nei mesi, Bryan College State",x="Anno",y="Rapporto Annunci/Vendite")

ggplot(tyler,aes(x=year,y=listing_effectiveness))+
  geom_bar(stat = "identity",fill="blue")+
  labs(title = "Efficacia degli Annunci negli anni, Tyler",x="Anno",y="Rapporto Annunci/Vendite")

ggplot(tyler,aes(x=month,y=listing_effectiveness))+
  geom_bar(stat = "identity",fill="blue")+
  labs(title = "Efficacia degli Annunci nei mesi, Tyler",x="Anno",y="Rapporto Annunci/Vendite")

ggplot(wichita_falls,aes(x=year,y=listing_effectiveness))+
  geom_bar(stat = "identity",fill="purple")+
  labs(title = "Efficacia degli Annunci negli anni, Wichica Falls",x="Anno",y="Rapporto Annunci/Vendite")

ggplot(wichita_falls,aes(x=month,y=listing_effectiveness))+
  geom_bar(stat = "identity",fill="purple")+
  labs(title = "Efficacia degli Annunci nei mesi, Wichica Falls",x="Anno",y="Rapporto Annunci/Vendite")

# Statistiche condizionate per città
city_stats <- data %>%
  group_by(city) %>%
  summarise(
    mean_sales = mean(sales, na.rm = TRUE),
    sd_sales = sd(sales, na.rm = TRUE)
  )

print(city_stats)

# Grafico delle vendite medie per città
ggplot(city_stats, aes(x = city, y = mean_sales,fill = city)) +
  geom_bar(stat = "identity") +
  labs(title = "Vendite medie per città", x = "Città", y = "Vendite medie")

# Boxplot per prezzo mediano tra città
ggplot(data, aes(x = city, y = median_price,fill=city)) +
  geom_boxplot() +
  labs(title = "Distribuzione del prezzo mediano per città", x = "Città", y = "Prezzo mediano")

# Grafico a barre per vendite totali per mese e città
ggplot(data, aes(x = month, y = sales, fill = city)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Vendite totali per mese e città", x = "Mese", y = "Vendite totali")

# Assumendo che 'year' e 'month' siano numerici
data <- data %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d"))

head(data)
ggplot(data, aes(x = date, y = sales, color = city)) +
  geom_line() +
  labs(title = "Andamento storico delle vendite", x = "Data", y = "Vendite") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Line chart per andamento storico delle vendite per anno
ggplot(data, aes(x = year, y = sales, fill = city)) +
  geom_bar(stat = "identity",position="dodge") +
  labs(title = "Andamento storico delle vendite", x = "Tempo", y = "Vendite")
# Line chart per andamento storico delle vendite per mese
ggplot(data, aes(x = month, y = sales, color = city)) +
  geom_bar(stat = "identity") +
  labs(title = "Andamento storico delle vendite", x = "Tempo", y = "Vendite")
# Calcolo della media delle vendite per ogni combinazione di city, year, month e listings_class
nuovo_dataset <- data %>%
  group_by(city, year, month, listings_class) %>%
  summarise(mean_sales = mean(sales, na.rm = TRUE)) %>%
  ungroup()

print(nuovo_dataset)

ggplot(data, aes(x = listings, y = sales, color = city)) +
  geom_point(size = 3, alpha = 0.7) +  # Punti semi-trasparenti
  geom_smooth(method = "lm", se = FALSE) +  # Linea di regressione per ogni città
  labs(title = "Relazione tra Listings e Sales per Città",
       x = "Listings",
       y = "Sales",
       color = "Città") +
  theme_minimal()

mean_sales_city_month <- data %>%
  group_by(city,month) %>%
  summarize(media_sales=mean(sales,na.rm=TRUE))

mean_sales_city_month
print(mean_sales_city_month,n=60)


ggplot(mean_sales_city_month, aes(x = factor(month), y = media_sales, fill = city)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Media delle vendite per città e mese",
       x = "Mese",
       y = "Media vendite",
       fill = "Città") +
  theme_minimal()










