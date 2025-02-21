getwd()
setwd("/Users/feliziano/RWorkspace")
dati <- read.csv("realestate_texas.csv",sep=",")
dim(dati)
N=dim(dati)[1]

install.packages("ggplot2")

#distribuzione di frequenze
table(dati["city"])
freq_ass <- table(dati$city)
freq_rel <- table(dati$city)/N
distr_freq_city<-cbind(freq_ass,freq_rel)
distr_freq_city


freq_ass_year <- table(dati$year)
freq_rel_year <- table(dati$year)/N
distr_freq_year<-cbind(freq_ass_year,freq_rel_year)
distr_freq_year

freq_ass_month_year <- table(dati$month,dati$year)
freq_ass_month_year
freq_rel_month_year <- freq_ass_month_year/sum(freq_ass_month_year)
freq_rel_month_year

cat("probabilita che la riga estratta casualmente riporti dicembre 2012:", freq_rel_month_year["12","2012"]*100, "%")
cat("Probabilità che la riga estratta casualmente riporti dicembre 2012:", 
    ceiling(freq_rel_month_year["12","2012"] * 10000) / 100, "%")

sum(freq_rel_month_year)
freq_rel_city_beaumont <- distr_freq_city["Beaumont","freq_rel"]
prob_city_beaumont <- freq_rel_city_beaumont*100

cat("Probabilita di estrarre una riga del dataset della citta di Beaumont = ",prob_city_beaumont,"%")

sales_city <- table(dati$city,dati$sales)
sales_city_relative <- sales_city/N

cat("Frequenze assolute di vendite per citta")
sales_city

summary(dati)

head(dati)

install.packages("dplyr")

library("dplyr")

# Calcolo degli indici raggruppati per city
risultati <- dati%>%
  group_by(city) %>%
  summarise(
    media_sales = mean(sales, na.rm = TRUE),          # Media delle vendite
    mediana_sales = median(sales, na.rm = TRUE),      # Mediana delle vendite
    deviazione_standard_sales = sd(sales, na.rm = TRUE), # Deviazione standard delle vendite
    media_volume = mean(volume, na.rm = TRUE),        # Media del volume
    mediana_volume = median(volume, na.rm = TRUE),    # Mediana del volume
    media_median_price = mean(median_price, na.rm = TRUE), # Media del prezzo mediano
    mediana_median_price = median(median_price, na.rm = TRUE), # Mediana del prezzo mediano
    media_listings = mean(listings, na.rm = TRUE),    # Media delle inserzioni
    mediana_listings = median(listings, na.rm = TRUE), # Mediana delle inserzioni
    media_months_inventory = mean(months_inventory, na.rm = TRUE), # Media dei mesi di inventario
    mediana_months_inventory = median(months_inventory, na.rm = TRUE) # Mediana dei mesi di inventario
  )

# Visualizzazione dei risultati
print(risultati)

install.packages("moments")

library("moments")

# Calcolo degli indici raggruppati per city
risultati <- dati %>%
  group_by(city) %>%
  summarise(
    # Indici di posizione
    media_sales = mean(sales, na.rm = TRUE),
    mediana_sales = median(sales, na.rm = TRUE),
    media_volume = mean(volume, na.rm = TRUE),
    mediana_volume = median(volume, na.rm = TRUE),
    media_median_price = mean(median_price, na.rm = TRUE),
    mediana_median_price = median(median_price, na.rm = TRUE),
    media_listings = mean(listings, na.rm = TRUE),
    mediana_listings = median(listings, na.rm = TRUE),
    media_months_inventory = mean(months_inventory, na.rm = TRUE),
    mediana_months_inventory = median(months_inventory, na.rm = TRUE),
    
    # Indici di variabilità
    deviazione_standard_sales = sd(sales, na.rm = TRUE),
    varianza_sales = var(sales, na.rm = TRUE),
    range_sales = max(sales, na.rm = TRUE) - min(sales, na.rm = TRUE),
    deviazione_standard_volume = sd(volume, na.rm = TRUE),
    varianza_volume = var(volume, na.rm = TRUE),
    range_volume = max(volume, na.rm = TRUE) - min(volume, na.rm = TRUE),
    deviazione_standard_median_price = sd(median_price, na.rm = TRUE),
    varianza_median_price = var(median_price, na.rm = TRUE),
    range_median_price = max(median_price, na.rm = TRUE) - min(median_price, na.rm = TRUE),
    deviazione_standard_listings = sd(listings, na.rm = TRUE),
    varianza_listings = var(listings, na.rm = TRUE),
    range_listings = max(listings, na.rm = TRUE) - min(listings, na.rm = TRUE),
    deviazione_standard_months_inventory = sd(months_inventory, na.rm = TRUE),
    varianza_months_inventory = var(months_inventory, na.rm = TRUE),
    range_months_inventory = max(months_inventory, na.rm = TRUE) - min(months_inventory, na.rm = TRUE),
    
    # Indici di forma
    asimmetria_sales = skewness(sales, na.rm = TRUE),
    curtosi_sales = kurtosis(sales, na.rm = TRUE),
    asimmetria_volume = skewness(volume, na.rm = TRUE),
    curtosi_volume = kurtosis(volume, na.rm = TRUE),
    asimmetria_median_price = skewness(median_price, na.rm = TRUE),
    curtosi_median_price = kurtosis(median_price, na.rm = TRUE),
    asimmetria_listings = skewness(listings, na.rm = TRUE),
    curtosi_listings = kurtosis(listings, na.rm = TRUE),
    asimmetria_months_inventory = skewness(months_inventory, na.rm = TRUE),
    curtosi_months_inventory = kurtosis(months_inventory, na.rm = TRUE)
  )

# Visualizzazione dei risultati
print(risultati)

library(ggplot2)

# Grafico a barre per la media delle vendite
ggplot(risultati, aes(x = city, y = media_sales, fill = city)) +
  geom_bar(stat = "identity") +
  labs(title = "Media delle vendite per città",
       x = "Città",
       y = "Media delle vendite") +
  theme_minimal()

#Grafico a dispersione per le vendite rispetto al numero di annunci attivi
ggplot(dati, aes(x = sales,y = listings))+
  geom_point(size = 3)+
  labs(title = "Numero di vendite vs Annunci attivi",
       x = "Annunci attivi",
       y = "Vendite")+
  theme_light()



ggplot(risultati, aes(x = media_listings, y = media_sales)) +
  geom_point(color = "blue", size = 3) +  # Punti di colore blu
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Regressione lineare con intervallo di confidenza
  labs(title = "Relazione tra Media delle Vendite e Listings",
       x = "Media Listings",
       y = "Media Sales") +
  theme_minimal()



ggplot(dati, aes(x = listings, y = sales, color = median_price)) +
  geom_point(size = 3) +  # Punti colorati in base a median_price
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Linea di regressione
  labs(title = "Relazione tra Vendite e Listings",
       x = "Listings",
       y = "Sales",
       color = "Median Price") +  # Etichetta della legenda
  theme_minimal()



# Creare classi di prezzo
library("moments")
dati <- dati %>%
  mutate(price_category = cut(median_price, 
                              breaks = quantile(median_price, probs = seq(0, 1, 0.25), na.rm = TRUE), 
                              include.lowest = TRUE, 
                              labels = c("Basso", "Medio", "Alto", "Molto Alto")))

# Grafico con colori distinti per classi di prezzo
ggplot(dati, aes(x = listings, y = sales, color = price_category)) +
  geom_point(size = 3) +  
  geom_smooth(method = "lm", se = TRUE, color = "black") +  
  labs(title = "Relazione tra Vendite e Listings per Categoria di Prezzo",
       x = "Listings",
       y = "Sales",
       color = "Categoria Prezzo") +
  theme_minimal()

ggplot(dati, aes(x = listings, y = sales, color = city)) +
  geom_point(size = 3, alpha = 0.7) +  # Punti semi-trasparenti
  geom_smooth(method = "lm", se = FALSE) +  # Linea di regressione per ogni città
  labs(title = "Relazione tra Listings e Sales per Città",
       x = "Listings",
       y = "Sales",
       color = "Città") +
  theme_minimal()

str(risultati)
head(dati)

# Dividere il dataset in liste di matrici per ogni città
matrici_per_citta <- split(dati[, c("listings", "sales","month")], dati$city)



# Visualizzare la matrice della città "Beaumont" come esempio
listings_sales_beaumont<-matrici_per_citta[["Beaumont"]]

#range listings citta 
listings_sales_beaumont_range <- max(listings_sales_beaumont$listings)-min(listings_sales_beaumont$listings)

listings_sales_beaumont_range

listings_sales_bryan_cllg_stat<-matrici_per_citta[["Bryan-College Station"]]

listings_sales_tyler<-matrici_per_citta[["Tyler"]]

listings_sales_wichita_falls<-matrici_per_citta[["Wichita Falls"]]

#ggplot(listings_sales_beaumont, aes(x = sales)) +
 # geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") 


# ggplot(listings_sales_beaumont, aes(x = sales)) +
#   geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
#   stat_function(fun = dnorm, args = list(mean = mean(dati$sales, na.rm = TRUE), 
#                                          sd = sd(dati$sales, na.rm = TRUE)), 
#                 color = "red", size = 1) +
#   labs(title = "Distribuzione di Sales con Curva Normale Sovrapposta",
#        x = "Sales",
#        y = "Densità") +
#   theme_minimal()


mean_sales_city_month <- dati %>%
  group_by(city,month) %>%
  summarize(media_sales=mean(sales,na.rm=TRUE))

mean_sales_city_month
print(mean_sales_city_month,n=60)

ggplot(mean_sales_city_month,aes(x=month,y=media_sales,fill = city))+
  geom_histogram()


ggplot(mean_sales_city_month, aes(x = factor(month), y = media_sales, fill = city)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Media delle vendite per città e mese",
       x = "Mese",
       y = "Media vendite",
       fill = "Città") +
  theme_minimal()


ggplot(dati, aes(x = listings, y = sales)) +
  geom_point(aes(color = city), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(city ~ year) +  # Griglia con città nelle righe e anni nelle colonne
  labs(title = "Relazione tra Listings e Sales per Città e Anno",
       x = "Listings",
       y = "Sales",
       color = "Città") +
  theme_minimal()


dati_standardizzati <- dati %>%
  mutate(listings_scaled = scale(listings),
         sales_scaled = scale(sales))

# Grafico con i dati standardizzati
ggplot(dati_standardizzati, aes(x = listings_scaled, y = sales_scaled)) +
  geom_point(aes(color = city), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(city ~ year) +  # Griglia con città nelle righe e anni nelle colonne
  labs(title = "Relazione tra Listings e Sales (Standardizzati) per Città e Anno",
       x = "Listings (Standardizzati)",
       y = "Sales (Standardizzati)",
       color = "Città") +
  theme_minimal()

# Normalizza sales e listings nell'intervallo [0, 1]
dati_normalizzati <- dati %>%
  mutate(
    listings_norm = (listings - min(listings)) / (max(listings) - min(listings)),
    sales_norm = (sales - min(sales)) / (max(sales) - min(sales))
  )

# Crea il grafico con listings e sales normalizzati
ggplot(dati_normalizzati, aes(x = listings_norm, y = sales_norm)) +
  geom_point(aes(color = city), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(city ~ year) +  # Griglia con città nelle righe e anni nelle colonne
  labs(title = "Relazione tra Listings e Sales (Normalizzati) per Città e Anno",
       x = "Listings (Normalizzato)",
       y = "Sales (Normalizzato)",
       color = "Città") +
  theme_minimal()
  
head(dati,n=80)

# Creazione dei break points arrotondati
breaks <- round(seq(min(dati$listings, na.rm = TRUE), 
                    max(dati$listings, na.rm = TRUE), 
                    length.out = 21))

# Creazione delle classi senza virgole
dati <- dati %>%
  mutate(listings_class = cut(listings, 
                              breaks = breaks, 
                              include.lowest = TRUE, 
                              dig.lab = 10))

# Calcolo della media delle vendite per ogni combinazione di city, year, month e listings_class
nuovo_dataset <- dati %>%
  group_by(city, year, month, listings_class) %>%
  summarise(mean_sales = mean(sales, na.rm = TRUE)) %>%
  ungroup()

# Visualizzazione del nuovo dataset
print(nuovo_dataset)

ggplot(data = nuovo_dataset,aes(x=listings_class,y=mean_sales,fill = city))+
  geom_bar(stat="identity",position = "dodge")+
  labs(title="Media delle vendite per citta e annunci attivi",
       x="Annunci attivi(Intervalli)",
       y="Media di vendite")+
  theme_minimal()

