# Caricamento dei pacchetti necessari
# ggplot2 serve per creare grafici; dplyr serve per manipolare i dati; readr serve per leggere i file CSV


library(ggplot2)
library(dplyr)


getwd()

setwd("/Users/felizianocopponi/RWorkspace")
getwd()

dati <- read.csv("realestate_texas.csv")



# 1. Lettura del dataset
# Leggiamo i dati dal file CSV. Sostituisci "Real Estate Texas.csv" con il percorso corretto del tuo file.

data <- read.csv("realestate_texas.csv")

# Controlliamo i primi record del dataset per capire come sono strutturati i dati
head(data)

# 2. Conversione delle variabili (opzionale ma utile)
# Convertiamo alcune colonne in tipi più appropriati:
# - `year` in numerico per analisi temporali
# - `month` in fattore (categorico ordinato) per cicli stagionali
# - `city` in fattore per identificare chiaramente le città
data <- data %>%
  mutate(
    year = as.integer(year),
    month = as.factor(month),
    city = as.factor(city)
  )

data

# 3. Analisi dei trend storici delle vendite
# Raggruppiamo i dati per anno e mese e sommiamo le vendite totali
sales_trend_grouped_by <- data %>%
  group_by(year, month)

sales_trend_grouped_by

sales_trend <- data %>%
  group_by(year, month) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE))

# Creiamo un grafico a linee per mostrare il trend delle vendite nel tempo
ggplot(sales_trend, aes(x = as.numeric(paste(year, as.numeric(month), sep = ".")), 
                        y = total_sales)) +
  geom_line(color = "blue") +  # Linea blu per rappresentare le vendite
  labs(
    title = "Trend Storico delle Vendite Immobiliari",  # Titolo del grafico
    x = "Anno (frazionato in mesi)",  # Etichetta asse X
    y = "Numero di Vendite"          # Etichetta asse Y
  ) +
  theme_minimal()  # Tema grafico semplice e pulito

# 4. Distribuzione dei prezzi mediani per città
# Usando un boxplot per mostrare come variano i prezzi mediani tra le città
ggplot(data, aes(x = city, y = median_price)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +  # Boxplot con colori personalizzati
  labs(
    title = "Distribuzione dei Prezzi Median per Città",  # Titolo del grafico
    x = "Città",                                         # Etichetta asse X
    y = "Prezzo Mediano ($)"                             # Etichetta asse Y
  ) +
  theme_minimal()  # Tema grafico semplice

# 5. Relazione tra annunci e vendite
# Creiamo un grafico a dispersione per vedere se esiste una relazione tra annunci e vendite
ggplot(data, aes(x = listings, y = sales, color = city)) +
  geom_point(alpha = 0.6) +  # Ogni punto rappresenta una città; alpha regola la trasparenza
  labs(
    title = "Relazione tra Annunci e Vendite",  # Titolo del grafico
    x = "Numero di Annunci",                   # Etichetta asse X
    y = "Numero di Vendite"                    # Etichetta asse Y
  ) +
  theme_minimal()

# 6. Trend dei mesi di inventario nel tempo
# Calcoliamo la media dei mesi di inventario per ogni mese e anno
inventory_trend <- data %>%
  group_by(year, month) %>%
  summarise(average_inventory = mean(months_inventory, na.rm = TRUE))

inventory_trend

# Creiamo un grafico a linee per mostrare come cambiano i mesi di inventario nel tempo
ggplot(inventory_trend, aes(x = as.numeric(paste(year, as.numeric(month), sep = ".")), 
                            y = average_inventory)) +
  geom_line(color = "darkgreen") +  # Linea verde per rappresentare l'inventario
  labs(
    title = "Trend dei Mesi di Inventario nel Tempo",  # Titolo del grafico
    x = "Anno (frazionato in mesi)",                  # Etichetta asse X
    y = "Mesi di Inventario"                          # Etichetta asse Y
  ) +
  theme_minimal()

# 7. Volume delle vendite per città
# Sommiamo il volume delle vendite totale per ogni città
volume_by_city <- data %>%
  group_by(city) %>%
  summarise(total_volume = sum(volume, na.rm = TRUE))

# Creiamo un grafico a barre per mostrare il volume delle vendite per città
ggplot(volume_by_city, aes(x = reorder(city, -total_volume), y = total_volume)) +
  geom_bar(stat = "identity", fill = "orange") +  # Barre arancioni
  labs(
    title = "Volume Totale delle Vendite per Città",  # Titolo del grafico
    x = "Città",                                     # Etichetta asse X
    y = "Volume Totale ($ Milioni)"                 # Etichetta asse Y
  ) +
  theme_minimal()

# 8. Stagionalità delle vendite (heatmap)
# Calcoliamo la media delle vendite per ogni città e mese
seasonality <- data %>%
  group_by(city, month) %>%
  summarise(mean_sales = mean(sales, na.rm = TRUE))

# Creiamo una heatmap per visualizzare la stagionalità delle vendite
ggplot(seasonality, aes(x = month, y = city, fill = mean_sales)) +
  geom_tile() +  # Creazione della griglia colorata
  scale_fill_gradient(low = "white", high = "red") +  # Colori da bianco (basse vendite) a rosso (alte vendite)
  labs(
    title = "Stagionalità delle Vendite per Città",  # Titolo del grafico
    x = "Mese",                                     # Etichetta asse X
    y = "Città",                                    # Etichetta asse Y
    fill = "Vendite Medie"                          # Etichetta della scala colore
  ) +
  theme_minimal()

# Salvataggio dei risultati (opzionale, salva i dati in file CSV per usi futuri)
write_csv(sales_trend, "sales_trend.csv")
write_csv(volume_by_city, "volume_by_city.csv")
write_csv(inventory_trend, "inventory_trend.csv")



summary(dati)
N <- dim(dati)[1]
freq_ass_city <- table(dati$city)
freq_rel_city <- freq_ass_city/N
freq_rel_city

head(dati,10)

# Calcola gli indici di posizione
media_sales <- mean(dati$sales)
mediana_sales <- median(dati$sales)
moda_sales <- as.numeric(names(sort(table(dati$sales), decreasing = TRUE)[1]))

# Calcola gli indici di variabilità
deviazione_standard_sales <- sd(dati$sales)
varianza_sales <- var(dati$sales)
iqr_sales <- IQR(dati$sales)

# Calcola gli indici di posizione per volume
media_volume <- mean(dati$volume)
mediana_volume <- median(dati$volume)
moda_volume <- as.numeric(names(sort(table(dati$volume), decreasing = TRUE)[1]))

# Calcola gli indici di variabilità per volume
deviazione_standard_volume <- sd(dati$volume)
varianza_volume <- var(dati$volume)
iqr_volume <- IQR(dati$volume)

# Calcola gli indici di posizione per median_price
media_median_price <- mean(dati$median_price)
mediana_median_price <- median(dati$median_price)
moda_median_price <- as.numeric(names(sort(table(dati$median_price), decreasing = TRUE)[1]))

# Calcola gli indici di variabilità per median_price
deviazione_standard_median_price <- sd(dati$median_price)
varianza_median_price <- var(dati$median_price)
iqr_median_price <- IQR(dati$median_price)

# Calcola gli indici di posizione per listings
media_listings <- mean(dati$listings)
mediana_listings <- median(dati$listings)
moda_listings <- as.numeric(names(sort(table(dati$listings), decreasing = TRUE)[1]))

# Calcola gli indici di variabilità per listings
deviazione_standard_listings <- sd(dati$listings)
varianza_listings <- var(dati$listings)
iqr_listings <- IQR(dati$listings)

# Visualizza i risultati
list(
  media_sales = media_sales,
  mediana_sales = mediana_sales,
  moda_sales = moda_sales,
  deviazione_standard_sales = deviazione_standard_sales,
  varianza_sales = varianza_sales,
  iqr_sales = iqr_sales,
  media_volume = media_volume,
  mediana_volume = mediana_volume,
  moda_volume = moda_volume,
  deviazione_standard_volume = deviazione_standard_volume,
  varianza_volume = varianza_volume,
  iqr_volume = iqr_volume,
  media_median_price = media_median_price,
  mediana_median_price = mediana_median_price,
  moda_median_price = moda_median_price,
  deviazione_standard_median_price = deviazione_standard_median_price,
  varianza_median_price = varianza_median_price,
  iqr_median_price = iqr_median_price,
  media_listings = media_listings,
  mediana_listings = mediana_listings,
  moda_listings = moda_listings,
  deviazione_standard_listings = deviazione_standard_listings,
  varianza_listings = varianza_listings,
  iqr_listings = iqr_listings
)



