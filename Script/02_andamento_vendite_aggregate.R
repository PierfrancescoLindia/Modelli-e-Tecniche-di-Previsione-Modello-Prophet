############################################################
# PROGETTO: Analisi e Forecasting del dataset Walmart Sales
# CORSO: Modelli e Tecniche di Previsione
#
# FILE: 02_andamento_vendite_aggregate.R
#
# OBIETTIVO:
# Questo script sviluppa il punto 1.2 dell'analisi esplorativa,
# concentrandosi sull'andamento temporale delle vendite aggregate.
#
# In particolare, il codice permette di:
# 1. aggregare le vendite settimanali a livello complessivo;
# 2. costruire una serie storica aggregata;
# 3. ottenere statistiche descrittive di base;
# 4. visualizzare il profilo temporale delle vendite;
# 5. evidenziare eventuali pattern generali, picchi e anomalie.
#
# MOTIVAZIONE:
# Prima di analizzare store e department separatamente, è utile
# studiare il comportamento complessivo delle vendite Walmart.
# Questa visione aggregata consente di individuare trend generali,
# possibili effetti stagionali e settimane anomale, fornendo una
# base interpretativa per le successive fasi di forecasting.
############################################################


############################################################
# 0. LIBRERIE
############################################################

# readr: importazione dati
# dplyr: manipolazione e aggregazione
# ggplot2: visualizzazione grafica
# scales: formattazione leggibile degli assi dei grafici
library(readr)
library(dplyr)
library(ggplot2)
library(scales)


############################################################
# 1. IMPORTAZIONE DEL DATASET
############################################################

# Carichiamo il dataset.
# Si assume una struttura di progetto con il file CSV nella cartella data/.
walmart <- read_csv("data/wallmart_sales.csv")

# Controllo rapido del formato della data.
# In questo dataset read_csv() la importa già correttamente come Date.
str(walmart$Date)
class(walmart$Date)


############################################################
# 2. COSTRUZIONE DELLA SERIE AGGREGATA DELLE VENDITE
############################################################

# In questa fase vogliamo passare dal livello micro
# (Store-Dept-Date) al livello macro (Date).
#
# L'idea è sommare, per ogni settimana, tutte le vendite presenti
# nel dataset, ottenendo così una singola serie storica aggregata.
#
# Questa serie rappresenta il volume complessivo delle vendite Walmart
# osservato settimana per settimana.

sales_agg <- walmart %>%
  group_by(Date) %>%
  summarise(
    Total_Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Date)

# Visualizziamo le prime righe della serie aggregata
head(sales_agg)

# Visualizziamo le ultime righe per controllare anche la parte finale
tail(sales_agg)


############################################################
# 3. CONTROLLO DELLA SERIE AGGREGATA
############################################################

# Verifichiamo:
# - numero di settimane presenti;
# - data iniziale e finale;
# - struttura dell'oggetto costruito.

dim(sales_agg)
str(sales_agg)

min(sales_agg$Date)
max(sales_agg$Date)
nrow(sales_agg)


############################################################
# 4. STATISTICHE DESCRITTIVE DI BASE
############################################################

# Qui otteniamo una prima descrizione quantitativa delle vendite
# aggregate settimanali:
# - minimo
# - massimo
# - media
# - mediana
# - deviazione standard
#
# Questo aiuta a capire il livello medio delle vendite e la loro
# variabilità nel tempo.

summary(sales_agg$Total_Weekly_Sales)

mean_sales <- mean(sales_agg$Total_Weekly_Sales, na.rm = TRUE)
median_sales <- median(sales_agg$Total_Weekly_Sales, na.rm = TRUE)
sd_sales <- sd(sales_agg$Total_Weekly_Sales, na.rm = TRUE)
min_sales <- min(sales_agg$Total_Weekly_Sales, na.rm = TRUE)
max_sales <- max(sales_agg$Total_Weekly_Sales, na.rm = TRUE)

mean_sales
median_sales
sd_sales
min_sales
max_sales


############################################################
# 5. IDENTIFICAZIONE DELLE SETTIMANE CON VENDITE MASSIME E MINIME
############################################################

# Oltre al valore minimo e massimo, è utile identificare anche
# le settimane specifiche in cui tali valori si verificano.
# Questo è importante perché eventuali picchi o cadute possono
# essere collegati a festività, shock esterni o anomalie.

week_max_sales <- sales_agg %>%
  filter(Total_Weekly_Sales == max(Total_Weekly_Sales, na.rm = TRUE))

week_min_sales <- sales_agg %>%
  filter(Total_Weekly_Sales == min(Total_Weekly_Sales, na.rm = TRUE))

week_max_sales
week_min_sales


############################################################
# 6. GRAFICO BASE DELLA SERIE AGGREGATA
############################################################

# Questo è il grafico principale del punto 1.2.
# Mostra l'evoluzione delle vendite aggregate nel tempo.
#
# Obiettivo interpretativo:
# - osservare il profilo generale della serie;
# - verificare se ci sono trend visibili;
# - individuare picchi o cadute;
# - cogliere eventuali pattern ricorrenti.

plot_sales_agg <- ggplot(sales_agg, aes(x = Date, y = Total_Weekly_Sales)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Andamento temporale delle vendite aggregate settimanali",
    subtitle = "Dataset Walmart Sales",
    x = "Data",
    y = "Vendite aggregate settimanali"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_sales_agg


############################################################
# 7. GRAFICO CON PUNTI + LINEA
############################################################

# Aggiungiamo anche una versione con punti, utile per evidenziare
# che la serie è composta da osservazioni discrete settimanali.
# Questa visualizzazione è spesso efficace nelle relazioni.

plot_sales_points <- ggplot(sales_agg, aes(x = Date, y = Total_Weekly_Sales)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.5) +
  labs(
    title = "Vendite aggregate settimanali: linea e punti",
    subtitle = "Ogni punto rappresenta una settimana osservata",
    x = "Data",
    y = "Vendite aggregate settimanali"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_sales_points


############################################################
# 8. MEDIA MOBILE PER EVIDENZIARE L'ANDAMENTO GENERALE
############################################################

# Per leggere meglio il profilo della serie, affianchiamo al grafico
# originale una media mobile.
#
# La media mobile non è ancora un modello previsivo, ma uno strumento
# descrittivo che aiuta a smussare le oscillazioni di breve periodo e
# a rendere più leggibile l'andamento di fondo.
#
# Usiamo qui una finestra di 4 settimane, ma questo parametro può
# essere modificato in base alle esigenze analitiche.

sales_agg <- sales_agg %>%
  mutate(
    MA_4 = stats::filter(Total_Weekly_Sales, rep(1/4, 4), sides = 1)
  )

plot_sales_ma <- ggplot(sales_agg, aes(x = Date, y = Total_Weekly_Sales)) +
  geom_line(linewidth = 0.6, alpha = 0.7) +
  geom_line(aes(y = MA_4), linewidth = 1) +
  labs(
    title = "Vendite aggregate settimanali con media mobile a 4 settimane",
    subtitle = "La media mobile aiuta a evidenziare il profilo di fondo della serie",
    x = "Data",
    y = "Vendite aggregate settimanali"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_sales_ma


############################################################
# 9. TABELLA DELLE PRIME E ULTIME SETTIMANE
############################################################

# Questa tabella è utile per controllare il livello iniziale e finale
# della serie e per una lettura più concreta della dinamica temporale.

first_weeks <- head(sales_agg, 10)
last_weeks  <- tail(sales_agg, 10)

first_weeks
last_weeks


############################################################
# 10. VARIAZIONE SETTIMANALE DELLE VENDITE
############################################################

# Calcoliamo la differenza assoluta rispetto alla settimana precedente.
# Questo indicatore permette di individuare settimane con variazioni
# particolarmente intense, utili per riconoscere discontinuità,
# shock o possibili effetti holiday.

sales_agg <- sales_agg %>%
  mutate(
    diff_abs = Total_Weekly_Sales - lag(Total_Weekly_Sales),
    diff_pct = (Total_Weekly_Sales / lag(Total_Weekly_Sales) - 1) * 100
  )

# Visualizziamo le settimane con le maggiori variazioni assolute
largest_changes <- sales_agg %>%
  arrange(desc(abs(diff_abs))) %>%
  select(Date, Total_Weekly_Sales, diff_abs, diff_pct) %>%
  head(10)

largest_changes


############################################################
# 11. GRAFICO DELLE VARIAZIONI SETTIMANALI
############################################################

# Questo grafico serve a visualizzare quanto cambiano le vendite
# da una settimana alla successiva.
# Può essere molto utile per individuare punti di rottura o periodi
# di particolare instabilità.

plot_diff <- ggplot(sales_agg, aes(x = Date, y = diff_abs)) +
  geom_col() +
  labs(
    title = "Variazione assoluta delle vendite rispetto alla settimana precedente",
    x = "Data",
    y = "Differenza assoluta"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_diff


############################################################
# 12. SALVATAGGIO DEGLI OUTPUT PRINCIPALI
############################################################

# Salviamo la serie aggregata e le principali tabelle di supporto.
# Questo è utile sia per la relazione sia per la replicabilità del
# progetto su GitHub.

write_csv(sales_agg, "output/sales_agg.csv")
write_csv(largest_changes, "output/largest_changes_agg.csv")
write_csv(week_max_sales, "output/week_max_sales.csv")
write_csv(week_min_sales, "output/week_min_sales.csv")


############################################################
# 13. SALVATAGGIO DEI GRAFICI
############################################################

# Salviamo i grafici in formato PNG per poterli usare nella relazione.

ggsave(
  filename = "output/plot_sales_agg.png",
  plot = plot_sales_agg,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_sales_points.png",
  plot = plot_sales_points,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_sales_ma.png",
  plot = plot_sales_ma,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_diff.png",
  plot = plot_diff,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# 14. CONCLUSIONI OPERATIVE DELLA FASE 1.2
############################################################

# Al termine di questo script dovremmo essere in grado di rispondere
# alle seguenti domande:
#
# - Qual è l'andamento complessivo delle vendite Walmart nel tempo?
# - Le vendite aggregate mostrano un trend visibile?
# - Sono presenti picchi o cadute particolarmente marcati?
# - Esistono settimane con variazioni anomale rispetto alla precedente?
#
# Questa fase rappresenta il primo vero passaggio dall'analisi
# strutturale del dataset alla lettura dinamica della serie temporale.
# I risultati ottenuti qui guideranno la fase successiva dedicata
# all'analisi di trend, stagionalità e holiday effects.
############################################################
