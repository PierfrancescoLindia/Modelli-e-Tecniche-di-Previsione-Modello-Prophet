############################################################
# PROGETTO: Analisi e Forecasting del dataset Walmart Sales
# CORSO: Modelli e Tecniche di Previsione
#
# FILE: 03_trend_e_cambi_di_regime.R
#
# OBIETTIVO:
# Questo script sviluppa il punto 1.3 dell'analisi esplorativa,
# concentrandosi sul trend della serie aggregata e sulla possibile
# presenza di cambi di regime nel tempo.
#
# In particolare, il codice permette di:
# 1. visualizzare la serie aggregata delle vendite;
# 2. affiancare strumenti di smoothing per leggere il trend;
# 3. confrontare in modo sintetico inizio e fine periodo;
# 4. individuare settimane con variazioni marcate;
# 5. fornire una base descrittiva per discutere l'eventuale
#    presenza di rotture strutturali.
#
# MOTIVAZIONE:
# In una serie temporale business è importante verificare se esista
# una direzione di fondo nelle vendite e se tale andamento sia
# stabile oppure soggetto a cambiamenti nel tempo. Questa lettura è
# preliminare alla modellazione previsiva e coerente con l'attenzione
# posta dal paper ai changepoints e alla flessibilità del trend.
############################################################


############################################################
# 0. LIBRERIE
############################################################

library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# Creazione della cartella output se non esiste già
if (!dir.exists("output")) dir.create("output")


############################################################
# 1. IMPORTAZIONE DEL DATASET E COSTRUZIONE DELLA SERIE AGGREGATA
############################################################

walmart <- read_csv("wallmart_sales.csv")

sales_agg <- walmart %>%
  group_by(Date) %>%
  summarise(
    Total_Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Date)


############################################################
# 2. GRAFICO BASE DELLA SERIE AGGREGATA
############################################################

# Questo grafico consente una prima lettura del profilo complessivo
# della serie nel tempo.

plot_trend_base <- ggplot(sales_agg, aes(x = Date, y = Total_Weekly_Sales)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Serie aggregata delle vendite Walmart",
    subtitle = "Andamento temporale delle vendite settimanali aggregate",
    x = "Data",
    y = "Vendite aggregate settimanali"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_trend_base


############################################################
# 3. MEDIA MOBILE A 4 E 12 SETTIMANE
############################################################

# Le medie mobili aiutano a rendere più leggibile l'andamento di
# fondo della serie, smussando la variabilità di breve periodo.
#
# - MA_4: lettura di breve-medio periodo
# - MA_12: lettura più regolare del profilo di fondo

sales_agg <- sales_agg %>%
  mutate(
    MA_4  = stats::filter(Total_Weekly_Sales, rep(1/4, 4), sides = 1),
    MA_12 = stats::filter(Total_Weekly_Sales, rep(1/12, 12), sides = 1)
  )

plot_ma <- ggplot(sales_agg, aes(x = Date)) +
  geom_line(aes(y = Total_Weekly_Sales), linewidth = 0.5, alpha = 0.5) +
  geom_line(aes(y = MA_4), linewidth = 0.9) +
  geom_line(aes(y = MA_12), linewidth = 1.1) +
  labs(
    title = "Serie aggregata con medie mobili",
    subtitle = "Smussamento a 4 e 12 settimane per evidenziare il trend",
    x = "Data",
    y = "Vendite aggregate settimanali"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_ma


############################################################
# 4. TREND LINEARE DI RIFERIMENTO
############################################################

# Aggiungiamo una retta di tendenza lineare solo come riferimento
# descrittivo. Non rappresenta un modello definitivo, ma aiuta a
# capire se esiste una direzione media crescente o decrescente.

plot_linear_trend <- ggplot(sales_agg, aes(x = Date, y = Total_Weekly_Sales)) +
  geom_line(linewidth = 0.6, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Serie aggregata con trend lineare di riferimento",
    subtitle = "La retta di regressione fornisce una lettura sintetica della direzione media",
    x = "Data",
    y = "Vendite aggregate settimanali"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_linear_trend


############################################################
# 5. SMOOTHING NON PARAMETRICO (LOESS)
############################################################

# Il LOESS permette una lettura più flessibile del profilo della
# serie rispetto alla semplice retta lineare e può aiutare a far
# emergere fasi diverse o possibili cambiamenti di regime.

plot_loess <- ggplot(sales_agg, aes(x = Date, y = Total_Weekly_Sales)) +
  geom_line(linewidth = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, span = 0.2, linewidth = 1) +
  labs(
    title = "Serie aggregata con smoothing LOESS",
    subtitle = "Profilo flessibile del trend nel tempo",
    x = "Data",
    y = "Vendite aggregate settimanali"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_loess


############################################################
# 6. CONFRONTO TRA INIZIO E FINE PERIODO
############################################################

# Per una prima verifica descrittiva del trend confrontiamo il livello
# medio delle vendite nelle prime 12 settimane e nelle ultime 12
# settimane osservate.

first_12_mean <- sales_agg %>%
  slice(1:12) %>%
  summarise(mean_sales = mean(Total_Weekly_Sales, na.rm = TRUE))

last_12_mean <- sales_agg %>%
  slice((n() - 11):n()) %>%
  summarise(mean_sales = mean(Total_Weekly_Sales, na.rm = TRUE))

first_12_mean
last_12_mean

trend_comparison <- tibble(
  Periodo = c("Prime 12 settimane", "Ultime 12 settimane"),
  Media_vendite = c(first_12_mean$mean_sales, last_12_mean$mean_sales)
)

trend_comparison


############################################################
# 7. CONFRONTO PER ANNO
############################################################

# Aggiungiamo una variabile anno per confrontare il livello medio
# delle vendite aggregate nei diversi anni del campione.

sales_agg <- sales_agg %>%
  mutate(Year = format(Date, "%Y"))

yearly_summary <- sales_agg %>%
  group_by(Year) %>%
  summarise(
    mean_sales = mean(Total_Weekly_Sales, na.rm = TRUE),
    median_sales = median(Total_Weekly_Sales, na.rm = TRUE),
    min_sales = min(Total_Weekly_Sales, na.rm = TRUE),
    max_sales = max(Total_Weekly_Sales, na.rm = TRUE),
    .groups = "drop"
  )

yearly_summary


############################################################
# 8. VARIAZIONI SETTIMANALI E POSSIBILI ROTTURE
############################################################

# Calcoliamo le variazioni rispetto alla settimana precedente.
# Questo consente di individuare settimane in cui la serie subisce
# cambiamenti particolarmente intensi.

sales_agg <- sales_agg %>%
  mutate(
    diff_abs = Total_Weekly_Sales - lag(Total_Weekly_Sales),
    diff_pct = (Total_Weekly_Sales / lag(Total_Weekly_Sales) - 1) * 100
  )

largest_changes <- sales_agg %>%
  arrange(desc(abs(diff_abs))) %>%
  select(Date, Total_Weekly_Sales, diff_abs, diff_pct) %>%
  head(10)

largest_changes


############################################################
# 9. GRAFICO DELLE VARIAZIONI SETTIMANALI
############################################################

plot_changes <- ggplot(sales_agg, aes(x = Date, y = diff_abs)) +
  geom_col() +
  labs(
    title = "Variazioni assolute rispetto alla settimana precedente",
    subtitle = "Settimane con cambiamenti più marcati nelle vendite aggregate",
    x = "Data",
    y = "Differenza assoluta"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_changes


############################################################
# 10. TOP SETTIMANE DI CRESCITA E DI CALO
############################################################

# Separiamo le principali settimane con incremento e con riduzione
# delle vendite rispetto alla settimana precedente.

top_increases <- sales_agg %>%
  arrange(desc(diff_abs)) %>%
  select(Date, Total_Weekly_Sales, diff_abs, diff_pct) %>%
  head(10)

top_decreases <- sales_agg %>%
  arrange(diff_abs) %>%
  select(Date, Total_Weekly_Sales, diff_abs, diff_pct) %>%
  head(10)

top_increases
top_decreases


############################################################
# 11. SALVATAGGIO OUTPUT
############################################################

write_csv(sales_agg, "output/sales_agg_trend.csv")
write_csv(trend_comparison, "output/trend_comparison.csv")
write_csv(yearly_summary, "output/yearly_summary.csv")
write_csv(largest_changes, "output/largest_changes_trend.csv")
write_csv(top_increases, "output/top_increases.csv")
write_csv(top_decreases, "output/top_decreases.csv")


############################################################
# 12. SALVATAGGIO GRAFICI
############################################################

ggsave(
  filename = "output/plot_trend_base.png",
  plot = plot_trend_base,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_ma.png",
  plot = plot_ma,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_linear_trend.png",
  plot = plot_linear_trend,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_loess.png",
  plot = plot_loess,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_changes.png",
  plot = plot_changes,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# 13. CONCLUSIONI OPERATIVE DELLA FASE 1.3
############################################################

# Al termine di questo script dovremmo essere in grado di rispondere
# alle seguenti domande:
#
# - La serie aggregata mostra una direzione di fondo nel tempo?
# - Esistono differenze tra l'inizio e la fine del campione?
# - Il profilo della serie appare regolare oppure articolato in fasi?
# - Sono presenti settimane con variazioni molto marcate che possano
#   suggerire cambi di regime o shock temporanei?
#
# Questi risultati costituiranno la base per la scrittura del punto
# 1.3 della relazione e introdurranno naturalmente la fase successiva
# dedicata alla stagionalità e agli effetti delle festività.
############################################################
