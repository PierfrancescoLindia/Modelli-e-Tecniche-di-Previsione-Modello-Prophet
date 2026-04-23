############################################################
# PROGETTO: Analisi e Forecasting del dataset Walmart Sales
# CORSO: Modelli e Tecniche di Previsione
#
# FILE: 04_stagionalita_e_holiday_effect.R
#
# OBIETTIVO:
# Questo script sviluppa il punto 1.4 dell'analisi esplorativa,
# concentrandosi sulla presenza di pattern stagionali e sull'effetto
# delle settimane festive nelle vendite aggregate Walmart.
#
# In particolare, il codice permette di:
# 1. studiare la distribuzione delle vendite per mese e per anno;
# 2. verificare se alcune parti dell'anno mostrano livelli di vendita
#    sistematicamente differenti;
# 3. confrontare settimane festive e non festive;
# 4. osservare il comportamento della serie in corrispondenza delle
#    settimane holiday;
# 5. produrre tabelle e grafici utili per la relazione.
#
# MOTIVAZIONE:
# Nel forecasting delle serie business, stagionalità ed effetti legati
# alle festività rappresentano spesso componenti centrali. In linea con
# il paper di riferimento, questa fase serve a verificare se tali
# elementi siano effettivamente presenti nel dataset Walmart e se
# debbano essere considerati esplicitamente nella modellazione. 
############################################################


############################################################
# 0. LIBRERIE
############################################################

library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

# Creazione cartella output se non esiste
if (!dir.exists("output")) dir.create("output")


############################################################
# 1. IMPORTAZIONE DEL DATASET
############################################################

walmart <- read_csv("data/wallmart_sales.csv")


############################################################
# 2. COSTRUZIONE DELLA SERIE AGGREGATA CON INFORMAZIONE HOLIDAY
############################################################

# Aggregazione delle vendite per settimana.
# Oltre al totale delle vendite, manteniamo anche l'informazione sulla
# presenza della festività.
#
# Poiché per una stessa settimana il valore di IsHoliday è lo stesso per
# tutte le righe, possiamo sintetizzarlo con max() oppure any().

sales_agg <- walmart %>%
  group_by(Date) %>%
  summarise(
    Total_Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE),
    IsHoliday = any(IsHoliday),
    .groups = "drop"
  ) %>%
  arrange(Date)


############################################################
# 3. CREAZIONE DI VARIABILI TEMPORALI UTILI
############################################################

# Per studiare la stagionalità costruiamo alcune variabili derivate:
# - anno
# - mese
# - nome del mese
# - numero settimana dell'anno
#
# Queste variabili permettono di confrontare periodi omogenei e di
# verificare l'eventuale presenza di pattern ricorrenti.

sales_agg <- sales_agg %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Month_Label = month(Date, label = TRUE, abbr = FALSE),
    Week_of_Year = isoweek(Date)
  )


############################################################
# 4. STATISTICHE DESCRITTIVE PER MESE
############################################################

# Calcoliamo alcune statistiche delle vendite aggregate per mese,
# aggregando tutte le settimane appartenenti allo stesso mese.

monthly_summary <- sales_agg %>%
  group_by(Month, Month_Label) %>%
  summarise(
    mean_sales = mean(Total_Weekly_Sales, na.rm = TRUE),
    median_sales = median(Total_Weekly_Sales, na.rm = TRUE),
    min_sales = min(Total_Weekly_Sales, na.rm = TRUE),
    max_sales = max(Total_Weekly_Sales, na.rm = TRUE),
    sd_sales = sd(Total_Weekly_Sales, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  ) %>%
  arrange(Month)

monthly_summary


############################################################
# 5. GRAFICO DELLA MEDIA DELLE VENDITE PER MESE
############################################################

# Questo grafico aiuta a capire se alcune parti dell'anno presentano
# livelli medi di vendita più elevati o più bassi.

plot_monthly_mean <- ggplot(monthly_summary,
                            aes(x = factor(Month_Label, levels = unique(Month_Label)),
                                y = mean_sales)) +
  geom_col() +
  labs(
    title = "Media delle vendite aggregate per mese",
    subtitle = "Confronto della stagionalità media mensile",
    x = "Mese",
    y = "Vendite medie aggregate"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_monthly_mean


############################################################
# 6. BOXPLOT DELLE VENDITE PER MESE
############################################################

# Il boxplot mostra non solo il livello medio, ma anche la dispersione
# delle vendite per ciascun mese. Questo aiuta a capire se alcuni mesi
# risultino più volatili di altri.

plot_monthly_box <- ggplot(sales_agg,
                           aes(x = factor(Month_Label,
                                          levels = unique(monthly_summary$Month_Label)),
                               y = Total_Weekly_Sales)) +
  geom_boxplot() +
  labs(
    title = "Distribuzione delle vendite aggregate per mese",
    subtitle = "Boxplot mensile delle vendite settimanali aggregate",
    x = "Mese",
    y = "Vendite aggregate"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_monthly_box


############################################################
# 7. CONFRONTO MENSILE PER ANNO
############################################################

# Per capire se il profilo stagionale sia relativamente stabile nel
# tempo, osserviamo l'andamento medio per mese distinguendo per anno.

monthly_year_summary <- sales_agg %>%
  group_by(Year, Month, Month_Label) %>%
  summarise(
    mean_sales = mean(Total_Weekly_Sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year, Month)

monthly_year_summary

plot_monthly_year <- ggplot(monthly_year_summary,
                            aes(x = Month,
                                y = mean_sales,
                                group = factor(Year),
                                color = factor(Year))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.name
  ) +
  labs(
    title = "Profilo mensile delle vendite aggregate per anno",
    subtitle = "Confronto della stagionalità nei diversi anni",
    x = "Mese",
    y = "Vendite medie aggregate",
    color = "Anno"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_monthly_year


############################################################
# 8. CONFRONTO TRA SETTIMANE HOLIDAY E NON-HOLIDAY
############################################################

# In questa parte verifichiamo se le settimane festive mostrano livelli
# di vendita sistematicamente diversi rispetto alle settimane ordinarie.

holiday_summary <- sales_agg %>%
  group_by(IsHoliday) %>%
  summarise(
    mean_sales = mean(Total_Weekly_Sales, na.rm = TRUE),
    median_sales = median(Total_Weekly_Sales, na.rm = TRUE),
    min_sales = min(Total_Weekly_Sales, na.rm = TRUE),
    max_sales = max(Total_Weekly_Sales, na.rm = TRUE),
    sd_sales = sd(Total_Weekly_Sales, na.rm = TRUE),
    n_weeks = n(),
    .groups = "drop"
  )

holiday_summary

plot_holiday_box <- ggplot(sales_agg,
                           aes(x = factor(IsHoliday,
                                          levels = c(FALSE, TRUE),
                                          labels = c("Non-Holiday", "Holiday")),
                               y = Total_Weekly_Sales)) +
  geom_boxplot() +
  labs(
    title = "Confronto tra settimane festive e non festive",
    subtitle = "Distribuzione delle vendite aggregate",
    x = "Tipologia di settimana",
    y = "Vendite aggregate"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_holiday_box


############################################################
# 9. DIFFERENZA MEDIA TRA HOLIDAY E NON-HOLIDAY
############################################################

# Calcoliamo esplicitamente la differenza media tra le due categorie.

holiday_difference <- holiday_summary %>%
  select(IsHoliday, mean_sales) %>%
  tidyr::pivot_wider(names_from = IsHoliday, values_from = mean_sales) %>%
  mutate(
    diff_abs = `TRUE` - `FALSE`,
    diff_pct = (`TRUE` / `FALSE` - 1) * 100
  )

holiday_difference


############################################################
# 10. SETTIMANE HOLIDAY NELLA SERIE TEMPORALE
############################################################

# Evidenziamo graficamente le settimane festive all'interno della serie
# aggregata per osservare se corrispondano a picchi o cadute rilevanti.

plot_holiday_time <- ggplot(sales_agg, aes(x = Date, y = Total_Weekly_Sales)) +
  geom_line(linewidth = 0.7) +
  geom_point(data = sales_agg %>% filter(IsHoliday == TRUE),
             size = 2) +
  labs(
    title = "Vendite aggregate e settimane festive",
    subtitle = "Le settimane holiday sono evidenziate come punti",
    x = "Data",
    y = "Vendite aggregate"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_holiday_time


############################################################
# 11. ELENCO DELLE SETTIMANE FESTIVE
############################################################

# Questa tabella è utile per identificare con precisione le settimane
# festive presenti nel campione e il relativo livello di vendita.

holiday_weeks <- sales_agg %>%
  filter(IsHoliday == TRUE) %>%
  select(Date, Total_Weekly_Sales, Year, Month, Month_Label)

holiday_weeks


############################################################
# 12. SETTIMANE CON VENDITE PIÙ ALTE E RELAZIONE CON HOLIDAY
############################################################

# Verifichiamo se tra le settimane con vendite aggregate più elevate
# compaiono settimane festive.

top_weeks_sales <- sales_agg %>%
  arrange(desc(Total_Weekly_Sales)) %>%
  select(Date, Total_Weekly_Sales, IsHoliday, Year, Month_Label) %>%
  head(15)

top_weeks_sales


############################################################
# 13. SETTIMANE CON VENDITE PIÙ BASSE E RELAZIONE CON HOLIDAY
############################################################

# Facciamo lo stesso controllo per le settimane con vendite più basse.

bottom_weeks_sales <- sales_agg %>%
  arrange(Total_Weekly_Sales) %>%
  select(Date, Total_Weekly_Sales, IsHoliday, Year, Month_Label) %>%
  head(15)

bottom_weeks_sales


############################################################
# 14. SALVATAGGIO OUTPUT
############################################################

write_csv(sales_agg, "output/sales_agg_seasonality.csv")
write_csv(monthly_summary, "output/monthly_summary.csv")
write_csv(monthly_year_summary, "output/monthly_year_summary.csv")
write_csv(holiday_summary, "output/holiday_summary.csv")
write_csv(holiday_difference, "output/holiday_difference.csv")
write_csv(holiday_weeks, "output/holiday_weeks.csv")
write_csv(top_weeks_sales, "output/top_weeks_sales.csv")
write_csv(bottom_weeks_sales, "output/bottom_weeks_sales.csv")


############################################################
# 15. SALVATAGGIO GRAFICI
############################################################

ggsave(
  filename = "output/plot_monthly_mean.png",
  plot = plot_monthly_mean,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_monthly_box.png",
  plot = plot_monthly_box,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_monthly_year.png",
  plot = plot_monthly_year,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_holiday_box.png",
  plot = plot_holiday_box,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_holiday_time.png",
  plot = plot_holiday_time,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# 16. CONCLUSIONI OPERATIVE DELLA FASE 1.4
############################################################

# Al termine di questo script dovremmo essere in grado di rispondere
# alle seguenti domande:
#
# - Le vendite aggregate mostrano un profilo stagionale?
# - Esistono mesi mediamente più forti o più deboli?
# - Il profilo mensile appare coerente nei diversi anni?
# - Le settimane festive presentano livelli di vendita diversi
#   rispetto alle settimane ordinarie?
# - Le settimane con valori estremi coincidono spesso con holiday?
#
# I risultati ottenuti costituiranno la base per la scrittura del
# punto 1.4 della relazione e guideranno le successive scelte di
# modellazione previsiva.
############################################################
