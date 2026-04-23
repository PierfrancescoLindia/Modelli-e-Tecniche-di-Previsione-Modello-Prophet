############################################################
# PROGETTO: Analisi e Forecasting del dataset Walmart Sales
# CORSO: Modelli e Tecniche di Previsione
#
# FILE: 06_outlier_valori_negativi_qualita_dato.R
#
# OBIETTIVO:
# Questo script sviluppa il punto 1.6 dell'analisi esplorativa,
# concentrandosi su outlier, valori negativi e qualità del dato.
#
# In particolare, il codice permette di:
# 1. descrivere la distribuzione di Weekly_Sales;
# 2. quantificare vendite nulle, negative e valori estremi;
# 3. identificare outlier con un criterio statistico semplice;
# 4. localizzare le anomalie per store, department e serie Store-Dept;
# 5. produrre evidenze utili per decidere come trattare i dati nella
#    successiva fase di forecasting.
#
# MOTIVAZIONE:
# In un problema di forecasting è fondamentale verificare la presenza
# di osservazioni anomale, valori negativi o comportamenti irregolari,
# poiché tali elementi possono influenzare fortemente sia l'analisi
# descrittiva sia le performance dei modelli previsivi.
############################################################


############################################################
# 0. LIBRERIE
############################################################

library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# Creazione cartella output se non esiste
if (!dir.exists("output")) dir.create("output")


############################################################
# 1. IMPORTAZIONE DEL DATASET
############################################################

walmart <- read_csv("data/wallmart_sales.csv")


############################################################
# 2. STATISTICHE DESCRITTIVE DI BASE DI WEEKLY_SALES
############################################################

# Prima di identificare anomalie specifiche, descriviamo la variabile
# Weekly_Sales nel suo complesso.

summary(walmart$Weekly_Sales)

weekly_sales_summary <- tibble(
  Indicatore = c(
    "Numero osservazioni",
    "Media",
    "Mediana",
    "Deviazione standard",
    "Minimo",
    "Massimo",
    "Primo quartile",
    "Terzo quartile"
  ),
  Valore = c(
    length(walmart$Weekly_Sales),
    mean(walmart$Weekly_Sales, na.rm = TRUE),
    median(walmart$Weekly_Sales, na.rm = TRUE),
    sd(walmart$Weekly_Sales, na.rm = TRUE),
    min(walmart$Weekly_Sales, na.rm = TRUE),
    max(walmart$Weekly_Sales, na.rm = TRUE),
    quantile(walmart$Weekly_Sales, 0.25, na.rm = TRUE),
    quantile(walmart$Weekly_Sales, 0.75, na.rm = TRUE)
  )
)

weekly_sales_summary


############################################################
# 3. DISTRIBUZIONE DI WEEKLY_SALES
############################################################

# Istogramma complessivo delle vendite.
# La distribuzione può risultare molto asimmetrica; per questo motivo
# salviamo sia una visualizzazione generale sia una versione limitata
# ai percentili più centrali.

plot_hist_all <- ggplot(walmart, aes(x = Weekly_Sales)) +
  geom_histogram(bins = 60) +
  labs(
    title = "Distribuzione complessiva di Weekly_Sales",
    x = "Vendite settimanali",
    y = "Frequenza"
  ) +
  scale_x_continuous(labels = comma) +
  theme_minimal()

plot_hist_all

# Limitiamo la visualizzazione al 99° percentile per leggere meglio
# il corpo centrale della distribuzione.
p99 <- quantile(walmart$Weekly_Sales, 0.99, na.rm = TRUE)

plot_hist_99 <- ggplot(walmart %>% filter(Weekly_Sales <= p99),
                       aes(x = Weekly_Sales)) +
  geom_histogram(bins = 60) +
  labs(
    title = "Distribuzione di Weekly_Sales fino al 99° percentile",
    x = "Vendite settimanali",
    y = "Frequenza"
  ) +
  scale_x_continuous(labels = comma) +
  theme_minimal()

plot_hist_99

# Boxplot complessivo
plot_box_all <- ggplot(walmart, aes(y = Weekly_Sales)) +
  geom_boxplot() +
  labs(
    title = "Boxplot complessivo di Weekly_Sales",
    y = "Vendite settimanali"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_box_all


############################################################
# 4. QUANTIFICAZIONE DI ZERI E VALORI NEGATIVI
############################################################

# In questa parte quantifichiamo esplicitamente il numero di
# osservazioni nulle e negative.

n_zero <- sum(walmart$Weekly_Sales == 0, na.rm = TRUE)
n_negative <- sum(walmart$Weekly_Sales < 0, na.rm = TRUE)

pct_zero <- 100 * n_zero / nrow(walmart)
pct_negative <- 100 * n_negative / nrow(walmart)

zero_negative_summary <- tibble(
  Categoria = c("Vendite uguali a zero", "Vendite negative"),
  Numero = c(n_zero, n_negative),
  Percentuale = c(pct_zero, pct_negative)
)

zero_negative_summary


############################################################
# 5. IDENTIFICAZIONE DEGLI OUTLIER CON CRITERIO IQR
############################################################

# Usiamo il criterio classico basato sull'intervallo interquartile:
# outlier inferiori: Q1 - 1.5*IQR
# outlier superiori: Q3 + 1.5*IQR

Q1 <- quantile(walmart$Weekly_Sales, 0.25, na.rm = TRUE)
Q3 <- quantile(walmart$Weekly_Sales, 0.75, na.rm = TRUE)
IQR_value <- IQR(walmart$Weekly_Sales, na.rm = TRUE)

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outlier_summary <- tibble(
  Indicatore = c("Q1", "Q3", "IQR", "Lower bound", "Upper bound"),
  Valore = c(Q1, Q3, IQR_value, lower_bound, upper_bound)
)

outlier_summary

# Creiamo una variabile che segnala il tipo di osservazione
walmart_outliers <- walmart %>%
  mutate(
    outlier_iqr = case_when(
      Weekly_Sales < lower_bound ~ "Lower outlier",
      Weekly_Sales > upper_bound ~ "Upper outlier",
      TRUE ~ "Regular"
    )
  )

# Conteggio outlier
outlier_counts <- walmart_outliers %>%
  count(outlier_iqr) %>%
  mutate(percentuale = 100 * n / sum(n))

outlier_counts


############################################################
# 6. OSSERVAZIONI PIÙ ESTREME
############################################################

# Elenchiamo le osservazioni con vendite massime e minime e le prime
# anomalie in ordine crescente e decrescente.

top_positive_values <- walmart %>%
  arrange(desc(Weekly_Sales)) %>%
  head(20)

top_negative_values <- walmart %>%
  arrange(Weekly_Sales) %>%
  head(20)

top_positive_values
top_negative_values


############################################################
# 7. LOCALIZZAZIONE DELLE ANOMALIE PER STORE
############################################################

# Verifichiamo se zeri, negativi e outlier si concentrino in alcuni
# store più che in altri.

store_anomalies <- walmart_outliers %>%
  group_by(Store) %>%
  summarise(
    n_obs = n(),
    n_zero = sum(Weekly_Sales == 0, na.rm = TRUE),
    n_negative = sum(Weekly_Sales < 0, na.rm = TRUE),
    n_lower_outlier = sum(outlier_iqr == "Lower outlier", na.rm = TRUE),
    n_upper_outlier = sum(outlier_iqr == "Upper outlier", na.rm = TRUE),
    pct_negative = 100 * n_negative / n_obs,
    pct_outlier_total = 100 * (n_lower_outlier + n_upper_outlier) / n_obs,
    .groups = "drop"
  ) %>%
  arrange(desc(pct_outlier_total))

store_anomalies


############################################################
# 8. LOCALIZZAZIONE DELLE ANOMALIE PER DEPARTMENT
############################################################

dept_anomalies <- walmart_outliers %>%
  group_by(Dept) %>%
  summarise(
    n_obs = n(),
    n_zero = sum(Weekly_Sales == 0, na.rm = TRUE),
    n_negative = sum(Weekly_Sales < 0, na.rm = TRUE),
    n_lower_outlier = sum(outlier_iqr == "Lower outlier", na.rm = TRUE),
    n_upper_outlier = sum(outlier_iqr == "Upper outlier", na.rm = TRUE),
    pct_negative = 100 * n_negative / n_obs,
    pct_outlier_total = 100 * (n_lower_outlier + n_upper_outlier) / n_obs,
    .groups = "drop"
  ) %>%
  arrange(desc(pct_outlier_total))

dept_anomalies


############################################################
# 9. LOCALIZZAZIONE DELLE ANOMALIE PER SERIE STORE-DEPT
############################################################

store_dept_anomalies <- walmart_outliers %>%
  group_by(Store, Dept) %>%
  summarise(
    n_obs = n(),
    n_zero = sum(Weekly_Sales == 0, na.rm = TRUE),
    n_negative = sum(Weekly_Sales < 0, na.rm = TRUE),
    n_lower_outlier = sum(outlier_iqr == "Lower outlier", na.rm = TRUE),
    n_upper_outlier = sum(outlier_iqr == "Upper outlier", na.rm = TRUE),
    pct_negative = 100 * n_negative / n_obs,
    pct_outlier_total = 100 * (n_lower_outlier + n_upper_outlier) / n_obs,
    .groups = "drop"
  ) %>%
  arrange(desc(pct_outlier_total), desc(n_negative))

store_dept_anomalies

top_problematic_series <- store_dept_anomalies %>%
  head(20)

top_problematic_series


############################################################
# 10. DISTRIBUZIONE TEMPORALE DELLE ANOMALIE
############################################################

# Verifichiamo se le anomalie si concentrino in certi periodi del tempo.

anomalies_by_date <- walmart_outliers %>%
  group_by(Date) %>%
  summarise(
    n_obs = n(),
    n_negative = sum(Weekly_Sales < 0, na.rm = TRUE),
    n_lower_outlier = sum(outlier_iqr == "Lower outlier", na.rm = TRUE),
    n_upper_outlier = sum(outlier_iqr == "Upper outlier", na.rm = TRUE),
    n_total_outlier = n_lower_outlier + n_upper_outlier,
    .groups = "drop"
  ) %>%
  arrange(desc(n_total_outlier), desc(n_negative))

anomalies_by_date

top_anomaly_dates <- anomalies_by_date %>%
  head(20)

top_anomaly_dates


############################################################
# 11. GRAFICI DI SUPPORTO
############################################################

# Anomalie per store: top 15
top_store_anomalies <- store_anomalies %>%
  head(15)

plot_store_anomalies <- ggplot(top_store_anomalies,
                               aes(x = reorder(factor(Store), pct_outlier_total),
                                   y = pct_outlier_total)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 15 store per incidenza di outlier",
    x = "Store",
    y = "Percentuale di outlier"
  ) +
  theme_minimal()

plot_store_anomalies

# Anomalie per department: top 15
top_dept_anomalies <- dept_anomalies %>%
  head(15)

plot_dept_anomalies <- ggplot(top_dept_anomalies,
                              aes(x = reorder(factor(Dept), pct_outlier_total),
                                  y = pct_outlier_total)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 15 department per incidenza di outlier",
    x = "Department",
    y = "Percentuale di outlier"
  ) +
  theme_minimal()

plot_dept_anomalies

# Numero totale di outlier per data: top 20
plot_top_anomaly_dates <- ggplot(top_anomaly_dates,
                                 aes(x = reorder(as.character(Date), n_total_outlier),
                                     y = n_total_outlier)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Date con il maggior numero di outlier",
    x = "Data",
    y = "Numero di outlier"
  ) +
  theme_minimal()

plot_top_anomaly_dates


############################################################
# 12. SERIE ESEMPLIFICATIVE CON ANOMALIE
############################################################

# Selezioniamo alcune tra le serie Store-Dept più problematiche per
# poterle visualizzare direttamente.

selected_problem_series <- top_problematic_series %>%
  slice(1:4) %>%
  mutate(Series_Label = paste("Store", Store, "- Dept", Dept))

selected_problem_data <- walmart %>%
  inner_join(selected_problem_series %>% select(Store, Dept, Series_Label),
             by = c("Store", "Dept"))

plot_problem_series <- ggplot(selected_problem_data,
                              aes(x = Date, y = Weekly_Sales)) +
  geom_line() +
  facet_wrap(~ Series_Label, scales = "free_y") +
  labs(
    title = "Esempi di serie Store-Dept con anomalie rilevanti",
    x = "Data",
    y = "Vendite settimanali"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_problem_series


############################################################
# 13. TABELLA RIASSUNTIVA FINALE
############################################################

quality_summary <- tibble(
  Indicatore = c(
    "Numero totale osservazioni",
    "Vendite uguali a zero",
    "Percentuale vendite uguali a zero",
    "Vendite negative",
    "Percentuale vendite negative",
    "Lower bound IQR",
    "Upper bound IQR",
    "Numero lower outlier",
    "Numero upper outlier"
  ),
  Valore = c(
    nrow(walmart),
    n_zero,
    pct_zero,
    n_negative,
    pct_negative,
    lower_bound,
    upper_bound,
    sum(walmart_outliers$outlier_iqr == "Lower outlier"),
    sum(walmart_outliers$outlier_iqr == "Upper outlier")
  )
)

quality_summary


############################################################
# 14. SALVATAGGIO OUTPUT
############################################################

write_csv(weekly_sales_summary, "output/weekly_sales_summary.csv")
write_csv(zero_negative_summary, "output/zero_negative_summary.csv")
write_csv(outlier_summary, "output/outlier_summary.csv")
write_csv(outlier_counts, "output/outlier_counts.csv")
write_csv(store_anomalies, "output/store_anomalies.csv")
write_csv(dept_anomalies, "output/dept_anomalies.csv")
write_csv(store_dept_anomalies, "output/store_dept_anomalies.csv")
write_csv(top_problematic_series, "output/top_problematic_series.csv")
write_csv(anomalies_by_date, "output/anomalies_by_date.csv")
write_csv(top_anomaly_dates, "output/top_anomaly_dates.csv")
write_csv(top_positive_values, "output/top_positive_values.csv")
write_csv(top_negative_values, "output/top_negative_values.csv")
write_csv(quality_summary, "output/quality_summary.csv")


############################################################
# 15. SALVATAGGIO GRAFICI
############################################################

ggsave(
  filename = "output/plot_hist_all.png",
  plot = plot_hist_all,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_hist_99.png",
  plot = plot_hist_99,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_box_all.png",
  plot = plot_box_all,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_store_anomalies.png",
  plot = plot_store_anomalies,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "output/plot_dept_anomalies.png",
  plot = plot_dept_anomalies,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "output/plot_top_anomaly_dates.png",
  plot = plot_top_anomaly_dates,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "output/plot_problem_series.png",
  plot = plot_problem_series,
  width = 12,
  height = 8,
  dpi = 300
)


############################################################
# 16. CONCLUSIONI OPERATIVE DELLA FASE 1.6
############################################################

# Al termine di questo script dovremmo essere in grado di rispondere
# alle seguenti domande:
#
# - La distribuzione di Weekly_Sales è simmetrica oppure fortemente
#   asimmetrica?
# - Quante osservazioni negative o nulle sono presenti?
# - Gli outlier sono numerosi oppure marginali?
# - Le anomalie si concentrano in particolari store, department o date?
# - Esistono serie Store-Dept particolarmente problematiche?
# - È necessario trattare o monitorare tali osservazioni prima della
#   modellazione previsiva?
#
# Questa fase conclude l'analisi esplorativa e fornisce la base per
# la successiva definizione del problema previsivo, della strategia di
# validazione e dei modelli da confrontare.
############################################################
