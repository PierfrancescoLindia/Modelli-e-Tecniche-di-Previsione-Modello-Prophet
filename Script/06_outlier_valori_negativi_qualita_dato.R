############################################################
# FILE: 07_outlier_reali_serie_aggregata.R
#
# OBIETTIVO:
# Identificare gli outlier della serie aggregata distinguendo
# tra:
# - outlier giustificabili da calendario/stagionalità;
# - outlier reali da imputare.
#
# OUTPUT:
# 1. tabella completa degli outlier;
# 2. serie finale pronta per Prophet e SARIMA;
# 3. grafici diagnostici.
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

walmart <- read_csv("wallmart_sales.csv")


############################################################
# 2. COSTRUZIONE DELLA SERIE AGGREGATA
############################################################
# Manteniamo anche IsHoliday aggregato, utile per Prophet.
############################################################

serie <- walmart %>%
  group_by(Date) %>%
  summarise(
    Sales = sum(Weekly_Sales, na.rm = TRUE),
    IsHoliday = any(IsHoliday),
    .groups = "drop"
  ) %>%
  arrange(Date)


############################################################
# 3. IDENTIFICAZIONE DEGLI OUTLIER CON CRITERIO IQR
############################################################

Q1 <- quantile(serie$Sales, 0.25, na.rm = TRUE)
Q3 <- quantile(serie$Sales, 0.75, na.rm = TRUE)
IQR_value <- IQR(serie$Sales, na.rm = TRUE)

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

serie <- serie %>%
  mutate(
    raw_outlier_flag = case_when(
      Sales < lower_bound ~ "Lower outlier",
      Sales > upper_bound ~ "Upper outlier",
      TRUE ~ "Regular"
    )
  )

table(serie$raw_outlier_flag)


############################################################
# 4. IDENTIFICAZIONE DEI PERIODI "GIUSTIFICABILI"
############################################################
# Logica:
# non tutti gli outlier sono errori.
# Consideriamo "giustificabili" gli outlier in periodi coerenti
# con il profilo commerciale emerso nell'EDA.
#
# Regole adottate:
# - settimane holiday dichiarate dal dataset;
# - finestra Black Friday / Thanksgiving: 20-30 novembre;
# - finestra natalizia: 8-25 dicembre;
# - fine anno / post-Natale: 26-31 dicembre;
# - fine gennaio post-holiday: 20-31 gennaio;
# - finestra pasquale: 1-10 aprile.
#
# Tutto ciò che cade fuori da queste finestre viene trattato
# come vero outlier candidato all'imputazione.
############################################################

serie <- serie %>%
  mutate(
    month_num = month(Date),
    day_num   = day(Date),
    justified_period = case_when(
      IsHoliday ~ "Holiday week",
      month_num == 11 & day_num >= 20 & day_num <= 30 ~ "Black Friday / Thanksgiving window",
      month_num == 12 & day_num >= 8  & day_num <= 25 ~ "Christmas sales window",
      month_num == 12 & day_num >= 26 & day_num <= 31 ~ "Year-end adjustment window",
      month_num == 1  & day_num >= 20 & day_num <= 31 ~ "Late January post-holiday window",
      month_num == 4  & day_num >= 1  & day_num <= 10 ~ "Easter window",
      TRUE ~ "Outside justified periods"
    )
  )


############################################################
# 5. DISTINZIONE TRA OUTLIER GIUSTIFICATI E OUTLIER REALI
############################################################
# Un vero outlier è:
# - classificato come outlier IQR
# - e fuori dai periodi commercialmente giustificabili
############################################################

serie <- serie %>%
  mutate(
    real_outlier = if_else(
      raw_outlier_flag != "Regular" &
        justified_period == "Outside justified periods",
      TRUE,
      FALSE
    ),
    final_outlier_class = case_when(
      raw_outlier_flag == "Regular" ~ "Regular",
      raw_outlier_flag != "Regular" & !real_outlier ~ "Justified outlier",
      raw_outlier_flag != "Regular" & real_outlier ~ "Real outlier"
    )
  )

table(serie$final_outlier_class)


############################################################
# 6. TABELLA COMPLETA DEGLI OUTLIER
############################################################

outlier_table_complete <- serie %>%
  filter(raw_outlier_flag != "Regular") %>%
  select(
    Date, Sales, IsHoliday,
    raw_outlier_flag,
    justified_period,
    final_outlier_class
  )

outlier_table_complete

write_csv(outlier_table_complete,
          "output/outlier_table_classificati.csv")


############################################################
# 7. GRAFICO: OUTLIER GIUSTIFICATI VS OUTLIER REALI
############################################################

plot_outlier_classified <- ggplot(serie, aes(x = Date, y = Sales)) +
  geom_line(linewidth = 0.7, color = "black") +
  geom_point(
    data = serie %>% filter(final_outlier_class == "Justified outlier"),
    color = "orange",
    size = 2.5
  ) +
  geom_point(
    data = serie %>% filter(final_outlier_class == "Real outlier"),
    color = "red",
    size = 2.8
  ) +
  labs(
    title = "Serie aggregata con classificazione degli outlier",
    subtitle = "Arancione = outlier giustificati, Rosso = outlier reali",
    x = "Data",
    y = "Vendite"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_outlier_classified

ggsave("output/grafico_outlier_classificati.png",
       plot_outlier_classified, width = 10, height = 6)


############################################################
# 8. IMPUTAZIONE SOLO DEI VERI OUTLIER
############################################################
# Imputiamo solo gli outlier reali.
# Gli outlier giustificati vengono lasciati invariati.
#
# Metodo:
# media tra osservazione precedente e successiva.
############################################################

serie_model <- serie %>%
  mutate(
    Sales_original = Sales,
    Sales_model = Sales
  )

for (i in 2:(nrow(serie_model) - 1)) {
  if (serie_model$real_outlier[i]) {
    serie_model$Sales_model[i] <-
      mean(c(serie_model$Sales_model[i - 1],
             serie_model$Sales_model[i + 1]),
           na.rm = TRUE)
  }
}


############################################################
# 9. GRAFICO CONFRONTO SERIE ORIGINALE VS SERIE FINALE
############################################################

plot_model_series <- ggplot() +
  geom_line(data = serie_model,
            aes(x = Date, y = Sales_original),
            linewidth = 0.6, color = "grey60") +
  geom_line(data = serie_model,
            aes(x = Date, y = Sales_model),
            linewidth = 0.9, color = "blue") +
  geom_point(
    data = serie_model %>% filter(final_outlier_class == "Justified outlier"),
    aes(x = Date, y = Sales_original),
    color = "orange",
    size = 2.3
  ) +
  geom_point(
    data = serie_model %>% filter(final_outlier_class == "Real outlier"),
    aes(x = Date, y = Sales_original),
    color = "red",
    size = 2.5
  ) +
  labs(
    title = "Serie originale e serie finale per i modelli",
    subtitle = "Blu = serie usata per Prophet/SARIMA; Rosso = veri outlier imputati; Arancione = outlier mantenuti",
    x = "Data",
    y = "Vendite"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_model_series

ggsave("output/grafico_serie_finale_modelli.png",
       plot_model_series, width = 10, height = 6)


############################################################
# 10. DATASET FINALE PRONTO PER I MODELLI
############################################################
# Questo file è quello che poi userai per:
# - Prophet  -> colonna Sales_model come y
# - SARIMA   -> colonna Sales_model come serie finale
#
# Manteniamo anche:
# - Sales_original
# - IsHoliday
# - etichette di classificazione
# così hai piena tracciabilità metodologica.
############################################################

dataset_modelli <- serie_model %>%
  select(
    Date,
    Sales_original,
    Sales_model,
    IsHoliday,
    raw_outlier_flag,
    justified_period,
    final_outlier_class,
    real_outlier
  )

dataset_modelli

write_csv(dataset_modelli,
          "output/dataset_serie_aggregata_per_modelli.csv")


############################################################
# 11. VERSIONE MINIMALE PER SARIMA E PROPHET
############################################################
# File essenziale:
# - Date
# - Sales
# - IsHoliday
############################################################

dataset_modelli_min <- dataset_modelli %>%
  transmute(
    Date,
    Sales = Sales_model,
    IsHoliday
  )

write_csv(dataset_modelli_min,
          "output/dataset_serie_aggregata_modelli_min.csv")


############################################################
# 12. CONTROLLO FINALE
############################################################

table(dataset_modelli$final_outlier_class)

dataset_modelli %>%
  filter(real_outlier)
