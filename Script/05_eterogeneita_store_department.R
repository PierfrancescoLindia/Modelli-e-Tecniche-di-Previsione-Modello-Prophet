############################################################
# PROGETTO: Analisi e Forecasting del dataset Walmart Sales
# CORSO: Modelli e Tecniche di Previsione
#
# FILE: 05_eterogeneita_store_department.R
#
# OBIETTIVO:
# Questo script sviluppa il punto 1.5 dell'analisi esplorativa,
# concentrandosi sull'eterogeneità tra store e department.
#
# In particolare, il codice permette di:
# 1. misurare il contributo dei diversi store alle vendite;
# 2. misurare il contributo dei diversi department;
# 3. confrontare media, variabilità e dispersione delle vendite;
# 4. individuare store e department più stabili o più volatili;
# 5. produrre evidenze utili a decidere il livello di aggregazione
#    più adatto per la fase di forecasting.
#
# MOTIVAZIONE:
# La serie aggregata offre una visione complessiva del fenomeno,
# ma può nascondere forti differenze interne tra punti vendita e
# dipartimenti. In un problema di forecasting è importante capire
# se le serie siano relativamente omogenee oppure molto eterogenee,
# poiché questa caratteristica influenza la scelta dell'unità
# previsiva e dei modelli da confrontare.
############################################################


############################################################
# 0. LIBRERIE
############################################################

library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# Creazione cartella output se non esiste già
if (!dir.exists("output")) dir.create("output")


############################################################
# 1. IMPORTAZIONE DEL DATASET
############################################################

walmart <- read_csv("data/wallmart_sales.csv")


############################################################
# 2. ANALISI PER STORE
############################################################

# In questa sezione sintetizziamo il comportamento delle vendite
# per ciascun store, calcolando:
# - vendite totali nel periodo osservato
# - vendite medie settimanali
# - mediana
# - deviazione standard
# - minimo e massimo
# - coefficiente di variazione (CV), utile per misurare la volatilità
#
# Il coefficiente di variazione è dato da:
# CV = deviazione standard / media
# e consente di confrontare la variabilità relativa tra unità con
# livelli medi diversi di vendita.

store_summary <- walmart %>%
  group_by(Store) %>%
  summarise(
    total_sales = sum(Weekly_Sales, na.rm = TRUE),
    mean_sales = mean(Weekly_Sales, na.rm = TRUE),
    median_sales = median(Weekly_Sales, na.rm = TRUE),
    sd_sales = sd(Weekly_Sales, na.rm = TRUE),
    min_sales = min(Weekly_Sales, na.rm = TRUE),
    max_sales = max(Weekly_Sales, na.rm = TRUE),
    n_obs = n(),
    cv_sales = sd_sales / mean_sales,
    .groups = "drop"
  ) %>%
  arrange(desc(total_sales))

store_summary


############################################################
# 3. CLASSIFICA DEGLI STORE PER VENDITE TOTALI
############################################################

# Questa tabella permette di identificare i punti vendita con il
# contributo maggiore al volume complessivo delle vendite.

top_store_total <- store_summary %>%
  arrange(desc(total_sales)) %>%
  head(10)

bottom_store_total <- store_summary %>%
  arrange(total_sales) %>%
  head(10)

top_store_total
bottom_store_total


############################################################
# 4. CLASSIFICA DEGLI STORE PER VENDITE MEDIE
############################################################

# Qui osserviamo gli store con valore medio per osservazione più alto
# e più basso.

top_store_mean <- store_summary %>%
  arrange(desc(mean_sales)) %>%
  head(10)

bottom_store_mean <- store_summary %>%
  arrange(mean_sales) %>%
  head(10)

top_store_mean
bottom_store_mean


############################################################
# 5. CLASSIFICA DEGLI STORE PER VOLATILITÀ
############################################################

# Usiamo il coefficiente di variazione per individuare gli store più
# stabili e quelli più volatili in termini relativi.

top_store_cv <- store_summary %>%
  arrange(desc(cv_sales)) %>%
  head(10)

bottom_store_cv <- store_summary %>%
  arrange(cv_sales) %>%
  head(10)

top_store_cv
bottom_store_cv


############################################################
# 6. GRAFICI PER STORE
############################################################

# Grafico delle vendite totali per store
plot_store_total <- ggplot(store_summary,
                           aes(x = reorder(factor(Store), total_sales),
                               y = total_sales)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Vendite totali per store",
    subtitle = "Contributo dei singoli punti vendita al totale del campione",
    x = "Store",
    y = "Vendite totali"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_store_total

# Grafico della media delle vendite per store
plot_store_mean <- ggplot(store_summary,
                          aes(x = reorder(factor(Store), mean_sales),
                              y = mean_sales)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Vendite medie per store",
    subtitle = "Confronto del livello medio di vendita tra punti vendita",
    x = "Store",
    y = "Vendite medie"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_store_mean

# Grafico del coefficiente di variazione per store
plot_store_cv <- ggplot(store_summary,
                        aes(x = reorder(factor(Store), cv_sales),
                            y = cv_sales)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Volatilità relativa per store",
    subtitle = "Coefficiente di variazione delle vendite",
    x = "Store",
    y = "Coefficiente di variazione"
  ) +
  theme_minimal()

plot_store_cv


############################################################
# 7. ANALISI PER DEPARTMENT
############################################################

# Ripetiamo la stessa logica per i department, così da confrontare
# il peso economico e la variabilità delle diverse categorie.

dept_summary <- walmart %>%
  group_by(Dept) %>%
  summarise(
    total_sales = sum(Weekly_Sales, na.rm = TRUE),
    mean_sales = mean(Weekly_Sales, na.rm = TRUE),
    median_sales = median(Weekly_Sales, na.rm = TRUE),
    sd_sales = sd(Weekly_Sales, na.rm = TRUE),
    min_sales = min(Weekly_Sales, na.rm = TRUE),
    max_sales = max(Weekly_Sales, na.rm = TRUE),
    n_obs = n(),
    cv_sales = sd_sales / mean_sales,
    .groups = "drop"
  ) %>%
  arrange(desc(total_sales))

dept_summary


############################################################
# 8. CLASSIFICA DEI DEPARTMENT
############################################################

top_dept_total <- dept_summary %>%
  arrange(desc(total_sales)) %>%
  head(15)

bottom_dept_total <- dept_summary %>%
  arrange(total_sales) %>%
  head(15)

top_dept_mean <- dept_summary %>%
  arrange(desc(mean_sales)) %>%
  head(15)

bottom_dept_mean <- dept_summary %>%
  arrange(mean_sales) %>%
  head(15)

top_dept_cv <- dept_summary %>%
  arrange(desc(cv_sales)) %>%
  head(15)

bottom_dept_cv <- dept_summary %>%
  arrange(cv_sales) %>%
  head(15)

top_dept_total
bottom_dept_total
top_dept_mean
bottom_dept_mean
top_dept_cv
bottom_dept_cv


############################################################
# 9. GRAFICI PER DEPARTMENT
############################################################

plot_dept_total <- ggplot(top_dept_total,
                          aes(x = reorder(factor(Dept), total_sales),
                              y = total_sales)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 15 department per vendite totali",
    subtitle = "Department con il maggior contributo alle vendite complessive",
    x = "Department",
    y = "Vendite totali"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_dept_total

plot_dept_mean <- ggplot(top_dept_mean,
                         aes(x = reorder(factor(Dept), mean_sales),
                             y = mean_sales)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 15 department per vendite medie",
    subtitle = "Department con livello medio di vendita più elevato",
    x = "Department",
    y = "Vendite medie"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_dept_mean

plot_dept_cv <- ggplot(top_dept_cv,
                       aes(x = reorder(factor(Dept), cv_sales),
                           y = cv_sales)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 15 department per volatilità relativa",
    subtitle = "Department con coefficiente di variazione più elevato",
    x = "Department",
    y = "Coefficiente di variazione"
  ) +
  theme_minimal()

plot_dept_cv


############################################################
# 10. ANALISI DELLE SERIE STORE-DEPT
############################################################

# Qui scendiamo al livello più granulare.
# Per ogni combinazione Store-Dept calcoliamo alcune statistiche utili
# per capire:
# - quante serie esistono;
# - quanto differiscono tra loro;
# - se alcune risultano molto piccole, molto variabili o molto
#   intermittenti.
#
# Questo passaggio è importante perché il forecasting finale potrebbe
# essere sviluppato proprio a questo livello, oppure a un livello più
# aggregato se emergesse eccessiva irregolarità.

store_dept_summary <- walmart %>%
  group_by(Store, Dept) %>%
  summarise(
    total_sales = sum(Weekly_Sales, na.rm = TRUE),
    mean_sales = mean(Weekly_Sales, na.rm = TRUE),
    median_sales = median(Weekly_Sales, na.rm = TRUE),
    sd_sales = sd(Weekly_Sales, na.rm = TRUE),
    min_sales = min(Weekly_Sales, na.rm = TRUE),
    max_sales = max(Weekly_Sales, na.rm = TRUE),
    n_obs = n(),
    n_zero = sum(Weekly_Sales == 0, na.rm = TRUE),
    n_negative = sum(Weekly_Sales < 0, na.rm = TRUE),
    cv_sales = sd_sales / mean_sales,
    .groups = "drop"
  )

store_dept_summary


############################################################
# 11. DISTRIBUZIONE DELLE MEDIE E DELLA VOLATILITÀ STORE-DEPT
############################################################

# Istogramma della media vendite per serie Store-Dept
plot_sd_mean_dist <- ggplot(store_dept_summary,
                            aes(x = mean_sales)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribuzione delle vendite medie per serie Store-Dept",
    x = "Vendite medie",
    y = "Frequenza"
  ) +
  scale_x_continuous(labels = comma) +
  theme_minimal()

plot_sd_mean_dist

# Istogramma del coefficiente di variazione
plot_sd_cv_dist <- ggplot(store_dept_summary,
                          aes(x = cv_sales)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribuzione della volatilità relativa per serie Store-Dept",
    x = "Coefficiente di variazione",
    y = "Frequenza"
  ) +
  theme_minimal()

plot_sd_cv_dist


############################################################
# 12. SERIE STORE-DEPT PIÙ GRANDI, PIÙ PICCOLE E PIÙ VOLATILI
############################################################

top_sd_total <- store_dept_summary %>%
  arrange(desc(total_sales)) %>%
  head(15)

bottom_sd_total <- store_dept_summary %>%
  arrange(total_sales) %>%
  head(15)

top_sd_cv <- store_dept_summary %>%
  arrange(desc(cv_sales)) %>%
  head(15)

bottom_sd_cv <- store_dept_summary %>%
  arrange(cv_sales) %>%
  head(15)

top_sd_total
bottom_sd_total
top_sd_cv
bottom_sd_cv


############################################################
# 13. SERIE CON ZERI O VALORI NEGATIVI
############################################################

# Questo controllo è utile per individuare combinazioni Store-Dept
# che potrebbero essere problematiche nella modellazione.

series_with_zero <- store_dept_summary %>%
  filter(n_zero > 0) %>%
  arrange(desc(n_zero))

series_with_negative <- store_dept_summary %>%
  filter(n_negative > 0) %>%
  arrange(desc(n_negative))

series_with_zero
series_with_negative


############################################################
# 14. SELEZIONE DI ALCUNE SERIE RAPPRESENTATIVE
############################################################

# Selezioniamo alcune serie esemplificative che potranno essere
# utilizzate nei passi successivi per analisi grafiche più dettagliate.
#
# Criteri:
# - una serie con vendite molto elevate
# - una serie con alta volatilità
# - una serie con presenza di valori negativi
# - una serie relativamente stabile

example_high_sales <- top_sd_total %>% slice(1)
example_high_cv <- top_sd_cv %>% filter(is.finite(cv_sales)) %>% slice(1)
example_negative <- series_with_negative %>% slice(1)
example_stable <- bottom_sd_cv %>% filter(is.finite(cv_sales)) %>% slice(1)

example_high_sales
example_high_cv
example_negative
example_stable


############################################################
# 15. GRAFICO DI ALCUNE SERIE RAPPRESENTATIVE
############################################################

# Costruiamo un piccolo dataset con le serie selezionate per osservare
# visivamente come possano differire tra loro.

selected_series <- bind_rows(
  example_high_sales %>% mutate(Type = "High Sales"),
  example_high_cv %>% mutate(Type = "High Volatility"),
  example_negative %>% mutate(Type = "Negative Values"),
  example_stable %>% mutate(Type = "Stable")
) %>%
  distinct(Store, Dept, Type)

selected_series_data <- walmart %>%
  inner_join(selected_series, by = c("Store", "Dept"))

plot_selected_series <- ggplot(selected_series_data,
                               aes(x = Date, y = Weekly_Sales)) +
  geom_line() +
  facet_wrap(~ Type, scales = "free_y") +
  labs(
    title = "Serie Store-Dept rappresentative",
    subtitle = "Esempi di comportamento eterogeneo nel dataset Walmart",
    x = "Data",
    y = "Vendite settimanali"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

plot_selected_series


############################################################
# 16. SALVATAGGIO OUTPUT
############################################################

write_csv(store_summary, "output/store_summary.csv")
write_csv(dept_summary, "output/dept_summary.csv")
write_csv(store_dept_summary, "output/store_dept_summary.csv")
write_csv(top_store_total, "output/top_store_total.csv")
write_csv(top_store_mean, "output/top_store_mean.csv")
write_csv(top_store_cv, "output/top_store_cv.csv")
write_csv(top_dept_total, "output/top_dept_total.csv")
write_csv(top_dept_mean, "output/top_dept_mean.csv")
write_csv(top_dept_cv, "output/top_dept_cv.csv")
write_csv(series_with_zero, "output/series_with_zero.csv")
write_csv(series_with_negative, "output/series_with_negative.csv")
write_csv(selected_series, "output/selected_series.csv")


############################################################
# 17. SALVATAGGIO GRAFICI
############################################################

ggsave(
  filename = "output/plot_store_total.png",
  plot = plot_store_total,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "output/plot_store_mean.png",
  plot = plot_store_mean,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "output/plot_store_cv.png",
  plot = plot_store_cv,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "output/plot_dept_total.png",
  plot = plot_dept_total,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "output/plot_dept_mean.png",
  plot = plot_dept_mean,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "output/plot_dept_cv.png",
  plot = plot_dept_cv,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "output/plot_sd_mean_dist.png",
  plot = plot_sd_mean_dist,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_sd_cv_dist.png",
  plot = plot_sd_cv_dist,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/plot_selected_series.png",
  plot = plot_selected_series,
  width = 12,
  height = 8,
  dpi = 300
)


############################################################
# 18. CONCLUSIONI OPERATIVE DELLA FASE 1.5
############################################################

# Al termine di questo script dovremmo essere in grado di rispondere
# alle seguenti domande:
#
# - Gli store contribuiscono in modo simile oppure molto diverso alle
#   vendite complessive?
# - I department mostrano forti differenze in termini di peso economico
#   e di volatilità?
# - Le serie elementari Store-Dept appaiono omogenee oppure molto
#   eterogenee?
# - Sono presenti combinazioni Store-Dept particolarmente problematiche
#   per via di zeri, negativi o forte variabilità?
# - A quale livello di aggregazione conviene iniziare la modellazione?
#
# Queste evidenze saranno fondamentali per definire la strategia
# previsiva del progetto e per motivare la scelta delle serie su cui
# applicare e confrontare i modelli di forecasting.
############################################################
