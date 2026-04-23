############################################################
# PROGETTO: Analisi e Forecasting del dataset Walmart Sales
# CORSO: Modelli e Tecniche di Previsione
#
# FILE: 01_struttura_dataset_walmart.R
#
# OBIETTIVO:
# Questo script svolge la prima parte dell'analisi esplorativa,
# concentrandosi sulla struttura del dataset Walmart Sales.
#
# In particolare, il codice permette di:
# 1. importare e controllare il dataset;
# 2. descrivere dimensioni e variabili;
# 3. verificare l'intervallo temporale coperto dai dati;
# 4. individuare il numero di store, department e serie temporali;
# 5. controllare qualità e coerenza del dato
#    (missing values, duplicati, valori anomali);
# 6. produrre una tabella riassuntiva utile per la relazione.
#
# MOTIVAZIONE:
# Questa fase è fondamentale perché, prima di costruire modelli
# previsivi, è necessario comprendere la struttura informativa del
# dataset e verificare la presenza di eventuali criticità che
# potrebbero influenzare le analisi successive.
############################################################


############################################################
# 0. IMPOSTAZIONE AMBIENTE DI LAVORO
############################################################

# Impostiamo la working directory.
setwd("C:/Users/lindi/Desktop/Tecniche di Previsione")

# Carichiamo le librerie necessarie.
# - readr: per leggere file CSV in modo rapido e pulito
# - dplyr: per manipolazione dati
# - lubridate: per gestire e convertire le date
library(readr)
library(dplyr)
library(lubridate)


############################################################
# 1. IMPORTAZIONE DEL DATASET
############################################################

# Leggiamo il file CSV contenente le vendite Walmart.
# Il dataset viene salvato nell'oggetto "walmart".
walmart <- read_csv("wallmart_sales.csv")

# Prime ispezioni rapide del dataset.
# Questi comandi servono a prendere confidenza con il contenuto
# del file appena importato.
head(walmart)      # mostra le prime righe
str(walmart)       # mostra struttura e tipo delle variabili
glimpse(walmart)   # panoramica compatta del dataset


############################################################
# 2. DIMENSIONE DEL DATASET E NOMI DELLE VARIABILI
############################################################

# In questa sezione vogliamo capire:
# - quante osservazioni sono presenti;
# - quante variabili abbiamo a disposizione;
# - quali sono i nomi delle colonne.
#
# Questa è la descrizione più elementare della struttura del dataset
# e costituisce il primo passo di qualunque EDA.

# Dimensioni del dataset: numero di righe e colonne
dim(walmart)

# Numero di osservazioni (righe)
nrow(walmart)

# Numero di variabili (colonne)
ncol(walmart)

# Nomi delle variabili
names(walmart)


############################################################
# 3. TIPOLOGIA DELLE VARIABILI
############################################################

# Qui verifichiamo il tipo di ogni variabile.
# Questo controllo è importante perché:
# - le variabili temporali devono essere riconosciute come date;
# - le variabili categoriche (Store, Dept, IsHoliday) devono essere
#   interpretate correttamente;
# - la variabile di interesse Weekly_Sales deve essere numerica.

# Struttura completa del dataset
str(walmart)

# Classe di ogni variabile
sapply(walmart, class)


############################################################
# 4. CONTROLLO DELLA VARIABILE DATA
############################################################

# In molti dataset la colonna Date viene letta come testo e deve
# essere convertita esplicitamente in formato Date.
#
# In questo caso, però, read_csv() ha già riconosciuto correttamente
# la variabile come data durante l'importazione.
# Per questo motivo non applichiamo una nuova conversione con mdy(),
# perché risulterebbe ridondante e potrebbe generare errori.

# Verifichiamo il formato della variabile Date
str(walmart$Date)
summary(walmart$Date)
class(walmart$Date)

############################################################
# 5. INTERVALLO TEMPORALE COPERTO DAI DATI
############################################################

# Un dataset di serie storiche va sempre descritto anche dal punto
# di vista temporale. In particolare vogliamo sapere:
# - la data iniziale;
# - la data finale;
# - il numero di settimane osservate.
#
# Questo ci aiuta a capire l'orizzonte informativo disponibile per
# il forecasting.

# Data minima e massima
min(walmart$Date)
max(walmart$Date)

# Numero di date distinte, quindi numero di settimane osservate
length(unique(walmart$Date))


############################################################
# 6. NUMERO DI STORE, DEPARTMENT E SERIE TEMPORALI
############################################################

# Nel dataset ogni osservazione rappresenta una specifica combinazione
# di:
# - Store
# - Dept
# - Date
#
# Per un progetto di forecasting è essenziale capire a quanti livelli
# di aggregazione possiamo lavorare.
#
# In particolare vogliamo sapere:
# - quanti punti vendita (Store) sono presenti;
# - quanti dipartimenti (Dept) sono presenti;
# - quante serie temporali distinte esistono a livello Store-Dept.

# Numero di store distinti
n_distinct(walmart$Store)

# Numero di department distinti
n_distinct(walmart$Dept)

# Numero di serie temporali distinte Store-Dept
walmart %>%
  distinct(Store, Dept) %>%
  nrow()


############################################################
# 7. CONTROLLO DEI VALORI MANCANTI
############################################################

# Prima di procedere con analisi e modelli è necessario verificare
# se il dataset contiene missing values (NA).
#
# La presenza di valori mancanti può influenzare:
# - statistiche descrittive;
# - grafici;
# - stima dei modelli previsivi.
#
# Facciamo sia un controllo totale, sia un controllo per variabile.

# Numero totale di valori mancanti nel dataset
sum(is.na(walmart))

# Numero di valori mancanti per ciascuna variabile
colSums(is.na(walmart))


############################################################
# 8. CONTROLLO DEI DUPLICATI
############################################################

# In un dataset panel-temporale come questo, una possibile criticità
# è la presenza di:
# 1. righe duplicate esatte;
# 2. duplicati logici sulla chiave Store-Dept-Date.
#
# Se una combinazione Store-Dept-Date comparisse più di una volta,
# avremmo un problema di coerenza del dato da risolvere prima della
# modellazione.

# Numero di righe duplicate esatte
sum(duplicated(walmart))

# Verifica di duplicati sulla chiave logica Store-Dept-Date
duplicati_chiave <- walmart %>%
  count(Store, Dept, Date) %>%
  filter(n > 1)

duplicati_chiave

# Se "duplicati_chiave" risulta vuoto, significa che non esistono
# ripetizioni della stessa combinazione Store-Dept-Date.


############################################################
# 9. ANALISI PRELIMINARE DELLA VARIABILE WEEKLY_SALES
############################################################

# La variabile Weekly_Sales è la variabile target del progetto.
# In questa fase non facciamo ancora analisi approfondite, ma è utile
# verificare alcune caratteristiche di base:
# - distribuzione sintetica;
# - presenza di valori pari a zero;
# - presenza di valori negativi.
#
# I valori negativi meritano particolare attenzione, perché in un
# contesto di vendite possono indicare resi, rettifiche contabili o
# anomalie di registrazione.

# Statistiche descrittive di base
summary(walmart$Weekly_Sales)

# Numero di osservazioni con vendite uguali a zero
sum(walmart$Weekly_Sales == 0, na.rm = TRUE)

# Numero di osservazioni con vendite negative
sum(walmart$Weekly_Sales < 0, na.rm = TRUE)


############################################################
# 10. DISTRIBUZIONE DELLA VARIABILE IsHoliday
############################################################

# La variabile IsHoliday indica se una determinata settimana è
# associata a una festività.
#
# Questo controllo è utile perché le festività sono spesso una
# componente rilevante nelle serie temporali di vendita e potranno
# essere trattate esplicitamente nelle fasi successive del progetto.

# Frequenze assolute
table(walmart$IsHoliday)

# Frequenze relative
prop.table(table(walmart$IsHoliday))


############################################################
# 11. CONTROLLO DELLA REGOLARITÀ TEMPORALE DEL DATASET
############################################################

# Verifichiamo quante osservazioni sono presenti per ciascuna data.
# Questo controllo serve a capire se il dataset ha una struttura
# regolare nel tempo oppure se alcune settimane presentano un numero
# anomalo di record.
#
# Una struttura regolare è importante per l'analisi delle serie e per
# i confronti tra periodi temporali.

obs_per_week <- walmart %>%
  count(Date)

# Prime righe del conteggio per settimana
head(obs_per_week)

# Riassunto statistico del numero di osservazioni per settimana
summary(obs_per_week$n)


############################################################
# 12. NUMEROSITÀ DELLE OSSERVAZIONI PER STORE
############################################################

# Qui controlliamo quante osservazioni sono presenti per ciascun store.
# Questo aiuta a verificare se tutti gli store sono rappresentati in
# modo simile o se esistono differenze nella numerosità.

obs_per_store <- walmart %>%
  count(Store)

head(obs_per_store)
summary(obs_per_store$n)


############################################################
# 13. NUMEROSITÀ DELLE OSSERVAZIONI PER DEPARTMENT
############################################################

# In modo analogo, controlliamo la frequenza delle osservazioni per
# ciascun department.
# Questo permette di valutare se tutti i dipartimenti sono presenti
# con la stessa intensità oppure se alcuni compaiono meno spesso.

obs_per_dept <- walmart %>%
  count(Dept)

head(obs_per_dept)
summary(obs_per_dept$n)


############################################################
# 14. LUNGHEZZA DELLE SERIE TEMPORALI STORE-DEPT
############################################################

# Una delle verifiche più importanti per il forecasting è controllare
# la lunghezza delle serie temporali elementari.
#
# Ogni coppia Store-Dept definisce una serie temporale distinta.
# Vogliamo sapere:
# - quante osservazioni ha ciascuna serie;
# - se tutte le serie hanno la stessa lunghezza;
# - se esistono serie incomplete.
#
# Questo è un punto molto rilevante perché alcune tecniche previsive
# funzionano meglio con serie sufficientemente lunghe e regolari.

# Numero totale di osservazioni per combinazione Store-Dept
series_length <- walmart %>%
  count(Store, Dept)

head(series_length)
summary(series_length$n)

# Numero di settimane distinte osservate per ogni serie Store-Dept
series_weeks <- walmart %>%
  group_by(Store, Dept) %>%
  summarise(n_weeks = n_distinct(Date), .groups = "drop")

head(series_weeks)
summary(series_weeks$n_weeks)


############################################################
# 15. CREAZIONE DI UNA TABELLA RIASSUNTIVA FINALE
############################################################

# Per la relazione e per la documentazione GitHub è utile costruire
# una tabella riassuntiva che raccolga in modo sintetico tutti gli
# indicatori principali ottenuti fin qui.

summary_table <- tibble(
  Indicatore = c(
    "Numero osservazioni",
    "Numero variabili",
    "Data iniziale",
    "Data finale",
    "Numero settimane",
    "Numero store",
    "Numero department",
    "Numero serie Store-Dept",
    "Valori mancanti totali",
    "Righe duplicate esatte",
    "Vendite uguali a zero",
    "Vendite negative"
  ),
  Valore = c(
    nrow(walmart),
    ncol(walmart),
    as.character(min(walmart$Date)),
    as.character(max(walmart$Date)),
    length(unique(walmart$Date)),
    n_distinct(walmart$Store),
    n_distinct(walmart$Dept),
    walmart %>% distinct(Store, Dept) %>% nrow(),
    sum(is.na(walmart)),
    sum(duplicated(walmart)),
    sum(walmart$Weekly_Sales == 0, na.rm = TRUE),
    sum(walmart$Weekly_Sales < 0, na.rm = TRUE)
  )
)

summary_table


############################################################
# 16. SALVATAGGIO DELLA TABELLA RIASSUNTIVA
############################################################

# Salviamo la tabella riassuntiva in CSV.
# Questo è utile per:
# - riutilizzare i risultati nella relazione;
# - documentare il progetto in modo replicabile;
# - avere un output strutturato già pronto.

write_csv(summary_table, "summary_table_struttura_dataset.csv")


############################################################
# 17. OGGETTO RIASSUNTIVO IN FORMATO LIST
############################################################

# Creiamo anche una lista R con gli indicatori principali.
# Questo formato può essere utile in script successivi o per
# eventuali controlli automatici.

dataset_summary <- list(
  n_observations = nrow(walmart),
  n_variables = ncol(walmart),
  variables = names(walmart),
  start_date = min(walmart$Date),
  end_date = max(walmart$Date),
  n_weeks = length(unique(walmart$Date)),
  n_stores = n_distinct(walmart$Store),
  n_departments = n_distinct(walmart$Dept),
  n_store_dept_series = walmart %>% distinct(Store, Dept) %>% nrow(),
  missing_values_total = sum(is.na(walmart)),
  duplicated_rows = sum(duplicated(walmart)),
  zero_sales = sum(walmart$Weekly_Sales == 0, na.rm = TRUE),
  negative_sales = sum(walmart$Weekly_Sales < 0, na.rm = TRUE)
)

dataset_summary


############################################################
# 18. CONCLUSIONI OPERATIVE DELLA FASE 1.1
############################################################

# Al termine di questo script dovremmo essere in grado di rispondere
# alle seguenti domande:
#
# - Quante osservazioni e quante variabili contiene il dataset?
# - Qual è il periodo temporale coperto dai dati?
# - Quanti store, department e serie temporali distinte esistono?
# - Sono presenti valori mancanti o duplicati?
# - Esistono vendite nulle o negative?
# - La struttura del dataset è regolare e coerente per le analisi
#   successive?
#
# Queste informazioni costituiscono la base della sezione 1.1
# della relazione e permettono di passare alla fase successiva:
# l'analisi dell'andamento temporale delle vendite aggregate.
############################################################
