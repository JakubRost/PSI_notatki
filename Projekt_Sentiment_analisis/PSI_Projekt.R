#' ---
#' title: "Sentiment analysis of Chair Jerome Powell's statements (10.2023 - 04.2026)"
#' author: "Author"
#' date: "date"
#' output:
#'    html_document:
#'      df_print: paged
#'      theme: cerulean
#'      highlight: default
#'      toc: yes
#'      toc_depth: 3
#'      toc_float:
#'         collapsed: false
#'         smooth_scroll: true
#'      code_fold: show
#' ---

library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(SnowballC)
library(tidytext)
library(dplyr)
library(textdata)

#' # 1. Stworzenie funkcji do przetwarzania tekstu ------
process_text_stem <- 
  function(file_path) {
  #' 1.1 Wczytanie tekstu i zamiana na małe litery ----
  text <- tolower(readLines(file_path, encoding = "UTF-8", warn = FALSE))
  
  #' 1.2 Usunięcie niestandardowych "śmieci" transkrypcyjnych (custom_stopwords)----
  custom_stopwords <- c("—", "–", "’s", "’re", "s", "re", "well", "will", "today", "or") 
  text <- removeWords(text, custom_stopwords)
  
  #' 1.3 Usunięcie interpunkcji i cyfr----
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  
  #' 1.4 Usunięcie standardowych słów stopu (the, and, is itp.)----
  text <- removeWords(text, stopwords("en"))
  
  #' 1.5 Stemming (sprowadzanie do rdzenia, np. "economic" i "economy" -> "econom")----
  text <- stemDocument(text, language = "english")
  
  #' 1.6 Podział na pojedyncze słowa i usunięcie pustych przestrzeni----
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[words != ""]
  
  return(words)
}

process_text <- 
  function(file_path) {
  text <- tolower(readLines(file_path, encoding = "UTF-8", warn = FALSE))
  custom_stopwords <- c("—", "–", "’s", "’re", "s", "re", "well", "will", "today", "or")
  text <- removeWords(text, custom_stopwords)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, stopwords("en")) # <-- BRAK STEMMINGU
  
  words <- unlist(strsplit(text, "\\s+"))
  return(words[words != ""])
}

#' # 2. Stworzenie funkcji do obliczania częstości występowania słów ----
word_frequency <- function(words) {
  freq <- table(words)
  freq_df <- data.frame(word = names(freq), freq = as.numeric(freq))
  freq_df <- freq_df[order(-freq_df$freq), ]
  return(freq_df)
}

#' # 3. Stworzenie funkcji do tworzenia chmury słów ----
plot_wordcloud <- function(freq_df, title, color_palette = "Dark2") {
  wordcloud(words = freq_df$word, 
            freq = freq_df$freq, 
            min.freq = 2,
            colors = brewer.pal(8, color_palette),
            scale = c(2, 0.3),     
            max.words = 16,          
            )
  title(title)
}

#' # 4. Definiowanie wektorów ścieżek (dla różnych okresów)
pliki_X_2023_V_2024 <- c("10.2023.txt","01.2024.txt", "03.2024.txt", "04.2024.txt", "05.2024.txt")
pliki_VI_2024_XII_2024 <- c("06.2024.txt", "07.2024.txt", "09.2024.txt", "10.2024.txt", "12.2024.txt")
pliki_I_2025_VII_2025 <- c("01.2025.txt", "03.2025.txt", "05.2025.txt", "06.2025.txt", "07.2025.txt")
pliki_IX_2025_IV_2026 <- c("09.2025.txt", "11.2025.txt", "12.2025.txt", "01.2026.txt", "03.2026.txt", "04.2026.txt")

#' # 5. Połączenie ich w listę z czytelnymi nazwami
lista_danych <- list(
  "(10.2023-05.2024)" = pliki_X_2023_V_2024,
  "(06.2024-12.2024)" = pliki_VI_2024_XII_2024,
  "(01.2025-07.2025)" = pliki_I_2025_VII_2025,
  "(09.2025-04.2026)" = pliki_IX_2025_IV_2026
)


#' # 6. Stworzenie funkcji do analizy sentymentu LMD ----
analyze_sentiment_lmd <- function(words, nazwa_grupy) {
  #' 6.1 Zamiana wektora słów na ramkę danych (tibble/data.frame)----
  df_words <- data.frame(word = words, stringsAsFactors = FALSE)
  
  #' 6.2 Pobranie słownika Loughran-McDonald----
  lmd_dict <- get_sentiments("loughran")
  
  #' 6.3 Przypisanie sentymentu do słów i zliczenie ich wystąpień----
  sentiment_counts <- df_words %>%
    inner_join(lmd_dict, by = "word") %>%
    count(sentiment, sort = TRUE)
  
  #' 6.4 Obliczenie wskaźnika Net Sentiment (Pozytywne - Negatywne)----
  pos <- sum(sentiment_counts$n[sentiment_counts$sentiment == "positive"], na.rm = TRUE)
  neg <- sum(sentiment_counts$n[sentiment_counts$sentiment == "negative"], na.rm = TRUE)
  net_sentiment <- pos - neg
  
  #' 6.5 Wyświetlenie wyników tekstowych----
  cat("=== Analiza Sentymentu (Loughran-McDonald) dla:", nazwa_grupy, "===\n")
  print(sentiment_counts)
  cat("Wskaźnik Net Sentiment (Positive - Negative):", net_sentiment, "\n\n")
  
  #' 6.6 Generowanie wykresu słupkowego dla sentymentu----
  if(nrow(sentiment_counts) > 0) {
    p <- ggplot(sentiment_counts, aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      coord_flip() + # Obrócenie wykresu dla lepszej czytelności
      labs(title = paste("Sentyment LMD (Jerome Powell) -", nazwa_grupy),
           subtitle = paste("Net Sentiment Score:", net_sentiment),
           x = "Typ sentymentu",
           y = "Liczba słów") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
    
    print(p)
  }
}
#' # 7. Generowanie wyników----
procesuj_i_rysuj <- function(sciezki_plikow, nazwa_grupy) {
  cat("--- Przetwarzanie:", nazwa_grupy, "---\n")
  
  #' 7.1 Przetwarzanie tekstów----
  slowa_stem <- unlist(lapply(sciezki_plikow, process_text_stem))
  slowa      <- unlist(lapply(sciezki_plikow, process_text))
  
  #' 7.2 Obliczanie częstości----
  freq_df_stem <- word_frequency(slowa_stem)
  freq_df      <- word_frequency(slowa)
  
  #' 7.3 Chmura słów i TOP 10 - ZE STEMMINGIEM----
  plot_wordcloud(freq_df_stem, paste("Chmura słów -", nazwa_grupy, "ze stemmingiem"), color_palette = "Dark2")
  cat("Najczęściej występujące słowa (ze stemmingiem) -", nazwa_grupy, ":\n")
  print(head(freq_df_stem, 10))
  cat("\n")
  
  #' 7.4 Chmura słów i TOP 10 - BEZ STEMMINGU----
  plot_wordcloud(freq_df, paste("Chmura słów -", nazwa_grupy, "bez stemmingu"), color_palette = "Dark2")
  cat("Najczęściej występujące słowa (bez stemmingu) -", nazwa_grupy, ":\n")
  print(head(freq_df, 10))
  cat("\n\n")
  
  #' 7.5 Analiza sentymentu ze słownikiem Loughran-McDonald----
  analyze_sentiment_lmd(slowa, nazwa_grupy)
}

# Pętla iteruje po nazwach elementów listy ("(10.2023-05.2024)", "(06.2024-12.2024)" itd.)
for (nazwa in names(lista_danych)) {
  pliki <- lista_danych[[nazwa]] # Pobieramy wektor plików dla danej nazwy
  procesuj_i_rysuj(pliki, nazwa)  # Wywołujemy naszą funkcję
}

#' # 8. Stopy procentowe USA

#' ## 8.1 Wczytanie danych z pliku CSV
dane_fed <- read.csv("FEDFUNDS.csv", stringsAsFactors = FALSE)

#' ## 8.2 Zamiana kolumny z datą na prawdziwy typ Date w R
dane_fed$observation_date <- as.Date(dane_fed$observation_date, format="%Y-%m-%d")

#' ## 8.3 Tworzenie wykresu przy użyciu ggplot2
wykres_stopy <- ggplot(data = dane_fed, aes(x = observation_date, y = FEDFUNDS)) +
  # Dodanie linii wykresu
  geom_line(color = "#2fa4e7", linewidth = 1.2) +
  # Dodanie punktów na każdym odczycie miesiąca
  geom_point(color = "#1d6fa5", size = 2) +
  # Dodanie etykiet i tytułów
  labs(
    title = "Efektywna stopa funduszy federalnych (FEDFUNDS)",
    subtitle = "Poziom stóp procentowych w USA w ujęciu miesięcznym",
    x = "Data obserwacji",
    y = "Procent (%)",
    caption = "Źródło: FRED (Federal Reserve Economic Data)"
  ) +
  # Zastosowanie czystego, minimalistycznego motywu wizualnego
  theme_minimal() +
  # Dodatkowe formatowanie wyglądu
  theme(
    plot_title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1) # Obrócenie dat dla lepszej czytelności
  )

#' ## 8.4 Wyświetlenie wykresu
print(wykres_stopy)


if(!require(readxl)) install.packages("readxl")
library(readxl)
library(ggplot2)

#' # 9. Inflacja CPI USA
#' ## 9.1. Wczytanie danych z pliku .xlsx
dane_inflacja <- read_excel("CPI_USA.xlsx", col_names = FALSE)
colnames(dane_inflacja) <- c("DATE", "CPI")

#' ## 9.2. Wymuszenie, aby DATE był czystym formatem Date, 
# a CPI czystą liczbą (usuwamy wszystkie tekstowe anomalie)
dane_inflacja$DATE <- as.Date(dane_inflacja$DATE)
dane_inflacja$CPI  <- as.numeric(dane_inflacja$CPI)

# Odrzucamy ewentualne puste wiersze
dane_inflacja <- na.omit(dane_inflacja)

#' ## 9.3. Tworzenie wykresu przy użyciu ggplot2
wykres_CPI <- ggplot(data = dane_inflacja, aes(x = DATE, y = CPI)) +
  # Rysowanie linii
  geom_line(color = "#2fa4e7", linewidth = 1.2, group = 1) +
  # Kropki miesięczne
  geom_point(color = "#1d6fa5", size = 2) +
  # Wymuszenie, aby oś X była traktowana jako oś czasu
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  # Tytuły i opisy
  labs(
    title = "Inflacja CPI w USA",
    subtitle = "Poziom inflacji CPI r/r w USA w ujęciu miesięcznym",
    x = "Data obserwacji",
    y = "Procent (%)",
    caption = "Źródło: FRBST (Federal Reserve Bank of St. Louis)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1) # Obrócenie dat, żeby na siebie nie nachodziły
  )

#' ## 9.4. Wyświetlenie gotowego wykresu
print(wykres_CPI)

