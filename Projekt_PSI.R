#' ---
#' title: "Sentiment analisys for Jerome Powell Chair statements"
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

## Stworzenie funkcji do przetwarzania tekstu ----
process_text_stem <- function(file_path) {
  # 1. Wczytanie tekstu i zamiana na małe litery
  text <- tolower(readLines(file_path, encoding = "UTF-8", warn = FALSE))
  
  # 2. Usunięcie niestandardowych "śmieci" transkrypcyjnych (custom_stopwords)
  custom_stopwords <- c("—", "–", "’s", "’re", "s", "re", "well", "will", "today", "or") 
  text <- removeWords(text, custom_stopwords)
  
  # 3. Usunięcie interpunkcji i cyfr
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  
  # 4. Usunięcie standardowych słów stopu (the, and, is itp.)
  text <- removeWords(text, stopwords("en"))
  
  # 5. Stemming (sprowadzanie do rdzenia, np. "economic" i "economy" -> "econom")
  text <- stemDocument(text, language = "english")
  
  # 6. Podział na pojedyncze słowa i usunięcie pustych przestrzeni
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[words != ""]
  
  return(words)
}

process_text <- function(file_path) {
  text <- tolower(readLines(file_path, encoding = "UTF-8", warn = FALSE))
  custom_stopwords <- c("—", "–", "’s", "’re", "s", "re", "well", "will", "today", "or")
  text <- removeWords(text, custom_stopwords)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, stopwords("en")) # <-- BRAK STEMMINGU
  
  words <- unlist(strsplit(text, "\\s+"))
  return(words[words != ""])
}

# Stworzenie funkcji do obliczania częstości występowania słów ----
word_frequency <- function(words) {
  freq <- table(words)
  freq_df <- data.frame(word = names(freq), freq = as.numeric(freq))
  freq_df <- freq_df[order(-freq_df$freq), ]
  return(freq_df)
}

# Stworzenie funkcji do tworzenia chmury słów ----
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

# Definiowanie wektorów ścieżek (dla różnych okresów)
pliki_X_2023_V_2024 <- c("10.2023.txt","01.2024.txt", "03.2024.txt", "04.2024.txt", "05.2024.txt")
pliki_VI_2024_XII_2024 <- c("06.2024.txt", "07.2024.txt", "09.2024.txt", "10.2024.txt", "12.2024.txt")
pliki_I_2025_VII_2025 <- c("01.2025.txt", "03.2025.txt", "05.2025.txt", "06.2025.txt", "07.2025.txt")
pliki_IX_2025_IV_2026 <- c("09.2025.txt", "11.2025.txt", "12.2025.txt", "01.2026.txt", "03.2026.txt", "04.2026.txt")

# Połączenie ich w listę z czytelnymi nazwami
lista_danych <- list(
  "(10.2023-05.2024)" = pliki_X_2023_V_2024,
  "(06.2024-12.2024)" = pliki_VI_2024_XII_2024,
  "(01.2025-07.2025)" = pliki_I_2025_VII_2025,
  "(09.2025-04.2026)" = pliki_IX_2025_IV_2026
)

procesuj_i_rysuj <- function(sciezki_plikow, nazwa_grupy) {
  cat("--- Przetwarzanie:", nazwa_grupy, "---\n")
  
  # 1. Przetwarzanie tekstów
  slowa_stem <- unlist(lapply(sciezki_plikow, process_text_stem))
  slowa      <- unlist(lapply(sciezki_plikow, process_text))
  
  # 2. Obliczanie częstości
  freq_df_stem <- word_frequency(slowa_stem)
  freq_df      <- word_frequency(slowa)
  
  # 3. Chmura słów i TOP 10 - ZE STEMMINGIEM
  plot_wordcloud(freq_df_stem, paste("Chmura słów -", nazwa_grupy, "ze stemmingiem"), color_palette = "Dark2")
  cat("Najczęściej występujące słowa (ze stemmingiem) -", nazwa_grupy, ":\n")
  print(head(freq_df_stem, 10))
  cat("\n")
  
  # 4. Chmura słów i TOP 10 - BEZ STEMMINGU
  plot_wordcloud(freq_df, paste("Chmura słów -", nazwa_grupy, "bez stemmingu"), color_palette = "Dark2")
  cat("Najczęściej występujące słowa (bez stemmingu) -", nazwa_grupy, ":\n")
  print(head(freq_df, 10))
  cat("\n\n")
}

# Pętla iteruje po nazwach elementów listy ("(10.2023-05.2024)", "(06.2024-12.2024)" itd.)
for (nazwa in names(lista_danych)) {
  pliki <- lista_danych[[nazwa]] # Pobieramy wektor plików dla danej nazwy
  procesuj_i_rysuj(pliki, nazwa)  # Wywołujemy naszą funkcję
}

