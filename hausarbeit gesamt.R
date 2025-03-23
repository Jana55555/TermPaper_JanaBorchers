#################HAUSARBEIT############################
#######################################################
#Jana Borchers
#Forschungspraktikum: Computational Social Science
# Matrikelnummer: 8007047
# Goethe-Universität Frankfurt
# Fachbereich 03 Gesellschaftswissenschaften

# Forschungspraktikum: 
# Computational Social Science
# Dozent: Dr. Christian Czymara



#Packages installieren
packages <- c(
  "LexisNexisTools",
  "quanteda",
  "quanteda.textplots",
  "tm",
  "tidyverse",
  "dplyr",
  "tidymodels",    
  "discrim",       
  "naivebayes",
  "topicmodels",
  "keyATM",
  "magrittr",
  "randomForest",
  "readxl",
  "ranger",
  "topicmodels",
  "ldatuning",
  "readxl",
  "ggplot2",
  "reshape2",
  "text2vec",
  "Rtsne",
  "keyATM",
  "textmineR",
  "Matrix",
  "reshape2"
)

install.packages(setdiff(packages, rownames(installed.packages()))) #installieren, falls nötig 
lapply(packages, library, character.only = TRUE) # laden

#seed
set.seed(123)

#) 1 + 2.
#####Datenvorbereitung#####
##Artikel einlesen

doc1 <- lnt_read("C:/Users/janab/Documents/Uni/CSS/Dateien palästina 1-500.docx")
doc2 <- lnt_read("C:/Users/janab/Documents/Uni/CSS/Dateien palästina 501-1000.docx")
doc3 <- lnt_read("C:/Users/janab/Documents/Uni/CSS/Dateien palästina 1001-1500.docx")
doc4 <- lnt_read("C:/Users/janab/Documents/Uni/CSS/Dateien palästina 1501-1866.docx")
doc5 <- lnt_read("C:/Users/janab/Documents/Uni/CSS/Dateien anti-israel 1-255.docx")

docs <- list(doc1, doc2, doc3, doc4, doc5) #Dokumente zusammenführen

meta_combined <- list() #leere Liste 
articles_combined <- list()
paragraphs_combined <- list()

for (i in 1:length(docs)) {
  # Offset für jedes Dokument
  offset <- (i - 1) * 1000
  
  # IDs für jedes Dokument anpassen
  docs[[i]]@meta$ID <- docs[[i]]@meta$ID + offset
  docs[[i]]@articles$ID <- docs[[i]]@articles$ID + offset
  docs[[i]]@paragraphs$Art_ID <- docs[[i]]@paragraphs$Art_ID + offset
  
  # Daten in die Listen einfügen 
  meta_combined[[i]] <- docs[[i]]@meta
  articles_combined[[i]] <- docs[[i]]@articles
  paragraphs_combined[[i]] <- docs[[i]]@paragraphs
}

# wieder zu einem Dataframe zusammenführen
meta_combined <- bind_rows(meta_combined)
articles_combined <- bind_rows(articles_combined)
paragraphs_combined <- bind_rows(paragraphs_combined)

doc_combined <- new("LNToutput",
                    meta = meta_combined,
                    articles = articles_combined,
                    paragraphs = paragraphs_combined)




##Duplikate entfernen

#duplicates <- lnt_similarity(LNToutput = doc_combined, threshold=.97)
#doc_reduced <- doc_combined[!doc_combined@meta$ID %in% duplicates$ID_duplicate, ]
#Duplikate werden nicht entfernt, alternativer Ansatz:

doc_combined@articles <- doc_combined@articles %>%
  distinct(Article, .keep_all = TRUE)

# ID basierend auf den verbleibenden Artikeln neu zuweisen
doc_combined@meta <- doc_combined@meta %>%
  filter(ID %in% doc_combined@articles$ID)

# ID in der Meta-Daten-Tabelle zurücksetzen 
doc_combined@meta$ID <- seq_len(nrow(doc_combined@meta))
doc_combined@articles$ID <- seq_len(nrow(doc_combined@articles))

# Filtern der paragraphs-Tabelle, sodass nur die Absätze übrig bleiben, deren Art_ID in den IDs der articles-Tabelle enthalten ist
doc_combined@paragraphs <- doc_combined@paragraphs %>%
  filter(Art_ID %in% doc_combined@articles$ID)






##Thematisch relevante Artikel identifizieren
articles_df <- doc_combined@articles
articles <- articles_df$Article

# Sample 
set.seed(123) 
sample_size <- floor(0.2 * nrow(articles_df))  # 20% der Gesamtzahl der Artikel
sample_data <- articles_df[sample(nrow(articles_df), sample_size), ]
rest_data <- articles_df[!(articles_df$ID %in% sample_data$ID), ] #restliche 80%

#write.csv(sample_data, "C:/Users/janab/Documents/Uni/CSS/sampling.csv", row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)
#manuelle Codierung!

codierte_daten <- read_xlsx("C:/Users/janab/Documents/Uni/CSS/sampling.xlsx")

clean_text <- function(text) {
  text <- tolower(text)  
  text <- removePunctuation(text)  # Entfernen von Satzzeichen
  text <- removeNumbers(text)  # Entfernen von Zahlen
  text <- removeWords(text, stopwords)  # Entfernen von Stoppwörtern
  text <- removeWords(text, garbagewords)  # Entfernen von Stoppwörtern
  text <- stripWhitespace(text)  # Entfernen von überflüssigem Leerraum
  return(text)
}

#Anwendung Funktion auf codierte Daten und Restdaten
codierte_daten$clean_text <- sapply(codierte_daten$Article, clean_text)
rest_data$clean_text <- sapply(rest_data$Article, clean_text)


# corpus und dtm und Matrix erstellen
dtm_codierte <- DocumentTermMatrix(Corpus(VectorSource(codierte_daten$clean_text)))
dtm_rest <- DocumentTermMatrix(Corpus(VectorSource(rest_data$clean_text)), 
                               control = list(dictionary = Terms(dtm_codierte)))

df_codierte <- as.data.frame(as.matrix(dtm_codierte))
df_codierte$thema <- as.factor(codierte_daten$Thema) # "thema" aus den codierten Daten hinzufügen
df_rest <- as.data.frame(as.matrix(dtm_rest)) # DTM für restliche Daten


# Modell
#rf_model <- randomForest(thema ~ ., data = df_codierte, ntree = 100)
# -> Dateien zu groß, Modell kann nicht berechnet werden

#alternativ: ranger-Modell: 
x <- df_codierte[, !names(df_codierte) %in% "thema"] # Entfernen der Zielvariable aus den Prädiktoren
y <- df_codierte$thema

rf_model <- ranger(x = x, y = y, num.trees = 100)

predictions <- predict(rf_model, newdata = df_rest) # Vorhersage für die restlichen Daten
rest_data$predicted_thema <- predictions


#Valdierung
set.seed(42) 
sample_rest_data <- rest_data[sample(1:nrow(rest_data), 100), ] # Sample von 100 zufälligen Beobachtungen aus rest_data ziehen

write.csv(sample_rest_data, "C:/Users/janab/Documents/Uni/CSS/predictions.csv", row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)


# als "nicht dem Thema entsprechend" identifizierte Dateien aus dem lntoutput löschen
sample_no <- codierte_daten[codierte_daten$Thema == "Nein", ]
rest_no <- rest_data[rest_data$predicted_thema == "Nein", ]
sample_no_ids <- sample_no$ID
rest_no_ids <- rest_no$ID
all_no_ids <- c(sample_no_ids, rest_no_ids)

doc_combined@articles <- doc_combined@articles %>% #Artikel mit den "Nein"-IDs aus doc_combined@articles
  filter(!ID %in% all_no_ids)
# Zeilen in doc_combined@meta, die Artikel-IDs enthalten, die das Thema "Nein" haben, entfernen
doc_combined@meta <- doc_combined@meta %>%
  filter(ID %in% doc_combined@articles$ID)
# ID in der articles-Tabelle neu setzen
doc_combined@articles$ID <- seq_len(nrow(doc_combined@articles))

doc_combined@meta$ID <- seq_len(nrow(doc_combined@meta)) #ID in der meta-Tabelle neu setzen

doc_combined@paragraphs <- doc_combined@paragraphs %>%
  filter(Art_ID %in% doc_combined@articles$ID) #die paragraphs-Tabelle filtern, sodass nur die Absätze übrig bleiben, deren Art_ID in den IDs der articles-Tabelle enthalten ist






##################################
##Textbereinigung, Tokenisierung
# 2.1 

articles <- tolower(slot(doc_combined, "articles")$Article) # Zugriff auf die Artikel im LNToutput-Objekt + Metadaten
meta_data <- doc_combined@meta


#Collocations ersetzen
collocations <- c("from the river to the sea", "fridays for future", "free palestine", "palestine will be free")


replace_collocations <- function(articles, collocations) {
  for (collocation in collocations) {
    placeholder <- paste0("COLLOC_", gsub(" ", "_", collocation))
    collocation_escaped <- gsub("\\.", "\\\\.", collocation)
    articles <- gsub(collocation_escaped, placeholder, articles, fixed = FALSE)  # fixed = FALSE für reguläre Ausdrücke
  }
  return(articles)
} # Funktion zum Ersetzen der Collocations durch lesbare Platzhalter


docnames <- paste0("doc_", seq_along(articles))
text_with_placeholders <- sapply(articles, replace_collocations, collocations = collocations) #Funktion anwenden: Collocations durch Platzhalter setzen

#Zeilenumbrüche ersetzen und Corpus erstellen
text_with_placeholders_ <- gsub("\n", " ", text_with_placeholders) # Zeilenumbrüche ersetzen
articles_corpus <- corpus(text_with_placeholders_, docnames = docnames) # Corpus erstellen





#Tokenisierung + Bereinigung

#Tokens erstellen
articles_toks <- tokens(articles_corpus,
                        remove_punct = TRUE,
                        remove_numbers = TRUE,
                        remove_symbols = TRUE,
                        remove_separators = TRUE,
                        include_docvars = T)


#Stopwords entfernen
filepath <- "C:/Users/janab/Documents/Uni/CSS/stopwords-de.txt" #german stopwords
stopwords <- readLines(filepath)
garbagewords <- c("graphic", "gesamtseiten-pdf", "weblink", "innen", "pdf", "pdf-dokument", "bild")

articles_toks <- tokens_remove(articles_toks, german_stop, case_insensitive = TRUE)
articles_toks <- tokens_remove(articles_toks, garbagewords, case_insensitive = TRUE)


# articles_toks <- tokens_trim(articles_toks,
#                              min_docfreq = 0.01,
#                              max_docfreq = 0.7,
#                              docfreq_type = "prop")



#individuelles Stemming

# Liste der patterns
patterns <- c("palästinensisch", "palästinensische", "palästinensischen", "palästinenser", "palästinensern", 
              "palästinas", 
              "israelisch", "israelische", "israelischen", "israelis", 
              "israels", 
              "luftangriff", "luftangriffe", "luftangriffen", "bombenangriff", "bombenangriffe", "bombenangriffen", 
              "einsaetze", 
              "jüdische", "jüdischen", 
              "antisemitische", "antisemitischen",
              "pro-palästinensisch", "pro-palästinensische", "pro-palästinensischen", "propalästinensische", "propalästinensischen",
              "gaza-streifen", "gazastreifen")

# Liste der replacements
replacements <- c(rep("palästin", 5), 
                  "palästina", 
                  rep("israeli", 4), 
                  "israel", 
                  rep("luftangriff", 6), 
                  "einsatz", 
                  rep("jüdisch", 2), 
                  rep("antisemitisch", 2),
                  rep("propalästinensisch", 5),
                  rep("gaza", 2)
                  )


length(patterns)  # Prüfen, ob Längen übereinstimmen: Sollte 29 sein
length(replacements)  # Sollte auch 29 sein


for (i in 1:length(patterns)) {
  articles_toks <- tokens_replace(articles_toks, pattern = patterns[i], replacement = replacements[i])
} # Anwenden der Ersetzungen mit purrr::reduce2
# Schleife, um jedes Muster und Ersetzung auf die Tokens anzuwenden


head(articles_toks)

#DFM erstellen 
articles_dfm <- dfm(articles_toks)


#) 2.2
#) Deskriptive Auswertung:

# 20 häufigste Begriffe
topfeatures(articles_dfm, 20)

#Bigrams
colloc_find <- textstat_collocations(articles_toks, size = 2) %>%
  arrange(desc(lambda)) # size = 2 for bigrams
top_collocations2 <- colloc_find %>%
  arrange(desc(count)) 
head(50)

top_collocations2 <- colloc_find %>%
  arrange(desc(count)) 
head(50)  

print(top_collocations2)

#3-grams
colloc_find_3 <- textstat_collocations(articles_toks, size = 3)  # size = 3 

top_collocations3 <- colloc_find_3 %>%
  arrange(desc(count)) %>% ##sorted by count
  head(50) 

print(top_collocations3)
### -> Ersetzen der Kollokationen durch Platzhalter in Abschnitt 2.1


#Visualisierung der Beziehungen der Top-Begriffe
articles_fcm <- fcm(articles_dfm)
top_terms <- names(topfeatures(articles_dfm, 50))
fcm_top_term <- fcm_select(articles_fcm, pattern = top_terms)

textplot_network(fcm_top_term, min_freq = 0.9, edge_alpha = 0.3, edge_size = 3)


####################################
###########KEY ATM - Model##########
####################################
#) 3. 


#) Konvertieren

keyATMdocs <- keyATM_read(articles_dfm)


#) Keywords definieren 

keywords <- list(
  kritik = c("völkermord", "genozid", "besatzung", "kriegsverbrechen", "luftangriff", "kolonialismus", "apartheid"), 
  forderungen = c("waffenruhe", "waffenstillstand", "waffenlieferungen", "humanitäre hilfe"), 
  antisemitismus = c("antisemitismus", "antisemitisch", "hass", "hetze", "juden", "jüdinnen", "jüdisch"),
  repressionen = c("repressionen", "polizei", "verboten", "polizeigewalt", "gericht", "prozess")
)


#) KeyATM Model
k <- 10

key_topics <- keyATM(
  docs              = keyATMdocs,  # text input
  no_keyword_topics = k-4,          # number of topics without keyword topics
  keywords          = keywords,     # keywords
  model             = "base",       # select the model
  options           = list(seed = 123)
)


top_words(key_topics, 20)





#Ideale Anzahl Topics bestimmen#


#Exklusivität

topic_nums <- 5:30 # hier ursprünglich noch Berechnung von 5 bis 30,
#im späteren Zusammenführen mit der Kohärenz werden nur noch 5er Schritte berücksichtigt.
#Alternativer Code, der nur 5er Schritte berücksichtigt:

###############################################################
# topic_nums <- seq(5, 30, by = 5)
# 
# calculate_exclusivity <- function(k) {
#   key_topics <- keyATM(
#     docs              = keyATMdocs,
#     no_keyword_topics = k - 4,
#     keywords          = keywords,
#     model             = "base",
#     options           = list(seed = 123)
#   )
# 
#   phi <- key_topics$phi  
# 
#   exclusivity_scores <- apply(phi, 1, function(topic_probs) {
#     sorted_probs <- sort(topic_probs, decreasing = TRUE)
#     top_words <- names(sorted_probs)[1:20]  # Top 20 Wörter
#     exclusivity_values <- sapply(top_words, function(word) {
#       word_index <- which(colnames(phi) == word)
#       word_probs <- phi[, word_index]
#       return(max(word_probs) / sum(word_probs))
#     })
#     
#     return(mean(exclusivity_values))
#   })
#   
#   return(mean(exclusivity_scores))
# }
# 
# exclusivity_scores <- sapply(topic_nums, calculate_exclusivity)
# exklusivity_df <- data.frame(Topics = topic_nums, Exclusivity = exclusivity_scores)
###########################################################################

#Berechnung Exklusivität:
calculate_exclusivity <- function(k) {
  key_topics <- keyATM(
    docs              = keyATMdocs,
    no_keyword_topics = k - 4,
    keywords          = keywords,
    model             = "base",
    options           = list(seed = 123)
  )
  
  # Phi-Matrix: Wahrscheinlichkeiten der Wörter pro Thema
  phi <- key_topics$phi  
  
  # Exklusivität berechnen
  exclusivity_scores <- apply(phi, 1, function(topic_probs) {
    sorted_probs <- sort(topic_probs, decreasing = TRUE)
    top_words <- names(sorted_probs)[1:20]  # Top 20 Wörter
    
    # Überprüfen, wie stark diese Wörter in anderen Themen erscheinen
    exclusivity_values <- sapply(top_words, function(word) {
      word_index <- which(colnames(phi) == word)
      word_probs <- phi[, word_index]
      
      # Exklusivität = Wahrscheinlichkeit des Wortes im aktuellen Thema
      # geteilt durch die Summe seiner Wahrscheinlichkeiten in allen Themen
      return(max(word_probs) / sum(word_probs))
    })
    
    # Durchschnittliche Exklusivität der Top 20 Wörter
    return(mean(exclusivity_values))
  })
  
  # Durchschnittliche Exklusivität über alle Themen
  return(mean(exclusivity_scores))
}


exclusivity_scores <- sapply(topic_nums, calculate_exclusivity) # Berechnung der Exklusivitätswerte
exklusivity_df <- data.frame(Topics = topic_nums, Exclusivity = exclusivity_scores) # Ergebnisse in einem Dataframe zusammenfassen



#Berechnung Kohärenz:
topic_range <- seq(5, 30, by = 5)
coherence_values <- numeric(length(topic_range))

# Schleife zur Berechnung der Kohärenzwerte
for (i in seq_along(topic_range)) {
  k <- topic_range[i]
  
  # keyATM-Modell trainieren
  key_topics <- keyATM(
    docs = keyATMdocs,
    no_keyword_topics = k - 4, 
    keywords = keywords,
    model = "base",
    options = list(seed = 123)
  )
  
  # Kohärenz berechnen
  coherence_values[i] <- mean(CalcProbCoherence(
    phi = key_topics$phi, 
    dtm = as(articles_dfm, "dgCMatrix"),
    M = 10
  ))
}

# Dataframe für die Visualisierung
coherence_df <- data.frame(
  Topics = topic_range,
  Coherence = coherence_values
)

# Visualisierung
ggplot(coherence_df, aes(x = Topics, y = Coherence)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Kohärenzwerte für verschiedene Themenanzahlen",
    x = "Anzahl der Themen (k)",
    y = "Kohärenz"
  ) +
  theme_minimal()

#######

#Zusammenbringen mit Exklusivität:
excoh_df <- merge(
  coherence_df,            # Enthält Kohärenzwerte für 5, 10, 15, 20, 25, 30
  exklusivity_df,         # Enthält Exklusivitätswerte für 5 bis 30
  by.x = "Topics",         # Themenanzahlen aus coherence_df
  by.y = "Topics",          # Themenanzahlen aus exklusivity_df
  all.x = TRUE             # Nur Werte aus coherence_df einbeziehen
)

# Visualisierung mit ggplot2
ggplot(excoh_df, aes(x = Topics)) +
  geom_point(aes(y = Coherence), color = "red", size = 3) +
  geom_line(aes(y = Coherence), color = "red", linewidth = 1) +  # Linie für Kohärenz
  geom_point(aes(y = Exclusivity), color = "blue", size = 3) +
  geom_line(aes(y = Exclusivity), color = "blue", linewidth = 1) +  # Linie für Exklusivität
  labs(
    title = "Kohärenz und Exklusivität nach Themenanzahl",
    x = "Anzahl der Themen",
    y = "Wert"
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~., name = "Exklusivität")
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "red"),
    axis.title.y.right = element_text(color = "blue")
  ) # schlechte Interpretierbarkeit, da unterschiedliche Skalen!

#Werte normalisieren für bessere Darstellung:
excoh_df$Coherence_scaled <- scales::rescale(excoh_df$Coherence, to = c(0, 1))
excoh_df$Exclusivity_scaled <- scales::rescale(excoh_df$Exclusivity, to = c(0, 1))


# Visualisierung der normalisierten Werte
ggplot(excoh_df, aes(x = Topics)) +
  geom_point(aes(y = Coherence_scaled, color = "Kohärenz"), size = 3) +
  geom_line(aes(y = Coherence_scaled, color = "Kohärenz"), linewidth = 1) +
  geom_point(aes(y = Exclusivity_scaled, color = "Exklusivität"), size = 3) +
  geom_line(aes(y = Exclusivity_scaled, color = "Exklusivität"), linewidth = 1) +
  scale_color_manual(
    values = c("Kohärenz" = "red", "Exklusivität" = "blue"),
    name = "Metrik"
  ) +
  labs(
    title = "Normalisierte Kohärenz und Exklusivität nach Themenanzahl",
    x = "Anzahl der Themen",
    y = "Normalisierte Werte (0-1)"
  ) +
  theme_minimal()



##################################
#######Finales KeyATM-Modell######
##################################
#) 4.

################################################################################
k <- 20

key_topics <- keyATM(
  docs              = keyATMdocs,  # text input
  no_keyword_topics = k-4,          # number of topics without keyword topics
  keywords          = keywords,     # keywords
  model             = "base",       # select the model
  options           = list(seed = 123)
)


top_words(key_topics, 20)


################################################################################

#Inhaltliche Interpretation


#Top Article pro Thema
# 
topic_assignments <- key_topics$theta
dominant_topics <- apply(topic_assignments, 1, which.max)
docs_with_topics <- data.frame(doc_id = 1:nrow(topic_assignments), 
                               dominant_topic = dominant_topics) # Dataframe

topic_labels <- colnames(topic_assignments)  # Entspricht den Namen der Themen
docs_with_topics$topic_name <- topic_labels[dominant_topics]


head(docs_with_topics, n=40)


#Texte mit höchsten Wahrscheinlichkeiten

top_articles_per_topic <- list()

for (topic in 1:ncol(topic_assignments)) {
  # Dokumente mit der höchsten Wahrscheinlichkeit für dieses Thema
  top_docs <- order(topic_assignments[, topic], decreasing = TRUE)[1:5]  # z.B. die 5 Dokumente mit der höchsten Wahrscheinlichkeit
  # Texte extrahieren
  top_articles <- articles_df$Article[top_docs]
  # Texte in der Liste speichern
  top_articles_per_topic[[colnames(topic_assignments)[topic]]] <- top_articles
}


head(top_articles_per_topic) # Topics mit Top-Artikeln (= Texte mit der höchsten Wahrscheinlichkeit für jeweiliges Topic)


#Beispieltexte anschauen
articles_df$Article[docs_with_topics$doc_id == 27]
articles_df$Article[articles_df$ID == 9]
docs_with_topics$topic_name[articles_df$ID == 120]

#Welche Artikel wurden antisemitismus zugeordnet?
topic_3_ids <- docs_with_topics[docs_with_topics$topic_name == "3_antisemitismus", "doc_id"]
print(topic_3_ids)




##Visualisierungen


# Korrelation der Themenverteilungen (Theta-Matrix)
topic_correlation <- cor(key_topics$theta)


ggplot(melt(topic_correlation), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(title = "Themenkorrelationen", x = "Thema", y = "Thema")



# tsne-Visualisierung
unique_theta <- unique(key_topics$theta)
tsne_result <- Rtsne(unique_theta, dims = 2)
tsne_data$Topic <- apply(unique_theta, 1, which.max) # dominantes Topic zuordnen


tsne_data <- data.frame(
  X = tsne_result$Y[, 1],  # 1. Dimension
  Y = tsne_result$Y[, 2],  # 2. Dimension
  Document = 1:nrow(unique_theta)  # Dokumente
)

ggplot(tsne_data, aes(x = X, y = Y)) +
  geom_point(size = 2, color = "steelblue") +
  labs(title = "t-SNE Visualisierung der Dokumente im Themenraum",
       x = "Dimension 1",
       y = "Dimension 2") +
  theme_minimal()

#bessere farbliche Absetzung der Themen#
custom_colors <- c("red", "blue", "green", "orange", viridis::viridis(length(unique(tsne_data$Topic)) - 4))

# Visualisierung mit angepasster Farbskala
ggplot(tsne_data, aes(x = X, y = Y, color = as.factor(Topic))) +
  geom_point(size = 2) +
  labs(
    title = "t-SNE Visualisierung der Dokumente nach Topics",
    x = "Dimension 1",
    y = "Dimension 2",
    color = "Topic"
  ) +
  scale_color_manual(values = custom_colors) +
  theme_minimal()

#Thema 18 bzw Other_14 nicht in der Graphik enthalten:

table(tsne_data$Topic) # wie viele Dokumente wurden jeweils welchem Thema zugeordnet
#bzw weisen die höchste Wahrscheinlichkeit für dieses Thema auf?
#18 kommt hier nicht vor -> für kein Dokument die höchste Wahrscheinlichkeit 

# statt Zuweisung bei Maximum: Neuen Schwellenwert definieren 
threshold <- 0.05  

tsne_data$Topic <- apply(unique_theta, 1, function(x) {
  if (x[18] >= threshold) {
    return(18)  
  } else {
    return(which.max(x))  # Sonst das dominante Thema nehmen
  }
})

custom_colors <- c("red", "blue", "green", "orange", viridis::viridis(length(unique(tsne_data$Topic)) - 4))

# Visualisierung mit angepasster Farbskala
ggplot(tsne_data, aes(x = X, y = Y, color = as.factor(Topic))) +
  geom_point(size = 2) +
  labs(
    title = "t-SNE Visualisierung der Dokumente nach Topics",
    x = "Dimension 1",
    y = "Dimension 2",
    color = "Topic"
  ) +
  scale_color_manual(values = custom_colors) +
  theme_minimal()


# Kohärenz per Topic
dtm_sparse <- as(articles_dfm, "dgCMatrix")
coherence_values <- CalcProbCoherence(phi = key_topics$phi, dtm = dtm_sparse, M = 10)
coherence_per_topic <- coherence_values
