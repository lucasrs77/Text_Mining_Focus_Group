   

# Mostramos las principales palabras
focus_tokenizado %>% count(word, sort = TRUE) %>% head()

## creamos objeto que calcule la proporción
frecuencia <- focus_tokenizado %>% 
  count(word, sort = TRUE) %>% 
  mutate(proportion = n / sum(n))

## Plot de palabras de todos los docs
nube <- frecuencia %>% with(wordcloud(word, n, max.words = 20))

## Miramos las oraciones como unidades en vez de palabras

df_text

sentences <- df_text %>% 
  unnest_tokens(sentence, text, token = "sentences")


## The bind_tf_idf() function
# Analiza el contenido del documento, dando menor peso a 
# palabras más usadas 


word_df <- focus_tokenizado %>% 
  group_by(n_focus) %>%   
  count(word, sort = TRUE)

focus_tf_idf <- word_df %>% 
  bind_tf_idf(word, n_focus, n) %>% 
  arrange(desc(tf_idf))

focus_tf_idf %>% head()

## bigrams
# tokenizamos cada dos palabras

bigrams <- df_text %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams %>% count(bigram, sort = T) %>% head()

# separamos para que cada palabra sea una columna

bigrams_separeted <- bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filter <- bigrams_separeted %>% 
  filter(!word1 %in% stop$word) %>% 
  filter(!word2 %in% stop$word) %>% 
  na.omit()

bigrams_filter %>% 
  count(word1, word2, sort = TRUE) %>% head()

## todo junto
bigram <- df_text %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop$word,
         !word2 %in% stop$word,
         !word1 %in% stop2$word,
         !word2 %in% stop2$word) %>% 
  filter(str_length(word1) > 3,
         str_length(word2) > 3) %>% 
  na.omit()



## Trigrams

trigram <- df_text %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop$word,
         !word1 %in% stop2$word,
         !word2 %in% stop$word,
         !word2 %in% stop2$word,
         !word3 %in% stop$word,
         !word3 %in% stop2$word) %>%
  count(word1, word2, word3, sort = TRUE) %>% na.omit()

## Analisis de bigramas y trigramas

bigrams_filter %>%
  filter(word1 == "cfk") %>%
  count(word2, sort = TRUE)

#contar bigramas
bigram_counts <- bigram %>% 
  count(word1, word2, sort = TRUE)

bigram_graph <- bigram_counts %>%
  filter(n > 9) %>% 
  na.omit() %>% 
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


## count words co-occuring within sections

word_pairs <- focus_tokenizado %>% 
  pairwise_count(word, linea, sort = T) %>% 
  filter(!item1 %in% stop$word,
         !item1 %in% stop2$word,
         !item2 %in% stop$word,
         !item1 %in% stop2$word)

word_pairs %>% 
  filter(item1 == "cordoba")

## Correlación entre palabras

word_cors <- focus_tokenizado %>% 
  group_by(word) %>% 
  filter(n() > 20) %>% 
  pairwise_cor(word, linea, sort = T)

word_cors

## esto no es muy útil, hasta que usamos los filtros para enfocar el análisis

word_cors %>% 
  filter(item1 == "juez")


## Convertir a formato DTM -Document Term Matrix-
# que sirve luego para aplicaciones de machine learning

word_dtm <- word_df %>% cast_dtm(n_focus, word, n)
word_dtm

word_dtm2 <- gofastr::filter_tf_idf(word_dtm)
word_dtm2

##### TOPIC MODEL ####

focus_lda <- LDA(word_dtm, k = 6, control = list(seed = 77))


focus_lda2 <- LDA(word_dtm2, k = 6, control = list(seed = 77))



focus_topics <- tidy(focus_lda2, matrix = "beta")



focus_top_terms <- focus_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

focus_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


beta_wide <- focus_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide




focus_topics_gamma <- tidy(focus_lda, matrix = "gamma")
focus_topics_gamma

focus_topics_gamma %>%
  mutate(title = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ document) +
  labs(x = "topic", y = expression(gamma))





topic_classifications <- focus_topics_gamma %>%
  group_by(document) %>%
  slice_max(gamma) %>%
  ungroup()

topic_classifications
## topic 1 -- Rio 4
## topic 2 -- JxC Medio alto 35-50
## topic 3 -- 20-30 años | mixto
## topic 4 -- variado
## topic 5 -- san francisco
## topic 6 -- 35-50 años

#### seleccionamos topico para hacer el filtro luego
topic_selection <- subset(topic_classifications, topic == 6)

## filtramos para ver bajo que variables de control entran los focus de cada topico
focus_categoria <- df_text[2:6] %>% 
  pivot_longer(!n_focus, names_to = "categ", values_to = "values") %>% 
  dplyr::group_by(n_focus, values) %>% 
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>% 
  filter(n_focus %in% topic_selection$document) %>% 
  pivot_wider(names_from = values, values_from = n)


all_focus_topics <- topic_classifications %>%
  count(document, topic) %>%
  group_by(document) %>%
  slice_max(n, n = 1) %>% 
  ungroup() %>%
  transmute(consensus = document, topic)

a <- topic_classifications %>%
  inner_join(all_focus_topics, by = "topic") %>%
  filter(document != consensus)



##### Graficos #####
