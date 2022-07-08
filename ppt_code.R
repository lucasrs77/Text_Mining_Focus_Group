
#write_csv(focus_tokenizado, "focus_tokenizado.csv")
#write_csv(df_text, "df_text.csv")
   

##### data frames #####

focus_tokenizado <- read_csv("focus_tokenizado.csv")

df_text <- read_csv("df_text.csv")

stop <- stopwords("es") %>%  # palabras comunes para eliminar del análisis
  as.tibble()

colnames(stop) <- "word"

stop2 <- c("osea", "como", "digamos", "masc",
           "tambien", "okey", "entiende") %>% 
  as.tibble()

colnames(stop2) <- "word"



##### distribución de frecuencias ######

freq_ciudad <- focus_tokenizado %>% 
  group_by(ciudad) %>% 
  count(word, sort = TRUE) %>% 
  mutate(proportion = n / sum(n)) %>% 
  pivot_wider(names_from = ciudad, values_from = proportion) %>% 
  pivot_longer(`San Francisco`:`Rio Cuarto`,
               names_to = "ciudad", values_to = "proportion")

h <- ggplot(freq_ciudad, aes(x = proportion, y = Cordoba, 
                             color = abs(Cordoba - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.3, size = 1.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = T, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~ciudad, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Cordoba Capital", x = NULL)


##### wordcloud #####

fre <- focus_tokenizado %>% 
  count(word, sort = TRUE) %>% 
  mutate(proportion = n / sum(n))

wc <- fre %>% 
  with(wordcloud(word, n, max.words = 15))


##### bigram connection #####

bigram <- df_text %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  select(n_focus, bigram) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop$word,
         !word2 %in% stop$word,
         !word1 %in% stop2$word,
         !word2 %in% stop2$word) %>% 
  filter(str_length(word1) > 3,
         str_length(word2) > 3) %>% 
  na.omit() %>% 
  count(word1, word2, sort = TRUE) 


bigram$word1 <- bigram$word1 %>% 
  str_replace_all(c("á"="a", "é"="e","í"="i","ó"="o", "ú"="u"))

bigram$word2 <- bigram$word2 %>% 
  str_replace_all(c("á"="a", "é"="e","í"="i","ó"="o", "ú"="u"))

bigram_graph <- bigram %>% 
  filter(n > 9) %>% 
  na.omit() %>% 
  graph_from_data_frame()

bigram_graph2 <- bigram %>% 
  filter(n > 24) %>% 
  na.omit() %>% 
  graph_from_data_frame()

arw <- grid::arrow(type = "closed", length = unit(.15, "inches"))

set.seed(2017)

bigraf <- ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = arw, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

bigraf2 <- ggraph(bigram_graph2, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = arw, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


##### word correlation #####

word_cors <- focus_tokenizado %>% 
  group_by(word) %>% 
  filter(n() > 19) %>% 
  pairwise_cor(word, linea, sort = T)


pol_word_cor <- word_cors %>%
  filter(item1 %in% c("larreta", "alberto", "juez", "macri")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 7) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

pol_word_cor


pol_word_cor2 <- word_cors %>%
  filter(item1 %in% c("inseguridad", "economia", "pandemia",
                      "obra", "inflacion", "salud")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 7) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

pol_word_cor2


##### word corr net #####

set.seed(77)

word_net <- word_cors %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


##### topic #####

word_df <- focus_tokenizado %>% 
  group_by(n_focus) %>%   
  count(word, sort = TRUE)

word_dtm <- word_df %>% cast_dtm(document = n_focus, 
                                 term = word, 
                                 value = n)

word_dtm2 <- gofastr::filter_tf_idf(word_dtm)

focus_lda <- LDA(x = word_dtm2, k = 6, control = list(seed = 77))

focus_topics <- tidy(focus_lda,           
                     matrix = "beta")

focus_top_terms <- focus_topics %>%    # objeto anterior
  group_by(topic) %>%                  # agrupamos por tópico
  slice_max(beta, n = 15) %>%           # definimos el máximo de términos que vamos a incluir
  ungroup() %>%                        # desagrupamos
  arrange(topic, beta) %>%             # ordenamos
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()







