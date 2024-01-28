library(tidyverse)
library(rvest)

# set working directory to script folder
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

EMBED <- read.table("glove.6B.50d.txt", header = FALSE, quote = "")

URL_RAW <- read_html("https://tryhardguides.com/nyt-connections-answers/") %>%
  html_elements("#post-827131 > div.entry-content > ul") %>%
  html_elements("li") %>%
  html_text()

CONNECTIONS <- tibble(
  raw_txt = URL_RAW
) %>%
  mutate(Game_ID = str_extract(raw_txt, "^NYT Connections [[:digit:]]+")) %>%
  fill(Game_ID, .direction = "down")  %>%
  filter(!str_detect(raw_txt, "^NYT Connections")) %>%
  mutate(Difficulty = rep(1:4, length.out = nrow(.)),
         words_raw = str_extract(raw_txt, "(?<= - ).*"))

CONNECTIONS <- bind_cols(
  CONNECTIONS,
  str_split(CONNECTIONS$words_raw, pattern = ",", n = 4, simplify = TRUE) %>%
    as_tibble(.name_repair = make.names) %>%
    setNames(., nm = paste0("Word_", 1:4))
)

CONNECTIONS <- CONNECTIONS %>% 
  pivot_longer(starts_with("Word_"), names_to = "Word_number", values_to = "Word") %>%
  mutate(Word = str_squish(Word) %>% str_to_lower(),
         Connect_ID = paste(Game_ID, Difficulty, sep = "_")
         ) %>%
  select(Game_ID, Connect_ID, Word_number, Difficulty, Word) %>%
  left_join(., EMBED,
            by = c("Word" = "V1"))

MISSING_SETS <- CONNECTIONS %>% filter(is.na(V2)) %>%
  pull(Connect_ID) %>% unique()

CON_CLEAN <- CONNECTIONS %>%
  filter(!Connect_ID %in% MISSING_SETS)


write.csv(CON_CLEAN, "connections_answers.csv", row.names = FALSE)
####

PCA <-prcomp(CON_CLEAN[, 6:55])

CON_CLEAN <- bind_cols(
  CON_CLEAN,
  as_tibble(PCA$x[,1:2]) %>% setNames(., c("PC1", "PC2"))
)


CON_CLEAN %>%
  filter(Game_ID %in% unique(CON_CLEAN$Game_ID)[1:3]) %>%
  ggplot(aes(x = PC1, y = PC2, colour = as.factor(Difficulty))) +
  geom_point(size = 2, alpha = 0.3) +
  geom_line(alpha = 0.3, show.legend = FALSE) +
  geom_text(aes(label = Word), size = 3, vjust = -1, show.legend = FALSE) +
  scale_colour_viridis_d(begin = 0.05, end = 0.7, option = "A") +
  facet_wrap(Game_ID~., ncol = 1)


CON_CLEAN %>%
  filter(Game_ID %in% unique(CON_CLEAN$Game_ID)[1:3]) %>%
  ggplot(aes(x = PC1, y = PC2, colour = as.factor(Difficulty))) +
  geom_point(size = 2, alpha = 0.3) +
  geom_line(aes(group = Game_ID), alpha = 0.3, show.legend = FALSE) +
  geom_text(aes(label = Word), size = 3, vjust = -1, show.legend = FALSE) +
  scale_colour_viridis_d(begin = 0.05, end = 0.7, option = "A") +
  facet_wrap(Difficulty~., ncol = 1)


####
library(lsa)

cosine_wrangler <- function(F1) {
  test <- F1 %>%
    select(starts_with("V")) %>%
    as.matrix() %>% t() %>%
    cosine()
  
  game <- test %>%
    as_tibble() %>%
    setNames(nm = make.unique(F1$Word)) %>%
    mutate(Word = F1$Word,
           Connect_ID = F1$Connect_ID) %>%
    pivot_longer(
      cols = -c(Word:Connect_ID),
      names_to = "Comp_Word",
      values_to = "Similarity"
    ) %>%
    filter(Word != Comp_Word & Similarity != 1) %>%
    filter(!str_detect(Connect_ID, "_0$")) %>%
    distinct(Similarity, .keep_all = TRUE) %>% 
    summarise(Game_ID = F1$Game_ID[1],
              Mean_G = mean(Similarity))
  
  test <- test %>%
    as_tibble() %>%
    setNames(nm = make.unique(F1$Word)) %>%
    mutate(Word = F1$Word,
           Connect_ID = F1$Connect_ID) %>%
    pivot_longer(
      cols = -c(Word:Connect_ID),
      names_to = "Comp_Word",
      values_to = "Similarity"
    ) %>%
    filter(Word != Comp_Word & Similarity != 1) %>%
    left_join(
      .,
      F1 %>%
        select(Word, Connect_ID) %>%
        distinct(Word, .keep_all = TRUE) %>%
        rename("Comp_Connect_ID" = "Connect_ID"),
      by = c("Comp_Word" = "Word")
    ) %>%
    filter(Connect_ID == Comp_Connect_ID) %>%
    group_by(Connect_ID) %>%
    distinct(Similarity, .keep_all = TRUE) %>%
    summarise(Mean_S = mean(Similarity)) %>%
    mutate(Mean_G = game$Mean_G)
  
  return(test)
}

set.seed(2023)
CON_SAMPLE <- slice_sample(CON_CLEAN, n = 4*n_distinct(CON_CLEAN$Game_ID), replace = TRUE) %>%
  mutate(Difficulty = 0,
         Game_ID = rep(unique(CON_CLEAN$Game_ID), each = 4),
         Connect_ID = paste0(Game_ID, "_", Difficulty))

CON_SIMI <- CON_CLEAN %>%
  bind_rows(., CON_SAMPLE) %>% 
  group_by(Game_ID) %>%
  group_split() %>%
  map(., ~cosine_wrangler(.x)) %>%
  bind_rows() %>%
  mutate(Difficulty = word(Connect_ID, 2, sep = fixed("_")),
         Difficulty = as.factor(Difficulty))

pLABELS <- bind_rows(EASIEST <- CON_SIMI %>%
                       filter(Difficulty != 0) %>%
                       group_by(Difficulty) %>%
                       slice_max(n = 1, order_by = Mean_S) %>%
                       left_join(., select(CON_CLEAN, Connect_ID, Word), by = "Connect_ID") %>%
                       mutate(Words = str_c(Word, collapse = ", ")) %>%
                       distinct(Connect_ID, .keep_all = TRUE),
                     
                     HARDEST <- CON_SIMI %>%
                       filter(Difficulty != 0) %>%
                       group_by(Difficulty) %>%
                       slice_min(n = 1, order_by = Mean_S) %>%
                       left_join(., select(CON_CLEAN, Connect_ID, Word), by = "Connect_ID") %>%
                       mutate(Words = str_c(Word, collapse = ", ")) %>%
                       distinct(Connect_ID, .keep_all = TRUE))

CON_SIMI %>%
  ggplot(aes(y = Difficulty, x = Mean_S)) +
  geom_jitter(aes(colour = Difficulty), height = 0.1) +
  geom_boxplot(alpha = 0.6, outlier.alpha = 0, width = 0.5) +
  geom_text(data = pLABELS, aes(label = str_wrap(Words, 15), colour = Difficulty), vjust = 0.5, hjust = ifelse(pLABELS$Mean_S < 0.5, 1.2, -0.2), size = 2.5) +
  scale_x_continuous(expand = expansion(add = 0.4)) +
  scale_colour_viridis_d(begin = 0.05, end = 0.7, option = "A")

CON_SIMI %>%
  ggplot(aes(x = Mean_S, fill = Difficulty)) +
  geom_density(alpha = 0.25) +
  scale_fill_viridis_d(begin = 0.05, end = 0.7, option = "A")

###
library(ggpubr)

compare_means(Mean_S ~ Difficulty, data = CON_SIMI, p.adjust.method = "bonferroni")

#### easiest and hardest games

GAMES <- CON_SIMI %>%
  filter(Difficulty != 0) %>%
  mutate(Game_ID = word(Connect_ID, 1, sep = fixed("_"))) %>%
  group_by(Game_ID) %>%
  mutate(Game_diff = abs(Mean_S - Mean_G)) %>%
  summarise(Mean_Game = mean(Game_diff))

easyHARD <- tibble(Game_ID = as_vector(c(GAMES[which.max(GAMES$Mean_Game), 1], GAMES[which.min(GAMES$Mean_Game), 1])),
                   Game_Rank = c("Easiest Game", "Hardest Game"))

CON_CLEAN %>%
  filter(Game_ID %in% easyHARD$Game_ID) %>%
  left_join(., easyHARD, by = "Game_ID") %>%
  ggplot(aes(x = PC1, y = PC2, colour = as.factor(Difficulty))) +
  geom_point(size = 2, alpha = 0.3) +
  geom_line(alpha = 0.3, show.legend = FALSE) +
  geom_text(aes(label = Word), size = 3, vjust = -1, show.legend = FALSE) +
  scale_colour_viridis_d(begin = 0.05, end = 0.7, option = "A") +
  facet_wrap(Game_Rank~Game_ID, ncol = 1)

