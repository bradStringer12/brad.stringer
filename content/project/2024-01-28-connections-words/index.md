---
title: How connected are the NYT Connections words?
excerpt: "Using cosine similarity and t-tests to figure out if the Connections categories are actually connected."
author: Brad Stringer
date: '2024-01-28'
slug: connections-words
categories:
  - R
  - word embedding
  - language analysis
  - hypothesis testing
  - web scraping
tags:
  - bonferroni correction
  - cosine similarity
  - PCA
  - dimensionality reduction
draft: no
---






```r
PCA <-prcomp(CON_CLEAN[, 6:55])

CON_CLEAN <- bind_cols(
  CON_CLEAN,
  as_tibble(PCA$x[,1:2]) %>% setNames(., c("PC1", "PC2"))
)


CON_CLEAN %>%
  mutate(Difficulty = factor(Difficulty, levels = c(1:4), labels = c("Straightforward", "Easy", "Moderate", "Tricky"))) %>%
  filter(Game_ID %in% unique(CON_CLEAN$Game_ID)[2:5]) %>%
  ggplot(aes(x = PC1, y = PC2, colour = Difficulty)) +
  geom_point(size = 2, alpha = 1) +
  geom_line(alpha = 0.3, show.legend = FALSE) +
  geom_text(aes(label = Word), size = 3, vjust = -1, family = "bitter", show.legend = FALSE) +
  scale_colour_manual(values = cPalette) +
  scale_x_continuous(expand = expansion(mult = 0.2)) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  labs(title = "Spread of Connections words based on GloVe\nword embeddings") +
  facet_wrap(Game_ID~., ncol = 2) +
  guides(colour = guide_legend(override.aes = list(size = 6), nrow = 2, byrow = TRUE))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="1400" />


```r
library(lsa)

cosine_wrangler <- function(F1) {
  test <- F1 %>%
    select(starts_with("V")) %>%
    as.matrix() %>% t() %>%
    cosine()
  
  ## average similarity of all words in the game (excluding control)
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
  
  ## average similarity of words within difficulty to each other within game
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
    mutate(Mean_G = game$Mean_G) ## add overall game similarity
  
  return(test)
}

## for each game in sample, create a completely random selection of 4 words as control
set.seed(2023)
CON_SAMPLE <- slice_sample(CON_CLEAN, n = 4*n_distinct(CON_CLEAN$Game_ID), replace = TRUE) %>%
  mutate(Difficulty = 0,
         Game_ID = rep(unique(CON_CLEAN$Game_ID), each = 4),
         Connect_ID = paste0(Game_ID, "_", Difficulty))

# for each game get the overall game similarity, and within group similarity for each game
CON_SIMI <- CON_CLEAN %>%
  bind_rows(., CON_SAMPLE) %>% 
  group_by(Game_ID) %>%
  group_split() %>%
  map(., ~cosine_wrangler(.x)) %>%
  bind_rows() %>%
  mutate(Difficulty = stringr::word(Connect_ID, 2, sep = stringr::fixed("_")),
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
                       distinct(Connect_ID, .keep_all = TRUE)) %>%
  mutate(Difficulty = factor(Difficulty, levels = c(1:4), labels = c("Straightforward", "Easy", "Moderate", "Tricky")))
```


Using a T-test, we can determine if we have evidence that the higher difficulty groups are actually less similar to each other than the easier groups. We can also determine if the connected groups in the games are actually more 'connected' than a random selection of 4 words. Since there will be a T-test for each pair of difficulty level, we'll include a Bonferroni correction to account for the larger number of hypothesis tests being done. This is important to do when doing multiple hypothesis tests, as the more test you do, the higher the chances there are you will make a Type I error (false positive).

The results below show:

* There is very strong evidence that all 4 difficulty levels are different (less similar to each other) to the control groups.
* There is very strong evidence that the highest difficulty level is different (less similar to each other) to all the other difficulty levels, i.e. Tricky does mean *tricky*.
* There is no evidence the middle 2 difficulty levels are that different.
* There is only a little evidence that the second difficulty level is different to the easiest difficulty level.




```r
library(ggpubr)

compare_means(Mean_S ~ Difficulty, data = CON_SIMI, p.adjust.method = "bonferroni", method = "t.test")
```

```
## # A tibble: 10 × 8
##    .y.    group1 group2        p    p.adj p.format p.signif method
##    <chr>  <chr>  <chr>     <dbl>    <dbl> <chr>    <chr>    <chr> 
##  1 Mean_S 0      1      5.88e-47 5.90e-46 < 2e-16  ****     T-test
##  2 Mean_S 0      2      1.35e-42 1.30e-41 < 2e-16  ****     T-test
##  3 Mean_S 0      3      1.99e-33 2   e-32 < 2e-16  ****     T-test
##  4 Mean_S 0      4      6.10e-15 6.1 e-14 6.1e-15  ****     T-test
##  5 Mean_S 1      2      1.61e- 2 1.6 e- 1 0.016    *        T-test
##  6 Mean_S 1      3      6.27e- 5 6.3 e- 4 6.3e-05  ****     T-test
##  7 Mean_S 1      4      1.12e-20 1.10e-19 < 2e-16  ****     T-test
##  8 Mean_S 2      3      7.95e- 2 7.9 e- 1 0.079    ns       T-test
##  9 Mean_S 2      4      2.60e-14 2.6 e-13 2.6e-14  ****     T-test
## 10 Mean_S 3      4      7.31e- 9 7.3 e- 8 7.3e-09  ****     T-test
```




```r
library(ggtext)

CON_SIMI %>%
  mutate(Difficulty = factor(Difficulty, levels = c(0:4), labels = c("Control", "Straightforward", "Easy", "Moderate", "Tricky"))) %>%
  ggplot(aes(y = Difficulty, x = Mean_S)) +
  geom_jitter(aes(colour = Difficulty), height = 0.1) +
  geom_boxplot(alpha = 0.6, outlier.alpha = 0, width = 0.5) +
  geom_text(data = pLABELS, aes(label = str_wrap(Words, 15), colour = Difficulty), vjust = 0.5, hjust = ifelse(pLABELS$Mean_S < 0.5, 1.2, -0.2), size = 3, family = "bitter") +
  scale_x_continuous(expand = expansion(add = 0.4)) +
  scale_colour_manual(values = c("grey20", cPalette)) +
  labs(title = "Average similarity of words in Connections difficulty\nlevels",
       subtitle = str_wrap("High similarity scores indicate words in group are used more similarly to each other than other words. The least (left) and most (right) similar groups are labelled. <span style = 'color:#B576BB;'>Tricky</span> words are actually less similar to each other than the easier groups, and all groups are more connected than a random collection of words is."),
       y = NULL,
       x = "Mean similarity within game",
       caption = "Similarity calculated using first 50 dimensions of a word's GloVe embeddings.") +
  theme(legend.position = "none",
        plot.subtitle = element_textbox_simple(size = 12))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="1400" />

### Visualising the hardest and easiest games


```r
GAMES <- CON_SIMI %>%
  filter(Difficulty != 0) %>%
  mutate(Game_ID = word(Connect_ID, 1, sep = stringr::fixed("_"))) %>%
  group_by(Game_ID) %>%
  mutate(Game_diff = Mean_S/Mean_G) %>%
  summarise(Mean_Game = mean(Game_diff))

easyHARD <- tibble(Game_ID = as_vector(c(GAMES[which.max(GAMES$Mean_Game), 1], GAMES[which.min(GAMES$Mean_Game), 1])),
                   Game_Rank = c("Easiest Game", "Hardest Game"))

CON_CLEAN  %>%
  mutate(Difficulty = factor(Difficulty, levels = c(1:4), labels = c("Straightforward", "Easy", "Moderate", "Tricky"))) %>%
  filter(Game_ID %in% easyHARD$Game_ID) %>%
  left_join(., easyHARD, by = "Game_ID") %>%
  mutate(Game_ID = paste0(Game_Rank, "\n", Game_ID)) %>%
  ggplot(aes(x = PC1, y = PC2, colour = Difficulty)) +
  geom_point(size = 2) +
  geom_line(alpha = 0.6, show.legend = FALSE) +
  geom_text(aes(label = Word), size = 3, vjust = -1, show.legend = FALSE) +
  scale_colour_manual(values = cPalette) +
  scale_x_continuous(expand = expansion(mult = 0.2)) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  labs(title = "Spread of Connections words based on GloVe\nword embeddings") +
  facet_wrap(Game_ID~., ncol = 2) +
  guides(colour = guide_legend(override.aes = list(size = 6)))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="1400" />



### Appendix

#### How to scrape the Connections answers data set


```r
EMBED <- read.table("glove.6B.50d.txt", header = FALSE, quote = "") ### you'll need to download and save a copy of the GloVe word embeddings seperately.

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
```

