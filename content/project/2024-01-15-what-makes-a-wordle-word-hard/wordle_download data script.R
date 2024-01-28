library(tidyverse)
# set working directory to script folder
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

DATES <- c(4:10, 17:23)
URLS <- paste0("https://raw.githubusercontent.com/Ivannoar/Twilight/main/inputs/data/april_", DATES, ".csv")

RAW <- map(URLS, ~read.csv(file = .x, stringsAsFactors = FALSE, fileEncoding = "latin1") %>% select(user_id, text, is_quote, is_retweet) %>%
             filter(is_quote == FALSE & is_retweet == FALSE))

WORDLE <- RAW %>%
  bind_rows() %>%
  mutate(wordle_raw = str_extract(text, pattern = "Wordle [[:digit:]]{3} .\\/6.")) %>%
  filter(!is.na(wordle_raw)) %>%
  mutate(Hard = str_detect(wordle_raw, pattern = "\\*$")) %>%
  mutate(wordle_raw = str_remove(wordle_raw, pattern = "\\*$") %>% str_squish(.),
         wordle_id = str_extract(wordle_raw, "Wordle [[:digit:]]{3}"),
         wordle_guesses = str_extract(wordle_raw, ".(?=\\/6)"),
         wordle_fail = str_detect(wordle_guesses, "[[:digit:]]", negate = TRUE)) %>%
  filter(!str_detect(wordle_id, "287|296|297|309"))

WORDS <- tibble(
  wordle_id = sort(unique(WORDLE$wordle_id)),
  word = c("shawl", "natal", "comma", "foray", "scare", "stair", "black", "ample", "flair", "foyer", "cargo", "oxide", "plant", "olive")
)

WORDLE <- WORDLE %>%
  left_join(., WORDS, by = "wordle_id")


write.csv(WORDLE, "wordle_results.csv")

WORDLE <- read.csv("wordle_results.csv")




WORDLE %>%
  filter(wordle_guesses != 0) %>%
  mutate(wordle_guesses = ifelse(str_detect(wordle_guesses, "[[:digit:]]"), wordle_guesses, "7"),
         wordle_guesses = as.numeric(wordle_guesses)) %>% 
  group_by(word) %>%
    summarise(mean = mean(wordle_guesses),
              var = var(wordle_guesses),
              pct_correct = mean(wordle_guesses != 7))
  

  
WORDLE <- filter(WORDLE, Hard == TRUE)


