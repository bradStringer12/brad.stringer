library(tidyverse)
library(rvest)

#### Scraping Billboard Number 1 singles 1960-2022 ####

YEARS <- c(1960:2022)
URL <- "https://en.wikipedia.org/wiki/List_of_Billboard_Hot_100_number_ones_of_"

get_n_ones <- function(year){
  
  dummy <- read_html(paste0(URL, year)) %>%
    html_table() %>%
    keep(., ~ncol(.x)==5) %>%
    pluck(1) %>%
    select("Song", "Artist(s)") %>%
    distinct() %>%
    mutate(Year = year)
}

B_ONES <- map(YEARS, ~get_n_ones(.x))
B_ONES <- bind_rows(B_ONES) %>%
  mutate(Song = str_remove_all(Song, "\""),
         Song = str_remove_all(Song, "†"),
         Song = str_squish(Song)) %>%
  rename("Artist" = "Artist(s)") %>%
  mutate(Artist = str_remove(Artist, "feat.*$"),
         Artist = str_replace_all(Artist, " and ", " & "),
         Song = str_to_title(Song) %>% str_squish(),
         Song_s = str_remove_all(Song, "[[:punct:]]"),
         Artist = str_to_title(Artist) %>% str_squish(),
         Artist_s = str_remove_all(Artist, "[[:punct:]]"))


write.csv(B_ONES, paste0("Billboard_Number1_", YEARS[1], "_", YEARS[length(YEARS)], ".csv"), row.names = FALSE)

##### Reading in lyrics from Genius ####

library(geniusr)
library(tidytext)

genius_token(TRUE)
### Paste Genius API token when prompted in console

B_ONES <- read.csv("Billboard_Number1_1960_2022.csv")

search_song_safely <- safely(search_song)

SONG_MD <- map(unique(B_ONES$Song), ~search_song_safely(search_term = .x))
SONG_MD <- keep(SONG_MD, ~is.null(.x$error)) 
SONG_MD <- map_df(SONG_MD, ~pluck(.x$result))
SONG_MD <- SONG_MD %>%
  mutate(song_name = str_to_title(song_name),
         song_name = str_remove(song_name, "\\(Ft.*\\)$"),
         song_name = str_remove(song_name, "\\(Feat.*\\)$"),
         song_name_s = str_remove_all(song_name, "[[:punct:]]"),
         song_name = str_squish(song_name),
         artist_name = str_to_title(artist_name),
         artist_name = str_replace_all(artist_name, " and ", " & "),
         artist_name = str_squish(artist_name),
         artist_name_s = str_remove_all(artist_name, "[[:punct:]]"))

write.csv(SONG_MD, "Genius_Track_IDs.csv", row.names = FALSE)

####
un_raw <- function(sn){charToRaw(sn)[! charToRaw(sn) %in% BLANK] %>% rawToChar()}

SONG_MD <- read.csv("Genius_Track_IDs.csv") %>%
  mutate(song_name_unraw = map_chr(song_name, ~un_raw(.x)),
         song_name_unraw = str_replace_all(song_name_unraw, "\\\x99", "'"))

B_ONES <- read.csv("Billboard_Number1_1960_2022.csv")

B_ONES <- B_ONES %>%
  left_join(., SONG_MD, by = c("Song" = "song_name_unraw", "Artist" = "artist_name"))

write.csv(B_ONES, "Billboard_Hits_GeniusIDs.csv", row.names = FALSE)

library(fuzzyjoin)

B_ONES_NA <- B_ONES %>%
  filter(is.na(song_id)) %>%
  select(Song:Artist_s)

SONG_MD <- SONG_MD %>%
  mutate(artist_name = str_replace_all(artist_name, " And ", " & "),
         artist_name = str_replace(artist_name, "Everley ", "Everly "),
         artist_name_s = str_remove_all(artist_name, "[[:punct:]]")) %>%
  mutate(artist_name_unraw = map_chr(artist_name_s, ~un_raw(.x)),
         artist_name_unraw = str_replace_all(artist_name_unraw, "\\\x99", "'")) %>%
  mutate(song_name_unraw = map_chr(song_name_s, ~un_raw(.x)),
         song_name_unraw = str_replace_all(song_name_unraw, "\\\x99", "'"))
  

B_ONES_NA <- B_ONES_NA %>%
  mutate(Song = str_remove_all(Song, "\\[.*\\]")) %>%
  mutate(Song_s = str_remove_all(Song, "[[:punct:]]"),
         Artist = str_remove_all(Artist, "\\[.*\\]"),
         Artist_s = str_remove_all(Artist, "[[:punct:]]")) %>%
  fuzzy_left_join(., SONG_MD, by = c("Song_s" = "song_name_unraw", "Artist_s" = "artist_name_unraw"), match_fun = str_detect)


B_ONES <- bind_rows(
  B_ONES,
  filter(B_ONES_NA, !is.na(song_id))
)


### fuzzy join stringdist


MISSING <- B_ONES_NA %>%
  filter(is.na(song_id)) %>%
  select(Song:Artist_s)

MISSING <- MISSING %>% 
  stringdist_left_join(.,
                       SONG_MD, by = c("Song_s" = "song_name_unraw", "Artist_s" = "artist_name_unraw"),
                       max_dist = 2)


MISSING <- MISSING %>%
  filter(!is.na(song_id))

SONGS <- unique(MISSING$song_id)

SG <- tibble(
  song_id = SONGS
) %>%
  filter(!is.na(song_id)) %>%
  mutate(song_genre = map_chr(song_id, ~get_genre(.x)))

MISSING <- MISSING %>%
  left_join(., SG, by = "song_id") %>%
  mutate(Decade = paste0(str_extract(Year, "^..."), "0s"))

B_ONES <- bind_rows(
  
  B_ONES %>% anti_join(., MISSING, by = c("Song", "Artist", "Year")),
  MISSING
  
)

### get genre tags
library(rvest)

SONGS <- unique(B_ONES$song_id)

get_genre <- function(x){
  dummy <- get_song_df(song_id = x)
  rvest::read_html(dummy$song_lyrics_url) %>%
    html_nodes("a") %>% html_attr("href") %>%
    discard(., ~is.na(.x)) %>% keep(., ~str_detect(.x, "\\/tags")) %>%
    map(., ~str_extract(.x, "(?<=\\/tags\\/).*$")) %>%
    str_c(., collapse = "_")
}

SONG_GENRES <- tibble(
  song_id = SONGS
) %>%
  filter(!is.na(song_id)) %>%
  mutate(song_genre = map_chr(song_id, ~get_genre(.x)))


B_ONES <- B_ONES %>%
  left_join(., SONG_GENRES, by = "song_id")

B_ONES <- B_ONES %>%
  mutate(Decade = paste0(str_extract(Year, "^..."), "0s"))

write.csv(B_ONES, "Billboard_Hits_GeniusIDs.csv", row.names = FALSE)


#### get song lyrics
B_final <- filter(B_ONES, !is.na(song_id))
B_ONES_NA <- filter(B_ONES, is.na(song_id))

get_gen_lyrics <- function(ID, URL){
  
  read_html(URL) %>%
  html_elements(".Dzxov") %>%
  html_text2() %>%
  str_c(., collapse = " ") %>%
  as_tibble() %>%
  rename("song_lyrics" = "value") %>%
  mutate(song_lyrics = str_replace_all(song_lyrics, "\\\n|,", " "),
         song_lyrics = str_remove_all(song_lyrics, "\\[\\w*\\d\\]"),
         song_lyrics = str_remove_all(song_lyrics, "\\*\\w*\\*"),
         song_lyrics = str_replace_all(song_lyrics, "([[:lower:]]{1})([[:upper:]]{1})", "\\1 \\2"),
         song_lyrics = str_squish(song_lyrics),
         lyric_count = str_count(song_lyrics, "\\b")/2,
         song_id = ID)
  
}

get_lyrics_safely <- safely(get_gen_lyrics)

lyrID <- B_final %>%
  select(song_id, song_lyrics_url) %>%
  distinct()

SONG_LYRICS <- map2(lyrID$song_id, lyrID$song_lyrics_url, ~get_lyrics_safely(ID = .x, URL = .y))


SONG_LYRICS <- SONG_LYRICS %>% keep(., ~is.null(.x$error)) %>%
  map(., ~.x$result) %>%
  bind_rows()

write.csv(SONG_LYRICS, "BillboardHits_songLyrics.csv", row.names = FALSE)

###