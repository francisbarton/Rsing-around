setwd("C:\\Users\\Francis\\Projects\\Rsing-around")
library("stringr")

download.file("https://github.com/dwyl/english-words/blob/master/words_alpha.zip", destfile = "words_alpha.zip")
curl::curl_download("https://sourceforge.net/projects/wordlist/files/speller/2019.10.06/wordlist-en_GB-large-2019.10.06.zip/download", destfile = "wordlist-en_GB-large-2019.10.06.zip")
download.file("https://proofingtoolgui.org/wordlist_marcoagpinto_20191201_230443w.txt", destfile = "pinto_words.txt")

unzip("wordlist-en_GB-large-2019.10.06.zip")

split_it <- . %>% str_split("") %>% 
  map(~ paste0(., "[aeiou]*")) %>% unlist() %>% str_c(collapse="")
build_it <- function(x) { str_c("^[aeiou]*", split_it(x), "$") }
grep_it <- function(x) { build_it(x) %>%
    grep(., readLines("en_GB-large.txt"), value = TRUE, ignore.case = TRUE) }

find_words <- . %>%
  str_split("") %>%
  map(~ str_replace_all(., "\\?", "[^aeiou]\\{1\\}")) %>% 
  map(~ paste0(., "[aeiou]*")) %>%
  unlist() %>%
  str_c(collapse="") %>% 
  str_c("^[aeiou]*", ., "$") %>% 
  grep(., readLines("en_GB-large.txt"), value = TRUE, ignore.case = TRUE)

find_words("rg")
