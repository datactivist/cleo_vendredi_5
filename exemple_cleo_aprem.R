library(dplyr)
# library(igraph)
library(readr)
library(stringr)
library(tidyr)
library(networkD3)

f <- "parcours_selected.tsv"
e <- read_tsv(f, col_types = "ccccccccccccccc") %>% 
  filter(siteId %in% 3, type == "action") %>% 
  mutate(uid = visitorId, # str_c(visitorId, "_", parcours_num),
         i = str_replace_all(url, "https?://|\\?(.*)|/$", ""), j = i) %>% 
  select(uid, i, j) %>% 
  distinct(.keep_all = TRUE) %>% 
  group_by(uid) %>%
  filter(n() > 1) %>% 
  expand(i, j) %>% 
  filter(i < j) %>% 
  mutate(
    i_root = str_extract(i, "(.*?)(revues|hypotheses|calenda|openedition)\\.org"),
    j_root = str_extract(j, "(.*?)(revues|hypotheses|calenda|openedition)\\.org")
  ) %>% 
  filter(
    i_root != j_root#,
    #!i_root %in% c("lectures.revues.org", "www.revues.org"),
    #!j_root %in% c("lectures.revues.org", "www.revues.org")
  ) %>%
  group_by(uid, i_root, j_root) %>% 
  distinct %>% 
  group_by(i_root, j_root) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  ungroup %>% 
  mutate(
    i_root = str_replace(i_root, "\\.revues\\.org", ""),
    j_root = str_replace(j_root, "\\.revues\\.org", "")
  )

simpleNetwork(e[, 1:2], fontSize = 14, fontFamily = "sans-serif", nodeColour = "steelblue")
