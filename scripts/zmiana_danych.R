# Żeby zmienić dane do UTF-8
pow.df <- pow.df %>% lapply(., iconv, to = "UTF-8") %>% tibble::as_tibble()


POW <- POW %>% lapply(., iconv, from = "latin1", to = "UTF-8") %>% tibble::as_tibble()
