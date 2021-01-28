ls <- as_tibble(data.frame(language=unique(clics$Language), stringsAsFactors = F))

ls <- ls %>%
  mutate(language_broad = str_match(language, "(.*?)( *[,(]|$)")[,2])
existing_ls <- unique(c(chir$Language, reflex$Language, IDS$Language, google$Language)) %>%
  str_match(., "(.*?)( *[,(]|$)")
existing_ls <- existing_ls[,2]

cat(ls$language[!(ls$language_broad %in% existing_ls)], sep="\n")

