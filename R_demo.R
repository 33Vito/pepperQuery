tbl %>% str()

tbl %>% count(primary_type, arrest)

tbl %>% count(primary_type, arrest) %>%
  ggplot(aes(reorder(primary_type,n), 
             n, fill=arrest)) + 
  geom_col() + 
  coord_flip() + 
  theme_grey(base_size=15)