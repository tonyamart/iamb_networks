library(tidyverse)
library(tidytext)
library(topicmodels)


theme_set(theme_minimal())

setwd("Documents/thesis1830s/corpus35_pr/")

#### Periodicals corpus preparation ####

meters <- read.csv("data/01_metrical_annotation_monometers_70.csv") %>% 
  select(-X) %>% mutate(formula = paste0(meter, "_", feet))
dat <- read.csv("data/01_id_text_lemm.csv") %>% 
  select(-X, -Unnamed..0) # lemmatized texts

glimpse(meters)
glimpse(dat)

dat <- left_join(dat, meters, by = "id") 

glimpse(dat)

dat <- dat %>%
  # add number of lines
  separate_rows(text, sep = "\\n") %>%
  #remove <>
  mutate(text = str_remove_all(text, "<.*?>")) %>%
  filter(text != "" | str_detect(text, "^[[:punct:]]+$")) %>%
  group_by(id) %>%
  mutate(n_lines = row_number(),
         n_lines = max(n_lines)) %>%
  mutate(text = paste0(text, collapse = "\n")) %>%
  distinct() %>% 
  ungroup() %>% 
  rename(text_raw = text)
  
glimpse(dat)

# fast n_lines density check
dat %>% 
  ggplot(aes(x = n_lines)) + geom_density()

max(dat$n_lines)
mean(dat$n_lines)
median(dat$n_lines)

#### sampling

source("scr/02_01_fn_random_sample_long.R")

sampled <- sample_long(dat,
                    starting_n = 1,
                    sample_size = "corpus median", 
                    over9000 = 5)

# test sampling results
sampled %>% 
  filter(n_lines > 200) %>% 
  head(10)

glimpse(sampled)
length(unique(sampled$id))

#write.csv(sampled, file = "data/02_01_per_sampled_labled.csv")

per1835 <- sampled %>% 
  separate(id, into = c("id", "sample"), sep = "-") %>% 
  left_join(meters, by = "id") %>% 
  mutate(id = ifelse(!is.na(sample), paste0(id, "_", sample), id)) %>% 
  mutate(year = "1836") %>% 
  select(id, text_raw, text_lemm, year, formula, meter, feet, n_lines)


#### remove intersection with nkrja ####
duplicated <- read.delim("meta/db_nkrja_intersection.tsv", sep = "\t")
duplicated <- duplicated %>% 
  rename("id" = "text_ID")

per1835_cln <- per1835 %>% 
  anti_join(duplicated, by = "id")

glimpse(per1835) # 1,821
glimpse(per1835_cln) # 1,743

rm(duplicated, meters, per1835, sampled)

#### Attach nkrja data ####

nkrja19 <- read.csv("data/02_01_nkrjalem.csv") %>% select(-X)

glimpse(nkrja19) 

# fast n text check
nkrja19 %>% 
  group_by(year) %>% 
  count() %>% 
  ggplot(aes(x = year, y = n)) + geom_col()

#### Merge & final prepoc #### 

glimpse(per1835_cln)

corpus_fin <- rbind(per1835_cln, nkrja19)

# change all inner sep = "_" to "-"
corpus_fin <- corpus_fin %>% 
  mutate_at(vars(c("id", "formula")), ~str_replace_all(., "_", "--")) %>% 
  rename("index" = "id")

glimpse(corpus_fin)

# check
corpus_fin %>% 
  separate(index, into = c("source", "id"), sep = "--") %>% 
  group_by(source) %>% 
  count()

# add doc_id
corpus_to_dtm <- corpus_fin %>% 
  mutate(first_line = str_extract(text_lemm, "^.*\n")) %>% 
  mutate(first_line = str_to_title(first_line)) %>% 
  mutate(first_line = str_remove_all(first_line, "\n|\\s|\\W+")) %>% 
  unite(doc, c("index", "year", "first_line", "formula")) %>% 
  select(doc, text_lemm)

head(corpus_to_dtm, 2)

## stoplist
ru_stop <- tibble(word = readLines("data/stopwords_ru.txt"))

#### Tokenization & dtm ####

corpus_tokens <- corpus_to_dtm %>% 
  unnest_tokens(input = text_lemm, output = word, token = "words") %>% 
  anti_join(ru_stop, by = "word") %>% 
  filter(str_detect(word, "[А-Яа-я]"))

# count words in each document
tokens_count <- corpus_tokens %>% 
  group_by(doc) %>% 
  count(word)

# count 5k MFW
ranks <- corpus_tokens %>% 
  count(word, sort = TRUE) %>% 
  head(5000) %>% 
  select(-n)

head(ranks, 10)

# select only MFW
counts_dtm <- tokens_count %>% 
  right_join(ranks, by = "word")

# check
length(unique(counts_dtm$word))

# save data for tm
corpus35dtm <- counts_dtm %>% cast_dtm(document = doc,
                                       term = word,
                                       value = n)

save(per1835_cln, nkrja19, ru_stop, corpus35dtm, file = "data/02_01_data_dtm.Rda")

###### Topic model #######

corpus35lda <- LDA(corpus35dtm,
                   k = 75,
                   method = "Gibbs", 
                   control = list(seed = 59012, alpha = 0.1, delta = 0.1))

gamma <- corpus35lda %>% 
  tidy(matrix = "gamma") %>% 
  separate(document, into = c("index", "year", "first_line", "formula"), sep = "_") %>% 
  mutate(corpus = str_replace_all(index, "^(\\w)--(\\d+.*)", "\\1"))

glimpse(gamma)

beta <- corpus35lda %>% 
  tidy(matrix = "beta")

save(corpus35lda, gamma, beta, file = "data/02_01_75k_lda_output.Rda")

#### LDA output exploration ####

load("data/02_01_75k_lda_output.Rda")

glimpse(beta)
glimpse(gamma)

# quick beta viz
beta  %>% 
  mutate(topic = as.factor(topic))  %>% 
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  ggplot(aes(x=reorder_within(term, beta, topic), y=beta, fill=topic)) +
  scale_x_reordered() + 
  geom_col() +
  coord_flip() +
  facet_wrap(~topic, scales = "free") +
  guides(fill="none") +
  theme(axis.text = element_text(size = 8))

ggsave(filename = "plots/02_beta_topics.png", plot = last_plot(), 
       width = 16, height = 14, bg = "white")

#### metadata in separate file & meter counts ####
lda_metadata <- corpus35lda  %>% 
  tidy(matrix = "gamma")  %>% 
  select(document)  %>% 
  unique() %>% 
  separate(document, into = c("index", "year", "first_line", "formula"), sep = "_") %>% 
  mutate(corpus = str_replace_all(index, "^(\\w)--(\\d+.*)", "\\1"))

head(lda_metadata)

length(unique(lda_metadata$index))
length(unique(gamma$index))

gamma %>% 
  filter(is.na(formula) | is.na(index) | formula == "NA" | formula == "") %>% 
  group_by(index) # 115 texts


# distribution of topics over corpus
gamma %>%
  group_by(topic) %>%
  summarise(avg_gamma = mean(gamma)) %>%
  ggplot(aes(reorder(topic,-avg_gamma), avg_gamma)) + geom_col()

# top metrical forms
lda_metadata %>% 
  group_by(formula) %>% 
  count(sort = T) %>% 
  head(100)


#### meters distribution plot ####
lda_metadata %>% 
  mutate(decade = floor(as.numeric(year)/5)*5) %>%  # 15,152 rows 
  filter(decade > 1800) %>% 
  filter(!str_detect(formula, "other|dolnik|регулярная")) %>% 
  mutate(decade = ifelse(corpus == "P", "periodicals_1835", decade)) %>% 
  group_by(decade, formula) %>% 
  count(sort = T) %>% 
  ungroup() %>% 
  filter(n > 10 & formula != "NA") %>% 
  ggplot(aes(decade, formula, label = n, size = n)) + 
  geom_text() + 
  labs(x = "Decade",
       y = "") + 
  theme(legend.position = "None", 
        axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 45),
        plot.margin = margin(1,2,1,1, "cm"))

ggsave(filename = "plots/02_meters_distribution.png", plot = last_plot(), 
       height = 6, width = 8, dpi = 300, bg = "white")

  
# quick gamma test
gamma %>% 
  mutate(year_span = floor(as.numeric(year)/5)*5) %>% 
  group_by(year_span, topic) %>% 
  summarise(gamma_avg = mean(gamma)) %>% 
  filter(topic %in% c(1,2,3,4,5, 10)) %>% 
  ggplot(aes(x = year_span, y = gamma_avg, fill = as.factor(topic))) + geom_col()



#### meters percentages count ####
  
lda_metadata %>% 
  mutate(decade = floor(as.numeric(year)/5)*5) %>%  # 15,152 rows 
  filter(decade > 1829 & decade < 1845) %>% 
  filter(!str_detect(formula, "other|dolnik|регулярная")) %>% 
  mutate(decade = ifelse(corpus == "P", "periodicals_1835", decade)) %>% 
  filter(corpus == "P") %>% 
  #filter(corpus != "P") %>% 
  #group_by(decade, formula) %>% 
  group_by(formula) %>% 
  count(sort = T) %>% 
  ungroup() %>% 
  filter(n > 10 & formula != "NA") %>% 
  #filter(decade == 1830) %>% 
  summarise(
            formula = formula,
            n = n,
            perc = n/sum(n)) 
  

  