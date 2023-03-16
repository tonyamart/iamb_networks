## general
library(tidyverse)
library(tidytext)



sample_long = function(x, 
                       starting_n = 1,
                       sample_size="corpus median",
                       over9000 = 8) {

  
message("Redetermining length of poems...")
### redetermine length

  
corpus_index = x %>%  
  mutate(text_lemm = str_split(text_lemm, "\n")) %>% #split text to lines
  unnest(text_lemm) %>% # unnest splitted lists
  filter(text_lemm != "") %>%  # omit empty lines
  group_by(id) %>%
  mutate(length = length(row_number())) %>% 
  select(id, length) %>% 
  distinct(id, .keep_all = T)

if (sample_size == "corpus median") {
  sample_length = median(corpus_index$length) %>% round()
}
else (sample_length = sample_size)

actually_short = corpus_index %>%  
  filter(length <= sample_length) 

actually_long = corpus_index %>%
  anti_join(actually_short, by="id")



###############
### samples ###

message("Preparing to sample...")
### set sample size


### set size depth 
size_depth = c(25, 50, 100, 200)

### start 1 sample for smallest "long" depth
n_samples = starting_n
sampled_together = c()


message("Sampling long poems...")
for (i in size_depth) {

sampled_long = x %>%
  inner_join(actually_long, by="id") %>% 
  filter(length >= i & length < i * 2) %>%
  group_by(id) %>%
  mutate(text_lemm = str_split(text_lemm, "\n")) %>% #split text to lines
  unnest() %>% # unnest splitted lists
  group_by(id) %>%
  mutate(samples = ceiling(row_number()/sample_length)) %>% # determine how many consequtive samples in a poem
  group_by(id, samples) %>%
  mutate(sample_len = length(row_number())) %>% 
  group_by(id) %>% 
  mutate(samples = case_when(
    sample_len < 3 ~ max(samples)-1,
    sample_len >= 8 ~ samples)) %>% 
  group_by(id, samples) %>%
  summarise(text = paste(text_lemm, collapse = "\n")) %>% # nest text back to tidy
  group_by(id) %>% 
  sample_n(n_samples) %>% # sample n times by each text
  inner_join(x, by="id") %>% # join with large table
  select(-text_lemm) %>% 
  rename(text_lemm = text) %>% 
  group_by(id) %>%
  mutate(samples = row_number()) %>% # rename samples to 1,2,3...
  unite(id, c("id", "samples"), sep="-") # make new id

sampled_together = bind_rows(sampled_together, sampled_long)


n_samples = n_samples + 1

}

message("Sampling poems over 9000!!!...")
## sample very long poems, 10 samples
length_over8000 = x %>%
  inner_join(actually_long, by="id") %>% 
  filter(length >= 300) %>%
  group_by(id) %>%
  mutate(text_lemm = str_split(text_lemm, "\n")) %>% #split text to lines
  unnest() %>% # unnest splitted lists
  group_by(id) %>%
  mutate(samples = ceiling(row_number()/sample_length)) %>% # determine how many consequtive samples in a poem
  group_by(id, samples) %>%
  mutate(sample_len = length(row_number())) %>% 
  group_by(id) %>% 
  mutate(samples = case_when(
    sample_len < 8 ~ max(samples)-1,
    sample_len >= 8 ~ samples)) %>% 
  group_by(id, samples) %>%
  summarise(text = paste(text_lemm, collapse = "\n")) %>% # nest text back to tidy
  group_by(id) %>% 
  sample_n(over9000) %>% # sample n times by each text
  inner_join(x, by="id") %>% # join with large table
  select(-text_lemm) %>% 
  rename(text_lemm = text) %>% 
  group_by(id) %>%
  mutate(samples = row_number()) %>% # rename samples to 1,2,3...
  unite(id, c("id", "samples"), sep="-") # make new id

length_over8000
message("Replacing long texts by samples...")
### ok we did that

ru19_fin = x %>%
  anti_join(actually_long, by="id") %>%
  mutate(id=as.character(id)) %>% 
  bind_rows(sampled_together, length_over8000)

return(ru19_fin)

}
