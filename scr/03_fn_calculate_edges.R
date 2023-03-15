###############################################
###############################################
####### COMPUTE NETWORK EDGES.
###############################################
###############################################
### Takes tibble with topic-to-documents probabilities "ru_gamma", counts all distinctive connections of topics 
### within a poem.  Topics are connected if present in a single poem with a probability that is above a certain threshold in overall mass of probabilities distribution (e.g. >95%)
################## ARGUMENTS
### gamma_thresh = "float", how large should be probability of a topic in a text to count as a link
### time_slice = "integer", what is the time window of aggregation (e.g. 5 years)
##################

compute_edges = function(x, gamma_thresh=0.05, time_slice) {

## calculates a cutoff based on 1-threshold quantile (e.g. if threshold is set to 0.05 , we only take topic expression that appear in more than 95% event mass.
p = quantile(x$gamma, 1-gamma_thresh)  

require(tidyverse)  
  
links = x %>%
  filter(gamma > p) %>% # filter topics by threshold
  group_by(index) %>% 
  mutate(from = sort(topic), # start building edges: 1) column with sorted topics
         count = max(row_number())) %>% # 2) "count" total number of connected topics in poem
  uncount(count) %>% # 3) uncount to create long list of potential connections: 1,1,1,2,2,2,3,3,3 (for 3 topics)
  mutate(to = rep(unique(topic), length(unique(topic)))) %>% # fill the new "to" column with repeating arranged topics: 1,2,3,1,2,3,1,2,3. "To" and "From" account for every combination of links possible between given set of topics
  filter(from != to) %>% # remove duplicates
  ungroup() %>%  
  # year slices
  mutate(slice = floor(as.numeric(year)/time_slice)*time_slice) %>%
  ungroup() %>% 
  rowwise() %>% 
  # sort edges to determine unique ones
  mutate(edge_id = paste(unique(sort(c(from, to))), collapse=" ")) %>% 
  group_by(index,edge_id) %>% 
  # drop duplicates
  distinct(edge_id, .keep_all = T) %>% 
  left_join(topic_labels, by=c("from" = "topic")) %>% # join topic labels
  rename(source=label) %>%
  left_join(topic_labels, by=c("to" = "topic")) %>% 
  rename(target=label)
  



return(links)
}

