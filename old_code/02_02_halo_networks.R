library(tidyverse)
library(tidytext)
library(topicmodels)

library(igraph)
library(sna)
library(ggraph)

library(wesanderson)
library(patchwork)
theme_set(theme_minimal())

setwd("Documents/thesis1830s/corpus35_pr/")

#### load data ####
load("data/02_01_75k_lda_output.Rda")

glimpse(beta)
glimpse(gamma)

source("scr/02_02_fn_calculate_edges.R")

lda_metadata <- corpus35lda  %>% 
  tidy(matrix = "gamma")  %>% 
  select(document)  %>% 
  unique() %>% 
  separate(document, into = c("index", "year", "first_line", "formula"), sep = "_") %>% 
  mutate(corpus = str_replace_all(index, "^(\\w)--(\\d+.*)", "\\1"))

head(lda_metadata)

meter_counts <- lda_metadata %>% 
  mutate(year_span = floor(as.numeric(year)/5)*5) %>% 
  rename("meter" = "formula") %>% 
  filter(!str_detect(meter, "other")) %>% 
  group_by(year_span, meter) %>% 
  count(sort = T) %>% 
  ungroup() %>% 
  filter(n > 10 & year_span != 1850)

head(meter_counts)

top_meters <- meter_counts %>% 
  count(meter) %>% 
  filter(n > 1) %>% 
  select(-n)

n_texts_corpus <- lda_metadata %>% 
  mutate(year_span = floor(as.numeric(year)/5)*5) %>% 
  rename("meter" = "formula") %>% 
  filter(year_span %in% c(1830, 1835) & meter %in% top_meters$meter) %>% 
  group_by(corpus, meter) %>% 
  count(sort = T)

glimpse(gamma)
gamma <- gamma %>% 
  rename("meter" = "formula")

#### Calculate edges ####

# topic labels fn (AS)
get_topic_labels = function(x, n_labels = 10) {
  
  library(tidyverse)
  library(tidytext)
  library(topicmodels)
  
  #takes beta-matrix as an input
  top_terms = x %>%
    group_by(topic) %>%
    top_n(20, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  topic_labels = top_terms %>%
    group_by(topic) %>%
    top_n(n_labels, beta) %>%
    summarise(label=paste(term, collapse=" ")) %>%
    mutate(label = paste(topic, label, sep="_"))
  
  return(topic_labels)
}

topic_labels <- get_topic_labels(beta, n_labels = 5)

edges_raw <- compute_edges(gamma,
                           gamma_thresh = 0.05,
                           time_slice = 5)

head(edges_raw)

edges_raw %>% 
  filter(corpus == "P")

## links

links <- edges_raw %>% 
  filter(meter %in% top_meters$meter) %>% 
  group_by(slice, meter, corpus) %>% 
  mutate(meter_conunt = max(row_number())) %>% 
  count(slice, meter, edge_id, source, target, sort = T)  %>% 
  filter(n > 2) %>% 
  ungroup() # filter out links based of frequency

links %>% 
  filter(corpus == "P")

edgelist <- links %>% 
  select(source, target, n, meter, slice, corpus) %>% 
  mutate(width = n/10) %>% 
  filter(slice != 1850)

nodelist <- tibble(source = unique(c(links$target, links$source))) %>% 
  mutate(idn = as.numeric(str_replace(source, "^([0-9].*?)_.*", "\\1"))) 


head(edgelist)
head(nodelist)
nrow(nodelist)


# igraph netowork
net <- graph_from_data_frame(d=edgelist, vertices=nodelist, directed=F)
net
#### some measures ####
betw = igraph::betweenness(net)
head(betw)

# named vector to tibble
str(betw)

betw_tib <- as_tibble(as.list(betw)) %>% 
  mutate(x = "x") %>% 
  pivot_longer(!x, names_to = "source", values_to = "betw") %>% 
  select(-x)
head(betw_tib)

nodelist <- inner_join(nodelist, betw_tib, by = "source")
nrow(nodelist)

deg <- igraph::degree(net, mode="all")
head(deg)

degree_tib <- as_tibble(as.list(deg)) %>% 
  mutate(x = "x") %>% 
  pivot_longer(!x, names_to = "source", values_to = "degree") %>% 
  select(-x)
nodelist <- inner_join(nodelist, degree_tib, by = "source")

head(nodelist)
head(edgelist)



#  upd net
net <- graph_from_data_frame(d=edgelist, vertices=nodelist, directed=F)

######## networks all ####
network_all <- function(network, meter_value, palette_v) {
  ggraph(network, layout = "stress") +
    geom_edge_fan(aes(color=meter,
                      filter = meter %in% c(meter_value) & corpus == "N", #!!!! select only nkrja
                      width = width, 
                      alpha = 0.4)) +
    geom_node_point(aes(alpha = degree)) +
    geom_node_text(aes(label=idn), 
                   hjust=0.1, 
                   vjust=-0.4, 
                   size=3, 
                   color="grey50") +
    theme_void() + 
    theme(strip.text = element_text(size = 12)) +
    facet_wrap(~slice, 
               scales="free_x", 
               drop=T,
               ncol = 5) + 
    scale_edge_color_manual(values = palette_v)
}

unique(edgelist$meter)

i_free <- network_all(net, c("iamb--free"), wes_palette("Darjeeling1")[4])
i4 <- network_all(net, c("iamb--4"), wes_palette("Darjeeling1")[2]) 
tr4 <- network_all(net, c("trochee--4"), wes_palette("Darjeeling1")[1])
i6 <- network_all(net, c("iamb--6"), wes_palette("Darjeeling1")[3])
all_nkrja <- network_all(net, c("iamb--6", "iamb--4", "trochee--4"), c(wes_palette("Darjeeling1")[2], wes_palette("Darjeeling1")[3], wes_palette("Darjeeling1")[1]))

ggsave("plots/02_networks_nkrja_all.png", plot = all_nkrja, 
       width = 12, height = 10, dpi = 300, bg = "white")

############################
#### networks 1835 #########
############################

edgelist1835 <- edgelist %>% 
  filter(slice %in% c(1830, 1835, 1840))

edgelist_only35 <- edgelist %>% 
  filter(slice %in% c(1835))

net35 <- graph_from_data_frame(d=edgelist_only35, vertices=nodelist, directed=F)
net <- graph_from_data_frame(d=edgelist1835, vertices=nodelist, directed=F)


#### ggraph ####

top_meters

network_gr <- function(network, meter_value, palette_v, subtitle) {
  ggraph(network, layout = "stress") +
    geom_edge_fan(aes(color=meter,
                      #### change for groupings!!!
                      filter = meter %in% meter_value & slice %in% c(1835), # this
                      width = width, 
                      alpha = 0.4)) +
    geom_node_point() +
    geom_node_text(aes(label=idn), 
                   hjust=0.1, 
                   vjust=-0.4, 
                   size=3, 
                   color="grey50") +
    theme_void() + 
    theme(strip.text = element_text(size = 12)) +
    facet_wrap(~corpus, 
               scales="free", 
               drop=T,
               ncol = 4) + 
    scale_edge_color_manual(values = palette_v) + 
    theme(legend.position = "None") + 
    labs(subtitle = subtitle)
}


n_texts_corpus

edgelist %>% 
  filter(corpus == "N" & meter == "iamb--5" & slice %in% c(1830, 1835)) %>% 
  distinct(source, target) %>% 
  nrow()

#titles for 1830-1835
# iamb4 <- network_gr(net, c("iamb--4"), wes_palette("Darjeeling1")[2], 
#                     "Iamb-4\nNumber of texts: National corpus = 778      Periodicals 1835-1840: 465\nNumber of connetions: N = 447       P = 348")
# trochee4 <- network_gr(net, c("trochee--4"), wes_palette("Darjeeling1")[1], 
#                        "Trochee-4\nNumber of texts: National corpus = 358      Periodicals 1835-1840: 227\nNumber of connetions:         N = 91         P = 89")
# iamb6 <- network_gr(net, c("iamb--6"), wes_palette("Darjeeling1")[4], 
#                     "Iamb-6\nNumber of texts: National corpus = 164      Periodicals 1835-1840: 93\nNumber of connetions:         N = 18         P = 27")
# iamb5 <- network_gr(net, c("iamb--5"), wes_palette("Darjeeling1")[3], 
#                     "Iamb-5\nNumber of texts: National corpus = 242      Periodicals 1835-1840: 89\nNumber of connetions:         N = 48         P = 17")

# titles for 1835 only
iamb4 <- network_gr(net, c("iamb--4"), wes_palette("Darjeeling1")[2], 
                    "Iamb-4\nNumber of texts: National corpus = 276      Periodicals 1835-1840: 465\nNumber of connetions: N = 127       P = 348")
trochee4 <- network_gr(net, c("trochee--4"), wes_palette("Darjeeling1")[1], 
                       "Trochee-4\nNumber of texts: National corpus = 185      Periodicals 1835-1840: 227\nNumber of connetions:         N = 63         P = 89")
iamb6 <- network_gr(net, c("iamb--6"), wes_palette("Darjeeling1")[4], 
                    "Iamb-6\nNumber of texts: National corpus = 87      Periodicals 1835-1840: 93\nNumber of connetions:         N = 5         P = 27")
iamb5 <- network_gr(net, c("iamb--5"), wes_palette("Darjeeling1")[3], 
                    "Iamb-5\nNumber of texts: National corpus = 73      Periodicals 1835-1840: 89\nNumber of connetions:         N = 6         P = 17")

trochee4 + iamb4 + iamb5 + iamb6 + plot_layout(nrow = 4)

ggsave(file = "plots/02_networks_comp_only35.png", plot = last_plot(), height = 10, width = 7, bg = "white")


head(edgelist)
tr <- edgelist %>% 
  filter(corpus == "P" & meter == "trochee--4") %>% 
  top_n(20, n)


#### net w/o fn ####
ggraph(net35, layout = "stress") +
  geom_edge_fan(aes(color=meter,
                    filter = meter == "iamb--4",
                    width = width, 
                    alpha = 0.4)) +
  geom_node_point() +
  geom_node_text(aes(label=idn), 
                 hjust=0.1, 
                 vjust=-0.4, 
                 size=3, 
                 color="grey50") +
  theme_void() + 
  theme(strip.text = element_text(size = 12)) +
  facet_wrap(~corpus, 
             scales="free", 
             drop=T,
             ncol = 4) + 
  scale_edge_color_manual(values = wes_palette("Darjeeling1")[2]) + 
  theme(legend.position = "None") + 
  labs(subtitle = "Iamb-4")





#### barplot for n of connections ####

links
links1835 <- links %>% 
  filter(slice %in% c(1835) & meter %in% c("iamb--4", "iamb--6", "iamb--5", "trochee--4"))

n_texts_corpus 

# same counts as on networks (check)
links1835 %>% 
  group_by(meter, corpus) %>% 
  count()

links_n <- links1835 %>% 
  filter(corpus == "N") %>% 
  mutate(connection = paste0(meter, "__", edge_id)) %>% 
  select(connection) 

links_p <- links1835 %>% 
  filter(corpus == "P") %>% 
  mutate(connection = paste0(meter, "__", edge_id)) %>% 
  select(connection) 
  
t <- intersect(links_n$connection, links_p$connection)

head(t)
length(t)

# links1830_35 # variable for net 1830+1835
links35_pl <- links1835 %>% 
  mutate(connection = paste0(meter, "__", edge_id)) %>% 
  mutate(corpus_new = ifelse(connection %in% t, "Both", corpus)) %>% 
  mutate(corpus_new = recode(corpus_new, 
                           "N" = "National corpus", 
                           "P" = "Periodicals")) %>% 
  group_by(corpus_new, meter) %>% 
  count() %>% 
  ggplot(aes(x = meter,
           y = n,
           group = corpus_new,
           fill = corpus_new)) +
  geom_col(position = "dodge") +
  labs(x = "Meter",
       y = "", 
       fill = "Corpus",
       subtitle = "National corpus texts dated 1835-1840") + 
  scale_fill_manual(values = c(wes_palette("Rushmore1")[3:5])) + 
  theme(axis.text = element_text(size = 11), 
        legend.position = "None", 
        plot.subtitle = element_text(hjust = 0.5))

links1830_35 + links35_pl

ggsave(file = "plots/02_connections_diff_merged.png", plot = last_plot(),
       height = 7, width = 10, bg = "white")
