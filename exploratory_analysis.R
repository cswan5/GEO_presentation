library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)

## Bring in API helper functions
source("helpers.R")

## Get all GEO awards for time period from API
geo_awards <- search_nsf_award_api(expDateStart="01/01/2025",
                                   expDateEnd="01/01/2026",
                                   org_code_dir="06000000
                                   ")

## Terminated GEO awards
geo_terminated <- geo_awards |>  
  filter(awdSpAttnCode=='37')

## GEO not administratively terminated
geo_subset <- geo_awards |> 
  filter(!id%in%geo_terminated$id)

## Abstract word frequency
abstract_words <- geo_awards |> 
  unnest_tokens(word,abstractText) |> 
  count(id,word,sort=TRUE)

total_words <- abstract_words |> 
  group_by(id) |> 
  summarize(total = sum(n))


abstract_words <- left_join(abstract_words,total_words)

## Zipf's law
freq_by_rank = abstract_words |> 
  group_by(id) |> 
  mutate(rank = row_number(),
         `term frequency` = n/total)

## TF_IDF metric
abstract_words <- abstract_words |> 
  bind_tf_idf(word,id,n) |> 
  select(-total) |> 
  arrange(desc(tf_idf))

## LDA topic modeling
# geo_txt <- geo_awards$abstractText
# dtm <- DocumentTermMatrix(geo_txt)
# 
# raw.sum = apply(dtm,1,FUN=sum)
# dtm <- dtm[raw.sum!=0,]
# 
# geo_lda <- LDA(dtm,k=10,control=list(seed=42))


## Bigrams
library(tidyr)

geo_bigrams <- geo_awards |> 
  unnest_tokens(bigram,abstractText,token="ngrams",n=2)

geo_bigrams |> 
  count(bigram,sort=TRUE)

bigrams_separated <- geo_bigrams |> 
  separate(bigram,c("word1","word2"),sep=" ")


# removing stop words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% nsf_stop) %>%
  filter(!word2 %in% nsf_stop) 

# new bigrams count
bigram_counts = bigrams_filtered %>% 
  group_by(word1,word2) |> 
  mutate(n = n()) |> 
  ungroup() |> 
  filter(n < 1000) |> 
  select(word1,word2,n,divAbbr)

bigram_counts

# recombing bigrams
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united

# Measuring the tf-idf values of bigrams
bigram_tf_idf = bigrams_united %>%
  count(divAbbr,bigram) |> 
  bind_tf_idf(bigram,divAbbr,n) |> 
  arrange(desc(tf_idf))

top_15 <- bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(divAbbr) %>% 
  top_n(15)

# distinct_props <- top_15 |> 
#   group_by(divAbbr) |> 
#   summarize(n_distinct(id))

props_by_div <- geo_awards |> 
  count(divAbbr)
# plotting the results

bigram_plot <- bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(divAbbr) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(bigram, tf_idf)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "TF-IDF") +
  facet_wrap(~divAbbr, ncol = 2, scales = "free") +
  coord_flip()+
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_), # necessary to avoid drawing panel outline
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_), # necessary to avoid drawing plot outline
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent")
  )

# Bigram networks
library(igraph)
library(tidygraph)

bigram_graph <- bigram_counts |>
  filter(n>40) |>
  as_tbl_graph()

bigram_graph

library(ggraph)

a <- grid::arrow(type = "closed", length=unit(.15,"inches"))

ear_bigram <- bigram_counts |>
  filter(divAbbr=="EAR" & n>83) |>
  as_tbl_graph()

ags_bigram <- bigram_counts |>
  filter(n>40 & divAbbr=="AGS") |>
  as_tbl_graph()

oce_bigram <- bigram_counts |>
  filter(n>40 & divAbbr == "OCE") |>
  as_tbl_graph()

ear <- ggraph(ear_bigram,layout="stress") +
  geom_edge_fan() +
  geom_node_label(aes(label=name),repel=TRUE)+
  theme_void()

ags <- ggraph(ags_bigram,layout="stress") +
  geom_edge_fan()+
  geom_node_label(aes(label=name),repel=TRUE)+
  theme_void()

oce <- ggraph(oce_bigram,layout="stress") +
  geom_edge_fan()+
  geom_node_label(aes(label=name),repel=TRUE)+
  theme_void()

# ggraph(bigram_graph) +
#   geom_edge_fan() +
#   geom_node_point(aes(size = n),color = "lightblue", size = 3) +
#   geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
#   facet_graph(divAbbr~name)+
#   theme_void()

## Here lies geographic data
library(sf)
library(MetBrewer)
library(ggiraph)
library(glue)
library(ggtext)
library(patchwork)

geo_awards_sf <- geo_awards |> 
  count(awardeeStateCode) |> 
  full_join(us_states_hex,join_by(awardeeStateCode==st_code)) |> 
  mutate(q=case_when(n<quantile(n,0.98,na.rm=TRUE)~"White",
                             .default="Black"),
         tt=glue("<b>Jurisdiction: </b>{st_name}\n<b>Total awards: </b>{n}"),
         epscor=case_when(awardeeStateCode%in%epscor~"Y",
                          .default="N")) |> 
  st_as_sf()

geo_obs_sf <- geo_awards |> 
  group_by(awardeeStateCode) |> 
  mutate(total_obs = sum(as.numeric(fundsObligatedAmt))) |> 
  select(awardeeStateCode,total_obs) |> 
  distinct() |> 
  full_join(us_states_hex,join_by(awardeeStateCode==st_code)) |> 
  mutate(q=case_when(total_obs>quantile(total_obs,0.97,na.rm=TRUE)~"White",
                     .default="Black"),
         tt=glue("<b>Jurisdiction: </b>{st_name}\n<b>Total obligations: </b>{scales::dollar(total_obs)}"),
         epscor=case_when(awardeeStateCode%in%epscor~"Y",
                          .default="N")) |> 
  st_as_sf()

greens <- colorRampPalette(met.brewer("VanGogh3"),bias=1)

n_awds_title <- "Number of GEO awards by state"
n_awds_caption <- "Represents awards that expired between Jan. 1, 2025 and Jan. 1, 2026"

n_awds <- ggplot(geo_awards_sf)+
  geom_sf_interactive(aes(fill=n,tooltip=tt,data_id=epscor)) +
  geom_text(aes(label=awardeeStateCode,x=center_x,y=center_y,
                color=q)) +
  scale_color_manual(values=c("white","black"),guide="none")+
  scale_fill_met_c("VanGogh3")+
  labs(#title=n_awds_title,
       caption=n_awds_caption,
       fill="")+
  theme_void() #+
  # theme(
  #   panel.background = element_rect(fill = "transparent",
  #                                   colour = NA_character_), # necessary to avoid drawing panel outline
  #   panel.grid.major = element_blank(), # get rid of major grid
  #   panel.grid.minor = element_blank(), # get rid of minor grid
  #   plot.background = element_rect(fill = "transparent",
  #                                  colour = NA_character_), # necessary to avoid drawing plot outline
  #   legend.background = element_rect(fill = "transparent"),
  #   legend.box.background = element_rect(fill = "transparent"),
  #   legend.key = element_rect(fill = "transparent"),
  #   legend.position = "top"
  # )

obs_title <- "GEO obligations by state"
obs_caption <- "Represents awards that expired between Jan. 1, 2025 and Jan. 1, 2026"

options(scipen=10000)
obs <- ggplot(geo_obs_sf)+
  geom_sf_interactive(aes(fill=total_obs,tooltip=tt,data_id=epscor)) +
  geom_text(aes(label=awardeeStateCode,x=center_x,y=center_y,
                color=q)) +
  scale_color_manual(values=c("black","white"),guide="none")+
  scale_fill_met_c("VanGogh3",labels=scales::label_comma())+
  labs(#title=obs_title,
       caption=n_awds_caption,
       fill="")+
  theme_void() #+
  # theme(
  #   panel.background = element_rect(fill = "transparent",
  #                                   colour = NA_character_), # necessary to avoid drawing panel outline
  #   panel.grid.major = element_blank(), # get rid of major grid
  #   panel.grid.minor = element_blank(), # get rid of minor grid
  #   plot.background = element_rect(fill = "transparent",
  #                                  colour = NA_character_), # necessary to avoid drawing plot outline
  #   legend.background = element_rect(fill = "transparent"),
  #   legend.box.background = element_rect(fill = "transparent"),
  #   legend.key = element_rect(fill = "transparent")
  # )

n_awds_interactive <- girafe(ggobj=n_awds) |> 
  girafe_options(
    opts_hover(css="stroke:orange;stroke-width:2;")
  )

obs_interactive <- girafe(ggobj=obs) |> 
  girafe_options(
    opts_hover(css="stroke:orange;stroke-width:2;")
  )
combined_plot <- n_awds / obs

combined_plot <- girafe(ggobj=combined_plot)

combined_plot <- girafe_options(
  combined_plot,
  opts_hover(css="stroke:orange;stroke-width:3;")
)

## Data preview
geo_awards <- geo_awards |> 
  mutate(date = mdy(date),
         year = year(date))
n_awds_bar <- ggplot(geo_awards) +
  geom_bar(aes(y=year),stat="count")+
  labs(
    x = "Total awards",
    y = "Award start year"
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_), # necessary to avoid drawing panel outline
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_), # necessary to avoid drawing plot outline
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent")
  )


n_div_bar <- ggplot(geo_awards) +
  geom_bar(aes(y=divAbbr))+
  labs(
    y = "(Former) Division",
    x = "Total awards"
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_), # necessary to avoid drawing panel outline
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_), # necessary to avoid drawing plot outline
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent")
  )

terminated_bar <- geo_terminated |> 
  ggplot() +
  geom_bar(aes(y=divAbbr)) +
  labs(
    y = "(Former) Division",
    x = "Total terminated awards"
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_), # necessary to avoid drawing panel outline
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_), # necessary to avoid drawing plot outline
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent")
  )

terminated_sf <- geo_terminated |> 
  count(awardeeStateCode) |> 
  full_join(us_states_hex,join_by(awardeeStateCode==st_code)) |> 
  mutate(q=case_when(n<quantile(n,0.95,na.rm=TRUE)~"White",
                     .default="Black"),
         tt=glue("<b>Jurisdiction: </b>{st_name}\n<b>Total terminated awards: </b>{n}"),
         epscor=case_when(awardeeStateCode%in%epscor~"Y",
                          .default="N")) |> 
  st_as_sf()

terminated_geo_plot <- ggplot(terminated_sf)+
  geom_sf_interactive(aes(fill=n,tooltip=tt,data_id=epscor)) +
  geom_text(aes(label=awardeeStateCode,x=center_x,y=center_y,
                color=q)) +
  scale_color_manual(values=c("white","black"),guide="none")+
  scale_fill_met_c("VanGogh3")+
  labs(#title=n_awds_title,
    caption=n_awds_caption,
    fill="")+
  theme_void() #+

terminated_girafe<-girafe(ggobj=terminated_geo_plot)|> 
  girafe_options(
    opts_hover(css="stroke:orange;stroke-width:2;")
  )
