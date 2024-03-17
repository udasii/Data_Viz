#' Name: Suraj Udasi
#' Project: Individual Assignment - Text Mining / NLP for Unstructured Data
#' Date: 3/14/2024
#' Description: Using Airbnb data from MongoDB cluster analyzing descriptions of listings


# libraries
#install.packages("cld2")
library(mongolite)
library(tidytuesdayR)
library(tidytext)
library(tidyr)
library(purrr)
library(tm)
library(scales)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(plotly)
library(igraph)
library(ggraph)
library(topicmodels)
library(cld2)
# Working Directory
setwd("C:/Users/suraj/iCloudDrive/Hult/1. Masters in Business Analytics, 2023/2. Spring 2024/4. Unstructured Data/Unstructured Data/personalFiles/Individual Assignment")

# Establishing connection and downloading data from MongoDB cluster
# This is the connection_string to MongoDB server
connection_string <- 'mongodb+srv://surajudasi:<Password>@mbanddcluster.z1qmm2m.mongodb.net/?retryWrites=true&w=majority&appName=MBANDDCluster'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

#Download all the Airbnb data from Mongo
airbnb_all <- airbnb_collection$find()

# Convert 'descriptions' column to 'text' this will be the column of interest for our analysis
colnames(airbnb_all)
colnames(airbnb_all)[5] <- "text"

airbnb_us <- subset(airbnb_all, grepl("United States|US|NY", host$host_location))

airbnb <- airbnb_us[c("property_type", "room_type", "accommodates", "number_of_reviews", "price", "text")]

only_alphabets <- function(text) {
  # Replace non-aplhabetic characters with empty string
  str_replace_all(text, "[^a-zA-Z]", " ")
}

airbnb$text <- only_alphabets(airbnb$text)

# EDA
summary(airbnb)

# 1. Bar plot for count of properties in each property_type
airbnb %>%
  count(property_type)
airbnb %>%
  count(property_type) %>%
  top_n(10, n) %>%
  mutate(property_type = reorder(property_type, -n)) %>%
  ggplot(aes(x = property_type, y = n, fill = property_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Properties by Top 10 Property Type",
       x = "Property Type",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# 2. Bar plot for count of properties in each room_type
ggplot(airbnb, aes(x = reorder(room_type, -table(room_type)[room_type]), fill = room_type)) +
  geom_bar() +
  labs(title = "Count of Properties by Room Type",
       x = "Room Type",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set2")


#3. Calculate summary statistics for room types and top 3 property types
room_type_summary <- airbnb %>%
  group_by(room_type) %>%
  summarise(avg_accommodates = mean(accommodates),
            avg_number_of_reviews = mean(number_of_reviews),
            avg_price = mean(price),
            count = n())
room_type_summary # Entire home/apt , Private room and Shared room

Top3_prop_type_summary <- airbnb %>%
  group_by(property_type) %>%
  summarise(total_count = n()) %>%
  top_n(3, total_count) %>%
  inner_join(airbnb, by = "property_type") %>%
  group_by(property_type) %>%
  summarise(avg_accommodates = mean(accommodates),
            avg_number_of_reviews = mean(number_of_reviews),
            avg_price = mean(price),
            count = n())
Top3_prop_type_summary # Apartment, Condominium and House


#################
## Text mining ##
#################

# loading stop words
data("stop_words")

##################################################
## creating a tidy format for Property Types #####
##################################################

apt <- airbnb %>%
  filter(property_type== "Apartment")

tidy_apt <- apt %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_apt)

### creating a tidy format for Condominium
condo <- airbnb %>%
  filter(property_type== "Condominium")

tidy_condo <- condo %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_condo)

### creating a tidy format for House
house <- airbnb %>%
  filter(property_type== "House")

tidy_house <- house %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_house)

################################################################
## We want to combine all property types and check frequencies #
################################################################

frequency_pt <- bind_rows(mutate(tidy_apt, property_type="Apartment"),
                          mutate(tidy_condo, property_type= "Condominium"),
                          mutate(tidy_house, property_type="House")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(property_type, word) %>%
  group_by(property_type) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(property_type, proportion) %>%
  gather(property_type, proportion, `Condominium`, `House`)

#let's plot the correlograms:
ggplot(frequency_pt, aes(x=proportion, y=`Apartment`, 
                         color = abs(`Apartment` - proportion))) +
  geom_abline(color="grey40", lty=2, size=0.5) +  # Lighter reference line
  geom_jitter(alpha=0.1, size=2.5, width=0.3, height=0.3) +  
  geom_text(aes(label=word), check_overlap = TRUE, hjust=0.5, vjust=0, size=5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels= percent_format()) +
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "darkgrey") +  # Subtle gradient
  facet_wrap(~property_type, ncol=2) + 
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),  # Bold facet labels
        axis.text = element_text(size = 10),  # Legible axis text
        axis.title = element_text(size = 12, face = "bold"),  # Styled axis titles
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  # Styled plot title
  labs(y = "Apartment", x = "Overall Proportion (log scale)") 


##################################################
## creating a tidy format for Room Types #########
##################################################

eh <- airbnb %>%
  filter(room_type== "Entire home/apt")

tidy_eh <- eh %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_eh)

### creating a tidy format for Private Room
pr <- airbnb %>%
  filter(room_type== "Private room")

tidy_pr <- pr %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_pr)

### creating a tidy format for Shared Room
sr <- airbnb %>%
  filter(room_type== "Shared room")

tidy_sr <- sr %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_sr)

################################################################
## We want to combine all room types and check frequencies #####
################################################################

frequency_rt <- bind_rows(mutate(tidy_eh, room_type="Entire home/apt"),
                          mutate(tidy_pr, room_type= "Private room"),
                          mutate(tidy_sr, room_type="Shared room")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(room_type, word) %>%
  group_by(room_type) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(room_type, proportion) %>%
  gather(room_type, proportion, `Private room`, `Shared room`)

#let's plot the correlograms:
ggplot(frequency_rt, aes(x=proportion, y=`Entire home/apt`, 
                         color = abs(`Entire home/apt`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~room_type, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Entire home/apt", x=NULL)

######################################################
####### TF-IDF framework in Airbnb  #######
######################################################

# Grouping by the room-type this time
airbnb_rt_token <- airbnb %>%
  unnest_tokens(word, text) %>%
  count(room_type, word, sort=TRUE) %>%
  ungroup()

total_rt_words <- airbnb_rt_token %>%
  group_by(room_type) %>%
  summarize(total=sum(n))

airbnb_rt_words <- left_join(airbnb_rt_token, total_rt_words)%>%
  filter(room_type %in% c("Entire home/apt", "Private room", "Shared room"))

print(airbnb_rt_words)

ggplot(airbnb_rt_words, aes(n/total, fill = room_type))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~room_type, ncol=2, scales="free_y")
#what do the tails represent? 
#answer: exremely common words! 
# we are really interested in the not so common words. 


# Grouping by the property-type this time
airbnb_pt_token <- airbnb %>%
  unnest_tokens(word, text) %>%
  count(property_type, word, sort=TRUE) %>%
  ungroup()

total_pt_words <- airbnb_pt_token %>%
  group_by(property_type) %>%
  summarize(total=sum(n))

airbnb_pt_words <- left_join(airbnb_pt_token, total_pt_words)%>%
  filter(property_type %in% c("Apartment", "Condominium", "House"))

print(airbnb_pt_words)

ggplot(airbnb_pt_words, aes(n/total, fill = property_type))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~property_type, ncol=2, scales="free_y")

######################################
########## ZIPF's law ################
######################################

freq_by_rank_rt <- airbnb_rt_words %>%
  group_by(room_type) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank_rt

freq_by_rank_pt <- airbnb_pt_words %>%
  group_by(property_type) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank_pt

#let's plot ZIPF's Law
freq_by_rank_rt %>%
  ggplot(aes(rank, `term frequency`, color=room_type))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

freq_by_rank_pt %>%
  ggplot(aes(rank, `term frequency`, color=property_type))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

###################################################
################# TF_IDF ##########################
###################################################

rt_words <- airbnb_rt_words %>%
  bind_tf_idf(word, room_type, n)
rt_words # we get all the zeors because we are looking at stop words ... too common

rt_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?

pt_words <- airbnb_pt_words %>%
  bind_tf_idf(word, property_type, n)
pt_words # we get all the zeors because we are looking at stop words ... too common

pt_words %>%
  arrange(desc(tf_idf))

#######################################
## looking at the graphical approach: #
#######################################

tf_idf_plot_rt <- rt_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(room_type) %>%
  top_n(15) %>%
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = room_type)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "TF-IDF Score") +
  facet_wrap(~room_type, ncol = 3, scales = "free_y") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, hjust = 1),
        axis.text.y = element_text(size = 9),
        strip.text.x = element_text(size = 10, face = "bold"),
        strip.background = element_rect(fill = "lightblue"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold")) +
  scale_fill_brewer(palette = "Pastel1")


tf_idf_plot_pt <- pt_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(property_type) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = property_type)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "TF-IDF Score") +
  facet_wrap(~property_type, ncol = 3, scales = "free_y") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, hjust = 1),
        axis.text.y = element_text(size = 9),
        strip.text.x = element_text(size = 10, face = "bold"),
        strip.background = element_rect(fill = "lightblue"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold")) +
  scale_fill_brewer(palette = "Pastel1")

#############################################
###### N-grams and tokenizing ###############
#############################################

airbnb_bigrams <- airbnb %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
head(airbnb_bigrams)

bigram_counts <- airbnb_bigrams %>%
  count(word1, word2, sort = TRUE)
head(bigram_counts)

###########################################################
###### What if we are interested in the most common #######
################ 4 consecutive words - quadro-gram ########
###########################################################
airbnb_quadrogram <- airbnb %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 
head(airbnb_quadrogram)

quadrogram_counts <- airbnb_quadrogram %>%
  count(word1, word2, word3, word4, sort = TRUE)
head(quadrogram_counts)

###########################################################
###### We can also apply the tf_idf framework  ############
########### on our bigram and quadro-gram #################
###########################################################

bigram_united <- airbnb_bigrams %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

# For each room Type
rt_bigram_tf_idf <- bigram_united %>%
  count(room_type, bigram) %>%
  bind_tf_idf(bigram, room_type , n) %>%
  arrange(desc(tf_idf))
head(rt_bigram_tf_idf)

eh_bigram_tf_idf <- rt_bigram_tf_idf %>% 
  filter(room_type == "Entire home/apt")
head(eh_bigram_tf_idf)

pr_bigram_tf_idf <- rt_bigram_tf_idf %>% 
  filter(room_type == "Private room")
head(pr_bigram_tf_idf)

sr_bigram_tf_idf <- rt_bigram_tf_idf %>% 
  filter(room_type == "Shared room")
head(sr_bigram_tf_idf)

# For each property Type
pt_bigram_tf_idf <- bigram_united %>%
  count(property_type, bigram) %>%
  bind_tf_idf(bigram, property_type , n) %>%
  arrange(desc(tf_idf))
head(pt_bigram_tf_idf)

apt_bigram_tf_idf <- pt_bigram_tf_idf %>% 
  filter(property_type == "Apartment")
head(apt_bigram_tf_idf)

condo_bigram_tf_idf <- pt_bigram_tf_idf %>% 
  filter(property_type == "Condominium")
head(condo_bigram_tf_idf)

house_bigram_tf_idf <- pt_bigram_tf_idf %>% 
  filter(property_type == "House")
head(house_bigram_tf_idf)

##### lets do the same for a quadrogram
quadrogram_united <- airbnb_quadrogram %>%
  unite(quadrogram, word1, word2, word3, word4, sep=" ") #we need to unite what we split in the previous section

# For each room Type
rt_quadrogram_tf_idf <- quadrogram_united %>%
  count(room_type, quadrogram) %>%
  bind_tf_idf(quadrogram, room_type, n) %>%
  arrange(desc(tf_idf))
head(rt_quadrogram_tf_idf)

eh_quadrogram_tf_idf <- rt_quadrogram_tf_idf %>% 
  filter(room_type == "Entire home/apt")
head(eh_quadrogram_tf_idf)

pr_quadrogram_tf_idf <- rt_quadrogram_tf_idf %>% 
  filter(room_type == "Private room")
head(pr_quadrogram_tf_idf)

sr_quadrogram_tf_idf <- rt_quadrogram_tf_idf %>% 
  filter(room_type == "Shared room")
head(sr_quadrogram_tf_idf)

# For each Property Type
pt_quadrogram_tf_idf <- quadrogram_united %>%
  count(property_type, quadrogram) %>%
  bind_tf_idf(quadrogram, property_type, n) %>%
  arrange(desc(tf_idf))
head(pt_quadrogram_tf_idf)

apt_quadrogram_tf_idf <- pt_quadrogram_tf_idf %>% 
  filter(property_type == "Apartment")
head(apt_quadrogram_tf_idf)

condo_quadrogram_tf_idf <- pt_quadrogram_tf_idf %>% 
  filter(property_type == "Condominium")
head(condo_quadrogram_tf_idf)

house_quadrogram_tf_idf <- pt_quadrogram_tf_idf %>% 
  filter(property_type == "House")
head(house_quadrogram_tf_idf)

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

### For Property Type

# Property Type
pt_bigram_graph <- pt_bigram_tf_idf %>%
  separate(bigram, c("word1", "word2"), sep=" ") %>% 
  slice_head(n = 30) %>%
  select(word1, word2) %>%
  graph_from_data_frame()

pt_bigram_graph_plot <- ggraph(pt_bigram_graph, layout = "fr") +
  geom_edge_link(color = "darkgray", alpha = 0.8) + 
  geom_node_point(color = "#FF585D", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, fontface = "bold", color = "black", alpha = 0.9) + 
  theme_void()

# Apartment
apt_bigram_graph <- apt_bigram_tf_idf %>%
  separate(bigram, c("word1", "word2"), sep=" ") %>% 
  slice_head(n = 30) %>%
  select(word1, word2) %>%
  graph_from_data_frame()

apt_bigram_graph_plot <- ggraph(apt_bigram_graph, layout = "fr") +
  geom_edge_link(color = "darkgray", alpha = 0.8) + 
  geom_node_point(color = "#FF585D", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, fontface = "bold", color = "black", alpha = 0.9) + 
  theme_void()

# Condo
condo_bigram_graph <- condo_bigram_tf_idf %>%
  separate(bigram, c("word1", "word2"), sep=" ") %>% 
  slice_head(n = 30) %>%
  select(word1, word2) %>%
  graph_from_data_frame()

condo_bigram_graph_plot <- ggraph(condo_bigram_graph, layout = "fr") +
  geom_edge_link(color = "darkgray", alpha = 0.8) + 
  geom_node_point(color = "#FF585D", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, fontface = "bold", color = "black", alpha = 0.9) + 
  theme_void()

# House
house_bigram_graph <- house_bigram_tf_idf %>%
  separate(bigram, c("word1", "word2"), sep=" ") %>% 
  slice_head(n = 30) %>%
  select(word1, word2) %>%
  graph_from_data_frame()

house_bigram_graph_plot <- ggraph(house_bigram_graph, layout = "fr") +
  geom_edge_link(color = "darkgray", alpha = 0.8) + 
  geom_node_point(color = "#FF585D", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, fontface = "bold", color = "black", alpha = 0.9) + 
  theme_void()

### For Room Type

# Room Type
rt_bigram_graph <- rt_bigram_tf_idf %>%
  separate(bigram, c("word1", "word2"), sep=" ") %>% 
  slice_head(n = 30) %>%
  select(word1, word2) %>%
  graph_from_data_frame()

rt_bigram_graph_plot <- ggraph(rt_bigram_graph, layout = "fr") +
  geom_edge_link(color = "#FF585D", alpha = 0.8) + 
  geom_node_point(color = "#484848", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, fontface = "bold", color = "black", alpha = 0.9) + 
  theme_void()

# Entire home/apt
eh_bigram_graph <- eh_bigram_tf_idf %>%
  separate(bigram, c("word1", "word2"), sep=" ") %>% 
  slice_head(n = 30) %>%
  select(word1, word2) %>%
  graph_from_data_frame()

eh_bigram_graph_plot <- ggraph(eh_bigram_graph, layout = "fr") +
  geom_edge_link(color = "#FF585D", alpha = 0.8) + 
  geom_node_point(color = "#484848", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, fontface = "bold", color = "black", alpha = 0.9) + 
  theme_void()

# Private room
pr_bigram_graph <- pr_bigram_tf_idf %>%
  separate(bigram, c("word1", "word2"), sep=" ") %>% 
  slice_head(n = 30) %>%
  select(word1, word2) %>%
  graph_from_data_frame()

pr_bigram_graph_plot <- ggraph(pr_bigram_graph, layout = "fr") +
  geom_edge_link(color = "#FF585D", alpha = 0.8) + 
  geom_node_point(color = "#484848", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, fontface = "bold", color = "black", alpha = 0.9) + 
  theme_void()

# Shared room
sr_bigram_graph <- sr_bigram_tf_idf %>%
  separate(bigram, c("word1", "word2"), sep=" ") %>% 
  slice_head(n = 30) %>%
  select(word1, word2) %>%
  graph_from_data_frame()

sr_bigram_graph_plot <- ggraph(sr_bigram_graph, layout = "fr") +
  geom_edge_link(color = "#FF585D", alpha = 0.8) + 
  geom_node_point(color = "#484848", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, fontface = "bold", color = "black", alpha = 0.9) + 
  theme_void()


################################################
###### Latent Dirichlet algorithm ##############
################################################

# There are two principles:
#1. Every document is a combination of multiple topics
#2. Every topic is a combination of multiple words

# Airbnb DTM by room-type
airbnb_rt_dtm <- airbnb %>%
  unnest_tokens(word, text) %>%
  count(room_type, word) %>%
  anti_join(stop_words) %>% 
  cast_dtm(room_type, word, n)

airbnb_rt_dtm

# Airbnb DTM by property-type
airbnb_pt_dtm <- airbnb %>%
  filter(property_type %in% c("Apartment", "Condominium", "House")) %>%
  unnest_tokens(word, text) %>%
  count(property_type, word) %>%
  anti_join(stop_words) %>% 
  cast_dtm(property_type, word, n)
airbnb_pt_dtm

#calling the Latent Dirichlet Allocation algorithm

# LDA on Room type DTM
airbnb_rt_lda <- LDA(airbnb_rt_dtm, k=4, control=list(seed=123))
airbnb_rt_lda

#now we are looking for the per topic per word probabilities aka. beta
#beta - what is the probability that "this term" will be generated by "this topic"

airbnb_rt_topics <- tidy(airbnb_rt_lda, matrix="beta")
airbnb_rt_topics

rt_top_terms <- airbnb_rt_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
rt_top_terms

#lets plot the term frequencies by topic
rt_top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  labs(x = "Term", y = "Beta", title = "Term Frequencies by Topic by Room Type")

airbnb_rt_gamma <- tidy(airbnb_rt_lda, matrix="gamma")
airbnb_rt_gamma 

ggplot(airbnb_rt_gamma, aes(x = document, y = gamma, fill = factor(topic))) +
  geom_bar(stat = "identity") +
  labs(title = "Topic Probabilities by Document",
       x = "Document Type",
       y = "Gamma") +
  theme_minimal() +
  scale_fill_discrete(name = "Topic")

# LDA on Property type DTM
airbnb_pt_lda <- LDA(airbnb_pt_dtm, k=4, control=list(seed=123))
airbnb_pt_lda

#now we are looking for the per topic per word probabilities aka. beta
#beta - what is the probability that "this term" will be generated by "this topic"

airbnb_pt_topics <- tidy(airbnb_pt_lda, matrix="beta")
airbnb_pt_topics

pt_top_terms <- airbnb_pt_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
rt_top_terms

#lets plot the term frequencies by topic
pt_top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  labs(x = "Term", y = "Beta", title = "Term Frequencies by Topic by Property Type")

airbnb_pt_gamma <- tidy(airbnb_pt_lda, matrix="gamma")
airbnb_pt_gamma 

ggplot(airbnb_pt_gamma, aes(x = document, y = gamma, fill = factor(topic))) +
  geom_bar(stat = "identity") +
  labs(title = "Topic Probabilities by Document",
       x = "Document Type",
       y = "Gamma") +
  theme_minimal() +
  scale_fill_discrete(name = "Topic")


##########################################
## Sentiment Analysis ################
##########################################

# Airbnb Data
airbnb_token <- airbnb %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

afinn <- airbnb_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_nrc <- bind_rows(
  airbnb_token %>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  airbnb_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

airbnb_sent <- bind_rows(afinn, bing_nrc) %>%
  ggplot(aes(x = method, y = sentiment, fill = method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y") +
  scale_fill_brewer(palette = "Pastel1", name = "Sentiment Analysis Method") +
  labs(x = "Method",
       y = "Sentiment Score") +
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, size = 12),
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12))


# Entire Home
entire_home <- airbnb_token %>%
  filter(room_type == "Entire home/apt")

afinn_eh <- entire_home %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_nrc_eh <- bind_rows(
  entire_home %>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  entire_home %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

eh_sent <- bind_rows(afinn_eh, bing_nrc_eh) %>%
  ggplot(aes(x = method, y = sentiment, fill = method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y") +
  scale_fill_brewer(palette = "Pastel1", name = "Sentiment Analysis Method") +
  labs(x = "Method",
       y = "Sentiment Score") +
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, size = 12),
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12))

# Private Room
private_room <- airbnb_token %>%
  filter(room_type == "Private room")

afinn_pr <- private_room %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_nrc_pr <- bind_rows(
  private_room %>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  private_room %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

pr_sent <- bind_rows(afinn_pr, bing_nrc_pr) %>%
  ggplot(aes(x = method, y = sentiment, fill = method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y") +
  scale_fill_brewer(palette = "Pastel1", name = "Sentiment Analysis Method") +
  labs(x = "Method",
       y = "Sentiment Score") +
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, size = 12),
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12))

# Shared Room
shared_room <- airbnb_token %>%
  filter(room_type == "Shared room")

afinn_sr <- shared_room %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_nrc_sr <- bind_rows(
  shared_room %>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  shared_room %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

sr_sent <- bind_rows(afinn_sr, bing_nrc_sr) %>%
  ggplot(aes(x = method, y = sentiment, fill = method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y") +
  scale_fill_brewer(palette = "Pastel1", name = "Sentiment Analysis Method") +
  labs(x = "Method",
       y = "Sentiment Score") +
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, size = 12),
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12))

# Apartment
apt <- airbnb_token %>%
  filter(property_type == "Apartment")

afinn_apt <- apt %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_nrc_apt <- bind_rows(
  apt %>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  apt %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

apt_sent <- bind_rows(afinn_apt, bing_nrc_apt) %>%
  ggplot(aes(x = method, y = sentiment, fill = method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y") +
  scale_fill_brewer(palette = "Pastel1", name = "Sentiment Analysis Method") +
  labs(x = "Method",
       y = "Sentiment Score") +
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, size = 12),
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12))

# Condo
condo <- airbnb_token %>%
  filter(property_type == "Condominium")

afinn_condo <- condo %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_nrc_condo <- bind_rows(
  condo %>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  condo %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

condo_sent <- bind_rows(afinn_condo, bing_nrc_condo) %>%
  ggplot(aes(x = method, y = sentiment, fill = method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y") +
  scale_fill_brewer(palette = "Pastel1", name = "Sentiment Analysis Method") +
  labs(x = "Method",
       y = "Sentiment Score") +
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, size = 12),
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12))

# House
house <- airbnb_token %>%
  filter(property_type == "House")

afinn_house <- house %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_nrc_house <- bind_rows(
  house %>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  house %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

house_sent <- bind_rows(afinn_house, bing_nrc_house) %>%
  ggplot(aes(x = method, y = sentiment, fill = method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y") +
  scale_fill_brewer(palette = "Pastel1", name = "Sentiment Analysis Method") +
  labs(x = "Method",
       y = "Sentiment Score") +
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, size = 12),
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12))


##############################################################
######## Most common positive and negative words #############
##############################################################

bing_counts <- airbnb_token %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_counts

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

bing_counts_eh <- entire_home %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_counts_eh

bing_counts_eh %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

bing_counts_pr <- private_room %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_counts_pr

bing_counts_pr %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

bing_counts_sr <- shared_room %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_counts_sr

bing_counts_sr %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()





