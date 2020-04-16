####################################################
# Class: Anly-601
# Script: Starter code for embedding
# Author: Joshuah Touyz
# Version: 0.1
# Last updated: 03/28/20
####################################################

####################################
#####     Install libraries     #####
####################################
install.packages("devtools")
devtools::install_github("bmschmidt/wordVectors")

####################################
##### Loading libraries & data #####
####################################
library(wordVectors)
library(Rtsne)
library(tidytext)
library(tidyverse)

####################################
#####       Download data      #####
####################################
# -- Check to see  if file exists --
if (!file.exists("cookbooks.zip")) {
  download.file("http://archive.lib.msu.edu/dinfo/feedingamerica/cookbook_text.zip","cookbooks.zip")
}
unzip("cookbooks.zip",exdir="cookbooks")
if (!file.exists("cookbooks.txt")) prep_word2vec(origin="cookbooks",destination="cookbooks.txt",lowercase=T,bundle_ngrams=1)

# Training a Word2Vec model
if (!file.exists("cookbook_vectors.bin")) {
  model = train_word2vec("cookbooks.txt","cookbook_vectors.bin",
                         vectors=100,threads=4,window=6,
                         min_count = 10,
                         iter=5,negative_samples=15)
} else{
    model = read.vectors("cookbook_vectors.bin")
    }

####################################
#####      Proximity search    #####
####################################

# -- Select ingredient and cuisine --
ingredient = 'sour'
ingredient_2 = 'spicy'
ingredient_3 = 'hot'
list_of_ingredients = c(ingredient, ingredient_2, ingredient_3)
cuisine = 'mexico'

# Coordinages in 300D space of embedding for the word "sage" 
model[[ingredient]]

# Searching closest words to sage
model %>% closest_to(model[[ingredient]]) #<- set of closest ingredients to "sage"
model %>% closest_to(model[[cuisine]], 20) #<- set of closest cuisines to "italian"

# Set of closest words to "sage", "thyme","basil"
model %>% closest_to(model[[list_of_ingredients]],10)

#############################################
#####   Using TSNE to see similarity    #####
#############################################
# We have a list of potential herb-related words from old cookbooks. 
n_words = 100
closest_ingredients = closest_to(model,model[[list_of_ingredients]], n_words)$word
surrounding_ingredients = model[[closest_ingredients,average=F]]
plot(surrounding_ingredients,method="pca")

embedding = Rtsne(X = surrounding_ingredients, dims = 2, 
                  perplexity = 4, 
                  theta = 0.5, 
                  eta = 10,
                  pca = TRUE, verbose = TRUE, 
                  max_iter = 2000)
embedding_vals = embedding$Y
rownames(embedding_vals) = rownames(surrounding_ingredients)

# Looking for clusters for embedding
set.seed(10)
n_centers = 3
clustering = kmeans(embedding_vals,centers=n_centers,
                    iter.max = 5)

# Setting up data for plotting
embedding_plot = tibble(x = embedding$Y[,1], 
                        y = embedding$Y[,2],
                        labels = rownames(surrounding_ingredients)) %>% 
  bind_cols(cluster = as.character(clustering$cluster))

# Visualizing TSNE output
ggplot(aes(x = x, y=y,label = labels, color = cluster), data = embedding_plot) + 
  geom_text() +xlab('tSNE dimension 1') +ylab('tSNE dimension 2"')+theme(legend.position = 'none')

# Topics produced by the top 3 words
sapply(sample(1:n_centers,n_centers),function(n) {
  names(clustering$cluster[clustering$cluster==n][1:10])
})

##################################################
#####   Plotting Sweet and Salty Dimensions  #####
##################################################
# -- Plotting across the sweet-salty plane --
tastes = model[[c("sweet","salty"),average=F]]
sweet_and_saltiness = model[1:500,] %>% cosineSimilarity(tastes)

# Filter to the top n words for sweet or salty.
top_n_words = 10
sweet_and_saltiness = sweet_and_saltiness[
  rank(-sweet_and_saltiness[,1])<top_n_words | 
    rank(-sweet_and_saltiness[,2])<top_n_words,
  ]
plot(sweet_and_saltiness,type='n')
text(sweet_and_saltiness,labels=rownames(sweet_and_saltiness))

###########################################
#####   Plotting 5 Taste  Dimensions  #####
###########################################
# We can plot along mltiple dimensions:
set_of_tastes = c("salty","sweet","savory","bitter","sour")
tastes = model[[set_of_tastes,average=F]]

# --- Most similar terms  ---
high_similarities_to_tastes = common_similarities_tastes[rank(-apply(common_similarities_tastes,1,max)) < 30,]

# - Plotting
high_similarities_to_tastes %>% 
  as_tibble(rownames='word') %>%
  filter( ! (is.element(word,set_of_tastes))) %>%
  #mutate(total = salty+sweet+savory+bitter+sour) %>%
  #mutate( sweet=sweet/total,salty=salty/total,savory=savory/total,bitter=bitter/total, sour = sour/total) %>% 
  #select(-total) %>%
  gather(key = 'key', value = 'value',-word) %>%
  ggplot(aes(x = word,
             y = value, 
             fill = key)) + geom_bar(stat='identity') + 
  coord_flip() + theme_minimal() + scale_fill_brewer(palette='Spectral')
  
# - Plotting similarties by PCA - 
high_similarities_to_tastes %>% 
  prcomp %>% 
  biplot(main="Fifty words in a\nprojection of flavor space")

##################################
#####   Vector calculations  #####
##################################
model %>% closest_to("health") # words associated with haelthy living (if not a bit outdated)
model %>% closest_to(~("health" - "cream" ),15) # number 7 is cravings
model %>% closest_to(~"orange" + ("pretzel"- "salty"),15)

model %>% closest_to(~"french" + ("florentine" - "kebab"),15)

top_evaluative_words = model %>% 
  closest_to(~ "honey"+"toast",n=30)
goodness = model %>% 
  closest_to(~ "honey"-"toast",n=Inf) 
taste = model %>% 
  closest_to(~ "egg" + "tomato", n=Inf)

top_evaluative_words %>%
  inner_join(goodness) %>%
  inner_join(taste) %>%
  ggplot() + 
  geom_text(aes(x=`similarity to "honey" - "toast"`,
                y=`similarity to "egg" + "tomato"`,
                label=word))


if (!file.exists("cookbooks.txt")){prep_word2vec(origin="cookbooks",destination="cookbooks.txt",lowercase=T,bundle_ngrams=2)}
model = train_word2vec("cookbooks.txt","cookbook_vectors.bin",
                       vectors=100,threads=4,window=6,
                       min_count = 10,
                       iter=5,negative_samples=15)

install.packages("ngram")
require(ngram)
x<-read_file('cookbooks.txt')
ng <- ngram(x, n=2)


get.phrasetable(ng)
