#############################################
# Class: Anly-601
# Script: Explore tsne embeddings vs 
#         PCA using MNIST
# Author: Joshuah Touyz
# Version: 0.1
# Last updated: 03/19/20
#############################################

####################################
##### Loading libraries & data #####
####################################
library(tidyverse)
library(Rtsne)
library(RColorBrewer)

# Get MNIST data
mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)

# What is the dimension of the data set
dim(mnist_raw) # first column is the value, the rest are the pixels

# Rearranging the data
pixels_gathered <- mnist_raw %>% head(10000) %>%
  rename(label = X1) %>%
  mutate(instance = row_number()) %>%
  gather(pixel, value, -label, -instance) %>%
  extract(pixel, "pixel", "(\\d+)", convert = TRUE) %>%
  mutate(pixel = pixel - 2,
         x = pixel %% 28,
         y = 28 - pixel %/% 28)

first_10k_samples =  mnist_raw[1:10000,-1] #%>% as.matrix()
first_10k_samples_labels =  mnist_raw[1:10000,1] %>% unlist(use.names=F)
colors = brewer.pal(10, 'Spectral')

# Visualizing the data
theme_set(theme_light())
pixels_gathered %>%
  filter(instance <= 12) %>%
  ggplot(aes(x, y, fill = value)) +
  geom_tile() +
  facet_grid(label~ instance )

##############################################
##### Visualizing the PCA decomposition  #####
##############################################
pca = princomp(first_10k_samples)$scores[,1:2]
pca_plot = tibble(x = pca[,1], y =pca[,2], labels = as.character(first_10k_samples_labels))
ggplot(aes(x = x, y=y,label = labels, color = labels), data = pca_plot) + geom_text() + 
  xlab('PCA component 1') +ylab('PCA component 2')

##############################################
#####     Running the TSNE emebdding     #####
##############################################
itercost = c()
embedding = Rtsne(X = first_10k_samples, dims = 2, 
                  perplexity = 1, 
                  theta = 0.5, 
                  eta = 200,
                  pca = TRUE, verbose = TRUE, 
                  max_iter = 500)
itercost = c(itercost,embedding$itercosts[10]) 

# Visualizing TSNE output
embedding_plot = tibble(x = embedding$Y[,1], y = embedding$Y[,2], 
                        labels = as.character(first_10k_samples_labels))
ggplot(aes(x = x, y=y,label = labels, color = labels), data = embedding_plot) + 
  geom_text() +xlab('tSNE dimension 1') +ylab('tSNE dimension 2"')




plot(c(5,20,60,100,125,160),itercost,type='l',xlab='perplexity',ylab='KL Divergence')