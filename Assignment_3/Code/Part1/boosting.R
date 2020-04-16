####################################################
# Class: Anly-601
# Script: Starter code for creating boosted models
# Author: Joshuah Touyz
# Version: 0.1
# Last updated: 03/19/20
####################################################

####################################
##### Loading libraries & data #####
####################################
library(tidyverse)
library(splines)

# Generating sample data
n=500
set.seed(1)
u=sort(runif(n)*5*pi)
y = sin(u)+rnorm(n)/4
df = data.frame(x=u,y=y)

# Validation And Testing Data
test_idx=sample(seq_len(n), size =50)
test <- df[test_idx, ]
remain <- df[-test_idx,]
df<-remain
row.names(df) <- NULL

valid_idx=sample(seq_len(450), size =50)
valid<-df[valid_idx,]
remain<-df[-valid_idx,]
df<-remain
row.names(df) <- NULL


# Setting up parameters
v=.05
number_of_weak_learners = 100
number_of_knots_split = 6
polynomial_degree = 2

# Fit round 1
fit=lm(y~bs(x,degree=2,df=6),data=df)
yp = predict(fit,newdata=df)
df$yr = df$y - v*yp
YP = v*yp
list_of_weak_learners = list(fit)

#################################
##### Boosting with Splines #####
#################################
for(t in 2:number_of_weak_learners){
  # Fit linear spline
  fit = lm(yr ~ bs(x, 
                   degree=polynomial_degree,
                   df=number_of_knots_split),data=df) 
  # Generate new prediction
  yp=predict(fit,newdata=df)
  # Update residuals
  df$yr=df$yr - v*yp
  # Bind to new data point
  YP = cbind(YP,v*yp)
  # Store fitted model in list
  list_of_weak_learners[[t]] = fit
}

##############################################
##### Getting predictions for each boost #####
##############################################
for (i in 1:number_of_weak_learners){
  # Calculating performance of first i weak_learners
  
  # Summing weak learner residuals
  if(i==1){yp_i = YP[,1:i]
  }else{yp_i=apply(YP[,1:i],1,sum) #<- strong learner
  }
  
  # Binds new cols
  col_name = paste0('yp_',i)
  df = df %>% bind_cols(yp=yp_i)
}

# Re-arrange sequences to get pseudo residuals 
plot_wl = df %>% select(-y,-yr) %>% 
  pivot_longer(cols = starts_with("yp")) %>% 
  mutate(learner = str_match(name,"[0-9]+")) %>% 
  mutate(learner = as.integer(ifelse(is.na(learner),0,learner)))

# Plot final learner
final_learner = plot_wl %>% filter(learner == (number_of_weak_learners-1))

# Plot progression of learner
ggplot() + 
  # Visualizing all learners
  geom_line(aes(x = x, y = value, group = learner, color =learner),
            data = plot_wl,alpha=0.5) +
  # Final learner
  geom_line(aes(x = x, y = value, group = learner, color =learner),
            data = final_learner,alpha=0.5,color = 'firebrick1',size = 2)  +
  geom_point(aes(x = x, y= y),data = df)+ # true values
  ggtitle('Plot progression of learner with original learning paremater v=0.05')+
  theme_minimal()


##################################
##### Predicting on new data #####
##################################

new_data = tibble(x = sample(seq(0,4*3,0.001),size = 100,replace = T))

for (i in 1:number_of_weak_learners){
  weak_learner_i = list_of_weak_learners[[i]]
  
  if (i==1){pred = v*predict(weak_learner_i,new_data)}
  else{pred =pred + v*predict(weak_learner_i,new_data)}
  
  if(i==number_of_weak_learners){
    new_data = new_data %>% bind_cols(yp=pred)
  }
}




###################################################
##### Visualizing boosted vs predicted models #####
##################################################
ggplot(aes(x=x, y=y),data = tibble(x = df$x, y = df$y))+
  xlab('')+ylab('')+ 
  geom_point()+
  # Final learner from training data
  geom_line(aes(x = x, y = value, group = learner, color =learner), data = final_learner , color = 'firebrick1',size = 2)  +
  # True value
  geom_line(aes(x=x,y=y),data = tibble(x = u,y = sin(u)), color='black',linetype = 'dashed')+ # true values
  # Prediction on new data
  geom_line(aes(x=x,y=yp),data = new_data, color='blue',size = 2,alpha = 0.5)+ # predicted values
  ggtitle('boosted vs predicted models Original learning parameter v=0.05')+
  theme_minimal()
