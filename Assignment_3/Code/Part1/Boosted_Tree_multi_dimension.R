####################################################
# Class: Anly-601
# Script: Boosted Tree
# Author: Shaoyu Feng
# Version: 0.1
# Last updated: 03/19/20
####################################################
require(rpart)
# Read in data
df<-read.csv('kernel_regression_2.csv')

# Setting up parameters
v=.125
number_of_weak_learners = 100

# Fit round 1
fit=rpart(z~.,data=df)
zp = predict(fit,newdata=df)
df$zr = df$z - v*zp
ZP = v*zp
list_of_weak_learners = list(fit)

#################################
##### Boosting with trees #####
#################################
for(t in 2:number_of_weak_learners){
  # Fit linear spline
  fit = rpart(zr ~.,data=df) 
  # Generate new prediction
  zp=predict(fit,newdata=df)
  # Update residuals
  df$zr=df$zr - v*zp
  # Bind to new data point
  ZP = cbind(ZP,v*zp)
  # Store fitted model in list
  list_of_weak_learners[[t]] = fit
}

##############################################
##### Getting predictions for each boost #####
##############################################
for (i in 1:number_of_weak_learners){
  # Calculating performance of first i weak_learners
  
  # Summing weak learner residuals
  if(i==1){zp_i = ZP[,1:i]
  }else{yp_i=apply(ZP[,1:i],1,sum) #<- strong learner
  }
  
  # Binds new cols
  col_name = paste0('yp_',i)
  df = df %>% bind_cols(zp=zp_i)
}

# Re-arrange sequences to get pseudo residuals 
plot_wl = df %>% select(-z,-zr) %>% 
  pivot_longer(cols = starts_with("zp")) %>% 
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
  geom_point(aes(x = x, y= z),data = df)+ # true values
  ggtitle('Plot progression of learner with original learning paremater v=0.125')+
  theme_minimal()















