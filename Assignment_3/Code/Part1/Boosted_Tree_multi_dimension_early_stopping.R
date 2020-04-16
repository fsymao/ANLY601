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

# Validation And Testing Data
test_idx=sample(seq_len(1156), size =100)
test <- df[test_idx, ]
remain <- df[-test_idx,]
df<-remain
row.names(df) <- NULL

valid_idx=sample(seq_len(1056), size =100)
valid<-df[valid_idx,]
remain<-df[-valid_idx,]
df<-remain
row.names(df) <- NULL



# Setting up parameters
v=.05
stop=FALSE
t=2

# Fit round 1
fit=rpart(z~.,data=df)
zp = predict(fit,newdata=df)
df$zr = df$z - v*zp
ZP = v*zp
list_of_weak_learners = list(fit)

z_valid_p=v*predict(fit,newdata=valid)
z_rmse_valid=sqrt(mean(abs(valid$z-z_valid_p)**2))


#################################
##### Boosting with trees #####
#################################

while (!stop)
{
  # Fit linear spline
  fit = rpart(zr ~ .,data=df)
  # Generate new prediction
  zp=predict(fit,newdata=df)
  # Store fitted model in list
  list_of_weak_learners[[t]] = fit
  # Update residuals
  df$zr=df$zr - v*zp
  # Bind to new data point
  ZP = cbind(ZP,v*zp)
  # Test on Validation Dataset
  z_valid_p=z_valid_p+v*predict(fit,newdata=valid)
  z_rmse_valid_new=sqrt(mean(abs(valid$z-z_valid_p)**2))
  change=abs(z_rmse_valid_new-z_rmse_valid)
  z_rmse_valid=z_rmse_valid_new
  print(change)
  if (change<0.005)
  {
    stop=TRUE
  }
  else{
    t=t+1
  }
}


##############################################
##### Getting predictions for each boost #####
##############################################
for (i in 1:t){
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
final_learner = plot_wl %>% filter(learner == (t-1))

# Plot progression of learner
ggplot() + 
  # Visualizing all learners
  geom_line(aes(x = x, y = value, group = learner, color =learner),
            data = plot_wl,alpha=0.5) +
  # Final learner
  geom_line(aes(x = x, y = value, group = learner, color =learner),
            data = final_learner,alpha=0.5,color = 'firebrick1',size = 2)  +
  geom_point(aes(x = x, y= z),data = df)+ # true values
  ggtitle('Plot progression of learner with original learning paremater v=0.05')+
  theme_minimal()


for (i in 1:t){
  weak_learner_i = list_of_weak_learners[[i]]
  
  if (i==1){pred = v*predict(weak_learner_i,test)}
  else{pred =pred + v*predict(weak_learner_i,test)}
  
  if(i==t){
    test = test %>% bind_cols(zp=pred)
  }
}
z_rmse_test=sqrt(mean(abs(test$z-test$zp)**2))

z_rmse_test










