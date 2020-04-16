####################################################
# Class: Anly-601
# Script: Boosted Tree
# Author: Shaoyu Feng
# Version: 0.1
# Last updated: 03/19/20
####################################################
require(rpart)
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
stop=FALSE
t=2

# Fit round 1
fit=rpart(y~x,data=df)
yp = predict(fit,newdata=df)
df$yr = df$y - v*yp
YP = v*yp
list_of_weak_learners = list(fit)
y_valid_p=v*predict(fit,newdata=valid)
y_rmse_valid=sqrt(mean(abs(valid$y-y_valid_p)**2))


#################################
##### Boosting with trees #####
#################################
while (!stop)
{
  # Fit linear spline
  fit = rpart(yr ~ x,data=df)
  # Generate new prediction
  yp=predict(fit,newdata=df)
  # Store fitted model in list
  list_of_weak_learners[[t]] = fit
  # Update residuals
  df$yr=df$yr - v*yp
  # Bind to new data point
  YP = cbind(YP,v*yp)
  # Test on Validation Dataset
  y_valid_p=y_valid_p+v*predict(fit,newdata=valid)
  y_rmse_valid_new=sqrt(mean(abs(valid$y-y_valid_p)**2))
  change=abs(y_rmse_valid_new-y_rmse_valid)
  y_rmse_valid=y_rmse_valid_new
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
final_learner = plot_wl %>% filter(learner == (t-1))

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
##### Predicting on test data #####
##################################
for (i in 1:t){
  weak_learner_i = list_of_weak_learners[[i]]
  
  if (i==1){pred = v*predict(weak_learner_i,test)}
  else{pred =pred + v*predict(weak_learner_i,test)}
  
  if(i==t){
    test = test %>% bind_cols(yp=pred)
  }
}
y_rmse_test=sqrt(mean(abs(test$y-test$yp)**2))


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
  ggtitle('boosted vs predicted models Original learning parameter v=0.125')+
  theme_minimal()











