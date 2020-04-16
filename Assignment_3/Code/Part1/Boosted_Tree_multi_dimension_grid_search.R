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

# Define a named list of parameter values
gs <- list(minsplit = c(2, 5, 10), cp=c(0.005,0.01,0.15),
           maxdepth = c(2, 3, 5)) %>% cross_df() # Convert to data frame grid

model_fit<-function(...){
# Setting up parameters
v=.05
stop=FALSE
t=2
# Fit round 1
fit=rpart(z~.,data=df)
zp = predict(fit,newdata=df,control = rpart.control(...))
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
  fit = rpart(zr ~ .,data=df,control = rpart.control(...))
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
return (z_rmse_valid)
}

fit = pmap(gs, model_fit)
gs <- gs %>% mutate(fit)
gs[which.min(gs$fit),]

  








