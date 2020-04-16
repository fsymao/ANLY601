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

# Define a named list of parameter values
gs <- list(minsplit = c(2, 5, 10), cp=c(0.005,0.01,0.15),
           maxdepth = c(2, 3, 5)) %>% cross_df() # Convert to data frame grid

model_fit<-function(...)
{
# Setting up parameters
v=.05
stop=FALSE
t=2
# Fit round 1
fit=rpart(y~x,data=df,control = rpart.control(...))
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
  fit = rpart(yr ~ x,data=df,control = rpart.control(...))
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
return(y_rmse_valid)
}
fit = pmap(gs, model_fit)
gs <- gs %>% mutate(fit)
gs[which.min(gs$fit),]











