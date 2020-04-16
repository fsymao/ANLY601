

## PART 1 

## QUESTION 1
df = read.csv('kernel_regression_1.csv')
set.seed(111)
idx<- sample(seq_len(1001), size =20)
x_obv = df$x[-idx]
f=df$y[-idx]
x_prime=df$x

K = function(x,x_prime,theta){
  d = sapply(x,FUN=function(x_in)(x_in-x_prime)^2)
  return(t(exp(-1/(2*theta)*d)))
}
mu=0
mu_star=0
theta=0.001

K_f = K(x_obv,x_obv,theta)
for (i in 1:dim(K_f)[1]){
  K_f[i,i]=K_f[i,i]+0.01
}
K_star = K(x_obv,x_prime,theta)
K_starstar = K(x_prime,x_prime,theta )
mu_star = mu_star + t(K_star)%*%solve(K_f)%*%(f-mu)
Sigma_star = K_starstar - t(K_star)%*%t(solve(K_f))%*%K_star


### QUESTION 2 

model_fit<-function(theta)
{
  K_f = K(x_obv,x_obv,theta)
  for (i in 1:dim(K_f)[1]){
    K_f[i,i]=K_f[i,i]+0.01
  }
  K_star = K(x_obv,x_prime,theta)
  K_starstar = K(x_prime,x_prime,theta )
  mu_star = mu_star + t(K_star)%*%solve(K_f)%*%(f-mu)
  Sigma_star = K_starstar - t(K_star)%*%t(solve(K_f))%*%K_star
  Sigma_star_test = Sigma_star[idx,idx]
  logLL = -log(det(Sigma_star_test)) - t(df$y[idx]-mu_star[idx])%*%solve(Sigma_star_test)%*%(df$y[idx]-mu_star[idx])
  cat('theta=',theta,'Negative Log Likelihood is',-logLL,'\n')}

for (theta in c(0.001,0.01,0.05,0.1,0.125)){
  model_fit(theta)
}


## Part 3
# Plotting values

plot_gp = tibble(x = x_prime, 
                 y = mu_star %>% as.vector(),
                 sd_prime = sqrt(diag(Sigma_star)))

ggplot(aes(x = x, y = y), data = plot_gp) + 
  geom_line()+ 
  geom_ribbon(aes(ymin = y-sd_prime,ymax = y+sd_prime),color='red')+
  geom_point(aes(x =x , y= y), data = tibble(x = x_obv, y = f), alpha=0.5,
             color = 'blue') +
  xlim(c(-5,5))+ylim(c(-2,2))+coord_fixed(ratio = 1) +ylab('f(x)')





## QUESTION 1
df = read.csv('kernel_regression_2.csv')
set.seed(111)
idx<- sample(seq_len(1001), size =20)
x_obv = df[-idx,1:2]
f=df$z[-idx]
x_prime=df[,1:2]

K = function(x,x_prime,theta){
  d = sapply(x,FUN=function(x_in)(x_in-x_prime)^2)
  return(t(exp(-1/(2*theta)*d)))
}
mu=0
mu_star=0
theta=0.001

K_f = K(x_obv,x_obv,theta)
for (i in 1:dim(K_f)[1]){
  K_f[i,i]=K_f[i,i]+0.01
}
K_star = K(x_obv,x_prime,theta)
K_starstar = K(x_prime,x_prime,theta )
mu_star = mu_star + t(K_star)%*%solve(K_f)%*%(f-mu)
Sigma_star = K_starstar - t(K_star)%*%t(solve(K_f))%*%K_star









