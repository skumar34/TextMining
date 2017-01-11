# Mixture Models : These are the probabilistically-ground way of doing soft clustering.
# In this model each cluster corresponds to a generative model(Probability distribution)
# These probability distriution is typically a gaussian(for real valued observations) / multinomial models(if data is
# discrete like text data - pLSI is quite popular for modelling text).
# What we want to discover using EM algo are the parameters of the distributions i.e mean and variance 
# for each gaussian or probabilities for each group if it is multinomial models.
# Generate the dataset in 1-dimension
set.seed(2)
x1 = c(rnorm(30, mean=1,sd = 3))
x2 = c(rnorm(60,mean=9,sd=2))

data = sort(c(x1,x2))

data_df = data.frame(data,y=rep(0,length(data)),Distribution=
                                          as.factor(c(rep(1,length(x1)),rep(0,length(x2)))))

library(ggplot2)
ggplot(data_df,aes(x=data,y=y ))+#, color = Distribution))+
  geom_point(shape=1)

plot(data,y=c(rep(0,length(data))))

# Pick the number of clusters / topics wanted from the dataset
k = 2
# Randonmly choose two gaussian distributions with mean and variance
mu1 = -4
mu2 = -5
sig1_sqr = 1
sig2_sqr = 1
# Pick the Priors
P_dist1 = 0.5
P_dist2 = 0.5

for (i in 1:50){
# Now generate the P(xi | dist1 ) = (1/sqrt(2*pi*sig1_sqr)) * exp(-(xi-mu1)2/2*sig1_sqr)
# Use dnorm : It is gaussian probability density function
P_data_dist1 = dnorm(data, mean = mu1, sd = sqrt(sig1_sqr))
P_data_dist2 = dnorm(data, mean = mu2, sd = sqrt(sig2_sqr))

par(mfrow = c(3,1))
plot(data,y=c(rep(0,length(data))))
plot(data,y=pnorm(data, mean = mu1, sd = sqrt(sig1_sqr)), type = 'l')
plot(data,y=pnorm(data, mean = mu2, sd = sqrt(sig2_sqr)), type = 'l')

par(mfrow = c(3,1))
plot(data,y=c(rep(0,length(data))),ylab="",xlab="Observed Data in 1D")
plot(data,y=P_data_dist1, type = 'l',ylab='Probability',xlab='Observed Data',main='PDF using re-estimated parameters for dist1')
plot(data,y=P_data_dist2, type = 'l',ylab='Probability',xlab='Observed Data',main='PDF using re-estimated parameters for dist2')

# Then compute that the probability that a particular point belongs to a distribution i.e P(dist1 | xi)
# P(dist1 | xi) = P(xi | dist1) * P(dist1)/(P(xi | dist1) * P(dist1) + P(xi | dist2)* P(dist2))
# where P(dist1) and P(dist2) are the priors probability of observation of the data points from these two
# distributions.
# Similarly P(dist2 | xi) = 1 - P(dist1 | x1)
P_dist1_data = (P_data_dist1 * P_dist1)/(P_data_dist1*P_dist1 + P_data_dist2*P_dist2)
P_dist2_data = 1 - P_dist1_data

# Now Re-estimate the parameters of the distribution i.e mu1 and sig1_sqr and mu2 and sig2_sqr
mu1 = sum(P_dist1_data*data)/sum(P_dist1_data)
sig1_sqr = sum(P_dist1_data * ((data - mu1)^2))/sum(P_dist1_data)
mu2 = sum(P_dist2_data*data)/sum(P_dist2_data)
sig2_sqr = sum(P_dist2_data * ((data - mu2)^2))/sum(P_dist2_data)

# Also re-estimate the prior's
# P(dist1) = sum of probability of data points belongs to dist1 / Total no of data points
P_dist1 = sum(P_dist1_data)/length(data)
P_dist2 = 1 - P_dist1
}

plot(data,y=c(rep(0,length(data))))
plot(data,y=P_data_dist1, type = 'l')
plot(data,y=P_data_dist2, type = 'l')


#################################################################################
# Generate the 2D Gaussian distribution
# One Distribution
# Remove all previous variables before preecding this
mu_vec_1 = c(1,1)
cov_matrix_1 = as.matrix(diag(c(9,9)))

# Another Distribution
mu_vec_2 = c(6,6)
cov_matrix_2 = as.matrix(diag(c(4,4)))

# Generate the Multivariate(2D) Normal Distribution
set.seed(3)
library(mvtnorm)
dist_1 = rmvnorm(30,mean = mu_vec_1, sigma = cov_matrix_1)
dist_2 = rmvnorm(60,mean = mu_vec_2, sigma = cov_matrix_2)

labels = as.factor(c(rep(0,nrow(dist_1)),rep(1,nrow(dist_2))))

data_df = data.frame(x=c(dist_1[,1],dist_2[,1]),y=c(dist_1[,2],dist_2[,2]),"Distribution"=labels)

obs_mat = matrix(c(data_df$x,data_df$y),nrow  = nrow(data_df), ncol=2)

library(ggplot2)
ggplot(data_df,aes(x=x,y=y,color=Distribution))+
  geom_point()
plot_ly(data=data_df, x=~x, y=~y, color = ~Distribution)

# Now in real word we don't know the labels
ggplot(data_df,aes(x=x,y=y))+
  geom_point(shape=1)

# Plot Contour
library(plotly)
plot_ly(z = ~volcano, type="contour")

# Assume there are two multivariate random variables and assume the probability of generating each dataset from one 
# of the distribution is equal
P_dist1 = 0.5
P_dist2 = 0.5
k = 2
n = nrow(obs_mat)
rand_mu_vec_1 = c(0,0)
rand_mu_vec_2 = c(1,1)

rand_cov_matrix_1 = as.matrix(diag(c(1,1)))
rand_cov_matrix_2 = as.matrix(diag(c(1,1)))

for (i in 1:50){
# Now Compute the Probability fo observation of each data point given it belongs to randon dist1 or random dist2
# Multivariate Gaussian Density funtion f(x; mu_vec, cov_matrix)
P_data_dist1 = (1/sqrt(2*pi*det(rand_cov_matrix_1)))*exp(-(1/2)*diag(t(t(obs_mat) - rand_mu_vec_1)  %*% 
                                                                   solve(rand_cov_matrix_1) %*% (t(obs_mat)-rand_mu_vec_1)))

P_data_dist2 = (1/sqrt(2*pi*det(rand_cov_matrix_2)))*exp(-(1/2)*diag(t(t(obs_mat) - rand_mu_vec_2)  %*% 
                                                                       solve(rand_cov_matrix_2) %*% (t(obs_mat)-rand_mu_vec_2)))

# Now apply baye's rule and convert those probabilities to posterior
P_dist1_data = P_data_dist1 * P_dist1 / (P_data_dist1 * P_dist1 + P_data_dist2 * P_dist2)
P_dist2_data = P_data_dist2 * P_dist2 / (P_data_dist1 * P_dist1 + P_data_dist2 * P_dist2)

# Compute the weights : It reflects how important a particular point is for a particular gaussian. It is the weight
# that associates an instance i with Gaussian number c.
Weight_data_dist1 = P_dist1_data/sum(P_dist1_data)
Weight_data_dist2 = P_dist2_data/sum(P_dist2_data)

# Now recompute the parameters that is mean and covariance matrix
rand_mu_vec_1 = c(t(Weight_data_dist1 %*% obs_mat))
rand_mu_vec_2 = c(t(Weight_data_dist2 %*% obs_mat))

rand_cov_matrix_1 = (t(obs_mat) - c(rand_mu_vec_1)) %*% diag(Weight_data_dist1) %*%  t(t(obs_mat) - c(rand_mu_vec_1))
rand_cov_matrix_2 = (t(obs_mat) - c(rand_mu_vec_2)) %*% diag(Weight_data_dist2) %*%  t(t(obs_mat) - c(rand_mu_vec_2))

# Now recompute the Prior's
P_dist1 = sum(P_dist1_data)/n
P_dist2 = sum(P_dist2_data)/n
}
par(mfrow = c(2,1))
plot_ly(x=data_df$x, y=data_df$y, color = P_dist1_data)
plot_ly(data=data_df, x=~x, y=~y, color = ~Distribution)
