#Install pakages
install.packages("numbers", repos="http://cran.r-project.org")
install.packages("onehot", repos="http://cran.r-project.org")
install.packages("neuralnet", repos="http://cran.r-project.org")
install.packages("nnet", repos="http://cran.r-project.org")


options(max.print = .Machine$integer.max)
#Create (training) data frame with 2^16 rows and 514 columns(numbers, mu, lagmu from -2^8 to 2^8)
library(numbers)
numbers<- 2^16:(2^17-1)
mu<- sapply(numbers, moebius)
dat<- data.frame(numbers=numbers, mu=as.factor(mu))
#dat (65536 obs) contains numbers and corresponding mu values(factor/categorical).

#Create test data by using 20-80 spliting
test_numbers<- (2^17): (2^17+16384-1)
test_mu<- sapply(test_numbers, moebius)
test_dat<- data.frame(numbers=test_numbers, mu=as.factor(test_mu))
#test_dat (16384 obs) contains 2 variables

#Preprocess: encoding one-hot vector for mu values

library(onehot)
encoder<-onehot(dat)
x<- predict(encoder, dat)
colnames(x)<- NULL
train_dat<- as.data.frame(x)
#training data with one-hot transfered
names(train_dat)[1]<- "numbers"

encoder_test<- onehot(test_dat)
y<- predict(encoder_test, test_dat)
colnames(y)<- NULL
test_dat<- as.data.frame(y)
#test data with one-hot transfered
names(test_dat)[1]<- "numbers"


#Adding one column of single (maximum) prime factor to training and testing data
p_list<- lapply(train_dat$numbers, primeFactors)
s<- lapply(p_list, function(x) max(x))
s1<- unlist(s)
train_dat_p<- data.frame(train_dat, s1)
#train_dat_p is the ready-to-go training data

p_list_test<- lapply(test_dat$numbers, primeFactors)
s_t<- lapply(p_list_test, function(x) max(x))
s1_t<- unlist(s_t)
test_dat_p<- data.frame(test_dat, s1_t)
#test_dat_p is the ready-to-go testing data 


#Neural Network by neuralnet package
library(neuralnet)
library(nnet)

sink('nn_results.txt')
set.seed(10)
n<- names(train_dat_p)
f<- as.formula(paste("V2+V3+V4~", paste(n[!n %in% c("V2", "V3", "V4")], collapse = "+")))
nn<- neuralnet(f, data=train_dat_p, hidden=c(3), act.fct = "logistic", linear.output = FALSE, lifesign = "minimal")
plot(nn)


#Check the accuracy on the training set
pr.nn<- compute(nn, train_dat_p[, -c(2:4)])
pr.nn_<- pr.nn$net.result
head(pr.nn_)
original_values<- max.col(train_dat[, 2:4])
pr.nn_2<- max.col(pr.nn_)
mean(pr.nn_2 == original_values)

#Check the accuracy on the test set
pr.nn_test <- compute(nn, test_dat_p[, -c(2:4)])
# Extract results 
pr.nn_test_ <- pr.nn_test$net.result
# Accuracy (test set)
head(pr.nn_test_)
original_values_test <- max.col(test_dat_p[, 2:4])
pr.nn_2_test <- max.col(pr.nn_test_)
mean(pr.nn_2_test == original_values_test)

sink()
