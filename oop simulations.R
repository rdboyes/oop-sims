#Simulation: Effect on mean for addition of 5 extra people in uncertain dataset

#rethinking library contains the HPDI function - used for CI estimation from vector of estimates

library(rethinking)

#define a simulated effect function that incorporates error using calls to rnorm (assumes normally distributed data)

sim_effect <- function(meaneffect,sdev,infl=1.0315){
  average <- (40*3254+45*4674+39*4012)/124
  return(infl*((rnorm(n=100000,mean=meaneffect,sd=sdev)) - average))
}

#Calculate univariate expected difference from mean of each possible characteristic

west <- sim_effect(3254.35,2536.86)
mid <- sim_effect(4674.52,5691.97)
on <- sim_effect(4011.89,3922.2)
age_50 <- sim_effect(3616.51,4731.93)
age_60 <- sim_effect(3440.39,3219.22)
male <- sim_effect(4397.12,4704.29)
female <- sim_effect(3018.5,3012.73)
emp <- sim_effect(4598.75,5181.47)
retired <- sim_effect(4010.39,3489.65)
other <- sim_effect(2245.87,2788.41)
spouse	<- sim_effect(4413.61,3779.98)
child <- sim_effect(4315.58,4797.85)
college <- sim_effect(4163.11,4664.97)

#create 124 people that match the known sample

sim_results <- list()
for(i in 1:126){
  sim_results[[i]] <- rnorm(n=100000,mean=4133.96,sd=4511.71)
}

#Modify the last 5 people based on the characteristics of the missing data

sim_results[[122]] <- sim_results[[125]] + male + age_60 + on + retired + spouse + college
sim_results[[123]] <- sim_results[[126]] + male + college + age_60 + on + emp + spouse
sim_results[[124]] <- sim_results[[127]] + male + college + age_50 + on + emp + child
sim_results[[125]] <- sim_results[[128]] + male + college + age_50 + west + emp + child
sim_results[[126]] <- sim_results[[129]] + male + college + age_50 + mid + emp + child

final_mean1 <- 0
final_mean2 <- 0

#for each set of draws (n = 100,000), add the costs of:
#1. the first 124 people
#2. all people (first 124 + additional modified persons)

for (i in 1:100000){
  temp_mean <- 0
  for (j in 1:121){
    temp_mean <- temp_mean + sim_results[[j]][i]
  }
  final_mean1[i] <- temp_mean/121
  for(j in 122:126){
    temp_mean <- temp_mean + sim_results[[j]][i]
  }
  final_mean2[i] <- temp_mean/126
}

#vector subtraction of all calculated pairs of means

difference <- final_mean2 - final_mean1

#compute mean of difference

mean(difference)

#high posterior density interval calculation (Bayesian CI)

HPDI(difference, prob = 0.95)

#calculate percentage change

mean(difference)/mean(final_mean1)

