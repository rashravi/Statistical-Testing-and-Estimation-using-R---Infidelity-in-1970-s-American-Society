# ----------------------------------------------
# Hypothesis: >= 30% of Americans having affairs
# ----------------------------------------------

tstat = nrow(affairs[affairs$nbaffairs>0,])/nrow(affairs)*100
tstat

v = c(1,0)
p = c(0.3,0.7)
n = nrow(affairs)
f1 = function()
{
  s = sample(v,replace = T,prob = p,size = n)
  return(sum(s)/n*100)
}

f1()

# replicate the function to obtain distribution
dist = replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col='pink')
abline(v=tstat,lwd = 2,col='blue')

# Compute pvalue (one-tailed test)
lside = dist[dist<=tstat]
pvalue = length(lside)/length(dist)
pvalue
# p-value = 0.00359

# -------------------------------------------------------------------------
# Hypothesis: Male and female Americans are equally likely to have affairs.
# -------------------------------------------------------------------------
malerows = affairs[affairs$sex == "male",]
nrow(malerows) # number of male: 286
maleyes = malerows[malerows$nbaffairs>0,]
nrow(maleyes) # number of male having affairs: 78
femalerows = affairs[affairs$sex == "female",]
nrow(femalerows) # number of female: 315
femaleyes = femalerows[femalerows$nbaffairs>0,]
nrow(femaleyes) # number of male having affairs: 72

# percentage difference between men with affair(s) and women with affair(s)
tstat = (nrow(maleyes)/nrow(malerows) - nrow(femaleyes)/nrow(femalerows))*100
tstat

# create sampling function with bootstrapping
f1 = function()
{
  s = sample(affairs$nbaffairs,replace = T)
  s1 = s[1:286]
  s2 = s[286:601]
  s1yes = s1[s1>0]
  s2yes = s2[s2>0]
  diff = (length(s1yes)/length(s1) - length(s2yes)/length(s2))*100
  return(diff)
}

f1()

# replicate the function to obtain distribution
dist = replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col='yellow')
abline(v=tstat,lwd = 2,col='red')

# computing the p-value
gap = abs(mean(dist)-tstat)
lside = dist[dist<mean(dist)-gap]
rside = dist[dist>mean(dist)+gap]
pvalue = (length(lside)+length(rside))/length(dist)
pvalue
# pvalue = 0.21, not reject the null hypothesis


# -----------------------------------------------------------------------------
# Hypothesis: Americans having child or not are equally likely to have affairs.
# -----------------------------------------------------------------------------

childrows = affairs[affairs$child == "yes",]
nrow(childrows) # number of Americans having child = 430
yescases1 = childrows[childrows$nbaffairs>0,]
nrow(yescases1) # Having child and affairs = 123
nochildrows = affairs[affairs$child == "no",]
nrow(nochildrows) # number of Americans not having child = 171
yescases2 = nochildrows[nochildrows$nbaffairs>0,]
nrow(yescases2) # not having child and haing affairs = 27

# percentage difference between people with affair(s) with or without child
tstat = (nrow(yescases1)/nrow(childrows) - nrow(yescases2)/nrow(nochildrows))*100
tstat

# create function with bootstrapping
f1 = function()
{
  s = sample(affairs$nbaffairs,replace = T)
  s1 = s[1:430]
  s2 = s[431:601]
  s1yes = s1[s1 > 0]
  s2yes = s2[s2 > 0]
  diff = (length(s1yes)/length(s1) - length(s2yes)/length(s2))*100
  return(diff)
}

# replicate the function to obtain distribution
dist = replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col='blue')
abline(v=tstat,lwd = 2,col='red')

# computing the p-value
gap = abs(mean(dist)-tstat)
lside = dist[dist<mean(dist)-gap]
rside = dist[dist>mean(dist)+gap]
pvalue = (length(lside)+length(rside))/length(dist)
pvalue
# pvalue = 0.00056, reject null hypothesis

# ------------------------------------------------------------------------------------------
# Hypothesis: Americans with or without higher education are equally likely to have affairs.
# ------------------------------------------------------------------------------------------

higheredu = affairs[affairs$education > 12,]
nrow(higheredu)# with higher education: 550 rows
yescases1 = higheredu[higheredu$nbaffairs>0,]
nrow(yescases1) # Higher education and having affairs: 135 rows
nohigheredu = affairs[affairs$education <= 12,]
nrow(nohigheredu) # without higher education: 51 rows
yescases2 = nohigheredu[nohigheredu$nbaffairs>0,]
nrow(yescases2) # without higher education and haing affairs = 15

# percentage difference between people with affair(s) with or without child
tstat = (nrow(yescases2)/nrow(nohigheredu) - nrow(yescases1)/nrow(higheredu))*100
tstat

# create function with bootstrapping
f1 = function()
{
  f = sample(affairs$nbaffairs,replace = T)
  f1 = f[1:51]
  f2 = f[52:601]
  f1yes = f1[f1 > 0]
  f2yes = f2[f2 > 0]
  diff = (length(f1yes)/length(f1) - length(f2yes)/length(f2))*100
  return(diff)
}

# replicate the function to obtain distribution
dist = replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col='plum')
abline(v=tstat,lwd = 2,col='seagreen')

# computing the p-value
gap = abs(mean(dist)-tstat)
lside = dist[dist<mean(dist)-gap]
rside = dist[dist>mean(dist)+gap]
pvalue = (length(lside)+length(rside))/length(dist)
pvalue
# pvalue = 0.44,not reject null hypothesis
#--------------------------------------------------
#Hypothesis: If the happiness rate is 5, 5% tend to have affairs

p = c(0.05,0.95)
v = c("affairs","nonaffairs")
h5 = affairsneww[affairsneww$rate=='5',]
n=nrow(h5)
f1=function()
{
  s=sample(x=v, prob = p, replace = T, size = n)
  return(prop.table(table(s))[1])
} 
dist=replicate(10000,f1())
plot(density(dist))
polygon(density(dist),col = "red")

tstat=length(h5$nbaffairs[h5$nbaffairs!='0'])/length(h5$nbaffairs)
abline(v=tstat)
gap = abs(mean(dist) - tstat)
gap
lside = dist[dist<mean(dist)-gap]

rside = dist[dist>mean(dist)+gap] 
pvalue =  (length(lside)+length(rside)) / length(dist)
pvalue 

# try to compute pvalue (one-tailed test)
lside = dist[dist<=tstat]
pvalue = length(lside)/length(dist)
pvalue


#---------if the rate is 1(lowest, number), we guess that more than 30% will have affair-----------#
p = c(0.3,0.7)
v = c("affairs","nonaffairs")
h1 = affairsneww[affairsneww$rate=='1',]
n=nrow(h1)
f1=function()
{
  s=sample(x=v, prob = p, replace = T, size = n)
  return(prop.table(table(s))[1])
} 
dist=replicate(10000,f1())
plot(density(dist))
polygon(density(dist),col = "pink")

tstat=length(h1$nbaffairs[h1$nbaffairs!='0'])/length(h1$nbaffairs)
abline(v=tstat)

# Compute pvalue (one-tailed test)
lside = dist[dist<=tstat]
pvalue = length(lside)/length(dist)
pvalue

#-------------------
install.packages("corrgram")
library(corrgram)
corrgram(affairsneww,order = T)

# visualization
library(ggplot2)
is.numeric(affairsneww$ym)
ggplot(affairsneww, aes(x=affairsneww$nbaffairs,fill = as.factor((affairsneww$sex))))+
  geom_bar(position = 'dodge')



install.packages("car")
library(car)
install.packages("MASS")
library(MASS)
install.packages("bbmle")
library(bbmle)
library(ggplot2)
affairsneww$nbaffairs=recode(affairsneww$nbaffairs,"0=0;c('1','2','3','7','12')=1")
View(affairsneww)

# Function for mode
getmode = function(v) 
{
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#95% confidence interval for most common religious level among those who did not have affairs
t=affairsneww[affairsneww$nbaffairs==0,]
f1=function()
{
  
  s=sample(t$religious,replace=T)
  k=getmode(s)
  k
  return(k)
}
f1()
sdist=replicate(10000,f1())
plot(density(sdist))
polygon(density(sdist),col="yellow")
q=quantile(sdist,probs = c(0.025,1-0.025))
q
#95% confidence interval for most common religious level among those who had atleast one affair
t=affairsneww[affairsneww$nbaffairs==1,]
f1=function()
{
  
  s=sample(t$religious,replace=T)
  k=getmode(s)
  k
  return(k)
}
f1()
sdist=replicate(10000,f1())
plot(density(sdist))
polygon(density(sdist),col="yellow")
q=quantile(sdist,probs = c(0.025,1-0.025))
q

#Hypothesis:The average religious level for people who had an affair and those who did not is the same
# Non parametric two sample test
g = affairsneww$religious
f1 = function()
{
  x = sample(g)
  z = abs(mean(x[1:150])-mean(x[151:601]))
  return(z)
}
dist = replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col="green")
t1=affairsneww[affairsneww$nbaffairs==1,]
t2=affairsneww[affairsneww$nbaffairs==0,]
tstat = abs(mean(t1$religious) - mean(t2$religious))
tstat
abline(v=tstat)
rside=dist[dist>tstat]
pvalue=length(rside)/length(dist)
pvalue

#--------------------------------------------------------------
# Maximum likelihood estimation/ regression using mle2
f1 = function(a0,a1,a2,a3,a4,a5,a6,a7,a8)
{
  X = a0 + a1*affairsneww$sex+a2*affairsneww$age+a3*affairsneww$ym+a4*affairsneww$child+a5*affairsneww$religious+a6*affairsneww$education+a7*affairsneww$occupation+a8*affairsneww$rate
  p = exp(X)/(1+exp(X))
  LL = sum(log(ifelse(affairsneww$nbaffairs==0,1-p,p)))
  return(-1*LL)
}
res = mle2(minuslogl = f1,start=list(a0=0,a1=0,a2=0,a3=0,a4=0,a5=0,a6=0,a7=0,a8=0))
summary(res)

#logistic regression using glm

reg1 = glm(nbaffairs~.,family = "binomial",data=affairsneww) 
summary(reg1) 

# To add interaction terms
res = step(reg1,~.^2) 
res$anova 

reg2 = glm(nbaffairs~.+ym:education+ym:child-occupation+age:religious,family = "binomial",data=affairsneww)
summary(reg2)

# In order to drop variables which are of no significance
stepAIC(reg2, direction="both") 

reg3=  glm(formula = nbaffairs ~ age + ym + child + religious + education + 
             rate + ym:education + ym:child + age:religious, family = "binomial", 
           data = affairsneww)
summary(reg3)

#coefficients of the final model
coef(reg3)
exp(coef(reg3))

# Plotting the most significant variable "rate" with affairs

ggplot(affairsneww, aes(x=rate, y=nbaffairs)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)