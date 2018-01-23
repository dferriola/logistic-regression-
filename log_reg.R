library(ggplot2)
library(aod)
##create a dataset containing the information
patient=c(1:23)
age=c(25,26,28,30,31,32,34,35,36,37,39,40,50,51,52,53,54,55,56,57,58,59,60)
heart_disease=c(0,0,1,0,0,0,1,0,1,0,0,1,1,1,1,0,1,1,0,1,1,1,1)
#create a dataframe
df=data.frame(patient,age,heart_disease)
#attach it to make it easier
attach(df)
#look at the dataframe
str(df)
summary(df)
#create a jitter plot
data_space=ggplot(data=df, aes (x=age, y=heart_disease))+
  geom_jitter(width = 0, height=0.05, alpha=0.5)
data_space
#we will want to use glm
glm(heart_disease-age, data=df, family = binomial)
#visualizing with linear regression to show how it is not useful
data_space+
 geom_smooth(method = 'lm', se = 0, color='orange')

#visualizing logistic regression
data_space+
 geom_smooth(method = 'glm', se = 0, color='steelblue',
             method.args=list(family='binomial'))

#now with both
data_space+
 geom_smooth(method = 'lm', se = 0, color='orange')+
 geom_smooth(method = 'glm', se = 0, color='steelblue',
             method.args=list(family='binomial'))

#probability scale plot
ggplot(df, aes(x=age, y=heart_disease)) +
 geom_point()+geom_line()
scale_y_continuous("Probability of Heart Disease", limits = c(0,1))

sapply(df, sd)

xtabs(~heart_disease + age, data = df)

heartlogit <- glm(heart_disease ~ age, data = df, family = "binomial")

summary(heartlogit)

confint(heartlogit)

confint.default(heartlogit)

#odds ratio
exp(coef(heartlogit))

# odds ratios and 95% CI
exp(cbind(OR = coef(heartlogit), confint(heartlogit)))

with(heartlogit, null.deviance - deviance)
