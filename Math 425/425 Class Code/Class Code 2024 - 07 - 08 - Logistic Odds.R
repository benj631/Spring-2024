library(tidyverse)
library(ResourceSelection)

m425 <- read.csv("C:/Users/ghsaund/Downloads/Math425PastGrades.csv", stringsAsFactors=TRUE)
View(m425)


# Suppose I got a 70% on the Math 425 Midterm
# I only want to keep my midterm score, if my final exam is greater than 70%.
m425 <- m425 %>%
  mutate(Finalo70 = ifelse(FinalExam > 70, 1, 0))

pairs(m425, panel=panel.smooth, pch=16, col=rgb(.1,.1,.1,.1))


glm1 <- glm(Finalo70 ~ Midterm + AssessmentQuizzes + MagicTwoGroups + Theory_SamplingDists, 
            data=m425, family=binomial)
summary(glm1)


glm2<- glm(Finalo70 ~ Midterm + AssessmentQuizzes + Theory_SamplingDists, 
            data=m425, family=binomial)
summary(glm2)

glm3 <- glm(Finalo70 ~ Midterm + AssessmentQuizzes + MagicTwoGroups + Theory_SamplingDists + Midterm:AssessmentQuizzes, 
            data=m425, family=binomial)
summary(glm3)


glm3 <- glm(Finalo70 ~ ., 
            data=m425[,-1], family=binomial)
summary(glm3)

glm4 <- glm(Finalo70 ~ Midterm + AssessmentQuizzes + Theory_SamplingDists + AttendedAlmostAlways + MagicTwoGroups + Midterm:MagicTwoGroups + AttendedAlmostAlways:MagicTwoGroups, 
            data=m425, family=binomial)
summary(glm4)





m425 %>%
  select(Finalo70, Midterm, AssessmentQuizzes, Theory_SamplingDists, AttendedAlmostAlways, MagicTwoGroups) %>%
  na.omit() %>%
ggplot(aes(x=Midterm, y=Finalo70, color=interaction(AttendedAlmostAlways, MagicTwoGroups))) + 
  geom_jitter(width=0, height=0.05) + 
  geom_point(aes(y=glm4$fitted.values), cex=0.5) + 
  facet_wrap(~interaction(AttendedAlmostAlways, MagicTwoGroups)) + 
  geom_smooth(method="glm", formula=y~x, se=T, method.args=list(family="binomial"))


hoslem.test(glm4$y, glm4$fit, g=10)

predict(glm4, data.frame(Midterm=70, AttendedAlmostAlways="Y", MagicTwoGroups=2, AssessmentQuizzes=52, Theory_SamplingDists=15), type="response")

glm.gg <- glm(Finalo70 ~ Midterm + 
                AttendedAlmostAlways + Midterm:AttendedAlmostAlways + 
                MagicTwoGroups + Midterm:MagicTwoGroups + 
                AttendedAlmostAlways:MagicTwoGroups + AttendedAlmostAlways:MagicTwoGroups:Midterm, 
              data=m425, family=binomial)
summary(glm.gg)




set.seed(151)
keep <- sample(1:nrow(m425), 100)

mytrain <- m425[keep, ]
mytest <- m425[-keep, ]

glm.train <- glm(Finalo70 ~ Midterm + AssessmentQuizzes + Theory_SamplingDists + AttendedAlmostAlways + MagicTwoGroups + Midterm:MagicTwoGroups + AttendedAlmostAlways:MagicTwoGroups, 
            data=mytrain, family=binomial)

predictions <- predict(glm.train, mytest, type="response")

callit <- ifelse(predictions > 0.6, 1, 0)

table(callit, mytest$Finalo70) #confusion matrix

23/26 #percent correctly classified
