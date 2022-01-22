#Gan Qian Chao
#TP055444
#Load library
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(waffle)
#Import CSV (Data import)
student_data <- read.csv(file="D:\\PFDA assignment\\student.csv", skip=1,header= FALSE, sep=";",
                         quote="/")
#assign column name for data
names(student_data) = c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob",
                        "Fjob","reason","guardian","traveltime","studytime","failures","schoolsup",
                        "famsup","paid","activities","nursery","higher","internet","romantic","famrel",
                        "freetime","goout","Dalc","Walc","health","absences","G1","G2","G3")
#Study the data
str(student_data)
View(student_data)
#========================================================================================================
#Data Pre-processing
#find factors in an attribute and change datatype of the attribute (if needed)
#school attribute
factor(student_data$school)
student_data$school=factor(student_data$school)
#sex attribute
factor(student_data$sex)
student_data$sex=factor(student_data$sex)
#address attribute
factor(student_data$address)
student_data$address=factor(student_data$address)
#family size attribute
factor(student_data$famsize)
student_data$famsize=factor(student_data$famsize)
#Parent status attribute
factor(student_data$Pstatus)
student_data$Pstatus=factor(student_data$Pstatus)
#Mother job attribute
factor(student_data$Mjob)
student_data$Mjob=factor(student_data$Mjob,levels=c("at_home","health","other","services","teacher"))
#Father job attribute
factor(student_data$Fjob)
student_data$Fjob=factor(student_data$Fjob,levels=c("at_home","health","other","services","teacher"))
#Reason attribute
factor(student_data$reason)
student_data$reason=factor(student_data$reason,levels=c("course","home","reputation","other"))
#Guardian attribute
factor(student_data$guardian)
student_data$guardian=factor(student_data$guardian,levels=c("father","mother","other"))
#School support attribute
factor(student_data$schoolsup)
student_data$schoolsup=factor(student_data$schoolsup,levels=c("yes","no"))
#Family support attribute
factor(student_data$famsup)
student_data$famsup=factor(student_data$famsup,levels=c("yes","no"))
#Paid education attribute
factor(student_data$paid)
student_data$paid=factor(student_data$paid,levels=c("yes","no"))
#extra-curriculum activities attributes
factor(student_data$activities)
student_data$activities=factor(student_data$activities,levels=c("yes","no"))
#nursery attributes
factor(student_data$nursery)
student_data$nursery=factor(student_data$nursery,levels=c("yes","no"))
#higher education attribute
factor(student_data$higher)
student_data$higher=factor(student_data$higher,levels=c("yes","no"))
#internet attribute
factor(student_data$internet)
student_data$internet=factor(student_data$internet,levels=c("yes","no"))
#romantic attribute
factor(student_data$romantic)
student_data$romantic=factor(student_data$romantic,levels=c("yes","no"))
#G1,G2,G3
factor(student_data$G1)
factor(student_data$G2)
factor(student_data$G3)
#Replace value of attributes (school)
student_data["school"][student_data["school"]=="\"GP"]<-"GP"
student_data["school"][student_data["school"]=="\"MS"]<-"MS"
#Replace value of attributes (sex)
student_data["sex"][student_data["sex"]=="\"\"F\"\""]<-"F"
student_data["sex"][student_data["sex"]=="\"\"M\"\""]<-"M"
#Replace value of attributes (address)
student_data["address"][student_data["address"]=="\"\"R\"\""]<-"R"
student_data["address"][student_data["address"]=="\"\"U\"\""]<-"U"
#Replace value of attributes (famsize)
student_data["famsize"][student_data["famsize"]=="\"\"GT3\"\""]<-"GT3"
student_data["famsize"][student_data["famsize"]=="\"\"LE3\"\""]<-"LE3"
#Replace value of attributes (Pstatus)
student_data["Pstatus"][student_data["Pstatus"]=="\"\"A\"\""]<-"A"
student_data["Pstatus"][student_data["Pstatus"]=="\"\"T\"\""]<-"T"
#Replace value of attributes (Mjob)
student_data["Mjob"][student_data["Mjob"]=="\"\"at_home\"\""]<-"at_home"
student_data["Mjob"][student_data["Mjob"]=="\"\"health\"\""]<-"health"
student_data["Mjob"][student_data["Mjob"]=="\"\"other\"\""]<-"other"
student_data["Mjob"][student_data["Mjob"]=="\"\"services\"\""]<-"services"
student_data["Mjob"][student_data["Mjob"]=="\"\"teacher\"\""]<-"teacher"
#Replace value of attributes (Fjob)
student_data["Fjob"][student_data["Fjob"]=="\"\"at_home\"\""]<-"at_home"
student_data["Fjob"][student_data["Fjob"]=="\"\"health\"\""]<-"health"
student_data["Fjob"][student_data["Fjob"]=="\"\"other\"\""]<-"other"
student_data["Fjob"][student_data["Fjob"]=="\"\"services\"\""]<-"services"
student_data["Fjob"][student_data["Fjob"]=="\"\"teacher\"\""]<-"teacher"
#Replace value of attributes (reason)
student_data["reason"][student_data["reason"]=="\"\"course\"\""]<-"course"
student_data["reason"][student_data["reason"]=="\"\"home\"\""]<-"home"
student_data["reason"][student_data["reason"]=="\"\"other\"\""]<-"other"
student_data["reason"][student_data["reason"]=="\"\"reputation\"\""]<-"reputation"
#Replace value of attributes (guardian)
student_data["guardian"][student_data["guardian"]=="\"\"father\"\""]<-"father"
student_data["guardian"][student_data["guardian"]=="\"\"mother\"\""]<-"mother"
student_data["guardian"][student_data["guardian"]=="\"\"other\"\""]<-"other"
#Replace value of attributes (schoolsup)
student_data["schoolsup"][student_data["schoolsup"]=="\"\"yes\"\""]<-"yes"
student_data["schoolsup"][student_data["schoolsup"]=="\"\"no\"\""]<-"no"
#Replace value of attributes (famsup)
student_data["famsup"][student_data["famsup"]=="\"\"yes\"\""]<-"yes"
student_data["famsup"][student_data["famsup"]=="\"\"no\"\""]<-"no"
#Replace value of attributes (paid)
student_data["paid"][student_data["paid"]=="\"\"yes\"\""]<-"yes"
student_data["paid"][student_data["paid"]=="\"\"no\"\""]<-"no"
#Replace value of attributes (activities)
student_data["activities"][student_data["activities"]=="\"\"yes\"\""]<-"yes"
student_data["activities"][student_data["activities"]=="\"\"no\"\""]<-"no"
#Replace value of attributes (nursery)
student_data["nursery"][student_data["nursery"]=="\"\"yes\"\""]<-"yes"
student_data["nursery"][student_data["nursery"]=="\"\"no\"\""]<-"no"
#Replace value of attributes (higher)
student_data["higher"][student_data["higher"]=="\"\"yes\"\""]<-"yes"
student_data["higher"][student_data["higher"]=="\"\"no\"\""]<-"no"
#Replace value of attributes (internet)
student_data["internet"][student_data["internet"]=="\"\"yes\"\""]<-"yes"
student_data["internet"][student_data["internet"]=="\"\"no\"\""]<-"no"
#Replace value of attributes (romantic)
student_data["romantic"][student_data["romantic"]=="\"\"yes\"\""]<-"yes"
student_data["romantic"][student_data["romantic"]=="\"\"no\"\""]<-"no"
#Replace value of attributes (G1)
student_data["G1"][student_data["G1"]=="\"\"3\"\""]<-as.numeric(3)
student_data["G1"][student_data["G1"]=="\"\"4\"\""]<-as.numeric(4)
student_data["G1"][student_data["G1"]=="\"\"5\"\""]<-as.numeric(5)
student_data["G1"][student_data["G1"]=="\"\"6\"\""]<-as.numeric(6)
student_data["G1"][student_data["G1"]=="\"\"7\"\""]<-as.numeric(7)
student_data["G1"][student_data["G1"]=="\"\"8\"\""]<-as.numeric(8)
student_data["G1"][student_data["G1"]=="\"\"9\"\""]<-as.numeric(9)
student_data["G1"][student_data["G1"]=="\"\"10\"\""]<-as.numeric(10)
student_data["G1"][student_data["G1"]=="\"\"11\"\""]<-as.numeric(11)
student_data["G1"][student_data["G1"]=="\"\"12\"\""]<-as.numeric(12)
student_data["G1"][student_data["G1"]=="\"\"13\"\""]<-as.numeric(13)
student_data["G1"][student_data["G1"]=="\"\"14\"\""]<-as.numeric(14)
student_data["G1"][student_data["G1"]=="\"\"15\"\""]<-as.numeric(15)
student_data["G1"][student_data["G1"]=="\"\"16\"\""]<-as.numeric(16)
student_data["G1"][student_data["G1"]=="\"\"17\"\""]<-as.numeric(17)
student_data["G1"][student_data["G1"]=="\"\"18\"\""]<-as.numeric(18)
student_data["G1"][student_data["G1"]=="\"\"19\"\""]<-as.numeric(19)
class(student_data$G1)
student_data$G1=as.integer(student_data$G1)
#Replace value of attributes (G2)
student_data["G2"][student_data["G2"]=="\"\"0\"\""]<-as.numeric(0)
student_data["G2"][student_data["G2"]=="\"\"4\"\""]<-as.numeric(4)
student_data["G2"][student_data["G2"]=="\"\"5\"\""]<-as.numeric(5)
student_data["G2"][student_data["G2"]=="\"\"6\"\""]<-as.numeric(6)
student_data["G2"][student_data["G2"]=="\"\"7\"\""]<-as.numeric(7)
student_data["G2"][student_data["G2"]=="\"\"8\"\""]<-as.numeric(8)
student_data["G2"][student_data["G2"]=="\"\"9\"\""]<-as.numeric(9)
student_data["G2"][student_data["G2"]=="\"\"10\"\""]<-as.numeric(10)
student_data["G2"][student_data["G2"]=="\"\"11\"\""]<-as.numeric(11)
student_data["G2"][student_data["G2"]=="\"\"12\"\""]<-as.numeric(12)
student_data["G2"][student_data["G2"]=="\"\"13\"\""]<-as.numeric(13)
student_data["G2"][student_data["G2"]=="\"\"14\"\""]<-as.numeric(14)
student_data["G2"][student_data["G2"]=="\"\"15\"\""]<-as.numeric(15)
student_data["G2"][student_data["G2"]=="\"\"16\"\""]<-as.numeric(16)
student_data["G2"][student_data["G2"]=="\"\"17\"\""]<-as.numeric(17)
student_data["G2"][student_data["G2"]=="\"\"18\"\""]<-as.numeric(18)
student_data["G2"][student_data["G2"]=="\"\"19\"\""]<-as.numeric(19)
student_data$G2=as.integer(student_data$G2)
class(student_data$G2)
#Replace value of attributes (G3)
student_data["G3"][student_data["G3"]=="0\""]<-as.numeric(0)
student_data["G3"][student_data["G3"]=="4\""]<-as.numeric(4)
student_data["G3"][student_data["G3"]=="5\""]<-as.numeric(5)
student_data["G3"][student_data["G3"]=="6\""]<-as.numeric(6)
student_data["G3"][student_data["G3"]=="7\""]<-as.numeric(7)
student_data["G3"][student_data["G3"]=="8\""]<-as.numeric(8)
student_data["G3"][student_data["G3"]=="9\""]<-as.numeric(9)
student_data["G3"][student_data["G3"]=="10\""]<-as.numeric(10)
student_data["G3"][student_data["G3"]=="11\""]<-as.numeric(11)
student_data["G3"][student_data["G3"]=="12\""]<-as.numeric(12)
student_data["G3"][student_data["G3"]=="13\""]<-as.numeric(13)
student_data["G3"][student_data["G3"]=="14\""]<-as.numeric(14)
student_data["G3"][student_data["G3"]=="15\""]<-as.numeric(15)
student_data["G3"][student_data["G3"]=="16\""]<-as.numeric(16)
student_data["G3"][student_data["G3"]=="17\""]<-as.numeric(17)
student_data["G3"][student_data["G3"]=="18\""]<-as.numeric(18)
student_data["G3"][student_data["G3"]=="19\""]<-as.numeric(19)
student_data["G3"][student_data["G3"]=="20\""]<-as.numeric(20)
student_data$G3=as.integer(student_data$G3)
class(student_data$G3)

#Data cleaning
#Check missing value
summary(student_data)

#No missing value or NAs found
#The data is clean

#Transformation data
#add attribute of average score for G1,G2,G3
library(tidyverse)
studentdata <- student_data %>%
  mutate(average=(G1+G2+G3)/3)
student1<-student_data%>%
  arrange(G1,G2,G3)
student_data$average=as.numeric(average)

#=======================================================================================================
# Question 1: How the school can affect the average score of students
q1<-student_data %>%group_by(school)%>%
  select(school,reason,traveltime,average)
#========================================================================================================

# Analysis 1-1 : What is the relationship between school with the average score?
library(ggplot2)
ggplot(q1, aes(x=average, group=school)) +
  geom_histogram(binwidth=1, colour="black",aes(fill=..count..))+
  scale_fill_gradient("Frequency (Average Score)", low="blue",high="green")+
  facet_wrap(~school)+
  labs(title="Histogram of average score of GP & MS",
       y="Frequency",
       x="Average Score")

#GP shows a higher number of excellent average score (18-20) compare with MS

# Analysis 1-2 : What is the relationship between reason, school and average score?
ggplot(q1, aes(x=reason, y=average)) +
  geom_count(aes(colour=school))+
  facet_wrap(~school)+
  labs(title="Reason VS Average Score according to School",
       x="Reason")

#Both GP and MS's excellent average score student may like to choose the school according to the course and reputation
#of GP. 

# Analysis 1-3 : How the travel time to school related to the reason, school and average score?
ggplot(q1, aes(x=traveltime, y=average)) +
  geom_jitter(aes(colour=traveltime))+
  facet_wrap(~school)

#The students are more likely to have better average score when the travel time from school to home is 
#shorter. This is because the student need more time to do nothing during the journey to school and have
#no chance to revise their homework

#Conclusion: In my opinion, the student in GP has the higher number of excellent average score compare with
#the MS student. This may because the GP school has a high reputation and good coursework introduced to the
#student and attract them to choose GP school instead of other school. Besides that, the students can get a
#higher marks when the school is nearer their home. They may not feel tired when they has less journey time 
#to school. This will accordingly increase their attention during the class. 

#Suggestion: The school should study the coursework that is meaningful and make more activities and promotions
#to increase the reputation of the school so that the students will more relevant to study in the school. Not
#only that, the school can also provide transportation or give extra educational support to the students
#who live far away from school. This will help to increase their performance in school.
#=======================================================================================================
#Question 2: How does the gender and age affect from G1 to G3 score
q2<-student_data %>%
  select(sex,age,G1,G2,G3)
#=======================================================================================================

#Analysis 2-1 : What is the range of G1 to G3 score in term of gender?
G1<-ggplot(q2, aes(y=G1,x=sex,color=sex)) + geom_boxplot()+
ggtitle("Boxplot of sex against G1")+ylab("G1 Score")
G2<-ggplot(q2, aes(y=G2,x=sex,color=sex)) + geom_boxplot()+
ggtitle("Boxplot of sex against G2")+ylab("G2 Score")
G3<-ggplot(q2, aes(y=G3,x=sex,color=sex)) + geom_boxplot()+
ggtitle("Boxplot of sex against G3")+ylab("G3 Score")
library(ggpubr)
ggarrange(G1,G2,G3,ncol=1,nrow=3)

#In the three boxplot, male students are more likely to get higher score in G1,G2 and G3.

#Analysis 2-2 : How the student performed in the course in term of gender
#G1 against G2
library(ggplot2)
q2$col <-"yellow"
q2[!is.na(q2$G1) & q2$G1 < q2$G2, "col"]<-"green"
q2[!is.na(q2$G2) & q2$G2 < q2$G1,"col"]<-"red"
G1vG2<-ggplot(q2,aes(x=G1, y=G2, color=col)) + geom_point()+
  geom_abline(stat = "abline", colour="red",size=1)+
  xlab("G1 score")+ylab("G2 score")+ggtitle("G1 VS G2 in term of gender")+
  facet_wrap(~sex)+
  scale_color_identity()

#G2 against G3 
q2$final <-"yellow"
q2[!is.na(q2$G2) & q2$G2 < q2$G3, "final"]<-"green"
q2[!is.na(q2$G3) & q2$G3 < q2$G2, "final"]<-"red"
G2vG3<-ggplot(q2,aes(x=G2, y=G3, color=final)) + geom_point()+
  geom_abline(stat = "abline", colour="red",size=1)+
  xlab("G2 score")+ylab("G3 score")+ggtitle("G2 VS G3 in term of gender")+
  facet_wrap(~sex)+
scale_color_identity()

#G1 against G3
q2$com <-"yellow"
q2[!is.na(q2$G1) & q2$G1 < q2$G3, "com"]<-"green"
q2[!is.na(q2$G3) & q2$G3 < q2$G1, "com"]<-"red"
G1vG3<-ggplot(q2,aes(x=G1, y=G3, color=com)) + geom_point()+
  geom_abline(stat = "abline", colour="red",size=1)+
  xlab("G1 score")+ylab("G3 score")+ggtitle("G1 VS G3 in term of gender")+
  facet_wrap(~sex)+
  scale_color_identity()

#Combine graphs
library(ggpubr)
ggarrange(G1vG2,G2vG3,G1vG3,ncol=1,nrow=3)

#In G1 vs G2 graph and G1 vs G3 graph, male students have more increasing on their score than female student
#In G2 vs G3 graph, female students have more increase on their score than male students 

#Analysis 2-3 : What is the relationship of age with the score of G1 to G3 in term of gender 
library(ggplot2)
#Age vs G1
AgeVG1<-ggplot(q2, aes(x=age, y=G1)) +
  geom_point(aes(shape = factor(sex),colour = factor(sex)))+
  facet_wrap(~sex)+
  ggtitle ("Age vs G1 in term of gender")+
  stat_smooth(method=lm)

#Age vs G2
AgeVG2<-ggplot(q2, aes(x=age, y=G2)) +
  geom_point(aes(shape = factor(sex),colour = factor(sex)))+
  facet_wrap(~sex)+
  ggtitle ("Age vs G2 in term of gender")+
  stat_smooth(method=lm)

#Age vs G3
AgeVG3<-ggplot(q2, aes(x=age, y=G3)) +
  geom_point(aes(shape = factor(sex),colour = factor(sex)))+
  facet_wrap(~sex)+
  ggtitle ("Age vs G3 in term of gender")+
  stat_smooth(method=lm)

#Combine graphs
library(ggpubr)
ggarrange(AgeVG1,AgeVG2,AgeVG3,ncol=1,nrow=3)


#For both male and female students, the G1,G2 and G3 score falls when the students have higher age.

#Conclusion: In my opinion, male students have a better performance and get higher mark in the overall 
#test. However, female students are doing well in G3 test compare with G2 test.Besides that, the scores
#of the tests is higher when the students are younger. This may because the students with higher age may 
#have work outside and young students are just need to study.

#Suggestion: The school can take more attention to the female students who have potential to get higher 
#mark in the test.The school can give extra class for the students which are over age of 18 so that they
#can enforce their learning in the class.

#=======================================================================================================
#Question 3: Is the student live in rural or urban area have the higher possible to get higher score in
#            G3? 
#=======================================================================================================
library(tidyverse)
q3 <- student_data %>% 
  select(address,traveltime,absences,G3)
#=======================================================================================================
#Analysis 3-1 : What is the percentage distribution of the students' address 
pie <- q3 %>% 
  select(address)
library(dplyr)
pie<-pie %>%
  group_by(address) %>% 
  tally()
pie(pie$n, labels=paste0(pie$n, " - " ,round((pie$n/922*100),digit=0),"%"),radius=1,
    main="Percentage of rural and urban student",col=c("green","blue"),clockwise=TRUE)
legend("topleft",legend=pie$address, fill= c("green","blue"))

#The number of urban students are 3 times more than the students in rural area.

#Analysis 3-2 : According to Analysis 3-1, what is the relationship between the address and the G3 score?
library(ggplot2)
ggplot(q3, aes(x=address, y=G3)) +
  geom_count(aes(colour=address))+
  ggtitle ("G3 score of urban and rural area student")+
  xlab("Student Adress")+
  ylab("G3 Score")

#Urban area students get higher score in G3 test.

#Analysis 3-3: Is the travel time from school to home is the factor that affect the G3 score of students
#in urban/rural area?   
ggplot(q3, aes(x=traveltime, y=G3)) +
  geom_jitter(aes(colour=address))+
  ggtitle ("G3 score of urban and rural area student")+
  xlab("Travel time to school")+
  ylab("G3 Score")+
  facet_wrap(~address)

#Most of the urban students has less travel time to school than rural students
#The students who get higher score in G3 has travel time to school less than 2 hours.
 
#Analysis 3-4: What is the relationship of absences and G3 score according to student's address 
ggplot(q3, aes(x=absences,y=G3))+
  geom_jitter(aes(colour=address))+
  facet_wrap(~address)+
  labs(title="Number of absences VS G3 Score in term of students' address",
       x="Number of absences",
       y="G3 score")

#The higher the number of absences, the lower the mark in G3 score.
#There are rural students absence more than 60 days and not pass the G3 test.

#Conclusion: The rural students are have less education opportunity than urban area students. They may
#need to have longer time to travel to school. Somehow, the rural students are not coming to school due
#to the long time journey to school and in result to get less score in G3 test.

#Suggestion: The government can add in more schools  in rural area while having the same quality of 
#education as the urban students. The teachers can also come to the rural area students home to have 
#additional tuition with the student who always absence to school.

#=======================================================================================================
#Question 4: How a family structure and relationship affect the G1, G2, and G3 score
library(tidyverse)
q4 <-student_data %>%
  select(famsize,Pstatus,guardian,famrel,G1,G2,G3)

#=======================================================================================================
#Analysis 4-1: How was the performance of students from G1 to G3?
library(corrplot)
library(dplyr)
m<-student_data %>%
  select(G1,G2,G3)
M<-cor(m)
corrplot(M,method="number",type="lower")

#The score has a strong positive relationship compare to the next grade test

#Analysis 4-2: What are the distribution of G1,G2,G3 against the family size
#G1 distribution
library(ggplot2)
G1_famsize<-ggplot(q4, aes(G1,colour=famsize))+
  geom_freqpoly(stat="bin",binwidth=1)+
  labs(title="Frequency distribution of G1 score against family size",
       y="Frequency",
       x="G1 Score")

#G2 distribution
G2_famsize<-ggplot(q4, aes(G2,colour=famsize))+
  geom_freqpoly(stat="bin",binwidth=1)+
  labs(title="Frequency distribution of G2 score against family size",
       y="Frequency",
       x="G2 Score")

#G3 distribution
G3_famsize<-ggplot(q4, aes(G3,colour=famsize))+
  geom_freqpoly(stat="bin",binwidth=1)+
  labs(title="Frequency distribution of G3 score against family size",
       y="Frequency",
       x="G3 Score")

library(ggpubr)
ggarrange(G1_famsize,G2_famsize,G3_famsize,ncol=1,nrow=3)

#The students with less than 3 family member has less people fail in the test.

#Analysis 4-3: How is the relationship of parent's cohabitation status with the G1,G2 and G3 score in 
#term of family size?
#Pstatus VS G1
PstatusVG1 <- ggplot(q4,aes(x=Pstatus,y=G1))+
  geom_jitter(aes(colour=famsize))+
  facet_wrap(~famsize)+
  ggtitle("Jitter Plot of parent cohabitation status VS G1 score in term of family size")
#PStatus vs G2
PstatusVG2 <- ggplot(q4,aes(x=Pstatus,y=G2))+
  geom_jitter(aes(colour=famsize))+
  facet_wrap(~famsize)+
  ggtitle("Jitter Plot of parent cohabitation status VS G2 score in term of family size")
#PStatus vs G3
PstatusVG3 <- ggplot(q4,aes(x=Pstatus,y=G3))+
  geom_jitter(aes(colour=famsize))+
  facet_wrap(~famsize)+
  ggtitle("Jitter Plot of parent cohabitation status VS G3 score in term of family size")

library(ggpubr)
ggarrange(PstatusVG1,PstatusVG2,PstatusVG3,ncol=1,nrow=3)

#The parents cohabitation status which are still together have higher score for the tests 

#Analysis 4-4: What are the relationship between family relationship and G3 score in term
#guardian of student
ggplot(q4, aes(x=famrel, y=G3))+
  geom_count(aes(colour=guardian))+
  facet_wrap(~guardian)+
  ggtitle("Scatter plot of guardian vs G3 score in term of parent cohabitation status")+
  xlab("Family Relationship")+
  ylab("G3 Score")

#The score is higher when family relationship is above average (3).
#The students who have mother as their guardian perform well in G3.

#Conclusion: In my opinion, the students have a positive increase in their test.Most of the students who
#have less than 3 family member have less fail points than the student with more than 3 family members.
#The students that have good family relationship can help in increase their score. Mother is a better gurdian 
#compared with father and other family member as they give more caring to the children.

#Suggestion: The students who have no siblings can make friends with the students to study together so that
#they can exchange their ideas and knowledge. The family are also encourage to be more caring to their 
#children so that they can study in a good family environment. The school can also have a talk to
#the parents that had divorced or separated to discuss about how they can help their own children with a
#better way in their education. The students' mother can also give more attention to the students and 
#provide a helping hand when they faced problems during study.

#=======================================================================================================
#Question 5: Is the parents' education level or their job affect more on the students' G3 score
q5<-student_data%>%
    select(Medu,Fedu,Mjob,Fjob,G3)
#=======================================================================================================
#Analysis 5-1: What is the relationship between the father,mother job and education
MjobVMedu<-ggplot(q5,aes(x=Mjob,y=Medu))+
    geom_jitter(aes(colour=Mjob))+
    labs(title="Scatter Plot of Mother Job and Education",
         x="Mother Job",
         y="Mother Education Level")
FjobVFedu<-ggplot(q5, aes(x=Fjob,y=Fedu))+
  geom_jitter(aes(colour=Fjob))+
  labs(title="Scatter Plot of Father Job and Education",
       x="Father Job",
       y="Father Education Level")

library(ggpubr)
ggarrange(MjobVMedu,FjobVFedu,ncol=2,nrow=1)

#Most of the mother and father have higher education 
#Mother are mostly in other sectors and work at home
#Father are mostly in other sectors and work in services sector(administrative or police)

#Analysis 5-2 : What is the relationship between Mother education and Father education level with the G3
#score
MeduVG3<-ggplot(q5,aes(y=G3, x=Medu))+
    geom_count(aes(colour=Medu))+
    labs(title="Scatter Plot of Mother Education and G3 score",
         x="Mother education",
         y="G3 score")
  
FeduVG3<-ggplot(q5,aes(y=G3,x=Fedu))+
    geom_count(aes(colour=Fedu))+
    labs(title="Scatter Plot of Father Education and G3 score",
         x="Father education",
         y="G3 score")

library(ggpubr)
ggarrange(MeduVG3,FeduVG3,ncol=2,nrow=1)
 
#Mother education is a more important factor than Father education. 
#The higher the mother education level, the higher the G3 score of the students.

#Analysis 5-3: What is the relationship between Mother job and Father job with the G3 score?
MjobVG3 <- ggplot(q5, aes(x=G3)) +
  geom_histogram(binwidth=1, colour="black",aes(fill=..count..))+
  scale_fill_gradient("Frequency", low="white",high="red")+
  facet_wrap(~Mjob)+
  labs(title="Histogram of Mother Job and G3 score",
       x="G3 Score",
       y="Frequency")

FjobVG3 <- ggplot(q5, aes(x=G3)) +
  geom_histogram(binwidth=1, colour="black",aes(fill=..count..))+
  scale_fill_gradient("Frequency", low="white",high="red")+
  facet_wrap(~Fjob)+
  labs(title="Histogram of Father Job and G3 score",
       x="G3 Score",
       y="Frequency")

library(ggpubr)
ggarrange(MjobVG3,FjobVG3,ncol=1,nrow=2)

#Most of the student which high G3 score have mother who have job in services like administrative or 
#police while father is in other sector. 

#Conclusion: In my opinion, the most of the students' parents have higher education and most of the father
#work in service and other sectors while mother is playing role as a housewife or work in other sectors.
#The mother education level is more important than father educational level. The mother who have high 
#education level will help in increase the score of G3 test. The mother who are in services sector is 
#more likely to let their children to get higher score in G3.

#Suggestion: The parents can learn more things about the academic so that they can increase their 
#knowledge to taught their children (especially mother). The parents can teach the students to be more
#discipline as what the parents in services sector done to let the student to be more obey to the rules
#and be higher behavioural engagement in school.
  
#=======================================================================================================
#Question 6: Do the students that have study with internet get higher score in the average mark?
q6<-student_data%>%
  select(studytime,internet,average)
#=======================================================================================================
#Analysis 6-1: What is the relationship of study time and internet?
ggplot(q6, aes(x=internet,y=studytime))+
  geom_jitter(aes(colour=internet))+
  labs(title="Jitter plot of Study time and internet",
       x="Internet",
       y="Study Time(hour)")

#The study time of student increase when there is internet available at home
#Most of the students have 2 hours of study time

#Analysis 6-2: How internet affect the average score of student
ggplot(q6, aes(average,colour=internet))+
  geom_freqpoly(stat="bin",binwidth=1)+
  labs(title="Frequency distribution of average score against study time",
       y="Frequency",
       x="Average Score")

#Most of the students with internet get higher mark for their average mark.

#Analysis 6-3: What is the relationship between study time and average score in term of internet?
ggplot(q6, aes(x=studytime,y=average))+
  geom_count(aes(colour=internet))+
  facet_wrap(~internet)+
  labs(title="Scatter plot of study time and average score in term of internet",
       x="Study Time (Hour)",
       y="Average Score")

#Study time with 1-2 hours is optimal choice to get higher average score
#Students study using internet within 2 hours can get excellent score for average mark

#Conclusion: The student use internet to online study and finding questions through internet can increase
#their performance in the tests. The students that are study within 2 hours is getting optimal score for
#average mark. 

#Suggestion: The school can have internet and computers facility in library to let the students check the
#ideas and knowledge through internet. The students is encouraged to study for 2 hours so that they can
#optimize their performance in the academic.

#=======================================================================================================
#Question 7: There is a improvement of score comparing G1 and G2 score in Analysis 4-1. What type of 
#education support should be given to make the improvement as the statement above and how can it works?
q7<-student_data%>%
  select(schoolsup,famsup,paid,G1,G2)
#=======================================================================================================
#Analysis 7-1: What type of education support is more prefer or given to the students
library(ggplot2)
ggplot(q7,aes(x=schoolsup,y=famsup))+
       geom_jitter(aes(colour=paid))+
       facet_wrap(~paid)+
       labs(title="Education support of Student",
            x="School Educational Support",
            y="Family Educational Support")

#Most of the students are not getting school educational support, but get family and paid education
#support more.

#Analysis 7-2: What is the relationship between G1 and G2 in term of school support
q7%>%
  filter(G2>=G1)%>%
  ggplot(aes(x=G1,y=G2))+
  geom_point(aes(colour=schoolsup))+
  stat_smooth(method="gam")+
  labs(title="Scatter plot of G1 vs G2 Score in term of school education support",
       x="G1 Score",
       y="G2 Score")

#The students who get school education support has less significant improve in their score in G2 test.

#Analysis 7-3: What is the relationship between G1 and G2 in term of family support
q7%>%
  filter(G2>=G1)%>%
  ggplot(aes(x=G1,y=G2))+
  geom_point(aes(colour=famsup))+
  stat_smooth(method="gam")+
  labs(title="Scatter plot of G1 vs G2 Score in term of family education support",
       x="G1 Score",
       y="G2 Score")

#The students with family education support help increase in the G2 score significantly.

#Analysis 7-4: What is the relationship between G1 and G2 in term of paid education support
q7%>%
  filter(G2>=G1)%>%
  ggplot(aes(x=G1,y=G2))+
  geom_point(aes(colour=paid))+
  stat_smooth(method="gam")+
  labs(title="Scatter plot of G1 vs G2 Score in term of paid education support",
       x="G1 Score",
       y="G2 Score")
#The students that have paid education support help increase in improvement of G2 score but not as 
#effective as the family education support.

#Conclusion: The effectiveness of the education support is according from family support, paid support,
#then school support. The family give more care to the students while they give more help to the students
#in their academic. While tuition and school are a place which a teacher or tutor give lesson to a big
#group of people, therefore there will be less attention given to a student only.

#Suggestion: The parents can help the students to revise their lesson together to help them when they have
#problem in academic. The students can consult with the teacher for one to one consultation so that they 
#can know more about the problems they faced.
#========================================================================================================
#Question 8: How the health level affect the output of G3 score?
q8<-student_data%>%
  select(failures,Dalc,Walc,health,absences,G3)
#========================================================================================================
#Analysis 8-1: How is the health bifurcation of the students in the dataset?
library(waffle)
waffle<-q8%>%
  select(health)
library(dplyr)
waffle<-waffle %>%
  group_by(health) %>% 
  tally()
waffle
wf<-c('Very Bad (1) - 11.5% (108 students)'=108,'Bad (2) - 11.6% (107 students)'=107,
      'Average (3) - 22.9% (211 students)'=211,'Good (4) - 16.2% (148 students)'=148,
      'Very Good (5) - 37.8% (348 students)'=348)
wf
waffle(wf/12, rows=5, size=0.6, 
       colors=c("#44D2AC", "#E48B8B", "#B67093", 
                "#3A9ABD", "#CFE252"), 
       title="Students' Health Bifurcation", 
       xlab="1 square = 12 persons")

#Most of the students have good health level. 

#Analysis 8-2: What are the relationship between failures and G3 score according to health level of student
ggplot(q8,aes(x=failures,y=G3))+
  geom_jitter(aes(colour=health))+
  facet_wrap(~health)+
  labs(title="Number of Failures VS G3 Score in term of health ",
       x="Failures",
       y="G3 Score")  

#When the number of failure decrease, the students can get higher marker in G3 score.

#Analysis 8-3 What is the distribution of absences number according to health level of students?
ggplot(q8, aes(absences,colour=health))+
  geom_freqpoly(stat="bin",binwidth=1)+
  facet_wrap(~health)+
  labs(title="Ferquency Distribution of Absences Number in term of Health level",
       x="Number of Absences",
       y="Frequency")

#Most of students within range of average to very good(3-5)has less number of absences.

#Analysis 8-4: What is the relationship between G3 score and number of absences in term of health level?
ggplot(q8,aes(x=G3,y=absences))+
  geom_point(aes(colour=health))+
  facet_wrap(~health)+
  labs(title="Scatter plot of G3 Score VS Number of Absences",
       x="G3 Score",
       y="Number of Absences")  

#To get higher mark in G3, the students are having number of absence less than 10.
#The students in health level 1 and 2 have sudents with excellent score and better than students with 
#a very healthy level.

#Analysis 8-5: Is it the alcohol comsumption of students is the factor that affect the
#G3 score of students?
#Dalc VS G3
DalcVG3<-ggplot(q8,aes(x=Dalc,y=G3))+
  geom_count(aes(colour=Dalc))+
  facet_wrap(~Dalc)+
  labs(title="Weekdays alcohol comsumption VS G3 score",
       x="Weekdays alcohol comsumption",
       y="G3 Score")

WalcVG3<-ggplot(q8,aes(x=Walc,y=G3))+
  geom_count(aes(colour=Walc))+
  facet_wrap(~Walc)+
  labs(title="Weekends alcohol comsumption VS G3 score",
       x="Weekends alcohol comsumption",
       y="G3 Score")

library(ggpubr)
ggarrange(DalcVG3,WalcVG3,ncol=1,nrow=2)

#Both of the weekday and weekends alcohol that are very little can score higher score in G3.

#Conclusion: Health of the students is not a case to obstruct the student's academic performance as they 
#have the will to study. The students that are less absences have higher mark than the students who always
#not coming to school. The alcohol consumption either in weekdays or weekends is not a good choice for
#a students cause it will make them dumb and cannot concentrate on study.

#The school should take care of the students who has less health and assist them with financial and moral
#support to increase their will to study. The school can also give a reward to the students who never late
#or absent to school. The government should retrict the students to not drink alcohol so that they would not
#get addicted on it.

#=======================================================================================================