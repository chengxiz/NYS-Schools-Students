# Auothor: Chengxi Zhu
library(dplyr)
# Import data of schools
schools <- read.table("Schools.csv", fill=TRUE, sep=",")
cn=NULL
for (i in schools[1,]){
  cn<-c(cn,toString(i))
}
colnames(schools)<-cn
schools<-schools[-1,]
# Import data of students
students <- read.table("Students.csv", fill=TRUE, sep=",")
cn=NULL
for (i in students[1,]){
  cn<-c(cn,toString(i))
}
colnames(students)<-cn
students<-students[-1,]
# Problem 3
# group by the school student is attending
d3a<-students %>%
  group_by(School_201) %>%
  summarise(count=n()) %>%
  arrange(desc(count))
data_cat=NULL
AttSchoolList<-list()
SchoolList<-list()
F_R_LunchList<-list()
EthList<-list()
EllList<-list()
AttSchoolList<-group_by(students,School_201)
ll=0
for (i in as.numeric(as.matrix(d3a[,1]))){
  name<-paste0("Num",i) 
  st_subgp<-filter(students,School_201==i)
  # number of students in each school zone
  SchoolList[[name]]<-st_subgp %>%
    group_by(Home_Atten) %>%
    summarise(count=n()) %>%
    mutate(freq = 100 * count / sum(count))
  # % of population on F/RL
  F_R_LunchList[[name]]<-st_subgp %>%
    group_by(F_R_Lunch) %>%
    summarise(count=n()) %>%
    mutate(freq = 100 * count / sum(count))
  #  % ethnic population (this is a count display)
  EthList[[name]]<-st_subgp %>%
    group_by(Ethnicity) %>%
    summarise(count=n()) %>%
    mutate(freq = 100 * count / sum(count))
  #  % of ELL
  EllList[[name]]<-st_subgp %>%
    group_by(ELL_2015_2) %>%
    summarise(count=n()) %>%
    mutate(freq = 100 * count / sum(count))
}

#filter(F_R_Lunch=="YES")
#group_by(Home_Atten) %>%
# group_by(Home_Att_1) %>%



