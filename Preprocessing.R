# Auothor: Chengxi Zhu
#thanks for Dr Wilson
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
BallotList<-list()
BallotEthList<-list()
BallotEllList<-list()
BallotList1<-list()
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
  # number of students from their school zones who balloted (Choice 1, 2 or 3) school 534
  st_subgpCh1<-filter(students,Choice_1__==i)
  st_subgpCh2<-filter(students,Choice_2__==i)
  st_subgpCh3<-filter(students,Choice_3__==i)
  st_ballot<-rbind(st_subgpCh1,st_subgpCh2,st_subgpCh3)
  temp<-st_ballot %>%
    group_by(School_201) %>%
    summarise(count=n()) %>%
    mutate(freq = 100 * count / sum(count)) %>%
    arrange(desc(freq))
  BallotList[[name]]<-temp
  BallotList1[[name]]<-filter(temp,School_201==i)
  remove(temp)
  #  % ethnic population (this is a count display)
  BallotEthList[[name]]<-st_ballot %>%
    filter(School_201==i) %>%
    group_by(Ethnicity) %>%
    summarise(count=n()) %>%
    mutate(freq = 100 * count / sum(count))
  #  % of ELL
  BallotEllList[[name]]<-st_ballot %>%
    group_by(ELL_2015_2) %>%
    summarise(count=n()) %>%
    mutate(freq = 100 * count / sum(count))
}
rm(st_subgp)
rm(st_ballot)
rm(st_subgpCh1)
rm(st_subgpCh2)
rm(st_subgpCh3)
###
#filter(F_R_Lunch=="YES")
#group_by(Home_Atten) %>%
# group_by(Home_Att_1) %>%



