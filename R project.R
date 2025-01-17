### �������������� ������� 164738 ### 
#�����������
library(readxl)
library(ggplot)
library(arules)
require(ggplot2)

### ����-1�, �### 
#������� �� ������ xlsx ��� �� �������� ��� bs(������ ���������) 
#��� ������ ���� ���� ���� ��� ������� �� read �� ����� "\\" . �� ���� ���� ���� ���������� �� ��� �� ������ �� read. 
bs <- read_excel("C:\\Users\\user\\Desktop\\164738 Sarikyriakidis Stavros\\IDA_Ergasia_Data.xlsx")
bs

### ����-2�, �### 
#������ ������� �� ������ ���� ���� �� �������� ����� ������� �� �� ������
bs1= split(bs,bs$course)
bs1

### ����-3�, �###
#���� ������ �������� �� ����������� ��� bs �� ������ �� ����� C4, ����=2018 ��� � ���������� ������� ��������
#��� ��� ����� ������ �� summary ��� ��� ����� ��� �� ����������� ���� bs
vima3 <- bs[bs$course==c("C4") & bs$year == 2018 & bs$exam == 'S1',]
summary(bs$grade)

### ����-4�, �###
#����� ���� ������������ ������� ��� ������� 3 ��� ������ ������� �� grades ��� �� ������� 
vima4 <- subset(vima3, vima3$grade >= 5 )
grades <- vima4$grade
vima4

### ����-5�, �###
#�� �� discretize ������ ���� ��� �� vima4 ��� ���� �� ����� �� ���� ������� ��� ������������ ����������(���� ������������). ������� method=list  ��� ������������ ��� grade list
#�������� ��� ������ frequency �� ������� �� grades ������� �� ��� ��������� �� 4 ��������(break=4). To default �� ����� ��� �� ������� �� ��������� deafult.
#��� �������� ���� cbind �������� �� vima 4 �� ��� ����� ��� ������������� ���� ����������� ���� �� cell ��� ���� �� Medium, Good ���. ������ ���� rename �� cell �� ���� �� ���������.
str(grades)
hist(grades)

dis_vima5<-discretizeDF(vima4,methods=list(
  grade=list(method="frequency",breaks=4,
             labels=c("Medium","Good","Very Good","Excellent"))
  ),default=list(method="none")
  )


vima5 <- cbind(vima4,dis_vima5$grade)
names(vima5)[names(vima5)== "dis_vima5$grade" ] <- "g_grade"
vima5
 
### ����-6�, �###
#������� �� ������ ��� grade ������� �� ��� ���������
vima6<-summary(vima5$g_grade)
vima6

### ����-7�, �###
#���������� �� bar chart ��� vima6
barplot(vima6,border="red",col="blue",main="Vima 7")

### ����-8�, �###
#���� ��� ���� 3 ���� �� ���� ���� ��� �� �������� ��� 2018 ��� ����� ���������� ��� ������� ��������
y2018 <- bs[bs$year == 2018 & bs$exam == 'S1',]
y2018

### ����-9�, �###
#������ ��� ������� �� ������������ ��� �� ������������ ���� ���� �� ���� ���� ����� ���� ����
#������������� ��� ���� ����� �� ������������ �� ���� ��� ���� failed. ��� �������� ���� ������� �� ���� �� �� ����3 ���� ����� �� cbind 
#��� ���� �� ����� ��� �� 2 �� rbind ��� �� ���� � ������ �����.
prosvasimi <- subset(y2018, y2018$grade >= 5 )
mi_prosvasimi <- subset(y2018, y2018$grade < 5 )

prosv_vima9<-discretizeDF(prosvasimi,methods=list(
  grade=list(method="frequency",breaks=4,
             labels=c("Medium","Good","Very Good","Excellent"))
),default=list(method="none")

)
vima9_1 <- cbind(prosvasimi,prosv_vima9$grade)
names(vima9_1)[names(vima9_1)== "prosv_vima9$grade" ] <- "failed"

failed <- c("failed")
vima9_2 <- cbind(mi_prosvasimi,failed)

teliko <- rbind(vima9_2,vima9_1)

names(teliko)[names(teliko)== "failed" ] <- "d_grade"
final_ordered <- teliko[order(teliko$student),]
final_ordered

### ����-10�, �###
#������� �� ���� ��� ��� 100
prop.table(table(final_ordered$course, final_ordered$d_grade))*100
#���������� ������� ��������� ���� �� C8 
#���������� ������� ��������� ���� �� C9

### ����-11�, �###
#������ ���� ������������ ��� ������� ��� bar plot ���� ��� ��� pie plot
op4 = final_ordered[final_ordered$grade >= 5, ]

#BAR PLOT
ggplot(op4, aes(x=course, fill=d_grade)) + geom_bar()

#PIE PLOT
pp <- ggplot(op4, aes(x=course, fill=d_grade)) + geom_bar(position='fill') + ylab("proportions")
pp + coord_polar("y", start = 0)

### ����-12�, �###
#�������� ��� ����� ������ ��� ��� �������� ������ ���� �� ���� ����� ���� ������������ ��� ������, �� ������ ��� ��� ����� ��� 
rm(list=ls())

Data <- read_excel("C:\\Users\\user\\Desktop\\164738 Sarikyriakidis Stavros\\IDA_Ergasia_Data.xlsx")
Data_passed <- subset(Data, Data$grade >= 5 )
Data_passed_slim <-Data_passed[ c("student","course","grade")]

### ����-13�, �### 
#���� ���������� �� �� ����������� ���� ���� �� �������� ������� �� ��� ������ ��� ������������� ��� �� ��� 
#������������ ������� ��� ������ ��� ���� �����("--"). ������ ���� ��� ���� has ��� cell ��� ��������� ��������.
d_grade<-discretizeDF(Data_passed_slim,methods=list(
  grade=list(method="frequency",breaks=4,
             labels=c("Medium","Good","Very Good","Excellent"))
),default=list(method="none")
)
d_grade
Data_passed_slim$has <-paste(Data_passed_slim$course, "--" ,d_grade$grade)
write.csv(Data_passed_slim[c("student","has")], "C:\\Users\\user\\Desktop\\164738 Sarikyriakidis Stavros\\united_data_passed.csv" )

### ����-14�, �### 
my_transactions <- my_transactions <- read.transactions(file = "united_data_passed.csv", sep = ",",
  format="single", cols=c("student","has"), header=TRUE)

### ����-15�, �### 
#������� �� ���� �� summary ��� my_transactions ����� ������� ����� ������� �� ������� ���� ���������.
vima15<-summary(my_transactions)@lengths
vima15<-as.data.frame(vima15)
vima15

### ����-16�, �### 
#���� �� bar chart 
ggplot(data=vima15, aes(x=sizes, y=Freq)) + geom_bar(stat="identity")

### ����-17�, �### 
#��������� ��� ������� ��������� ��� ��� ������� decreasing ���� ����� ��� ���������. ������������ �� dataframe �� ����� vima17
#�� vima17 ���� ���� ������ ��� �� �5 �������� ������ ������� ��� [1:5]. 
vima17 = itemFrequency(my_transactions,type='relative')
vima17 = sort(vima17,decreasing = TRUE)[1:5]
fr_dataframe <- as.data.frame(vima17)
x_axis<-reorder(rownames(fr_dataframe), -vima17)
ggplot(fr_dataframe,aes(x=x_axis,y=vima17))+geom_bar(stat='identity')