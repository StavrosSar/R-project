### Σαρηκυριακίδης Σταύρος 164738 ### 
#Βιβλιοθήκες
library(readxl)
library(ggplot)
library(arules)
require(ggplot2)

### Βήμα-1, ### 
#Διαβάζω το αρχείο xlsx και το τοποθετώ στο bs(τυχαία μεταβλητή) 
#Για κάποιο λόγο μόνο έτσι μου δούλευε το read με διπλό "\\" . Σε εσάς κατα πασα πιθανότητα να μην το τρέχει το read. 
bs <- read_excel("C:\\Users\\user\\Desktop\\164738 Sarikyriakidis Stavros\\IDA_Ergasia_Data.xlsx")
bs

### Βήμα-2, ### 
#χωρίζω ανάλογα το μάθημα έτσι ώστε να υπαρχουν λίστα ανάλογα με το μάθημα
bs1= split(bs,bs$course)
bs1

### Βήμα-3, ###
#κάνω έλεγχο ζητώντας να τοποθετηθεί στο bs το μάθημα με όνομα C4, έτος=2018 και Α εξεταστική Εαρινού εξαμήνου
#και στο τέλος βρίσκω το summary που έχω βάλει απο το προηγούμενο βήμα bs
vima3 <- bs[bs$course==c("C4") & bs$year == 2018 & bs$exam == 'S1',]
summary(bs$grade)

### Βήμα-4, ###
#βάζει τους προσβάσιμους βαθμους του βήματος 3 και ύστερα διαλέγω τα grades για να τεστάρω 
vima4 <- subset(vima3, vima3$grade >= 5 )
grades <- vima4$grade
vima4

### Βήμα-5, ###
#με το discretize περνάω μεσα του το vima4 που έχει τη λιστα με τους βαθμούς του προηγούμενου ερωτηματος(τους προσβάσημους). Διαλέγω method=list  και συγκεκριμένα την grade list
#κάνοντας την μέθοδο frequency θα χωρίσει τα grades ανάλογα με την συχνότητα σε 4 κομμάτια(break=4). To default το έβαλα για να υπάρχει σε περίπτωση deafult.
#Στη συνέχεια κάνω cbind ενόνωτας το vima 4 με την λίστα που δημιουργήθηκε πριν διαλέγοντας μόνο το cell που έχει τα Medium, Good Κλπ. Υστερα κάνω rename το cell με βάση το ζητούμενο.
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
 
### Βήμα-6, ###
#Βρίσκει το πλήθος των grade ανάλογα με την κατηγορία
vima6<-summary(vima5$g_grade)
vima6

### Βήμα-7, ###
#παρουσιαση σε bar chart του vima6
barplot(vima6,border="red",col="blue",main="Vima 7")

### Βήμα-8, ###
#όπως στο βημα 3 κάνω το ίδιο απλα για τα μαθηματα του 2018 την πρώτη εξεταστικη του Εαρινου εξαμηνου
y2018 <- bs[bs$year == 2018 & bs$exam == 'S1',]
y2018

### Βήμα-9, ###
#Χωρίζω του βαθμούς σε προσβάσημους και μη προσβάσημους ετσι ώστε να κανω δωσω τιμες μονο τους
#πρσοσβασημους και απλα στους μη προσβασημους να δωσω την τιμη failed. Στη συνεχεια κανω ακριβως το ίδιο με το βημα3 οσων αφορα τα cbind 
#και μετα τα ενωνω και τα 2 με rbind για να βγει η τελική λίστα.
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

### Βήμα-10, ###
#βρίσκει τα ποσα επι της 100
prop.table(table(final_ordered$course, final_ordered$d_grade))*100
#Μεγαλυτερο ποσοστο αποτυχιας έχει το C8 
#Μεγαλυτερο ποσοστο εποτυχιας έχει το C9

### Βήμα-11, ###
#παιρνω τους προσβασημους και φτιάχνω για bar plot αλλα και για pie plot
op4 = final_ordered[final_ordered$grade >= 5, ]

#BAR PLOT
ggplot(op4, aes(x=course, fill=d_grade)) + geom_bar()

#PIE PLOT
pp <- ggplot(op4, aes(x=course, fill=d_grade)) + geom_bar(position='fill') + ylab("proportions")
pp + coord_polar("y", start = 0)

### Βήμα-12, ###
#καθαρίζω την μνημη αρχικα και στη συνεχεια παίρνω μονο με βάση όλους τους προσβάσημους τον μαθητη, το μάθημα και τον βαθμό του 
rm(list=ls())

Data <- read_excel("C:\\Users\\user\\Desktop\\164738 Sarikyriakidis Stavros\\IDA_Ergasia_Data.xlsx")
Data_passed <- subset(Data, Data$grade >= 5 )
Data_passed_slim <-Data_passed[ c("student","course","grade")]

### Βήμα-13, ### 
#ίδια διαδικασια με τα προηγούμενα απλά τωρα τα δεδομένα παιρναν σε ένα αρχείο που δημιουργησαμε και με ένα 
#διαχωριστικό ανάμεσα στο μάθημα και στον βαθμο("--"). Επισης δίνω την τιμη has στο cell που εξηγηθηκε παραπάνω.
d_grade<-discretizeDF(Data_passed_slim,methods=list(
  grade=list(method="frequency",breaks=4,
             labels=c("Medium","Good","Very Good","Excellent"))
),default=list(method="none")
)
d_grade
Data_passed_slim$has <-paste(Data_passed_slim$course, "--" ,d_grade$grade)
write.csv(Data_passed_slim[c("student","has")], "C:\\Users\\user\\Desktop\\164738 Sarikyriakidis Stavros\\united_data_passed.csv" )

### Βήμα-14, ### 
my_transactions <- my_transactions <- read.transactions(file = "united_data_passed.csv", sep = ",",
  format="single", cols=c("student","has"), header=TRUE)

### Βήμα-15, ### 
#βρίσκει με βαση το summary του my_transactions ποσοι μαθητες έχουν περασει το αναλογο ποσο μαθηματων.
vima15<-summary(my_transactions)@lengths
vima15<-as.data.frame(vima15)
vima15

### Βήμα-16, ### 
#κανω το bar chart 
ggplot(data=vima15, aes(x=sizes, y=Freq)) + geom_bar(stat="identity")

### Βήμα-17, ### 
#παιρνουμε την σχειτκη συχνοτητα και την κάνουμε decreasing οπως δινει στο ζητουμενο. Μετατρεπουμε σε dataframe το πλεον vima17
#το vima17 εχει στην κατοχη του τα μ5 μαθηματα γιαυτο βάζουμε και [1:5]. 
vima17 = itemFrequency(my_transactions,type='relative')
vima17 = sort(vima17,decreasing = TRUE)[1:5]
fr_dataframe <- as.data.frame(vima17)
x_axis<-reorder(rownames(fr_dataframe), -vima17)
ggplot(fr_dataframe,aes(x=x_axis,y=vima17))+geom_bar(stat='identity')