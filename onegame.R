rm(list=ls())
library(tidyverse)
laltor<-read_csv("laltor.csv")
laltor<-laltor%>%
  separate("score",into=c("score1","score2"),sep="-",convert=T)
nQ1<-which(laltor$detail=="End of the 1st Quarter")
nQ2<-which(laltor$detail=="End of the 2nd Quarter")
nQ3<-which(laltor$detail=="End of the 3rd Quarter")
nQ4<-which(laltor$detail=="End of the 4th Quarter")
print(nQ1)
print(nQ2)
print(nQ3)
print(nQ4)
laltor$Q<-c(1:length(laltor$detail))
laltor$time2<-laltor$time
laltor$Q[1:nQ1]<-"Quarter1"
laltor$Q[(nQ1+1):nQ2]<-"Quarter2"
laltor$Q[(nQ2+1):nQ3]<-"Quarter3"
laltor$Q[(nQ3+1):(nQ4+1)]<-"Quarter4"
laltor$time2[grep(":",laltor$time)]<-
  apply(sapply(str_split(grep(":",laltor$time,value = T),":"),as.numeric), 2, function(col)
    col[1]*60+col[2])

laltor$score1type<-c(0,diff(laltor$score1))
laltor$score2type<-c(0,diff(laltor$score2))
laltor$score1typem<-laltor$score1type
laltor$score2typem<-laltor$score2type
laltor$score1typem[laltor$score1typem<0]<-0
laltor$score2typem[laltor$score2typem<0]<-0
laltor$score1m<-cumsum(laltor$score1typem)
laltor$score2m<-cumsum(laltor$score2typem)
sum(diff(laltor$score1)<0)
sum(diff(laltor$score2)<0)


grep(pattern = "enters the game for ",laltor$detail,value = T)
lal<-c()
grep(pattern = ".+(?=( enters the game for ))","Kentavious Caldwell-Pope enters the game for LeBron James",value = T)
text2<-c("704232753@qq.com is my email address.") 
grep("[0-9.*]+@[a-z.*].[a-z.*]",text2,value = T) 
library(stringr)
a<-"Kentavious Caldwell-Pope enters the game for LeBron James"
a<-grep(pattern = "enters the game for ",laltor$detail,value = T)
str_extract(a,".+(?= enters the game for)")
str_extract(a,"(?<= enters the game for ).+")








playbyplay2$timeall<-c(playbyplay2$time3[1:nQ1]+2160,playbyplay2$time3[(nQ1+1):nQ2]+1440,
                       playbyplay2$time3[(nQ2+1):nQ3]+720,playbyplay2$time3[(nQ3+1):(nQ4+1)])
c(playbyplay2$time3[1:nQ1]+2160,playbyplay2$time3[1:nQ1]+1440,
  playbyplay2$time3[1:nQ1]+720)
c(playbyplay2$time3[1:nQ1]+2160,playbyplay2$time3[nQ1+1:nQ2]+1440,
  playbyplay2$time3[nQ2+1:nQ3]+720,playbyplay2$time3[nQ3+1:nQ4+1])
###########################################
number<-c(2,3,4,5,7,9)
c(0,diff(number))
diff(number,lag = 2)


##########################
playbyplay<-read.csv("onegame.csv")
playbyplay2<-read_csv("onegame.csv")
atime<-playbyplay2[1,2]
atime2<-as.character(playbyplay2[1,2])
length(atime)
nchar(atime)
x <- c('abcd', 1379, '偷闲阁', NA)
nchar(x)
nchar(x, type = 'bytes')
nchar(x, keepNA = F)
atime[1]
strsplit(x, split, fixed = FALSE, perl= FALSE, useBytes = FALSE)
strsplit(atime)
class(atime2)
atime2[2]
length(atime2)
class(strsplit(atime2,":"))
library(stringr)
a<-"dddddd"
x<-c("\"","\\")
x
print(x)
writeLines(x)
y67uhb
str_length(atime2)
b<-str_split(atime2)


head(USArrests)
USArrests<-USArrests
names(USArrests)
rownames(USArrests)
states<-rownames(USArrests)
substr(x=states,start = 1,stop = 4)
names(abbreviate(states,minlength = 5))
length(abbreviate(states,minlength = 5))
abbreviate(states,minlength = 5)[50]
nchar(states)
hist(nchar(states),main = "Histogram",xlab = "number of characters
     in the USA State names")
grep(pattern = "w", x = states, value = TRUE)
grep(pattern = "w", x = states)
states[grep(pattern = "w", x = states)]
states[which(state_chars == max(state_chars))]
state_chars <- nchar(states)
state_chars 
state_chars == max(state_chars)
which(state_chars == max(state_chars))
grep(":",playbyplay2$time,value = T)
states[state_chars == max(state_chars)]
# get states names with 'w'
grep(pattern = "w", x = states, value = TRUE)
grep(pattern = "W", x = states, value = TRUE)
grep(pattern = "[wW]", x = states, value = TRUE)
grep(pattern = "w",x=states,ignore.case = T,value = T)
grep(pattern = "W",x=states,ignore.case = T,value = T)
str_count(states,"a")

sum(str_count(playbyplay2$detail,"dunk"))
Derrick Rose

sum(str_count(playbyplay2$detail,"Derrick Rose offensive rebound"))
sum(str_count(playbyplay2$detail,"Derrick Rose defensive rebound"))
sum(str_count(playbyplay2$detail,"Derrick Rose assists"))
# number of vowels

# vector of vowels
vowels <- c("a","e","i","o","u")

# vector for storing results 
num_vowels <- vector(mode = "integer",length = 5)
num_vowels <- vector()
num_vowels
# calculate
for(i in seq_along(vowels)){
  num_aux <- str_count(tolower(states),vowels[i])
  num_vowels[i]<-sum(num_aux)
}

# add names
names(num_vowels)<-vowels

# total number of vowels
num_vowels

# barplot
barplot(num_vowels, main = "number of vowels in USA States names")

tolower(playbyplay2$detail)
toupper(playbyplay2$detail)
seq_along(vowels)
seq_along(playbyplay2$detail)
library(stringr)
metaChar = c("$","*","+",".","?","[","^","{","|","(","\\")
metaChar
grep(pattern="$", x=metaChar, value=TRUE)
grep(pattern="\\", x=metaChar, value=TRUE)
grep(pattern="(", x=metaChar, value=TRUE)
gsub(pattern="|", replacement=".", "gsub|uses|regular|expressions")
strsplit(x="strsplit.aslo.uses.regular.expressions", split=".")
grep(pattern = "[wW]", x = states, value = TRUE)
strsplit(x="strsplit.aslo.uses.regular.expressions", split=".")
# compare
strsplit(x="strsplit.aslo.uses.regular.expressions", split="\\.")
library(stringr)
str_extract_all(string = "my cridit card number: 34901358932236",pattern = "\\d")
grep("dunk",playbyplay2$detail,value = T)
grep("[dunkblock]",playbyplay2$detail,value = T)
str_extract_all(string = "my cridit card number: 34901358932236",pattern = "\\d")
strsplit(x="strsplit.aslo.uses.regular.expressions", split="\\.")
strsplit(x="strsplit.aslo.uses.regular.expressions", sep=".")
text<-readLines("atext.txt")
class(text)
text2<-readLines("atext.txt",encoding = "unicode")
text2
text3<-scan("atext.txt",what = character(0),encoding = "UTF-8")
text4<-scan("atext.txt",what = character(0),sep = "\n",encoding = "UTF-8")
text4<-scan("atext.txt",what = character(0),sep = "",encoding = "UTF-8")
x<- c("I love R","I'm fascinated by Statisitcs")
nchar(x)
str_count(x,pattern = " ")
str_length(x)
length(x)
DNA <- "AgCTaaGGGcctTagct"
chartr("Tt", "Uu", DNA)
DNA
str_replace_all(string = DNA,pattern = "T",replacement = "U") %>%
  str_replace_all(string = .,pattern = "t",replacement = "u")
# paste
paste("control",1:3,sep = "_")
paste("control",1:3,"abc",sep = "_")
paste(c("control",1:3,"abc"),sep = "_")
paste(c("control",1:3,"abc"),collapse = "_")
paste("control",1:3,"abc",collapse  = "_")
# str_c()
library(stringr)
str_c("control",1:3,sep = "_")
str_c("control",1:3,sep = "_")
str_c(c("control",1:3,"abc"),collapse = "_")
str_c("control",1:3,"abc",collapse = "_")
# strsplit
text <- "I love R.\nI'm fascinated by Statisitcs."
cat(text)
strsplit(text,split = " ")
strsplit(text,split = "\\s")

# str_split
library(stringr)
str_split(text,pattern = "\\s")
str_split(text,pattern = " ")

# grep
x<- c("I love R","I'm fascinated by Statisitcs","I")
grep(pattern = "love",x = x)
grep(pattern = "love",x = x,value = TRUE)
grepl(pattern = "love",x = x)
x[grep(pattern = "love",x = x)]
x[grepl(pattern = "love",x = x)]
# str_detect

str_detect(string = x, pattern = "love")

#################################
# 
# match,完全匹配， 常用的 %in% 由match()定义
match(x = "I",table = x)
"I'm" %in% x
match(x = "love",table = x)
"I love R" %in% x
grep(":",playbyplay2$time,value = T)
":" %in%playbyplay2$time

str_split(playbyplay2$time,pattern = ":")
grep("[a-f]",letters,fixed=T,value = T)  
grep("[a-f]",letters)  
grep("z",letters,value = T) 
txt <- c("arm","foot","lefroo", "bafoobar")
grep("foo",txt,value = T)
grep("fo",txt,value = T,fixed = T)
grep("Foo",txt,value = T)
grep("Foo",txt,value = T,ignore.case = T)
grep("Foo",txt,value = T,invert = T,ignore.case = T)
####################################################
playbyplay2[1,2]
str_split(playbyplay2$time,":")
str(str_split(playbyplay2$time,":"))
as.numeric(str_split(playbyplay2$time,":"))
grep(":",playbyplay2$time,value = T)
str_split(grep(":",playbyplay2$time,value = T),":")[2]
timemi<-as.data.frame(matrix(str_split(grep(":",playbyplay2$time,value = T),":")),byrow=F)
names(timemi)
byrow=T
strsplit(playbyplay2$score,"")
strsplit(playbyplay2$score,split = "-")

unlist(strsplit("a.b.c", "."))
## [1] "" "" "" "" ""
## following are right answers
unlist(strsplit("a.b.c", "[.]"))
## [1] "a" "b" "c"
unlist(strsplit("a.b.c", ".", fixed = TRUE))
unlist(strsplit("a.b.c","\\."))
unlist(strsplit("a.b.c","\\.",fixed=T))
strsplit("abcde",NULL)
rec<-list(name="李明",age=30,scores=c(85,76,90))
rec
m<-as.matrix(rec)
class(m)
m[1]
m[3,1]
m
m[1,]
typeof(m)
as.data.frame(matrix(rec))
l<-list(a=c(1,2,3),b=c(4,5,6),c=c(7,8,9))
lm<-as.matrix(l)
lm
lmd<-as.data.frame(lm)
lmd2<-as.data.frame(matrix(l))
data.frame(matrix(unlist(l),byrow=T,ncol = 3))
rec
rec[[1]]
typeof(rec[1])
typeof(rec[[3]])
names(rec)
x <- c('10, 8, 7', '5, 2, 2', '3, 7, 8', '8, 8, 9')
res <- strsplit(x, ','); res
sapply(res,as.numeric)
d.class<-read.csv("class.csv",header = T)
d.class2<-read.csv("class.csv",header = F)
typeof(d.class[,"age"])
sapply(d.class,typeof)
d.class3<-read.csv("class.csv",stringsAsFactors = F)
res <- strsplit(x, ',')
res
res2 <- strsplit(x, split=',')
res2
ma<-matrix(c(1:12),byrow = T,nrow = 3)
ma
colSums(ma)
s <- c('10, 8, 7', 
       '5, 2, 2', 
       '3, 7, 8', 
       '8, 8, 9')
sapply( strsplit(s, ',', fixed=TRUE),
        function(ss) sum(as.numeric(ss)) )
strsplit(s, ',', fixed=TRUE)[[1]]
typeof(strsplit(s, ',', fixed=TRUE)[1])
sum(c("3","4","5"))
as.numeric(c("3","4","5"))
sum(as.numeric(c("3","4","5")))
sapply(s, as.numeric)
as.numeric(s)
s1<-as.data.frame(s)
s1
class(s1[1,])
sapply(s1, as.numeric)
s1<-str_split(s,",")
s2<-sapply(s1,as.numeric)
typeof(s2)
s3<-lapply(s1, as.numeric)
s3
s2
s2[,1][2]
typeof(s2)
s2[2,1]
sapply(s2,com)
com<-function(s){
  return(s[1]*1+s[2]*2+s[3]*3)
}
sapply(s2,com)
sapply(s2,function(s) s[1]*1+s[2]*2+s[3]*3)
sapply(s2,sum)
sapply( strsplit(s, ',', fixed=TRUE),
        function(ss) sum(as.numeric(ss)) )
strsplit(s, ',', fixed=TRUE)
sapply(strsplit(s, ',', fixed=TRUE),function(s) s[1])
sapply(strsplit(s, ',', fixed=TRUE), function(s) as.numeric(s)[1]*1+as.numeric(s)[2]*2+
         as.numeric(s)[3]*3)

s2
apply(s2, 2, function(s) s[1]*1+s[2]*2+s[3]*3)
apply(s2, 1, function(s) s[1]*1+s[2]*2)
grep(":",playbyplay2$time,value = T)
str_split(playbyplay2$time,":")
sapply(str_split(playbyplay2$time,":"),as.numeric)
c<-c(1,2)
sapply(str_split(grep(":",playbyplay2$time,value = T),":"),as.numeric)
apply(sapply(str_split(grep(":",playbyplay2$time,value = T),":"),as.numeric), 2, function(col)
  col[1]*60+col[2])
playbyplay2$time2<-playbyplay2$time
playbyplay2$time2[grep(":",playbyplay2$time)]<-
  apply(sapply(str_split(grep(":",playbyplay2$time,value = T),":"),as.numeric), 2, function(col)
  col[1]*60+col[2])

apply(sapply(str_split(grep(":",playbyplay2$time,value = T),":"),as.numeric), 2, function(col) col[1]*60+col[2])
  
grep(":",playbyplay2$time,value = T)
playbyplay2$time3<-as.numeric(playbyplay2$time2)

which(playbyplay2$detail=="End of the 1th Quarter")
which(playbyplay2$detail=="End of the 1th Quarter")
c<-c(1,2,3)
c==2
which(c==3)
which(playbyplay2$detail=="End of the 1st Quarter")
playbyplay2$Q<-c(1:length(playbyplay2$detail))
playbyplay2$Q<-c(1,2,3)
nQ1<-which(playbyplay2$detail=="End of the 1st Quarter")
nQ2<-which(playbyplay2$detail=="End of the 2nd Quarter")
nQ3<-which(playbyplay2$detail=="End of the 3rd Quarter")
nQ4<-which(playbyplay2$detail=="End of the 4th Quarter")
nQ1
nQ2
nQ3
nQ4
(nQ1+1):nQ2

playbyplay2$Q[1:nQ1]<-"Quarter1"
playbyplay2$Q[(nQ1+1):nQ2]<-"Quarter2"
playbyplay2$Q[(nQ2+1):nQ3]<-"Quarter3"
playbyplay2$Q[(nQ3+1):(nQ4+1)]<-"Quarter4"
playbyplay2$timeall<-c(playbyplay2$time3[1:nQ1]+2160,playbyplay2$time3[(nQ1+1):nQ2]+1440,
  playbyplay2$time3[(nQ2+1):nQ3]+720,playbyplay2$time3[(nQ3+1):(nQ4+1)])
c(playbyplay2$time3[1:nQ1]+2160,playbyplay2$time3[1:nQ1]+1440,
  playbyplay2$time3[1:nQ1]+720)
c(playbyplay2$time3[1:nQ1]+2160,playbyplay2$time3[nQ1+1:nQ2]+1440,
  playbyplay2$time3[nQ2+1:nQ3]+720,playbyplay2$time3[nQ3+1:nQ4+1])
12*3*60
12*2*60
12*1*60
c(playbyplay2$time3[1:nQ1]+2160)
c(playbyplay2$time3[nQ1+1:nQ2]+1440)
playbyplay2$time3[nQ1+1:nQ2]
48*60
#playbyplay2$Q[,]
#playbyplay2$time3[,]

playbyplay2$score2<-playbyplay2$score
playbyplay3<-playbyplay2%>%
  separate("score2",into = c("ascore","bscore"),sep = "-",convert = T)
playbyplay3$timeall2<-(2880-playbyplay3$timeall)
plot(playbyplay3$timeall2,playbyplay3$ascore)
par(new=TRUE)
plot(playbyplay3$timeall2,playbyplay3$bscore,col="red")
score<-data.frame(time=playbyplay3$timeall2,score1=playbyplay3$ascore,score2=playbyplay3$bscore)
score2<-unique(score)
plot(score2$time,score2$score1,type = "l")
par(new=TRUE)
plot(score2$time,score2$score2,type = "l",col="red")
score2[length(score2$time),]
abline(h = score$score1[length()], col = "red")
c<-c(1,2,3)
c[-1]
c[1]
c[2]
c[3]
c[-1]
library(ggplot2)
ggplot(score2,aes(x=time,y=score1))+geom_line()+geom_point()
# Load plyr so we can use ddply() to create the example data set
library(plyr)
# Summarize the ToothGrowth data
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))
ToothGrowth
# Map supp to colour
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line()


for(i in 1:length(score$time)){
  
}

score2change<-apply(score2,2, dif)
dif<-function(a){
  b<-a
  for(i in 2:length(a)){
    b[i]<-a[i]-a[i-1]
  }
  return(b)
}
score2all<-cbind(score2,score2change)
table(acore2all$score1type)[2]
table(acore2all$score2type)
names(score2all)[c(4,5,6)]<-c("timesection","score1type","score2type")

pie(table(acore2all$score1type)[c(2,3,4)],labels = c("free throw","two-point","three point"))
write.csv(file = "playbyplay3.csv",playbyplay3)
grep("enters the game for",playbyplay3$detail,value = T)
strsplit(x="strsplit.aslo.uses.regular.expressions", split=".")
str_split(x="strsplit.aslo.uses.regular.expressions", split=".")
# compare
str_split("strsplit.aslo.uses.regular.expressions", split="\\.")
strsplit(x="strsplit.aslo.uses.regular.expressions", split="\\.")

################
# function 2:
library(stringr)
str_extract_all(string = "my cridit card number: 34901358932236",pattern = "\\d")
test_vector<-c("123","456","321")
library(stringr)
str_extract_all(test_vector,"3")
str_extract_all(test_vector,"^3")
# function 2
str_extract_all(test_vector,"[^3]")
# function 1
test_vector<-c("123","456$","321")
library(stringr)
str_extract_all(test_vector,"3$")
grep("3$",test_vector,value = T)

# function 2
str_extract_all(test_vector,"[3$]")
str_extract_all(string = c("regular.expressions\n","\n"), pattern ="\\.")
grep("\\.",c("regular.expressions\n","\n"),value = T)
test_vector2<-c("AlphaGo实在厉害！","alphago是啥","阿尔法狗是一条很凶猛的狗。")
str_extract_all(string = test_vector2, pattern ="AlphaGo|阿尔法狗")

grep(pattern ="AlphaGo|阿尔法狗",test_vector2,value = T,ignore.case = T)
str_extract_all(string = c("abc","ac","bc"),pattern = "ab?c")
grep("a?b",c("abc","ac","bc"),value = T)
str_extract_all(string = c("abababab","abc","ac"),pattern = "(ab)*")
str_extract_all(string = c("abababab","abc","ac"),pattern = "(ab)+")
str_extract_all(string = c("ababc","ac","cde"),pattern = "(ab)?c")
str_extract_all(string = c("abc","ac","cde"),pattern = "ab?c")

test_vector<-c("123","456","321")
library(stringr)
str_extract_all(test_vector,"3")
str_extract_all(test_vector,"^3")

# function 2
str_extract_all(test_vector,"[^3]")
str_extract_all(string = c("abababab","ababc","ababababc"),pattern = "(ab){2,3}")
library(plotly)
install.packages("plotly")
install.packages("quantmod")
head(presidents)
str(presidents)
presidents
pres_rating <- data.frame(rating = as.numeric(presidents), year = as.numeric(floor(time(presidents))), 
                          quarter = as.numeric(cycle(presidents)))
head(pres_rating)
require(graphics)

cycle(presidents)
# a simple series plot
plot(as.vector(time(presidents)), as.vector(presidents), type = "l")

help(cycle)
ggplot(score2all,aes(x=time))+geom_histogram()
head(faithful)
faithful
w <- faithful$waiting
ggplot(NULL, aes(x = w)) + geom_histogram()
ggplot(score2all, aes(x = time,y = score1type)) + geom_bar(stat = "identity")
library(gcookbook)  # For the data set
ggplot(pg_mean, aes(x = group, y = weight)) + geom_bar(stat = "identity")
ggplot(score2all, aes(x = time,y=score2all$score1type)) + geom_density(stat = "identity")
score2all$score1type
ggplot(score2all, aes(x = time,y=score1type)) + geom_freqpoly(stat = "identity",binwidth = 10)
p <- ggplot(faithful, aes(x = eruptions, y = waiting))

p + geom_point() + stat_density2d()
library(gcookbook)  # For the data set

# ecdf of heightIn
ggplot(heightweight, aes(x = heightIn)) + stat_ecdf()
heightweight<-heightweight

