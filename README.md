607week3quiz
============

third week quiz
#(1)
meanfunc<- function(l){
  
  me<- 0
  for (i in 1:length(l)){
    me<- (me+l[i])
  }
  return(me/length(l))
}

l<- c(1,2,3)
meanfunc(l)
#(2)
meanfunc2<- function(l){
  l<- na.omit(l)
  me<- 0
  for (i in 1:length(l)){
    me<- (me+l[i])
  }
  return(me/length(l))
}

l<- c(NA,1,2,3)
meanfunc2(l)

#(17)
seq(as.Date("2005-1-1"),as.Date("2014-12-31"),by="day")

#(4)
gcd<-  function(a,b) ifelse(b==0,a,gcd(b,a%%b))

#
sticker<-function(x,y){
   z<-paste(x,y,sep=" ")
   return(z) 
   }
x<-c("Data","Big","Data")
y<-c("Science","Data","Analytics")
sticker(x,y)

#(16)
date0<- as.Date("2005-12-31")
month0<- as.numeric(format(date1,"%m"))
month0
#(5)
det<- function(x,y){
  z= (x^2)*y+(2*x*y)-(x*(y^2))
  return(z)
}
det(2,2)

#(11)
ncharfunc<- function(v){
   z<- nchar(v)
   return(z)
   }
v<- c("bigData","dataAnlytics","dataScience","areAllTheSame")
ncharfunc(v)

#(15)
D<-"04-11-1976"
dateOfbirth<- as.Date(D,"%m-%d-%Y")
dateOfbirth
#(7)
week.3.price.data <- read.csv("~/GitHub/week-3-price-data.csv")
View(week.3.price.data)
week.3.make.model.data <- read.csv("~/GitHub/week-3-make-model-data.csv")
View(week.3.make.model.data)
total<-merge(week.3.price.data,week.3.make.model.data,by="ModelNumber")
total# number of observations is 27,No its not what I expected
#(8)
newdata<-merge(week.3.price.data,week.3.make.model.data,all.week.3.price.data=TRUE)
newdata
newdata2<- subset(newdata,Year==2010)
newdata2

#(9)
newdata3<-subset(newdata,Color=="Red"&Price>10000)
newdata3
#(10)
newdata3$Color<-NULL
newdata3$ModelNumber<-NULL
newdata3


#(14)

m1<- 1:12
d1<-  19:30
y1<- 2001:2012
datedataframe<-data.frame(m1,d1,y1)
datedataframe
View(datedataframe)
datestr<- paste(m1,d1,y1,sep="-")
datestr
newdataframe<-data.frame(m1,d1,y1,datestr)
View(newdataframe)
#(3)

Funcgcd <- function(a, b)
{
  a<-as.integer(a)
  b<-as.integer(b)
  stopifnot(!(a==0 && b==0))
  if ( a == 0 )
    return(b)
  
  if ( b == 0 )
    return(a)
  
  return(abs(Funcgcd(b, a-b*floor(a/b))))
}
Funcgcd(15,45)


