n<-read.csv("males.csv", skip=1, header=TRUE)
n.2010<-n[n$Year==2010,]
minombre<-n[n$Name=="MATTHEW", ]
str(minombre)
str(n)
x<-minombre$Year
y<-minombre$Frequency
plot(x,y)
