#Ejercicio1

miDNI<-c(4,5,3,8,5,7,8,0)
miDNI

##Ejercicio2.1

dnisort<-sort(miDNI)
count<-1

for(i in 1:(length(miDNI)-1)){
  if(dnisort[i]<dnisort[i+1]) 
  {count<-count+1}
}
count

##Ejercicio2.2

sort(miDNI, decreasing= FALSE)
unique(miDNI)
count2<-length(unique(miDNI))
count2

##Ejercicio3

x<-seq(0.5, 4, 0.1)
y<- log10(x)
z<- exp(x)

print(x)
print(y)

df<- data.frame(x,y,z)
df

exp(x)

##Ejercicio4

#?abline
plot(x, y, col="red",type="l")

plot(x,z, col="red",type="l")