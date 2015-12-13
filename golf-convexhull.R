d=read.csv(file("stdin"),sep=";",header=F)    
p=function(a,b) a[1]*b[1]+a[2]*b[2]
t=function(a,b,c) {
A=d[a,]
U=d[1,]-A
B=d[b,]-A
C=d[c,]-A
f=p(C,C)
g=p(B,C)
h=p(U,C)
i=p(B,B)
j=p(U,B)
k=f*i-g*g
u=i*h-g*j
v=f*j-g*h
min(u*k,v*k,k-u-v)>0}
any(apply(combn(2:dim(d)[1],3),2,function(v) t(v[1],v[2],v[3])))