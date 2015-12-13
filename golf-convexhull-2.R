d=read.csv(file("stdin"),sep=";",header=F)    
q=function(i,j,k) abs(det(as.matrix(cbind(d[c(i,j,k),],1))))
t=function(i,j,k) q(i,j,k)-q(1,i,j)-q(1,i,k)-q(1,j,k)==0
any(apply(combn(2:dim(d)[1],3),2,function(v) t(v[1],v[2],v[3])))
