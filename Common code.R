library(igraph)
library(Matrix)
# convert initial dataset to the array list whose i-th element contains the list of items that are rated by i-th user
makeuserSequence<-function(data){
  tseq=array(list(),2*max(data[,1]));
  print(length(tseq))
  iseq=array(list(),2*max(data[,1]));
  print(length(iseq))
  for(i in (1:length(iseq))){
    tseq[[i]]={};
    iseq[[i]]={};  
  }
  print(c("t",length(iseq)))
  for(i in 1:nrow(data)){
    uid=data[i,1];
    tseq[[uid]]=c(tseq[[uid]],data[i,4]);
    iseq[[uid]]=c(iseq[[uid]],data[i,2]);
  }
  for(i in 1:length(tseq)){
    t=tseq[[i]];
    u=iseq[[i]];
    for(j in 1:length(t)){
      if(length(t)>1){
        l=order(t,decreasing=F);
        tseq[[i]]=t[l];
        iseq[[i]]=u[l];
      }
    }    
  }
  itemseq=setClass("userSeq",slots=c("time","items"));
  it=new("userSeq",time=tseq,items=iseq);
  return(it@items);
}
# a function to creat  edge list 
tList<-function(i,s1,s2,rmat){
  m=matrix(0,length(s1)*length(s2),2) ;
  count=1; 
  for(j in 1:length(s1)){
    for(k in 1:length(s2)){
      m[count,]=c(i,hash(s2[k],s1[j],nrow(rmat),ncol(rmat)));
      count=count+1;
    }
  }
  return(m)
}

hash<-function(i1,i2,nu,ni){
  #i1 is larger and i2 is smaller
  h=nu+((i1-1)*ni+i2);
  return(h);
}
calUserLength<-function(userSeq){
  r=rep(0,length(userSeq))
  for(i in 1:length(r))
    r[i]=length(userSeq[[i]]);
  return(r);  
}
calDCG2<-function(rr){
  s=(1:length(rr))+1;
  x=2^rr;
  m=sum((x-1)/log(s));
  return(m)
}

writeTrain<-function(tr,trainList, direc){
  
  v=matrix(unlist(tr),length(tr),length(tr[[1]]),byrow = T)
  write.csv(v,direc);
  
}

makeRatmat<-function(data){
  rmat=sparseMatrix(i=data[,1],j=data[,2],x=data[,3],use.last.ij = T)
  return(rmat);
}
calNDCG<-function(r,score,k){
  k=min(k,length(r))
  recomList= order(r,decreasing=T)[1:k];
  rankScore=score[recomList];
  m=calDCG2(rankScore);
  realScore=sort(score,decreasing=T)[1:k];
  n=calDCG2(realScore);
  if(is.na(m/n)){
    x="****"
  }
  #print("n")
  return(m/n);
}
trList_random<-function(userSequence,j,testSet){
  trl<-array(list(),length(testSet))
  for(i in 1:length(trl)){
    #   print(testSet[[i]])
    m=userSequence[[testSet[i]]];
    l=sample(1:length(m),j)
    trl[[i]]=l
    #trl[[i]]=m[1:min(j,length(m))]
  }
  return(trl)
}
#SiBRank


makeTrainMat<-function(tr,rmat){
  #trm=rmat
  trm=sparseMatrix(i=1,j=1,x=0,dims=c(nrow(rmat),ncol(rmat)));
  for(i in 1:nrow(rmat)){
    #if(i%%1000==0)
    print(i)
    trm[i,tr[[i]]]=rmat[i,tr[[i]]];
  }
  return(trm)  
}

Yu_prepareData<-function(rmat,thr,userSeq){
  l=calUserLength(userSeq)
  u=which(l<thr)
  rmat2=rmat;
  if(length(u)>0){
    rmat2=rmat[-u,];  
  }    
  us=array(list(),nrow(rmat2));
  #items
  p=1:ncol(rmat);
  s=p;
  # new Indices of items 
  j=1;
  m=1:nrow(rmat)
  if(length(u)>0)
    m=m[-u];
  #   e=sample(1:length(m),floor(0.2*length(m)))
  e=1:length(m);
  m=m[e]
  p=1:ncol(rmat)
  # p=sample(p,floor(1*length(p)));
  
  #seprate userSequence
  for(i in 1:length(m)){
    us[[i]]= intersect(userSeq[[m[i]]],p);
    for(j in 1:length(us[[i]]))
      us[[i]][j]=which(p==us[[i]][j]);
  }    
  d=setClass("d",slots=c("us","m","rmat2"));
  t=new ("d",us=us,m=m,rmat2=rmat2[e,p])
  return(t);  
}

splitRating<-function(rmat, uid, r) {
  mlist = sort(unique(rmat[uid, r]))
  
  s = array(list(), length(mlist))
  
  for (i in 1:length(s)) {
    s[[i]] = which(rmat[uid, ] == mlist[i])
    
    s[[i]] = intersect(s[[i]], r)
    
  }
  return(s)
  
}


trList_random<-function(userSequence,j,testSet){
  trl<-array(list(),length(testSet))
  for(i in 1:length(trl)){
    #   print(testSet[[i]])
    m=userSequence[[testSet[i]]];
    #l=sample(1:length(m),j)
    #trl[[i]]=l
    trl[[i]]=m[1:min(j,length(m))]
  }
  return(trl)
}