oldissues<-read.csv(file="issues/allissues.csv")
oldissues<-as.data.frame(as.matrix(oldissues),stringsAsFactors = FALSE)
oldissues$Observation<-gsub(",","-",oldissues$Observation)
oldissues$Observation<-gsub(";","-",oldissues$Observation)
oldissues$question<-gsub(",","-",oldissues$question)
oldissues$question<-gsub(";","-",oldissues$question)
partest<-paramcheck


oldissuesexist<-function(line,x){
    #print(line)
    line<-as.matrix(line)
    line<-as.data.frame(line,stringsAsFactors = FALSE)
    c<-as.character(line$correction)
    p<-as.character(line$party)
    m<-as.character(line$meastype)
    s<-line$sector_number
    showf<-c(measfields)
    showf<-c("party",sectfields,measfields,"value","significant")
    showo<-c("File.Name.",sectfields,"check","next.","Phase.1","Phase.2","correction","Observation","question")
    nshow<-length(showf)+length(showo)
    #print(line)
    #print(nshow)
    select<-oldissues[,"X.1"]==p & oldissues$meastype==m & as.vector(vapply(oldissues$sector_number,function(x) grepl(x,s),0))
    #print(sum(select))
    if((c=="" | c=="0") & sum(select)>0){
        linen<-matrix(rep(c(x,line[,showf]),sum(select)),ncol=length(showf)+1,nrow=sum(select),byrow=TRUE)
        #print(line[,showf])
        #print(oldissues[select,showo])
        n<-(cbind(linen,oldissues[select,showo]))
        #n<-cbind(x,line[,showf],oldissues[select,showo])
    }else{
        n<-rep(NA,nshow+1)
    }
    
    #print(n)
    return(n)
    
}

showf<-c(measfields)
showf<-c("party",sectfields,measfields,"value","significant")
showo<-c("File.Name.",sectfields,"check","next.","Phase.1","Phase.2","correction","Observation","question")

xfr<-1
xto<-110
xto<-nrow(paramcheck)
noldissues<-Reduce(rbind,lapply(c(xfr:xto),function(x) Reduce(cbind,(oldissuesexist(partest[x,],x)))))
noldissues<-as.data.frame(as.matrix(noldissues))
names(noldissues)<-c("x",showf,paste0(showo,"2015"))
noldissues<-noldissues[!is.na(noldissues[,"sector_number"]),]
noldissues<-apply(noldissues,2,unlist)
View(noldissues)
write.csv(noldissues,file=paste0(invloc,"/checks/countryoutliers/link2oldissues.csv"))
