# Make first check of all issues are OK
#cannotbeaccessed<-checkaccessibility(issues)

#####################################
options(warn=0)
options(error=NULL)
source("eugirp_rselenium_functions.r")
user<-"gema"
user<-"adrian"
if(user=="adrian"){remDr<-emrt()}
if(user=="gema"){remDr<-emrtgema()}

# obtain list of issues: see listofselectedissues<-function(remDr,revyear,filter=NULL,criterion=NULL)
# write out issue details: see retrieveallissues<-function(revyear,filter=NULL,criterion)

# Add new observation
filename<-"countryoutliers/20160202colist_checked4emrt.csv"
filename<-"timeseries/checks20160202growthcheck.csv"
newobs<-read.csv(file=paste0(issuedir,filename),comment.char = "#")
newobs<-newobs[newobs$flag=="yes",]
newobs$question<-gsub("Please provide a justication/explanation for the observed irregularities.","",newobs$question)
addq<-unlist(lapply(c(1:nrow(newobs)),function(x) growthtype(newobs[x,])))
newobs$question<-paste0(newobs$question,addq," Please provide a justication/explanation for the observed irregularities.")
write.csv(newobs,file=paste0(issuedir,"timeseries/newobs.csv"))
x1<-1;x2<-3
x1<-27;x2<-nrow(newobs)
x1<-1;x2<-nrow(newobs)
newissues<-addnewissuecomplete(newobs,x1=x1,x2=x2,where="2016")

newobs<-checkrecalc[checkrecalc$years!="",]
newobs$invyear<-newobs$years
x1<-1;x2<-2
x1<-1;x2<-nrow(newobs)
x1<-26;x2<-nrow(newobs)
newissues<-addnewissuecomplete(newobs,x1=x1,x2=x2,where="2016")


newobs<-read.csv(file=paste0(issuedir,filename),comment.char = "#")
newissues<-read.csv(file=paste0(issuedir,"timeseries/checks20160202growthcheck_issues20160224-2336.csv"),comment.char = "#")
newobs<-newobs[,-which(grepl("\\.y$",names(newobs)))]
names(newobs)[which(grepl("\\.x$",names(newobs)))]<-years
newobs<-newobs[,-which(names(newobs)=="question")]
matchnames<-names(newissues)[names(newissues)%in%names(newobs)]
matchnames<-matchnames[-which(matchnames%in%c("X"))]
mergeissue<-merge(newobs,newissues,by=matchnames,all.x = TRUE)
sel<-!is.na(mergeissue$newissues)
mergeissue<-(mergeissue[,c("question","newissues","sent")])
write.csv(mergeissue,file=paste0(issuedir,"timeseries/checks20160202growthcheck_issues.csv"))

filename<-"agrichecks_gc.csv"
filename<-"lulucf/checklulucf4emrt.csv"
newobs<-read.csv(file=paste0(issuedir,filename),comment.char = "#")
newobs$invyear<-unlist(lapply(c(1:nrow(newobs)),function(x) convertyear(newobs$invyear[x])))
x1<-26;x2<-nrow(newobs)
x1<-1;x2<-2
x1<-2;x2<-nrow(newobs)
newissues<-addnewissuecomplete(newobs,x1=x1,x2=x2,where="2016")


#Possible functions:
#    check-out wrong issuenumbers
nwrong<-checkwrongissues()

# Check responses from MS (opens csv-files as well)
remDr<-emrt()
responses<-checkmsresponse(remDr)

# Check individual issue
remDr<-emrt()
ldetails<-c("obs","date","yr","par","ms","eu","flag","q","lfrom","ldate","ltext")
curissue<-"IS-3B-2015-0008"
curissue<-"BE-3H-2015-0001"
details<-as.data.frame(t(unlist(obsdetails(remDr,curissue))))
names(details)<-ldetails


# Close issue as 'resolved'
remDr<-emrt()
x<-20
line = i2resolve[x,]
r<-unlist(lapply(c(21:21),function(x) resolveissue(remDr,x,line = i2resolve[x,],do="resolve")))
r<-unlist(lapply(c(56:nrow(i2resolve)),function(x) resolveissue(remDr,x,line = i2resolve[x,],do="resolve")))

# Close issue as 'partly resolved'
remDr<-emrt()
x<-1
line = i2partly[x,]
r<-unlist(lapply(c(3:3),function(x) resolveissue(remDr,x,line = i2partly[x,],do="partly")))
r<-unlist(lapply(c(5:nrow(i2partly)),function(x) resolveissue(remDr,x,line = i2partly[x,],do="partly")))

# Close issue as 'explained' but where there explanation could not yet be cross-checked
remDr<-emrt()
curis<-issuelist
x<-1
line = curis[x,]
r<-unlist(lapply(c(2:4),function(x) resolveissue(remDr,x,line = curis[x,],do="")))
r<-unlist(lapply(c(1:nrow(curis)),function(x) resolveissue(remDr,x,line = curis[x,],do="")))

line[,linedetails]<-c("RO-3A-2015-0003","Explained","",0,0,1,0,0,0,0,0)
resolveissue(remDr,1,line,do="")
