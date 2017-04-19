today<-"20170403"

# 20170327a - upload new issues (agrichecks)
if(today=="20170327a"){

      curfile<-"agrichecks20170321-GC.csv"
      newobs<-preparefileforissueupload(newissuefile = curfile)
      #Attention: United Kingdom 
    
}
# 20170327b - follow up questions
if(today=="20170327b"){
    curfile<-"issue_comments20170314-GCalgc.csv"
    followup<-"follow.up.question.to.introduce"
    curfile<-paste0(issuedir,curfile)
    curfile<-read.csv(curfile,comment.char = "#",stringsAsFactors=FALSE)
    curfile<-curfile[c("issues",followup)]
    names(curfile)<-c("issuenr","communication")
    curfile<-curfile[curfile$communication!="",]
    
    remDr<-emrtgema()
    followed1<-Reduce(rbind,lapply(c(30:nrow(curfile)),function(x) unlist(followupissue(x,curfile[x,]))))
    #followed1<-Reduce(rbind,lapply(c(1:length(gris)),function(x) unlist(onlysendforapproval(gris[x]))))
    
}

# 20170327c - new issues
if(today=="20170327c"){
    curfile1<-"timeseries/checks20170317growthcheck-GC.csv"
    newobs1<-preparefileforissueupload(newissuefile = curfile1)
    
    #curfile2<-"nechecks/nenochecks20170317~20170321-GCal.csv"
    #newobs2<-preparefileforissueupload(newissuefile = curfile2)

    curfile3<-"countryoutliers/20170317colist_checked-GCal.csv"
    newobs3<-preparefileforissueupload(newissuefile = curfile3)
    
    curfile4<-"agrichecks20170321-GC.csv"
    newobs4<-preparefileforissueupload(newissuefile = curfile4)
    
    newobs<-rbind(newobs1,newobs3,newobs4)
    #addedobs1<-addnewissuecomplete(newobs = newobs,x1=1,x2=1,where=curyear)
    #addedobs2<-addnewissuecomplete(newobs = newobs,x1=2,x2=3,where=curyear)
    #addedobs3<-addnewissuecomplete(newobs = newobs,x1=4,x2=5,where=curyear)
    #addedobs4<-addnewissuecomplete(newobs = newobs,x1=6,x2=nrow(newobs),where=curyear)
    #addedobs5<-addnewissuecomplete(newobs = newobs,x1=21,x2=nrow(newobs),where=curyear)
    #addedobs6<-addnewissuecomplete(newobs = newobs,x1=25,x2=nrow(newobs),where=curyear)
    addedobs7<-addnewissuecomplete(newobs = newobs,x1=29,x2=nrow(newobs),where=curyear)
}

# 20170327d - new TERT issues
if(today=="20170327d"){
    curfile1<-"nechecks/nenochecks20170317~20170321-GCal.csv"
    newobs1<-preparefileforissueupload(newissuefile = curfile1)
    
    #newobs<-rbind(newobs1,newobs3,newobs4)
    #addedobs1<-addnewissuecomplete(newobs = newobs,x1=1,x2=1,where=curyear)
    #addedobs2<-addnewissuecomplete(newobs = newobs,x1=2,x2=3,where=curyear)
    #addedobs3<-addnewissuecomplete(newobs = newobs,x1=4,x2=5,where=curyear)
    #addedobs4<-addnewissuecomplete(newobs = newobs,x1=6,x2=nrow(newobs),where=curyear)
    #addedobs5<-addnewissuecomplete(newobs = newobs,x1=21,x2=nrow(newobs),where=curyear)
    #addedobs6<-addnewissuecomplete(newobs = newobs,x1=25,x2=nrow(newobs),where=curyear)
    #addedobs7<-addnewissuecomplete(newobs = newobs,x1=29,x2=nrow(newobs),where=curyear)
}

# Retrieve answers from countries
if(today=="20170403"){
    remDr<-emrt()
    issuesanswered<-listofselectedissues(criterion = "answered",remDr = remDr,filter = "workflow",revyear = curyear)
    issuesfinalised<-listofselectedissues(criterion = "finalised",remDr = remDr,filter = "workflow",revyear = curyear)
    discussion<-sapply(1:length(issuesanswered),function(x) getquestionanswers(remDr,issuesanswered[x]))
    responses<-as.data.frame(t(discussion))
    responses<-sapply(1:ncol(responses), function(x) unlist(responses[,names(responses)[x]]))
    responses<-as.data.frame(responses)
    names(responses)<-c("ncomments","attachments","datequestion","question","datelastansw","lastanswer","discussion")
    responses$discussion<-gsub("1 from   ; 2   from MS","",responses$discussion)
    #responses$datelastansw<-gsub(".* on ","",gsub(":.*","",responses$lastanswer))
    resphead<-c("issuenr","attachments","ncomments","datequestion","datelastansw","lastanswer","question","discussion")
    responses$issuenr<-issuesanswered
    responses<-responses[,resphead]
    
    curfile<-"issues_retrieved_20170405-GC.csv"
    curfile<-paste0(issuedir,curfile)
    curfile<-read.csv(curfile,comment.char = "#",stringsAsFactors=FALSE)
    curfile<-curfile[,c("issuenr","Type","comments","conclusion","attachments")]
    curresp<-merge(responses,curfile,by=c("issuenr","attachments"),all=TRUE)
    curresp<-curresp[,c("issuenr","Type","comments","conclusion","attachments","ncomments","datequestion","datelastansw","lastanswer","question","discussion")]
    curresp<-convert2char(curresp)
    curresp[is.na(curresp)]<-""
    write.csv(curresp,file=paste0(issuedir,"issues_retrieved_",curdate(),".csv"))
}
if(today=="20170405"){
    curfile<-"checks20170317growthcheck-GC.csv"
    curfile<-paste0(issuedir,curfile)
    curfile<-read.csv(curfile,comment.char = "#",stringsAsFactors=FALSE)
    
    curfile1<-"timeseries/checks20170317growthcheck-GC.csv"
    curfile1<-paste0(issuedir,curfile1)
    curfile1<-read.csv(curfile1,comment.char = "#",stringsAsFactors=FALSE)
    
    #curfile2<-"nechecks/nenochecks20170317~20170321-GCal.csv"
    #newobs2<-preparefileforissueupload(newissuefile = curfile2)
    
    curfile3<-"countryoutliers/20170317colist_checked-GCal.csv"
    curfile3<-paste0(issuedir,curfile3)
    curfile3<-read.csv(curfile3,comment.char = "#",stringsAsFactors=FALSE)
    
    curfile4<-"agrichecks20170321-GC.csv"
    curfile4<-paste0(issuedir,curfile4)
    curfile4<-read.csv(curfile4,comment.char = "#",stringsAsFactors=FALSE)

    curfile5<-"recalculation/Recalculations20170223.csv"
    curfile5<-paste0(issuedir,curfile5)
    curfile5<-read.csv(curfile5,comment.char = "#",stringsAsFactors=FALSE)
       
    curfile6<-"autocorrections/corrections20170317.csv"
    curfile6<-paste0(issuedir,curfile6)
    curfile6<-read.csv(curfile6,comment.char = "#",stringsAsFactors=FALSE)
    
    
    allhead<-Reduce(intersect,list(names(curfile1),names(curfile3),names(curfile4),names(curfile5)))
    curfiles<-rbind(curfile1[,allhead],curfile3[,allhead],curfile4[,allhead],curfile5[,allhead])
    curfiles<-curfiles[curfiles$issuenr!="",]
}
if(today=="20170418"){
    #Upload conclusions
    curfile<-"issues_retrieved_20170407al~20170418.csv"
    curfile<-paste0(issuedir,curfile)
    curfile<-read.csv(curfile,comment.char = "#",stringsAsFactors=FALSE)
    
    remDr<-emrt()
    allanswered<-listofselectedissues(remDr,curyear,filter="workflow",criterion = "answered")
    remDr<-emrtgema()
    acknowldeged<-sapply(1:length(allanswered),function(x) acknowledge(remDr,allanswered[x]))
    
    # 1. Resolved issues
    resissues<-curfile[curfile$conclusion=="resolved",]
    # #1-8 manually
    resissuesok<-sapply(9:9,function(x) resolveissue(x,line=resissues[x,],do="",step2=FALSE))
    resissuesok<-sapply(63:nrow(resissues),function(x) resolveissue(x,line=resissues[x,],do="",step2=FALSE))
    
    countriesvolunteer<-c("BG", "EE", "IS")
    countrieslate<-c("CY", "HR", "MT", "SE")
    countries2step2<-c(countriesvolunteer,countrieslate)
    
    # 2. not resolved issues
    notresissues<-curfile[curfile$conclusion!="resolved",]
    notresissues$party<-substr(notresissues$issuenr,1,2)
    
    
    notresnotstep2<-notresissues[!notresissues$party%in%countries2step2,]
    notresnotstep2att<-notresnotstep2[notresnotstep2$toattach!="",]
    notresnotstep2<-notresnotstep2[notresnotstep2$toattach=="",]
    notresissuesok<-sapply(2:2,              function(x) resolveissue(x,line=notresnotstep2[x,],do="",step2=FALSE))
    notresissuesok<-sapply(3:nrow(resissues),function(x) resolveissue(x,line=notresnotstep2[x,],do="",step2=FALSE))
    
    countries2step2<-countries2step2[!countries2step2=="IS"]
    notresstep2iceland<-notresissues[notresissues$party=="IS",]
    notresicelandok<-sapply(1:nrow(notresstep2iceland),function(x) resolveissue(x,line=notresstep2iceland[x,],do="",step2=FALSE))
    
    notres2step2<-notresissues[notresissues$party%in%countries2step2,]
    notres2step2ok<-sapply(2:2,function(x) resolveissue(x,line=notres2step2[x,],do="",step2=TRUE))
    notres2step2ok<-sapply(3:nrow(notres2step2),function(x) resolveissue(x,line=notres2step2[x,],do="",step2=TRUE))
    
    # Non-replied issues pushed back. Now answered
    remDr<-emrt()
    allanswered<-listofselectedissues(remDr,curyear,filter="workflow",criterion = "answered")
    allanswered<-as.data.frame(allanswered)
    names(allanswered)<-"issuenr"
    allanswered$comments<-"The MS Coordinator did not submit a reply"
    allanswered$conclusion<-"unresolved"
    allanswered$party<-substr(allanswered$issuenr,1,2)
    allanswered$step2<-TRUE
    allanswered$step2[!allanswered$party%in%countries2step2]<-FALSE
    allansweredok<-sapply(1:3,function(x) resolveissue(x,line=allanswered[x,],do="",step2=allanswered$step2[x]))
    allansweredok<-sapply(4:nrow(allanswered),function(x) resolveissue(x,line=allanswered[x,],do="",step2=allanswered$step2[x]))

    remDr<-emrt()
    forwardedissues<-listofselectedissues(remDr,curyear,filter="workflow",criterion = "sector")
    forwardedissues<-sort(forwardedissues)
    write.csv(forwardedissues,file="issues_agriculture_forwarded.csv")
    
    
}
    
