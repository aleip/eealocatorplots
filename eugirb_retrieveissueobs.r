# install.packages("RSelenium")
# RSelenium::checkForServer()
#vignette('RSelenium-basics')
options(warnings=0)
RSelenium::startServer()
require(RSelenium)

loginemrt<-function(remDr,issue=""){
    
    urlemrt <- paste0("https://emrt.eea.europa.eu/")
    login<-"login"
    usr<-"leipvadr"
    psw<-"0112rkf4"
    
    remDr$navigate(paste0(urlemrt,login))
    webelem<-remDr$findElement('id', '__ac_name')
    webelem$clearElement()
    webelem$sendKeysToElement(list(usr))
    webelem<-remDr$findElement('id', '__ac_password')
    webelem$clearElement()
    webelem$sendKeysToElement(list(psw))
    webelem$submitElement()
    remDr$navigate(paste0(urlemrt,issue))
    
}
emrt<-function(){
    # for chrome see https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-saucelabs.html#id1a
    remDr<-remoteDriver(browserName = "firefox",remoteServerAddr = "localhost", port = 4444)
    remDr$open()
    loginemrt(remDr)
    return(remDr)
}
addnewissue<-function(where,observation,party,sector,years,gas,mskey,eukey,para){
    # Add new issue ####
    newissue<- "++add++Observation"
    
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-0') #Not estimated (NE) - initial checks
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-1') #Gap filling - initial checks
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-2') #Potential significant issues (step 1)
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-3') #Technical correction (step 2)
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-4') #Recalculation (compared to previous year submission)
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-5') #Recalculation (compared to same year submission)
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-6') #UNFCCC Recommendation
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-7') #Union Recommendation
    
    remDr$navigate(paste0(urlemrttest,newissue))
    webelem<-remDr$findElement('id', 'form-widgets-text')
    webelem$clearElement()
    webelem$sendKeysToElement(list(observation))
    
    webelem<-remDr$findElement('id', 'form-widgets-country')
    webelem$sendKeysToElement(list(party))
    
    webelem<-remDr$findElement('id', 'form-widgets-crf_code')
    webelem$sendKeysToElement(list(sector))
    
    webelem<-remDr$findElement('id', 'form-widgets-year')
    webelem$clearElement()
    webelem$sendKeysToElement(list(years))
    
    webelem<-remDr$findElement('id', 'form-widgets-gas-0') #CH4
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    if(gas=="CH4" | gas=="Aggregate GHG")webelem$clickElement()
    webelem<-remDr$findElement('id', 'form-widgets-gas-1') #CO2
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    if(gas=="CO2" | gas=="Aggregate GHG")webelem$clickElement()
    webelem<-remDr$findElement('id', 'form-widgets-gas-2') #HFCs
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    webelem<-remDr$findElement('id', 'form-widgets-gas-3') #N2O
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    if(gas=="N2O" | gas=="Aggregate GHG")webelem$clickElement()
    webelem<-remDr$findElement('id', 'form-widgets-gas-4') #NF3
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    webelem<-remDr$findElement('id', 'form-widgets-gas-5') #PFCs
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    webelem<-remDr$findElement('id', 'form-widgets-gas-6') #SF6
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    
    webelem<-remDr$findElement('id', 'form-widgets-review_year')
    webelem$clearElement()
    webelem$sendKeysToElement(list(curyear))
    
    webelem<-remDr$findElement('id', 'form-widgets-ms_key_catagory-0') #CH4
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    if(mskey)webelem$clickElement()
    webelem<-remDr$findElement('id', 'form-widgets-eu_key_catagory-0') #CH4
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    if(eukey)webelem$clickElement()
    
    webelem<-remDr$findElement('id', 'form-widgets-parameter-0') #AD
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    webelem<-remDr$findElement('id', 'form-widgets-parameter-1') #EM
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    webelem<-remDr$findElement('id', 'form-widgets-parameter-2') #IEF
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    webelem<-remDr$findElement('id', 'form-widgets-parameter-3') #other
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    
    if(para=="AD"){
        webelem<-remDr$findElement('id', 'form-widgets-parameter-0')
    }else if(para=="EM"){
        webelem<-remDr$findElement('id', 'form-widgets-parameter-1') #EM
    }else if(para=="IEF"){
        webelem<-remDr$findElement('id', 'form-widgets-parameter-2') #IEF
    }else{
        webelem<-remDr$findElement('id', 'form-widgets-parameter-3') #other
    }
    webelem$clickElement()
    for (i in c(0:7)){
        webelem<-remDr$findElement('id', paste0('form-widgets-highlight-',i))
        if(unlist(webelem$isElementSelected())) webelem$clickElement()
    }
    for(i in highlight){
        webelem<-remDr$findElement('id', paste0('form-widgets-highlight-',i))
        webelem$clickElement()
        
    }
    webelem<-remDr$findElement('id', 'form-buttons-save')
    #webelem<-remDr$findElement('id', 'form-buttons-cancel')
    webelem$clickElement()
    newissue<-webelem$getCurrentUrl()
    return(newissue)
}
openissue<-function(remDr,issue,phase=""){
    year<-unlist(strsplit(as.character(issue),"-"))[3]
    if(is.na(year))year<-""
    path<-paste0(urlemrt,year,"/",issue,phase)
    cat(path)
    curissue<-remDr$navigate(path)
    return(curissue)
}

resolveissue<-function(remDr,x,line,do=""){
    issue <- line$issuenr
    note <- line$note
    expl <- line$next.
    if(do=="") do   <- line$next.
    issueflags = line[,flagnames]
    cat(x,": ",issue)
    cat(issue,"-",note,"-",do,"-",expl,"-")
    
    curissue<-openissue(remDr,issue)
    
    team2<-FALSE
    if(do=="Follow up" & line$sig==1){team2<-TRUE}
    if(do=="Not resolved"){team2<-TRUE}
    if(note=="No response"){expl<-note;note<-""}
    
    # Add Conclusion
    clickbutton<-"Add Conclusions"
    webelem<-remDr$findElements('class', 'defaultWFButton')
    
    if(length(webelem)==0){
        cat("No default button available!\n")
        return("to be checked")
    }
    cat("\n")
    
    text<-webelem[[1]]$getElementText()
    if(text==clickbutton) webelem[[1]]$clickElement()
    
    # Select reason: issue is resolved
    webelem<-remDr$findElement('id','form-widgets-closing_reason')
    
    if(do=="Resolved") {webelem$sendKeysToElement(list("resolved: issue dropped or clarified"))}
    if(do=="partly" | do=="Explained"){
        webelem$sendKeysToElement(list("partly resolved: maybe recommendation, but not forwarded to 2nd step review"))
    }
    if((do=="Follow up" & !team2)){
        webelem$sendKeysToElement(list("unresolved: maybe recommendation, but not forwarded to 2nd step review"))
    }
    if(team2){
        webelem$sendKeysToElement(list("significant issue: hand over to 2nd step"))
    }

    webelem<-remDr$findElement('id','form-widgets-text')
    
    if(do=="Resolved" | do=="Follow up" | do=="Not resolved") {webelem$sendKeysToElement(list(expl))}
    if(do=="partly"){webelem$sendKeysToElement(list(note))}
    if(do=="Explained"){webelem$sendKeysToElement(list("Explanation provided."))}
    
    
    # Select highlights
    for (i in c(0:7)){
        webelem<-remDr$findElement('id', paste0('form-widgets-highlight-',i))
        if(unlist(webelem$isElementSelected())) webelem$clickElement()
    }
    for(i in which(issueflags==1)){
        if(issueflags[i]==1){
                webelem<-remDr$findElement('id', paste0('form-widgets-highlight-',i-1))
                webelem$clickElement()
        }
    }
    
    # Save
    webelem<-remDr$findElement('id','form-buttons-save')
    webelem$clickElement()
    
    clickbutton<-"Request finalisation of the observation"
    webelem<-remDr$findElement('class', 'defaultWFButton')
    text<-webelem$getElementText()
    if(text==clickbutton) webelem$clickElement()
    
    webelem<-remDr$findElement('id','form-widgets-comments')
    if(do=="Explained" | do=="Follow up"){webelem$sendKeysToElement(list(note))}
    
    clickbutton<-"Request finalisation of the observation"
    webelem<-remDr$findElement('class', 'defaultWFButton')
    webelem$clickElement()
    
    clickbutton<-"Confirm finishing observation"
    clickbuttonnr<-1
    if(team2){
        clickbutton<-"Hand over to Team 2"
        clickbuttonnr<-2
    }
    webelem<-remDr$findElements('class', 'defaultWFButton')[[clickbuttonnr]]
    text<-webelem$getElementText()
    if(text==clickbutton) webelem$clickElement()
    
    return("resolved")
    
}
concludeissue<-function(remDr,issue,comment){
    curissue<-openissue(remDr,issue)
    
    # Request finalisation of the observation
    webelem<-remDr$findElement('class', 'defaultWFButton')
    text<-webelem$getElementText()
    if(text=="Request finalisation of the observation") webelem$clickElement()
    webelem<-remDr$findElement('id', 'form-widgets-comments')
    webelem$sendKeysToElement(list(comment))
    webelem<-remDr$findElement('class', 'submit-widget')
    webelem$clickElement()    
    
}
acknowledge<-function(remDr,issue){
    
    # uses openissue

    qaphase<-"#tab-qa"
    conclusionphase1<-"#tab-conclusion-phase-1"
    curissue<-openissue(remDr,issue)
    webelem<-remDr$findElement('class', 'eea-tabs-panel')
    #qapart<-webelem$findChildElements("class name","question")
    buttons<-webelem$findChildElements("class name","standardButton")
    buttontext<-unlist(lapply(buttons,function(x) x$getElementText()))
    if(length(buttontext)==1){
        mybutton<-buttons[[1]]
        if(buttontext[1]=="Acknowledge Answer") {mybutton$clickElement()}
    }
    webelem<-remDr$findElement('class', 'eea-tabs-panel')
    childs2<-webelem$findChildElements("class name","question")
    print(length(childs2))
    if(length(childs2)>0){
        discussion<-unlist(lapply(childs2,function(x) x$getElementText()))
        question<-discussion[1]
        last<-discussion[length(discussion)]
        last<-unlist(strsplit(last,"\n"))
        if(grepl("Sector Expert",last[1])){lfrom<-"EU"}else{lfrom<-"MS"}
        ldate<-gsub("Sent on: ","",unlist(strsplit(last[2],","))[1])
        ldate<-issuedate(ldate)
        ltext<-paste(last[3:length(last)],collapse=" ")
    }else{   
        childs2<-webelem$findChildElements("id","conclusions")
        conclusion<-webelem$getElementText()
    }
    return(list(c(lfrom,ldate,ltext)))
}
checkissues<-function(remDr,issue){
    year<-unlist(strsplit(as.character(issue),"-"))[3]
    if(is.na(year))year<-""
    path<-paste0(urlemrt,year,"/",issue)
    cat(path)
    cururl<-""
    attempt<-1
    success<-0
    while(attempt<5 & success==0){
        cat(" - ",attempt)
        curissue<-remDr$navigate(path)
        cururl<-gsub(paste0(urlemrt,year,"/"),"",remDr$getCurrentUrl())
        if(cururl!=issue & cururl!=paste0(issue,"#tab-qa")){
            attempt<-attempt+1
        }else{
            attempt<-5
            success<-1
            print(" success!")
        }
    }
    return(success)
}
obsdetails<-function(remDr,issue){
    curissue<-openissue(remDr,issue)
    webelem<-remDr$findElement('class', 'collapsiblePanelTitle')
    webelem$clickElement()
    webelem<-remDr$findElement('id', 'content-core')
    #observationdetails<-webelem$findChildElements("class name","observation-details")
    #subyear<-unlist(lapply(observationdetails,function(x) x$getElementText()))[4]
    childs<-webelem$findChildElements("class name","position-0")
    for (i in c(1:length(childs))){
        if(grepl("Review Year",unlist(childs[[i]]$getElementText()))) yr<-unlist(childs[[i+1]]$getElementText())
        if(grepl("Key flags",unlist(childs[[i]]$getElementText()))) flags<-unlist(childs[[i+1]]$getElementText())
        if(grepl("Description",unlist(childs[[i]]$getElementText()))) obs<-unlist(childs[[i+1]]$getElementText())
    }
    
    childs<-webelem$findChildElements("class name","position-2")
    par<-unlist(lapply(childs,function(x) x$getElementText()))[2]
    childs<-webelem$findChildElements("class name","position-7")
    key<-unlist(lapply(childs,function(x) x$getElementText()))[2]
    if(grepl("MS",key)){ms<-1}else{ms<-0}
    if(grepl("EU",key)){eu<-1}else{eu<-0}
    childs<-webelem$findChildElements("class name","position-11")
    date<-unlist(lapply(childs,function(x) x$getElementText()))[4]
    date<-unlist(strsplit(date,","))[1]
    date<-issuedate(date)
    
    flagnames<-c("ne","init","sig","corr","recalc1","recalc2","unfccc","union")
    flagelems<-c("NE","gap","significant","correction","previous","same","UNFCCC","Union")
    flag<-vector(length = length(flagnames))
    for (i in c(1:length(flagnames))) {if(grepl(flagelems[i],flags)){flag[i]<-1}else{flag[i]<-0}}
 
    # question
    
    # Conclusion Phase 1
    webelem<-remDr$findElement('class', 'eea-tabs-panels')
    childs1<-webelem$findChildElements("class name","conclusions")
    if(length(childs1)>0){
        reason<-webelem$findChildElements("id","conclusions")
        reason<-unlist(lapply(reason,function(x) x$getElementText()))
        reason<-gsub("\n",". ",reason)
    }
    
    # Questions and answers
    webelem<-remDr$findElement('class', 'eea-tabs-panel')
    childs2<-webelem$findChildElements("class name","question")
    #lapply(childs,function(x) x$elementId)
    #lapply(childs,function(x) x$getElementAttribute('id'))
    if(length(childs2)>0){
        reason<-NA
        discussion<-unlist(lapply(childs2,function(x) x$getElementText()))
        question<-discussion[1]
        question<-unlist(strsplit(question,"\n"))
        q<-paste0(question[3],". ")
        if(is.na(q))stop()
        
        if(strsplit(question,"\n")[5]!=""){q<-paste0(q,"Files: ",strsplit(question,"\n")[5],". ")}
        q<-gsub(",",". ",q)
        print(length(discussion))
        if(length(discussion)>1){
            last<-discussion[length(discussion)]
            last<-unlist(strsplit(last,"\n"))
            if(grepl("Sector Expert",last[1])){lfrom<-"EU"}else{lfrom<-"MS"}
            ldate<-gsub("Sent on: ","",unlist(strsplit(last[2],","))[1])
            ldate<-issuedate(ldate)
            ltext<-paste(last[3:length(last)],collapse=" ")
        }
    }else{
        q<-NA
        lfrom<-NA
        ldate<-NA
        ltext<-NA
    }
    
    return(list(c(reason,obs,date,yr,par,ms,eu,flag,q,lfrom,ldate,ltext)))
}
issuedate<-function(dstring){
    vals<-unlist(strsplit(dstring," "))
    day<-vals[1]
    year<-vals[3]
    if(vals[2]=="Jan") month<-1
    if(vals[2]=="Feb") month<-2
    if(vals[2]=="Mar") month<-3
    if(vals[2]=="Apr") month<-4
    if(vals[2]=="May") month<-5
    if(vals[2]=="Jun") month<-6
    if(vals[2]=="Jul") month<-7
    if(vals[2]=="Aug") month<-8
    if(vals[2]=="Sep") month<-9
    if(vals[2]=="Oct") month<-10
    if(vals[2]=="Nov") month<-11
    if(vals[2]=="Dec") month<-12
    curdate<-paste(year,month,day,sep="-")
    return(curdate)
}
exceldate<-function(string){
    lstring<-unlist(strsplit(string,"/"))
    month<-lstring[1]
    day<-lstring[2]
    year<-lstring[3]
    curdate<-paste(year,month,day,sep="-")
    return(curdate)
}
openlists<-function(checks){
    for(i in c(1:length(checks))){
        check<-checks[i]
        issuelist<-read.csv(paste0(invloc,"/checks/","allchecks_",check,".csv"),header=TRUE,comment.char = "#",stringsAsFactors=FALSE)
        issuelist[is.na(issuelist)]<-""
        issuelist<-issuelist[!issuelist$check=="",]
        issuelist$curcheck<-check
        issuenames<-names(issuelist)
        nissues[i]<-nrow(issuelist)
        if(i==1) {i2retrieve<-issuelist}else{i2retrieve<-rbind(i2retrieve,issuelist)}
    }
    return(i2retrieve)
}
writelists<-function(checks,issuelist){
    
    # uses: exceldate
    
    figdate<-format(Sys.time(), "%Y%m%d")
    for(i in c(1:length(checks))){
        check<-checks[i]
        print(check)
        curcheck<-issuelist[issuelist$curcheck==check,]
        curcheck<-curcheck[,-which(issuenames=="curcheck")]
        sel<-grepl("/",curcheck$ldate)
        curcheck$ldate[sel]<-unlist(lapply(c(1:sum(sel)),function(x) exceldate(curcheck$ldate[x])))
        curcheck<-curcheck[order(as.Date(curcheck$ldate),curcheck$party,curcheck$sector_number,decreasing = TRUE),]
        write.csv(curcheck,file=paste0(invloc,"/checks/","allchecks_",check,"_update~",figdate,".csv"))
    }
    return(1)
}
checkmsresponse<-function(remDr){
    
    # uses: openlists
    # uses: acknowledge (openissue)
    # uses: writelists (exceldate)
    
    # Check if there are updates from MS ####
    updatefields<-c("last","ldate","ltext")
    i2retrieve<-openlists(checks)
    #Check those where answer from MS is expected
    sel2check<-i2retrieve$last=="EU"
    retrieve<-unique(i2retrieve$issuenr[sel2check])
    update<-as.data.frame(Reduce(rbind,lapply(c(1:length(retrieve)),function(x) unlist(acknowledge(remDr,retrieve[x])))))
    updated<-cbind(retrieve,update)
    names(updated)<-c("issuenr",updatefields)
    select<-i2retrieve$issuenr%in%retrieve
    i<-merge(i2retrieve[select,-which(names(i2retrieve)%in%updatefields)],updated,by="issuenr")
    i<-i[,issuenames]
    i3<-rbind(i2retrieve[!select,issuenames],i[,issuenames])
    written<-writelists(checks,i3)
    return(i3)
}
checkaccessibility<-function(issues){
    remDr<-emrt()
    tmp<-unlist(lapply(c(1:nrow(issues)),function(x) checkissues(remDr,issues$issuenr[x])))
    failures<-length(tmp)-sum(tmp)
    if(failures>0){View(failures);stop("Some issues could not be accessed!")}
}

checkallissues<-function(remDr,i2retrieve){
    selection<-nchar(i2retrieve$issuenr)-nchar(gsub("-","",i2retrieve$issuenr))==3
    issuelist<-i2retrieve[selection,]

    # Remove fields that will be retrieved from data base
    issuelist<-issuelist[,names(issuelist)[!issuenames%in%obsnames]]
    
    # List of all issues to be checked
    issues<-data.frame(unique(issuelist$issuenr))
    names(issues)<-"issuenr"
    issues[,obsnames]<-""
    
    tmp<-lapply(c(1:nrow(issues)),function(x) Reduce(cbind,obsdetails(remDr,issues$issuenr[x])))
    issues[,obsnames]<-Reduce(rbind,tmp)
    
    tmpnames<-c(names(i2retrieve)[!issuenames%in%obsnames])
    issuelist<-merge(i2retrieve[,tmpnames],issues,by="issuenr")
    issuelist<-issuelist[,issuenames]
    selection<-nchar(i2retrieve$issuenr)-nchar(gsub("-","",i2retrieve$issuenr))!=3
    issuelist<-rbind(issuelist,i2retrieve[selection,])
    issuelist$id[issuelist$id==""]<-0
    return(issuelist)
    
}


logcurstatus<-function(i2retrieve){
    
    selection<-nchar(i2retrieve$issuenr)-nchar(gsub("-","",i2retrieve$issuenr))==3
    issuelist<-unique(i2retrieve[selection,names(i2retrieve)%in%c("check",resstatus,"issuenr","last")])
    nissues<-nrow(issuelist)
    
    selection<-issuelist$last=="EU"
    noresponse<-nrow(issuelist[selection,])
    
    issuelist<-issuelist[!selection,]

    
    issueres<-as.data.frame(unique(issuelist$res))
    names(issueres)<-"res"
    issueres$count<-unlist(lapply(c(1:nrow(issueres)),function(x) sum(issuelist$res==issueres$res[x])))

    issueres$explanation[issueres$res==0]<-"not resolved"
    issueres$explanation[issueres$res==6]<-"claimed res  - check CRF"
    issueres$explanation[issueres$res==7]<-"claimed resolved or justification provided - check response"
    issueres$explanation[issueres$res==8]<-"not resolved - asked support"
    issueres$explanation[issueres$res==9]<-"not resolved but will be in next years submission"
    issueres$explanation[issueres$res==10]<-"disagreement on issue"
    issueres$explanation[issueres$res==1]<-"resolved"
    issueres$explanation[issueres$res==2]<-"not significant"
    issueres$explanation[issueres$res==3]<-"parent issue"
    issueres$explanation[issueres$res==5]<-"postponed"
    return(issueres)
}

curyear<-"2015"
urlemrttest <- paste0("https://emrt.eea.europa.eu/test/")
urlemrt <- paste0("https://emrt.eea.europa.eu/")

flagnames<-c("ne","init","sig","corr","recalc1","recalc2","unfccc","union")
lastaction<-c("last","ldate","ltext")
obsnames<-c("res","obs","date","yr","par","ms","eu",flagnames,"question",lastaction)
resstatus<-c("res","correction","lyr","party")

checks<-c("ne","outl","agrichecks")
checks<-c("ne","unit","outl","agrichecks")
nissues<-vector()


# For adding test issue
observation<-"This is a test to submit issue via R"
party<-'Germany'
sector<-"3A Enteric Fermentation"
years<-"1990-2012"
gas<-"CH4"
mskey<-0
eukey<-1
para<-"DIGEST"
#newissue<-addnewissue(urlemrttest,observation,party,sector,years,gas,mskey,eukey,para)

# First bind the checks together
# Note the csv-files as exported from allchecls.xlsm are use (make sure they are uptodate!)
i2retrieve<-openlists(checks)
issuenames<-names(i2retrieve)

# Remove "issues" which have no issue number (e.g. postpones, no issue, general problem etc.)
#                 and check the other if they are incorrect (e.g. less than three dashes)   
linedetails<-c("issuenr","next.","note",flagnames)
selection<-nchar(i2retrieve$issuenr)-nchar(gsub("-","",i2retrieve$issuenr))!=3
issuelist<-i2retrieve[selection,]
selection<-nchar(issuelist$issuenr)-nchar(gsub("-","",issuelist$issuenr))>0
if(sum(selection)>0){View(issuelist);stop("There are mistakes in issue-nr, see issuelist!")}

i2resolve<-unique(i2retrieve[i2retrieve$next.%in%c("Resolved","Corrected","Justified"),c("issuenr","next.","note",flagnames)])

# Issues where correction has been anticipated (or only partly implemented) and which need to be
#        checked in the next submission.
# Also issues where some 'action' from our side is required: clarification with UNFCCC 
#      or advice requested
esdtrial<-c("AT","BE","BG","CY","CZ","EE","FI","DE","HR","HU","IE","LV","LT","MT","RO","SE","SK","UK")

i2partly<-unique(i2retrieve[grepl("Partly",i2retrieve$next.),linedetails])
i2explain<-unique(i2retrieve[grepl("Explained",i2retrieve$next.),linedetails])
i2follow<-unique(i2retrieve[grepl("Follow",i2retrieve$next.),linedetails])
i2unresolved<-unique(i2retrieve[grepl("Not",i2retrieve$next.),linedetails])
i2noresponse<-unique(i2retrieve[grepl("No response",i2retrieve$next.),linedetails])



# Make first check of all issues are OK
#cannotbeaccessed<-checkaccessibility(issues)

#####################################
stop()
remDr<-emrt()
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
