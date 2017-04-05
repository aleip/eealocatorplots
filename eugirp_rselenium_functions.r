#install.packages("RSelenium")
#RSelenium::checkForServer()
#vignette('RSelenium-basics')
#methods see help(remoteDriver)
options(warnings=0)
options(warn=0)
#RSelenium::startServer(dir = "RSelenium",log = FALSE)
require(RSelenium)
#rsDriver(port = 4567L, browser = "chrome", version = "latest", chromever = "latest",
#         geckover = "latest", iedrver = NULL, phantomver = "2.1.1",
#         verbose = TRUE, check = TRUE)

#20170223 - Selenium Server doesn't start any more from R 
#         --> Start it manually with 
# java -jar RSelenium/selenium-server-standalone-3.1.0.jar -port 4567
# launch it with bat-file
# Need also to start gecko-driver!



#source("curplot.r")

### LOGIN ####
loginemrt<-function(remDr,issue=""){
    
    urlemrt <- paste0("https://emrt.eea.europa.eu/")
    login<-"login"
    usr<-"leipvadr"
    psw<-"0112rkf4"
    
    remDr$navigate(paste0(urlemrt,login))
    Sys.sleep(0.5)
    webelem<-remDr$findElement('id', '__ac_name')
    webelem$clearElement()
    webelem$sendKeysToElement(list(usr))
    webelem<-remDr$findElement('id', '__ac_password')
    webelem$clearElement()
    webelem$sendKeysToElement(list(psw))
    webelem$submitElement()
    Sys.sleep(2)
    remDr$navigate(paste0(urlemrt,issue))
    
}
logingema<-function(remDr,issue=""){
    
    urlemrt <- paste0("https://emrt.eea.europa.eu/")
    login<-"login"
    usr<-"carmogem"
    psw<-"Mostoles76"
    
    remDr$navigate(paste0(urlemrt,login))
    Sys.sleep(0.5)
    webelem<-remDr$findElement('id', '__ac_name')
    webelem$clearElement()
    webelem$sendKeysToElement(list(usr))
    webelem<-remDr$findElement('id', '__ac_password')
    webelem$clearElement()
    webelem$sendKeysToElement(list(psw))
    webelem$submitElement()
    Sys.sleep(2)
    remDr$navigate(paste0(urlemrt,issue))
}

emrt<-function(){
    # for chrome see https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-saucelabs.html#id1a
    #startServer(args = c("-Dwebdriver.chrome.driver=RSelenium/chromedriver.exe"), dir = "RSelenium", log = FALSE, invisible = FALSE)
    #remDr<-remoteDriver(browserName = "firefox",remoteServerAddr = "localhost", port = 4444)
    remDr<-remoteDriver(browserName = "firefox",remoteServerAddr = "localhost", port = 4567)
    remDr$open(silent = TRUE)
    loginemrt(remDr)
    return(remDr)
}
emrtgema<-function(){
    # for chrome see https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-saucelabs.html#id1a
    #remDr<-remoteDriver(browserName = "firefox",remoteServerAddr = "localhost", port = 4444)
    remDr<-remoteDriver(browserName = "firefox",remoteServerAddr = "localhost", port = 4567)
    remDr$open(silent = TRUE)
    logingema(remDr)
    return(remDr)
}


### FUNCTIONS
issuedate<-function(dstring){
    dstring<-unlist(strsplit(dstring,","))[1]
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
openissue<-function(remDr,issue,phase=""){
    year<-unlist(strsplit(as.character(issue),"-"))[3]
    if(is.na(year))year<-""
    path<-paste0(urlemrt,year,"/",issue,phase)
    cat(path)
    curissue<-remDr$navigate(path)
    Sys.sleep(1)
    return(curissue)
}

#addnewissue<-function(observation,party,sector,revyear,invyear,gas,mskey,eukey,para,highlight){
addnewissue<-function(curobs,where=curyear,x=""){
    # Add new issue ####
    # where = "test" or "2016" or other year..
    remDr<-emrtgema()
    print("")
    print(paste0("New issue nr. ",x))
    newissue<- "++add++Observation"
    
    observation<-curobs$Obs
    party<-curobs$Country
    sector<-curobs$sec
    invyear<-as.character(curobs$invyear)
    revyear<-as.character(curobs$revyear)
    gas<-curobs$gas
    mskey<-curobs$key.ms
    eukey<-curobs$key.eu
    para<-curobs$par
    highlight<-c(curobs[,flagnames])
    highlight[is.na(highlight)]<-0
    highlight[highlight=="x"]<-1
    highlight<-unlist(highlight)
    question<-curobs$question
    
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-0') #Not estimated (NE) - initial checks
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-1') #Gap filling - initial checks
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-2') #Potential significant issues (step 1)
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-3') #Technical correction (step 2)
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-4') #Recalculation (compared to previous year submission)
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-5') #Recalculation (compared to same year submission)
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-6') #UNFCCC Recommendation
    # webelem<-remDr$findElement('id', 'form-widgets-highlight-7') #Union Recommendation
    
    
    remDr$navigate(paste0(urlemrt,where,"/",newissue))
    addtext(remDr,mytext = observation)
    
    webelem<-remDr$findElement('id', 'form-widgets-country')
    webelem$sendKeysToElement(list(party))
    
    webelem<-remDr$findElement('id', 'form-widgets-crf_code')
    Sys.sleep(2)
    webelem$sendKeysToElement(list(sector))
    
    webelem<-remDr$findElement('id', 'form-widgets-year')
    webelem$clearElement()
    Sys.sleep(2)
    webelem$sendKeysToElement(list(invyear))
    
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
    webelem$sendKeysToElement(list(revyear))
    
    webelem<-remDr$findElement('id', 'form-widgets-ms_key_catagory-0') #ms-key
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    if(as.numeric(mskey))webelem$clickElement()
    
    webelem<-remDr$findElement('id', 'form-widgets-eu_key_catagory-0') #eu-key
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    if(as.numeric(eukey))webelem$clickElement()
    
    webelem<-remDr$findElement('id', 'form-widgets-parameter-0') #AD
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    webelem<-remDr$findElement('id', 'form-widgets-parameter-1') #EM
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    webelem<-remDr$findElement('id', 'form-widgets-parameter-2') #IEF
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    webelem<-remDr$findElement('id', 'form-widgets-parameter-3') #other
    if(unlist(webelem$isElementSelected())) webelem$clickElement()
    
    if(para=="AD" | para=="Activity Data"){
        webelem<-remDr$findElement('id', 'form-widgets-parameter-0')
    }else if(para=="EM" | para=="Emissions" | para=="Emission"){
        webelem<-remDr$findElement('id', 'form-widgets-parameter-1') #EM
    }else if(para=="IEF" | grepl("Emission factor",para)){
        webelem<-remDr$findElement('id', 'form-widgets-parameter-2') #IEF
    }else{
        webelem<-remDr$findElement('id', 'form-widgets-parameter-3') #other
    }
    webelem$clickElement()
    for (i in c(0:10)){
        webelem<-remDr$findElement('id', paste0('form-widgets-highlight-',i))
        if(unlist(webelem$isElementSelected())) webelem$clickElement()
        if(highlight[i+1]==1){
            webelem<-remDr$findElement('id', paste0('form-widgets-highlight-',i))
            webelem$clickElement()
            
        }
    }
    webelem<-remDr$findElement('id', 'form-buttons-save')
    #webelem<-remDr$findElement('id', 'form-buttons-cancel')
    webelem$clickElement()
    while(grepl("\\+\\+add\\+\\+",webelem$getCurrentUrl()[[1]])){
        Sys.sleep(1)
        cat("wait..")
    }
    newissue<-"A"
    newissue<-strsplit(webelem$getCurrentUrl()[[1]],"\\/")[[1]][5]
    
    waitfor<-paste0(urlemrt,where,"/",newissue,"/view#tab-qa")
    cat(waitfor,"...")
    while(webelem$getCurrentUrl()[[1]]!=waitfor){
        Sys.sleep(1)
        cat("wait..")
    }
    print(webelem$getCurrentUrl()[[1]])
    qok<-addquestion(remDr,newissue,question)
    
    waitfor<-paste0(urlemrt,where,"/",newissue,"#tab-qa")
    cat(waitfor,"...")
    while(webelem$getCurrentUrl()[[1]]!=waitfor){
        Sys.sleep(1)
        cat("wait..")
    }
    print(webelem$getCurrentUrl()[[1]])
    
    sendapproval<-clickstandardbutton(remDr,btext = "Send Question for Approval")
    #Give time to change item status and notify users
    webelem<-remDr$findElement('class', 'documentEditable')
    cat(" Wait item state changing ... ")
    while(!grepl("Item state changed",webelem$getElementText())){
        cat("wait...")
        Sys.sleep(10)
        webelem<-remDr$findElement('class', 'documentEditable')
    }
    remDr$close()
    return(newissue)
}

acknowledge<-function(remDr,issue){
    curissue<-openissue(remDr,issue)
    clickstandardbutton(remDr,btext = "Acknowledge Answer")
}

addquestion<-function(remDr,issue,question){
    Sys.sleep(2)
    webelem<-remDr$findElement('id', 'add-question-link')
    webelem$clickElement()
    addtext(remDr,mytext = question)
    webelem<-remDr$findElement('class', 'submit-widget')
    webelem$clickElement()
    
    return(1)
}

clickstandardbutton<-function(remDr,btext){
    isPresent = length(remDr$findElements('class', 'eea-tabs-panel'))
    ok<-0
    if(isPresent>0){
        webelem<-remDr$findElement('class', 'eea-tabs-panel')
        buttons<-webelem$findChildElements("class name","standardButton")
        buttontext<-unlist(lapply(buttons,function(x) x$getElementText()))
        if(length(buttontext)>0){
            for(i in c(1:length(buttontext))){
                mybutton<-buttons[[i]]
                if(buttontext[i]==btext) {
                    mybutton$clickElement()
                    ok<-1
                }
            }
        }
    }
    return(ok)
    
}
clicksubmitbutton<-function(remDr,btext){
    isPresent = length(remDr$findElements('class', 'submit-widget'))
    ok<-0
    if(isPresent==1){
        webelem<-remDr$findElement('class', 'submit-widget')
        webelem$clickElement()
        ok<-1
    }
    if(isPresent>1){
        webelem<-remDr$findElements('class', 'submit-widget')
        webelem$clickElement()
        ok<-1
    }
    return(ok)
}
clicksave<-function(remDr){
    isPresent = length(remDr$findElements('id', 'form-buttons-save'))
    ok<-0
    if(isPresent>0){
        webelem<-remDr$findElement('id','form-buttons-save')
        webelem$clickElement()
        ok<-1
    }
    return(1)
}
addtext<-function(remDr,mytext){
    isPresent = length(remDr$findElements('id', 'form-widgets-text'))
    ok<-0
    if(isPresent>0){
        webelem<-remDr$findElement('id', 'form-widgets-text')
        webelem$clearElement()
        webelem$sendKeysToElement(list(mytext))
        ok<-1
    }
    return(ok)
}

approvequestionandsend<-function(remDr,issue,where=NULL){
    curissue<-openissue(remDr,issue)
    sent<-clickstandardbutton(remDr,btext = "Approve question and send")
    Sys.sleep(3)
    return(sent)
}

followupissue<-function(x,line){
    remDr<-emrtgema()
    curissue<-line$issuenr
    #https://emrt.eea.europa.eu/2017/BE-3A-2017-0005#tab-qa
    print(paste0("issue ",x,": ",curissue))
    print(line$communication)
    openissue(remDr,curissue)
    
    #https://emrt.eea.europa.eu/2017/BE-3A-2017-0005/question-1/++add++Comment
    clickstandardbutton(remDr,"Add follow up question")
    Sys.sleep(2)
    while(!grepl("\\+\\+add\\+\\+",remDr$getCurrentUrl()[[1]])){
        Sys.sleep(1)
        cat("wait..")
    }
    
    addtext(remDr,mytext = line$communication)
    
    #There are two 'submit-widget' buttons but the second is not visible, so difficult to handle
    #clicksubmitbutton(remDr,"Add question")
    #webelem<-remDr$findElement('class', 'submit-widget')
    #webelem$clickElement()
    Sys.sleep(1)
    clicksave(remDr)
    Sys.sleep(2)
    while(grepl("\\+\\+add\\+\\+",remDr$getCurrentUrl()[[1]])){
        #print(remDr$getCurrentUrl()[[1]])
        Sys.sleep(1)
        cat("wait..")
    }
    
    sent<-clickstandardbutton(remDr,"Send Question for Approval")
    Sys.sleep(5)
    remDr$close()
    return(list(curissue,sent))
}
onlysendforapproval<-function(line){
    #curissue<-line$issuenr
    curissue<-line
    openissue(remDr,curissue)
    sent<-clickstandardbutton(remDr,"Send Question for Approval")
    return(list(curissue,sent))
}

uploadfile<-function(remDr,file){
    
    webelem<-remDr$findElement('class', 'eea-tabs-panel')
    #qapart<-webelem$findChildElements("class name","question")
    buttons<-webelem$findChildElements("class name","standardButton")
    buttontext<-unlist(lapply(buttons,function(x) x$getElementText()))
    for(i in c(1:length(buttontext))){
        mybutton<-buttons[[i]]
        if(buttontext[i]=="Upload file") {mybutton$clickElement()}
    }
    webelem<-remDr$findElement('id', 'form-widgets-file-input')
    webelem<-remDr$findElement('id', 'form-widgets-file')
    webelem$clickElement()
}


# TASKS TO DO ####
convert2char<-function(DF,cols=NULL){
    
    if(is.null(cols)){
        for(i in 1:ncol(DF)) if(is.factor(DF[,i])) DF[,i]<-as.character(DF[,i])
    }
    return(DF)
}

addnewissuecomplete<-function(newobs,x1=NULL,x2=NULL,where="test"){
    if(is.null(x1)) x1<-1
    if(is.null(x2)) x2<-nrow(newobs)
    
    #Log-in as Gema
    newissues<-unlist(lapply(c(x1:x2),function(x) addnewissue(newobs[x,],where,x=x)))
    
    print("New issues added - now approve")
    #Log-in as Adrian
    remDr<-emrt()
    curissues<-listofselectedissues(remDr,revyear = curyear,filter = "workflow",criterion = "quality")
    cat("to be approved:",curissues)
    approved<-sapply(1:length(curissues),function(x) approvequestionandsend(remDr = remDr,issue = curissues[x]))
    cat("approved:",approved)
    #sent<-unlist(lapply(c(1:nrow(newissues)),function(x) approvequestionandsend(remDr,newissues$newissues[x],where)))
    #newissues$sent<-sent
    #write.csv(newissues,file=paste0(issuedir,"issues",format(Sys.time(), "%Y%m%d-%H%M"),".csv"))
    #return(newissues)
    return(curissues)
    
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

communication<-function(remDr,curphase){
    webelem<-remDr$findElement('class', 'eea-tabs-panels')
    communication<-webelem$getElementText()[[1]]
}

# Retrieve observation and main info ####
getobsdetails<-function(remDr,issue){
    curissue<-openissue(remDr,issue)
    webelem<-remDr$findElement('class', 'collapsiblePanelTitle')
    webelem$clickElement()
    webelem<-remDr$findElement('id', 'content-core')
    
    #observationdetails<-webelem$findChildElements("class name","observation-details")
    #subyear<-unlist(lapply(observationdetails,function(x) x$getElementText()))[4]
    childs<-webelem$findChildElements("class name","position-0")
    for (i in c(1:length(childs))){
        if(grepl("Review Year",unlist(childs[[i]]$getElementText()))) yr<-unlist(childs[[i+1]]$getElementText())
        if(grepl("Description flags",unlist(childs[[i]]$getElementText()))) flags<-unlist(childs[[i+1]]$getElementText())
        if(grepl("Short description",unlist(childs[[i]]$getElementText()))) obs<-unlist(childs[[i+1]]$getElementText())
    }
    
    childs<-webelem$findChildElements("class name","position-1")
    cou<-unlist(lapply(childs,function(x) x$getElementText()))[2]
    childs<-webelem$findChildElements("class name","position-3")
    sec<-unlist(lapply(childs,function(x) x$getElementText()))[2]
    childs<-webelem$findChildElements("class name","position-8")
    gas<-unlist(lapply(childs,function(x) x$getElementText()))[2]
    childs<-webelem$findChildElements("class name","position-11")
    invyr<-unlist(lapply(childs,function(x) x$getElementText()))[2]
    childs<-webelem$findChildElements("class name","position-2")
    par<-unlist(lapply(childs,function(x) x$getElementText()))[2]
    childs<-webelem$findChildElements("class name","position-7")
    key<-unlist(lapply(childs,function(x) x$getElementText()))[2]
    if(grepl("MS",key)){ms<-1}else{ms<-0}
    if(grepl("EU",key)){eu<-1}else{eu<-0}
    childs<-webelem$findChildElements("class name","position-11")
    
    date<-unlist(lapply(childs,function(x) x$getElementText()))[4]
    date<-issuedate(date)
    
    # Description flags
    # ---------------------------
    # Not estimated (NE) (step 1)
    # Gap filling (step 1) 
    # Recalculation (compared to previous year submission)
    # Recalculation (compared to same year submission)
    # UNFCCC recommendation
    # Union recommendation
    
    # Draft/final conclusion flags
    # ---------------------------------
    # Potential significant issue (PSI) (step 1)
    # Potential technical correction (PTC) (step 2)
    # Not sent to Member State
    # Technical correction (TC) (step 2)
    # Revised estimate 
    
    flagnames<-c("ne","init","recalc1","recalc2","unfccc","union","psi1","ptc","notsent","tc","revised")
    flagelems<-c("NE","Gap","previous","same","UNFCCC","Union","PSI","PTC","Not sent","TC","Revised")
    flag<-vector(length = length(flagnames))
    for (i in c(1:length(flagnames))) {if(grepl(flagelems[i],flags)){flag[i]<-1}else{flag[i]<-0}}
    print(flag)
    return(list(c(obs,date,yr,sec,gas,invyr,par,ms,eu,flag)))
}

# Retrieve question and answer communication from an issue
getquestionanswers<-function(remDr,issue){
    curissue<-openissue(remDr,issue,"#tab-qa")
    webelem<-remDr$findElement('class', 'eea-tabs-panels')
    communication<-webelem$getElementText()[[1]]
    if(communication!=""){
        communication1<-unlist(strsplit(communication," CET"))
        communication2<-unlist(strsplit(communication,"Sent on: "))
        communication2<-unlist(strsplit(communication2,"Updated on: "))
        #communication3<-unlist(strsplit(communication2,"from "))
        communication3<-gsub("\\nEdit Key Flags","",communication2)
        communication3<-gsub("\\nInternal comments on question between experts/reviewers/QE/LR","",communication3)
        communication4<-unlist(strsplit(communication3,"\\n"))
        communication4<-communication4[!communication4==""]
        communication4<-communication4[!communication4==""]
        
        #Determine start of comment:
        # a) Identify elements with a Date
        # b) Subtract one because if there is a date the splitted string returns a length of 2
        # c) Unlist and check at which places the data was found
        # d) The start of a comment is one element before that
        okdate<-which(unlist(lapply(c(1:length(communication4)),function(x) 
            max(0,length(unlist(strsplit(communication4[x],', [0-9][0-9]:[0-9][0-9]')))-1)))==1)-1
        
        ncomments<-length(okdate)
        comfrom<-vector(length=ncomments)
        comwhen<-vector(length=ncomments)
        comwhat<-vector(length=ncomments)
        
        for(i in c(1:(ncomments))){
            comfrom[i]<-communication4[okdate[i]]
            comwhen[i]<-issuedate(communication4[okdate[i]+1])
            if(i<ncomments){imax<-okdate[i+1]-1}else(imax<-length(communication4))
            comwhat[i]<-paste(communication4[(okdate[i]+2):imax],collapse=";")
        }
        comfroms<-paste(c(1:ncomments),comfrom,collapse="; ")
        comfroms<-gsub("from expert\\/reviewer to Member State  ","EU_1",comfroms)
        comfroms<-gsub("  from Member State to expert\\/reviewer","MS",comfroms)
        comwhens<-paste(c(1:ncomments),comwhen,collapse="; ")
        if(ncomments>2){
            comwhats<-paste(c(1:ncomments),comwhat,collapse="; ")
        }else{
            comwhats<-""
        }
        question<-paste0(gsub(" *from ","",comfrom[1])," on ",comwhen[1],": ",comwhat[1])
        lastanswer<-paste0(gsub(" *from ","",comfrom[ncomments])," on ",comwhen[ncomments],": ",comwhat[ncomments])
    }
    return(list(ncomments,question,lastanswer,comfroms,comwhens,comwhats))                              
}

# Retrieve info from Conclusion Phase 1
getconclusion1<-function(remDr,issue){
    curissue<-openissue(remDr,issue,"#tab-conclusions-step-1")
    
    webelem<-remDr$findElement('class', 'eea-tabs-panels')
    communication<-webelem$getElementText()[[1]]
    
    communication1<-unlist(strsplit(communication," CET"))
    communication2<-unlist(strsplit(communication,"Sent on: "))
    communication3<-unlist(strsplit(communication2,"from "))
    communication4<-unlist(strsplit(communication3,"\\n"))
    communication4<-communication4[!communication4==""]
    
    comreas<-communication4[2]
    comwho<-communication4[3]
    comcomm<-paste(communication4[4:length(communication4)],collapse=";")
    
    return(list(comreas,comwho,comcomm))                              
    
}

# Retrieve info from Conclusion Phase 2
getconclusion2<-function(remDr,issue){
    curissue<-openissue(remDr,issue,"#tab-conclusions-step-2")
    
    webelem<-remDr$findElement('class', 'eea-tabs-panels')
    communication<-webelem$getElementText()[[1]]
    
    communication1<-unlist(strsplit(communication," CET"))
    communication2<-unlist(strsplit(communication,"Sent on: "))
    communication3<-unlist(strsplit(communication2,"from "))
    communication4<-unlist(strsplit(communication3,"\\n"))
    communication4<-communication4[!communication4==""]
    
    comreas<-communication4[2]
    comwho<-communication4[3]
    communication4<-paste(communication4[4:length(communication4)],collapse=";")
    comcomm<-unlist(strsplit(communication4,";GHG estimates:"))[1]
    comghg<-unlist(strsplit(communication4,";GHG estimates:"))[2]
    return(list(comreas,comwho,comcomm,comghg))                              
    
}

# Check all details of issue
# - uses getobsdetails,getquestionanswer,getconclusion1,getconclusion2
issuedetail<-function(remDr,issue){
    curissue<-openissue(remDr,issue)
    
    webelem<-remDr$findElement('class','eea-tabs')
    curphases<-unlist(strsplit(unlist(webelem$getElementText()),"\\n"))
    
    obsdetails<-getobsdetails(remDr,issue)
    #return(list(c(obs,date,yr,par,ms,eu,flag,q)))
    infoissue<-data.frame(phase=character(),
                          expert=character(),
                          date=character(),
                          year=integer(),
                          par_reason=character(),
                          ms=character(),
                          eu=character(),
                          flag=character(),
                          text=character(),stringsAsFactors=FALSE)
    curline<-1
    infoissue[curline,]<-c("Observation","",obsdetails[[1]][2:7],obsdetails[[1]][1])
    curline<-curline+1
    
    if((phases%in%curphases)[1]){
        questionanswers<-getquestionanswers(remDr,issue)
        #return(list(ncomments,comfrom,comwhen,comwhat))                              
        #for(i in c(1:unlist(questionanswers[[1]]))){
        #    infoissue[curline,]<-c("Q&A",questionanswers[[2]][i],questionanswers[[3]][i],rep("",5),questionanswers[[4]][i])
        #    curline<-curline+1
        #}
    }
    reas1<-""
    if((phases%in%curphases)[2]){
        conclusions1<-getconclusion1(remDr,issue)
        #return(list(comreas,comwho,comcomm))                              
        infoissue[curline,]<-c("Conclusions 1",conclusions1[[2]],rep("",2),
                               conclusions1[[1]],rep("",3),conclusions1[[3]])
        reas1<-paste0(conclusions1[[1]],": ",conclusions1[[3]])
        curline<-curline+1
    }
    reas2<-""
    if((phases%in%curphases)[3]){
        conclusions2<-getconclusion2(remDr,issue)
        #return(list(comreas,comwho,comcomm))                              
        infoissue[curline,]<-c("Conclusions 2",conclusions2[[2]],rep("",2),
                               conclusions2[[1]],rep("",3),conclusions2[[3]])
        reas2<-paste0(conclusions2[[1]],": ",conclusions2[[3]])
        curline<-curline+1
    }
    
    write.csv(infoissue,file=paste0("issues/",issue,".csv"))
    return(list(unlist(issue),unlist(obsdetails),(questionanswers),reas1,reas2))
    
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


followupquestion<-function(){
    
    issuefile<-read.csv(paste0(issuedir,"/issues_allmerged20160330fin.csv"),comment.char = "#",stringsAsFactors=FALSE)
    followupfields<-c("issuenr","followup","attachment","communication")
    issues2followup<-issuefile[issuefile$communication!="",followupfields]
    issueswattachment<-issues2followup[issues2followup$attachment!="",]
    issues2followup<-issues2followup[issues2followup$attachment=="",]
    
    #issues 6-11 did are not yet Sent for approval
    remDr<-emrtgema()
    x1<-25
    x2<-nrow(issues2followup)
    x2<-25
    x3<-22
    followed1<-Reduce(rbind,lapply(c(x1:x2),function(x) unlist(followupissue(issues2followup[x,]))))
    followed1<-Reduce(rbind,lapply(c(1:length(gris)),function(x) unlist(onlysendforapproval(gris[x]))))
    
}

checkaccessibility<-function(issues){
    remDr<-emrt()
    tmp<-unlist(lapply(c(1:nrow(issues)),function(x) checkissues(remDr,issues$issuenr[x])))
    failures<-length(tmp)-sum(tmp)
    if(failures>0){View(failures);stop("Some issues could not be accessed!")}
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

listofselectedissues<-function(remDr,revyear,filter=NULL,criterion=NULL){
    issues<-as.vector("")
    #filter as vector of filters: country,workflow,revyear,invyear,status,step,crf
    #criterion as vector of criteria of same length
    nfilter<-length(filter)
    ncriter<-length(criterion)
    if(nfilter!=ncriter){stop("Filter and criteria do not match!")}
    addfilter<-as.vector("")
    if(nfilter>0){
        for(i in c(1:nfilter)){
            if(filter[i]=="country"){addfilter[i]<-paste0("country=",criterion[i])}
            if(filter[i]=="workflow"){
                if(criterion[i]=="quality"){crit<-"LRQE"}
                if(criterion[i]=="sector"){crit<-"SRRE"}
                if(criterion[i]=="country"){crit<-"MSC"}
                if(criterion[i]=="finalised"){crit<-"finalised"}
                if(criterion[i]=="answered"){crit<-"answered"}
                addfilter[i]<-paste0("wfStatus=",crit)
            }
            if(filter[i]=="step"){addfilter[i]<-paste0("step=",criterion[i])}
        }
        addfilter<-paste0("?",paste(addfilter,collapse="&"))
    }
    path<-paste0(urlemrt,revyear,addfilter)
    home<-remDr$navigate(path)
    more<-1
    while(more==1){
        webelem<-remDr$findElement('class', 'observationList')
        childs<-webelem$findChildElements('class', 'clickableRow')
        for(i in c(1:length(childs))){
            issues[length(issues)+1]<-strsplit(as.character(childs[[i]]$getElementText()),"\n")[[1]][1]
            print(issues[length(issues)])
        }
        
        # More...
        webelem<-remDr$findElement('id', 'observations')
        childs<-webelem$findChildElements('class', 'next')
        if(length(childs)>0) {
            childs[[1]]$clickElement()
            Sys.sleep(5)
        }else{
            more<-0
        }
    }
    issues<-issues[!issues==""]
    return(issues)
}

# FIRST LEVEL FUNCTIONS ####
# First Checks - select issues to be approved, approve and send to MS
preparefileforissueupload<-function(newissuefile="",revyear=curyear){
    #Add new issue as Sector Expert (Gema)

    # Flags determined automatically: 'par' (Activity Data, Emission, Other)
    #                                 'Country' 
    
    newissuefile<-paste0(issuedir,newissuefile)
    newissuefile<-read.csv(newissuefile,comment.char = "#",stringsAsFactors=FALSE)
    
    newissuefile$revyear<-revyear
    newissuefile$sec<-sapply(1:nrow(newissuefile),function(x) emrtsector(newissuefile$sector_number[x]))
    newissuefile$Country<-sapply(1:nrow(newissuefile),function(x) country4sub[country4sub==newissuefile$party[x],"name"])
    determinepar<-function(meast){
        if(meast=="EM"){par<-"Emission"}else 
            if(meast=="IEF"){par<-"Emission factor"}else
                if(meast%in%c("AD","AREA","POP")){par<-"Activity Data"}else{par<-"Other"}
    }
    newissuefile$par<-sapply(1:nrow(newissuefile),function(x) determinepar(newissuefile$meastype[x]))
    
    newissuefields<-c("resolved","Obs","Country","party","sec","invyear","revyear","gas","key.ms","key.eu","par",flagnames,"question","question_type")
    missfields<-(newissuefields[!newissuefields%in%names(newissuefile)])
    
    if("Obs"%in%missfields & sum(grepl("Observ",names(newissuefile)))) newissuefile$Obs<-newissuefile[,which(grepl("Observ",names(newissuefile)))]
    newissuefile$party[grepl("UK|GB",newissuefile$party)]<-"GB"
    newissuefile$Country<-unlist(lapply(c(1:nrow(newissuefile)),function(x) countriesl[which(countries2==newissuefile$party[x])]))
    newissuefile$Country[grepl("United Kingdom",newissuefile$Country)]<-"United Kingdom"
    #if("Country"%in%missfields & sum(grepl("party",names(newissuefile)))) {
    #}
    missfields<-(newissuefields[!newissuefields%in%names(newissuefile)])
    
    
    #If question_type is defined (for timeseries issues) copy to question
    if("question_type"%in%names(newissuefile)){
        newissuefile$question<-sapply(1:nrow(newissuefile),function(x) if(newissuefile$question_type[x]!=""){newissuefile$question_type[x]}else{newissuefile$question[x]})
        newissuefile<-newissuefile[,-which(names(newissuefile)=="question_type")]
        newissuefile<-newissuefile[newissuefile$question!="",]
    }
    missfields<-missfields[!missfields=="question_type"]
    newissuefields<-newissuefields[!newissuefields=="question_type"]
    
    if(length(missfields>0))stop(paste0("missing fields: ",paste(missfields,collapse="-")))
    
    newobs<-newissuefile[,newissuefields]
    newobs<-newobs[newobs$Obs!="",]
    
    #Keep only flag 0 (new significant issue) and 4 (follow-up of previously unresolved issue)
    newobs<-newobs[newobs$resolved%in%c(0,4),]
    
    
    return(newobs)
}

approveandsendissues<-function(revyear=curyear,focus="",details=0){
    remDr<-emrt()
    filter<-"workflow"
    criterion<-"quality"
    issues<-listofselectedissues(remDr = remDr,revyear = revyear,filter = filter,criterion = criterion)
    approved<-sapply(2:length(issues),function(x) approvequestionandsend(remDr = remDr,issue = issues[x]))
}

retrievecountryresponses<-function(issues,remDr=remDr){
    #remDr<-emrt()
    #filter<-"workflow"
    #criterion<-"finalised"
    #issues<-listofselectedissues(remDr = remDr,revyear = curyear,filter = filter,criterion = criterion)
    history<-sapply(1:length(issues),function(x) getquestionanswers(remDr,issue=issues[x])[[3]])
    ncomments<-sapply(1:length(issues),function(x) length(strsplit(history[1],";")[[1]]))
    answers<-sapply(1:length(issues),function(x) getquestionanswers(remDr,issue=issues[x])[[4]])
    response<-sapply(1:length(answers),function(x) strsplit(answers[x],"LR; 2 ")[[1]][2])
    response<-gsub(";Edit Key Flags","",response)
    issuesan<-as.data.frame(issues)
    issuesan$answer<-response
    
}


selectissuesandwritedetails<-function(revyear=2017,filter="workflow",criterion="answered",focus="",details=0){
    remDr<-emrt()
    if(is.null(filter)){crit<-"all"}else{crit<-paste(criterion,collapse="-")}
    if(focus=="answers"){filter<-"workflow";criterion<-"answered"}
    if(focus=="approve"){filter<-"workflow";criterion<-"quality"}
    if(focus=="forwarded"){filter<-"step";criterion<-"step2"}
    if(focus=="newissues"){filter<-"workflow";criterion<-"country"}
    
    openissues<-listofselectedissues(remDr,revyear,filter,criterion)
    openissues<-sort(openissues)
    
    x1<-1;x2<-length(openissues)
    if(focus=="answers") sent<-unlist(lapply(c(x1:x2),function(x) acknowledge(remDr,openissues[x])))
    
    if(details==1){
        # Now get Details of the Issues and write into file    
        issuedetails<-as.data.frame(Reduce(rbind,lapply(c(x1:x2),function(x) unlist(issuedetail(remDr,openissues[x])))))
        namisdet<-c("issue","Observation","date","revyear","sec","gas","invyear","par","key-ms","key-eu",flagnames,"QA","Who comments","Date comments","Comments","Phase 1","Phase 2")
        names(issuedetails)<-namisdet
        write.csv(issuedetails,file=paste0("issues/issues",revyear,"_",crit,"~",curtime(),".csv"))
        
        # Split Q&A section into question and comments
        #issuedetails<-read.csv(paste0("issues/issues2016_answered~20160408.csv"),comment.char = "#")
        issuedetails$Comments<-gsub(";Add follow up question Add Conclusions Edit Key Flags","",issuedetails$Comments)
        issuedetails$Comments<-gsub(";Internal comments on question between experts/reviewers/QE/LR","",issuedetails$Comments)
        commentssplit<-strsplit(as.character(issuedetails$Comments),"; [2-9] ")
        maxresponses<-max(unlist(lapply(commentssplit,length)))
        issuedetails$question<-unlist(lapply(c(1:nrow(issuedetails)),function(x) commentssplit[[x]][1]))
        for(i in 2:maxresponses){issuedetails[,paste0("comment",i)]<-unlist(lapply(c(1:nrow(issuedetails)),function(x) commentssplit[[x]][i]))}
        issuedetails<-issuedetails[,-which(names(issuedetails)=="Comments")]
        write.csv(issuedetails,file=paste0("issues/issues",revyear,"_",crit,"~",curtime(),".csv"))
        
        return(issuedetails)
    }else{
        return(openissues)
    }
    
}

mergeanswerswithissues<-function(issuedetails){
    
    getissuenr<-function(x,line,check){
        sec<-line$sector_number
        par<-as.character(line$party)
        cat<-line$category
        msr<-line$measure
        mea<-paste0("\\(",line$meastype,"\\)")
        chk<-line$check
        if(chk=="N2O-NIEF")chk<-"N2O-IEF"
        if(chk=="NexRATE")chk<-"N-excretion rate"
        if(chk=="CLIMA")chk<-"climate regions"
        if(chk=="NexTOT")chk<-"Total manure excreted"
        if(line$meastype=="MCF"&check=="unit")mea<-paste0("\\(","MFC","\\)")
        #sel<-grepl(sec,issuedetails$Observation) &
        if(check!="unit") sel<-grepl(paste0(sec," -"),issuedetails$Observation) & grepl(par,issuedetails$issuenr) 
        if(check=="unit") sel<-grepl(par,issuedetails$issuenr) 
        if(check=="agric") sel<-grepl(paste0(sec," "),issuedetails$Observation) & grepl(par,issuedetails$issuenr) 
            
        if(check%in%c("coutl","recalc")) sel<-sel & grepl(paste0("- ",cat),issuedetails$Observation)
        if(check%in%c("agric")) sel<-sel & grepl(paste0("\\(",cat),issuedetails$Observation)
        if(check%in%c("times")) sel<-sel & grepl(paste0("- ",msr," \\("),issuedetails$Observation)
        if(check%in%c("coutl","times","unit")) sel<-sel & grepl(mea,issuedetails$Observation)
        if(check=="recalc") sel<-sel & grepl("Recalculation",issuedetails$Observation)
        if(check=="times") sel<-sel & grepl("Irregularities",issuedetails$Observation)
        if(check=="agric") sel<-sel & grepl(chk,issuedetails$Observation)
        if(check=="unit") sel<-sel & !grepl("outlier",issuedetails$Observation)
        if(sum(sel)>0) {
            issuenr<-issuedetails$issuenr[sel]
            observa<-issuedetails$Observation[sel]
        }else{
            issuenr<-NA
            observa<-NA
        }
        if(length(issuenr)>1 ){
            print(paste(x,par,cat,mea))
            print(issuenr)
            print(observa)
        }
        return(issuenr)
        
    }
    issuedetails$question<-gsub("^1 ","",issuedetails$question)
    stri<-";Internal comments on question between experts/reviewers/QE/LR"
    issuedetails$question<-gsub(stri,"",issuedetails$question)
    stri<-";Recall Question Edit Key Flags"
    issuedetails$question<-gsub(stri,"",issuedetails$question)
    stri<-";Acknowledge Answer Edit Key Flags"
    issuedetails$Comments<-gsub(stri,"",issuedetails$Comments)
    
    # LULUCF questions
    isslulucf<-read.csv(paste0(issuedir,"lulucf/","checklulucf4emrt.csv"))
    names(isslulucf)[which(names(isslulucf)=="test")]<-"check"
    lulucfx<-c("Observation","question")
    lulucfy<-c("Obs","question")
    isslulucf<-merge(issuedetails,isslulucf[,c("check",lulucfy)],by.x=lulucfx,by.y=lulucfy)
    
    # Country outliers
    listissues<-read.csv(paste0(issuedir,"countryoutliers/","20160202colist_checked_GC2.csv"),comment.char = "#")
    listissues<-convert2char(listissues)
    listissues$comment<-paste0(listissues$comment,listissues$gemacomments,listissues$note)
    listissues<-listissues[,-which(names(listissues)%in%c(obsnames,"issueflag","issuedate",
                                                    "gas","ref2006","ref1997","communication","gemacomments",
                                                    "followup"))]
    names(issuedetails)[which(names(issuedetails)=="issue")]<-"issuenr"
    listissues$check<-"outlier"
    
    listissues$issuenr<-unlist(lapply(1:nrow(listissues),function(x) getissuenr(x,listissues[x,],"coutl")))
    outliers<-issuedetails[grepl("outlier",issuedetails$Observation),]
    isscoutl<-merge(outliers,listissues,by=c("issuenr"),all.x=TRUE)
    isscoutl<-isscoutl[,-which(names(isscoutl)%in%c("X.x","X.y"))]

    # NE and empty cell checks
    issne<-read.csv(paste0(issuedir,"nechecks/","necheck20160216_JS.csv"),comment.char = "#")
    issne$check<-"ne_empty"
    issne<-merge(issuedetails,issne[,-which(names(issne)%in%c("party","gas","source","target","method","option","type"))],by="issuenr")

    # Unitchecks
    issunit<-read.csv(paste0(issuedir,"autocorrections/","unitchecks20160224.csv"),comment.char = "#")
    issunit$check<-"unit"
    issunit$issuenr<-unlist(lapply(1:nrow(issunit),function(x) getissuenr(x,issunit[x,],"unit")))
    issunit<-issunit[!is.na(issunit$issuenr)]
    i#ssunit<-unique(issunit[,-which(names(issunit)%in%c("category","variableUID",paste0("X",years)))])
    
    issunit<-merge(issuedetails,issunit[,-which(names(issunit)%in%c(obsnames,"party","gas","source","target","method","option","type"))],by="issuenr")
    
    # Recalculations
    listissues<-read.csv(paste0(issuedir,"recalculations/","checkrecalc~20160225.csv"),comment.char = "#")
    #listissues<-listissues[,-which(names(listissues)%in%c(obsnames))]
    listissues$issuenr<-unlist(lapply(1:nrow(listissues),function(x) getissuenr(x,listissues[x,],"recalc")))
    recalculations<-issuedetails[grepl("Recalculation",issuedetails$Observation),]
    issrecalc<-merge(recalculations,listissues,by=c("issuenr"),all.x=TRUE)
    issrecalc<-issrecalc[,-which(names(issrecalc)%in%c("X.x","X.y"))]
    
    # Time series
    listissues<-read.csv(paste0(issuedir,"timeseries/","checks20160202growthcheck.csv"),comment.char = "#")
    listissues<-listissues[listissues$flag=="yes",]
    listissues$check<-"timeseries"
    #listissues<-listissues[,-which(names(listissues)%in%c(obsnames))]
    listissues$issuenr<-unlist(lapply(1:nrow(listissues),function(x) getissuenr(x,listissues[x,],"times")))
    timesculations<-issuedetails[grepl("Irregularities",issuedetails$Observation),]
    isstimes<-merge(timesculations,listissues,by=c("issuenr"),all.x=TRUE)
    isstimes<-isstimes[,-which(names(isstimes)%in%c("X.x","X.y"))]
    isstimes$check<-"timeseries"

    # Agri-checks
    listissues<-read.csv(paste0(issuedir,"agrichecks/","agrichecks_gc.csv"),comment.char = "#")
    listissues$issuenr<-unlist(lapply(1:nrow(listissues),function(x) getissuenr(x,listissues[x,],"agric")))
    listissues<-listissues[,-which(names(listissues)%in%c("gas","ref1997","ref2006"))]
    agricculations<-issuedetails[grepl("Check:",issuedetails$Observation),]
    issagric<-merge(agricculations,listissues,by=c("issuenr"),all.x=TRUE)
    issagric<-issagric[,-which(names(issagric)%in%c("X"))]
    
    
    isslist<-list(issagric,isscoutl,isslulucf,issne,issunit,issrecalc,isstimes)
    issnames<-unique(unlist(lapply(isslist,names)))
    isslist<-lapply(isslist,function(x) filldf(DF=x,cols=issnames))
    issnumbers<-lapply(isslist,nrow)
    issall<-Reduce(rbind,isslist)
    issall$communication<-""
    issall<-convert2char(DF = issall)
    issall$question[!is.na(issall$question.y)]<-issall$question.y[!is.na(issall$question.y)]
    
    issorder<-c("issuenr","check",resolved,"Who.comments","Date.comments",
                "Observation","Obs","question","Comments")
    issorder<-c(issorder,issnames[!issnames%in%issorder])
    
    issall<-issall[,issorder]
    issall<-issall[issall$issuenr!=0,-which(names(issall)%in%c("question.y","question.x","note","Obs"))]
    issall[is.na(issall)]<-""
    issall[issall==0]<-""
    
    issall<-issall[order(issall$check,issall$issuenr),]
    
    isscheck<-merge(issuedetails[,c("issuenr","Observation")],issall[,c("issuenr","check")],by="issuenr",all=TRUE)
    isscheck<-isscheck[is.na(isscheck$check),]
    
    write.csv(issall,file=paste0(issuedir,"issues_allmerged",curtime(),".csv"))
    
}

resolvedissues<-function(){
    
    doresolve<-function(line){
        curissue<-line$issuenr
        todo<-tolower(line$Finalization)
        openissue(remDr,curissue)
        clickstandardbutton(remDr,"Add Conclusions")
        webelem<-remDr$findElement('id', 'form-widgets-closing_reason')
        if(todo=="resolved") webelem$sendKeysToElement(list("Resolved"))
        if(todo=="unresolved") webelem$sendKeysToElement(list("Unresolved"))
        if(todo=="partly resolved") webelem$sendKeysToElement(list("Partly resolved"))
        mytext<-line$Conclusion
        if(tolower(mytext)=="corrected") {
            mytext<-"Issue has been corrected!"
        }else if(tolower(mytext)=="explained"){
            mytext<-paste0("Issue has been explained: ",line$explanation)
        }else if(tolower(mytext)=="clarified"){
            mytext<-paste0("Issue has been clarified!")
        }else{
            mytext<-paste0(mytext)
        }
        addtext(remDr,mytext)
        clicksave()
        sent1<-clickstandardbutton(remDr,"Request finalisation of the observation")
        sent2<-clicksubmitbutton("Request finalisation of the observation")
        return(list(curissue,sent2))
    }
    finalise<-function(line){
        curissue<-line$issuenr
        todo<-tolower(line$Finalization)
        openissue(remDr,curissue)
        if(todo=="resolved") {
            clickstandardbutton(remDr,"Confirm finishing observation")
            sent1<-clickstandardbutton(remDr,"Confirm finishing observation")
        }
        if(todo%in%c("unresolved","partly resolved")) clickstandardbutton(remDr,"Hand over to Team 2")
        return(1)
    }
    
    curfile<-"issues2016_answered~20160411-2244-GC2.csv"
    curfile<-"issues2016_answered~20160418-2244_JS.csv"
    curfile<-"issues2016_answered~20160415-1718.csv"
    curfile<-"issues2016_answered~20160419.csv"
    issuefile<-read.csv(paste(issuedir,curfile,sep="/"),comment.char = "#",stringsAsFactors=FALSE)
    
    resolvefields<-c("issuenr","Conclusion","explanation","followup","resolved","communication")
    issues2deal<-unique(issuefile[tolower(issuefile$Finalization)%in%c("resolved","unresolved","partly resolved"),])
    View(issues2deal)
    View(issues2deal[,c("Finalization","Conclusion","explanation")],"finaliz")
    #Add Conclusion and request finalisaion
    x1<-1
    x2<-1
    x2<-nrow(issues2deal)
    remDr<-emrtgema()
    resolved1<-Reduce(rbind,lapply(c(x1:x2),function(x) unlist(doresolve(issues2deal[x,]))))
    
    #Finalise
    remDr<-emrt()
    #x1<-1
    #x2<-49
    #x2<-nrow(issues2resolve)
    resolved1<-Reduce(rbind,lapply(c(x1:x2),function(x) unlist(finalise(issues2deal[x,]))))

}



curyear<-"2017"
revyear<-"2017"
urlemrttest <- paste0("https://emrt.eea.europa.eu/test/")
urlemrt <- paste0("https://emrt.eea.europa.eu/")

flagnames<-c("ne","init","sig","corr","recalc1","recalc2","unfccc","union")
flagnames<-c("ne","init","recalc1","recalc2","unfccc","union","psi1","ptc","notsent","tc","revised")
lastaction<-c("last","ldate","ltext")
obsnames<-c("res","obs","date","yr","par","ms","eu",flagnames,"question",lastaction)
resstatus<-c("res","correction","lyr","party")
phases<-c("Q&A","Conclusions Step 1","Conclusions Step 2")

checks<-c("ne","outl","agrichecks")
checks<-c("ne","unit","outl","agrichecks")
nissues<-vector()



stop()
#Approve and send issues added by Sector Expert
remDr<-emrt()
curissues<-listofselectedissues(remDr,revyear = curyear,filter = "workflow",criterion = "quality")
approved<-sapply(1:length(curissues),function(x) approvequestionandsend(remDr = remDr,issue = curissues[x]))
approved<-sapply(1:1,function(x) approvequestionandsend(remDr = remDr,issue = curissues[x]))

# Add recalculationissues
issuefile<-paste0(issuedir,"recalculation/Recalculations20170223.csv")
newobs<-preparefileforissueupload(issuefile)
test<-addnewissuecomplete(newobs = newobs,x1=1,x2=1,where=curyear)

# For adding test issue
observation<-"This is a test to submit issue via R"
party<-'Germany'
sector<-"3A Enteric Fermentation"
period<-"1990-2012"
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
selection<-nchar(i2retrieve$issuenr)-nchar(gsub("-","",i2retrieve$issuenr))==3
issuelist<-i2retrieve[selection,]
selection<-nchar(issuelist$issuenr)-nchar(gsub("-","",issuelist$issuenr))==0
if(sum(selection)>0){View(issuelist);stop("There are mistakes in issue-nr, see issuelist!")}


# Issues where correction has been anticipated (or only partly implemented) and which need to be
#        checked in the next submission.
# Also issues where some 'action' from our side is required: clarification with UNFCCC 
#      or advice requested
esdtrial<-c("AT","BE","BG","CY","CZ","EE","FI","DE","HR","HU","IE","LV","LT","MT","RO","SE","SK","UK")

#i2resolve<-unique(i2retrieve[i2retrieve$next.%in%c("Resolved","Corrected","Justified"),c("issuenr","next.","note",flagnames)])
#i2partly<-unique(i2retrieve[grepl("Partly",i2retrieve$next.),linedetails])
#i2explain<-unique(i2retrieve[grepl("Explained",i2retrieve$next.),linedetails])
#i2follow<-unique(i2retrieve[grepl("Follow",i2retrieve$next.),linedetails])
#i2unresolved<-unique(i2retrieve[grepl("Not",i2retrieve$next.),linedetails])
#i2noresponse<-unique(i2retrieve[grepl("No response",i2retrieve$next.),linedetails])

stop()
# Drop-down Conclusion
# - Partly resolved
# - Resolved
# - Unresolved
# 
# Internal note for expert/reviewers (required)






