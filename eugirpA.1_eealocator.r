# Jump over the generation of 'alldata' in case this has already been done
generatealldata <- 1
if(file.exists(rdatallem)){
    if(file.info(paste0(csvfil,".txt"))$mtime<file.info(rdatallem)$mtime){
        print(paste0("Load existing file ",rdatallem))
        load(rdatallem)    
        generatealldata <- 0
    }
}
if(generatealldata==1){
    # ---> Read text-file if no RData file exists of if the text file is more recent
    rdatfile<-paste0(csvfil,".RData")
    if(!file.exists(rdatfile)){
        print(paste0("Load ",csvfil,".txt and generate new ",rdatfile))
        alldata<-read.csv(paste0(csvfil,".txt"),na.string="-999")
        save(alldata,file=rdatfile)
    }else if(file.info(paste0(csvfil,".txt"))$mtime>file.info(rdatfile)$mtime){
        print(paste0("Load updated",csvfil,".txt and generate new ",rdatfile))
        alldata<-read.csv(paste0(csvfil,".txt"),na.string="-999")
        save(alldata,file=rdatfile)
    }else{
        print(paste0("Retrieve ",rdatfile))
        load(rdatfile)
    }
    
    # measureacronyms --------------------------------------------------------------
    # Keep long text with the exception of 'measure' which is needed to identify
    # if it is an activity data, emissions, emission factor or parameter or other
    # alldata$measurelong<-alldata$measure
    #measureacronyms<-read.csv("metadim_row8measure.txt",stringsAsFactors=FALSE)
    #alldata<-subset(temp1,select=-dummy)
    measureacronyms<-read.csv("measures_20150731.txt",stringsAsFactors=FALSE)
    alldata<-merge(alldata,measureacronyms,by.x="measure",by.y="measname")
    
    # Correction of meastypes for specific emission sources ####
    
    # table 3B(b) ------------------------------------------------------------------
    #
    #  ---> Table 3.B(b) is rather complex...
    #
    # sector_number is read as factor, which does not allow to define new elements 
    #               therefore transformation to 'character' required
    alldata$sector_number<-as.character(alldata$sector_number)
    alldata[alldata$sector_number=="3.B.2.5" & alldata$classification=="Indirect N2O Emissions" & alldata$unit=="kt","meastype"]<-"EM"
    alldata[alldata$sector_number=="3.B.2.5" & alldata$classification=="Indirect N2O Emissions" & alldata$unit=="kg N2O/kg N","meastype"]<-"IEF"
    
    alldata[alldata$sector_number=="3.B.2.5" & alldata$measure=="Total N volatilised as NH3 and Nox","sector_number"]<-"3.B.2.5 indirect volatilisation"
    alldata[alldata$sector_number=="3.B.2.5" & alldata$measure=="N lost through leaching and run-off","sector_number"]<-"3.B.2.5 indirect leaching"
    alldata[alldata$sector_number=="3.B.2.5" & grepl("Atmospheric deposition",alldata$measure),"sector_number"]<-"3.B.2.5 indirect volatilisation"
    alldata[alldata$sector_number=="3.B.2.5" & grepl("Nitrogen leaching",alldata$measure),"sector_number"]<-"3.B.2.5 indirect leaching"
    #View(alldata[grepl("3.B.2.5",alldata$sector_number),]) 
    
    # Biomass burning ------------------------------------------------------------------------
    alldata[grepl("3.F.1.",alldata$sector_number) & alldata$measure=="Crop  production","meastype"]<-"PROD"
    alldata[grepl("3.F.1.",alldata$sector_number) & alldata$measure=="Biomass available","meastype"]<-"YIELD"
    
    
    # Select gases
    gases2keep<-c("Aggregate GHGs","CH4","no gas","CO2","N2O")
    allgases<-sort(unique(alldata$gas))
    selectGas<-alldata$gas %in% gases2keep
    alldata<-alldata[selectGas,]
    
    # Remove UK (use GB) ####
    selectParty<-! alldata$party == "UK"
    alldata<-alldata[selectParty,]
    
    # Remove category substrings -----------------------------------------------------------------------------
    #  ---> Category contains sometimes substring of sector_name (e.g. Dairy Cattle)
    # takes too long
    alldata<-alldata
    alldata$category<-as.character(alldata$category)
    alldata$category<-unlist(lapply(c(1:nrow(alldata)),function(x) 
        if(!is.null(alldata$category[x])){
            if(grepl(alldata$category[x],alldata$sector_number[x])){
                ""
            }else{
                alldata$category[x]
            }
        }else{""}
    ))
    
    # Remove duplicate UIDs -----------------------------------------------------------------------------
    # ---> there are duplicate UIDs...
    alldata$variableUID<-as.character(alldata$variableUID)
    duplicateuids<-read.csv("duplicateUIDs.csv",header=FALSE)
    names(duplicateuids)<-c("UID","SEC")
    for(changeuid in c(1:nrow(duplicateuids))){
        duplicateUID<-as.vector(duplicateuids[changeuid,"UID"])
        duplicateSEC<-as.vector(duplicateuids[changeuid,"SEC"])
        duplicateNEW<-gsub(substr(duplicateUID,1+nchar(duplicateUID)-nchar(duplicateSEC),nchar(duplicateUID)),duplicateSEC,duplicateUID)
        alldata$variableUID[alldata$variableUID==duplicateUID & alldata$sector_number==duplicateSEC]<-duplicateNEW
    }
    
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Remove sector_number "-"  ----
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    alldata<-alldata[alldata$sector_number!="-",]
    
    
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Create vectors for categories, years, and unique rows (not considering years) ----
    # allcategories is created as 'factor'
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    allcategories<-sort(unique(alldata$sector_number))
    # allyears is created as 'integer'
    allyears<-sort(unique(alldata$year))
    allsources<-sort(unique(alldata$source))
    allmethods<-sort(unique(alldata$method))
    alltargets<-sort(unique(alldata$target))
    alloptions<-sort(unique(alldata$option))
    allnotations<-sort(unique(alldata$notation))
    alltypes<-sort(unique(alldata$type))
    allmeasures<-sort(unique(alldata$measure))
    alluids<-sort(unique(alldata$variableUID))
    
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Store keys into separate data file =======================================
    # Keep 'alldatanovalues' for reference in case of problems
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    alldatanovalues<-as.data.frame(unique(subset(alldata,select=-c(value,year,party,country_name))))
    
    notationkeys<-c("NO","NE","IE","NA")
    alldatanotations<-alldata[alldata$notation %in% notationkeys,]
    alldata<-alldata[!(alldata$notation %in% notationkeys),]
    
    # Fields giving additional info on the 'method' ----
    fields2merge<-sort(c("category","source","method","target","option","type"))
    fields2keep<-c("sector_number","gas","unit","allmethods","party","meastype")
    
    # ---> Fields that will be needed for information but they are not needed 
    #      to identify the cells
    fields4info<-sort(c("notation","classification","country_name"))
    # ---> Fields that are identical per country 
    fields4coun<-c("submission_version","submission_year")
    listofuniquefields<-sort(c(fields2keep,fields2merge))
    
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Replace all 'method' fields which will not be needed individually with ----
    # a field where the content is concantenated and simplified
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    alldatauniq<-lapply(fields2merge,function(x) as.vector(alldata[,x]))
    alldatauniq<-lapply(alldatauniq,function(x) paste0(x,"_"))
    
    # ---> Concetenate
    alldatauniq<-as.vector(Reduce(paste0,alldatauniq))
    # ---> Eleminate the 'no' texts .... they do not add information
    no<-"no method_|no option_|no source_|no target_|no type_|Additional Information_"
    t<-gsub(no,"",alldatauniq)
    alldatauniq<-as.data.frame(gsub("_$","",t))
    listofoptions<-unique(alldatauniq)
    names(alldatauniq)<-"allmethods"
    if(nrow(alldata)==0){stop("Stop! alldata contains no data!")}
    alldata<-cbind(alldata,alldatauniq)
    alldata<-subset(alldata,select=names(alldata)[! names(alldata) %in% fields2merge])
    #View(alldatauniq)
    #View(t)
    
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Replace all fields which are needed to identify the rows (except year) ----
    # a field where the content is concantenated and simplified
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    alldatauniq<-lapply(fields2keep,function(x) as.vector(alldata[,x]))
    alldatauniq<-lapply(alldatauniq,function(x) paste0(x,"_"))
    alldatauniq<-as.vector(Reduce(paste0,alldatauniq))
    alldatauniq<-gsub("__","_",alldatauniq)
    alldatauniq<-as.data.frame(gsub("_$","",alldatauniq))
    names(alldatauniq)<-"unique"
    listofuniq<-unique(alldatauniq)
    alldata<-cbind(alldata,alldatauniq)
    
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Store away columns not needed immediately together with unique fields -----
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    allnotations<-alldata[alldata[,"meastype"]=="METHOD",]
    alldata$meastype[is.na(alldata$meastype)]<-""
    alldata<-alldata[alldata[,"meastype"]!="METHOD",]
    subfields<-c(fields4info[! fields4info %in% "notation"],"value")
    allnotations<-subset(allnotations,select=names(allnotations)[! names(allnotations) %in% subfields])
    allnotations<-subset(allnotations,select=names(allnotations)[! names(allnotations) %in% fields4coun])
    allnotations<-subset(allnotations,select=names(allnotations)[! names(allnotations) %in% fields2keep])
    
    #allinfos<-alldata[alldata[,"meastype"]=="INFO" | alldata[,"allmethods"]=="Additional Information",]
    #alldata <-alldata[alldata[,"meastype"]!="INFO" & alldata[,"allmethods"]!="Additional Information",]
    allinfos<-alldata[alldata[,"meastype"]=="INFO",]
    alldata <-alldata[alldata[,"meastype"]!="INFO",]
    allinfos<-subset(allinfos,select=names(allinfos)[! names(allinfos) %in% fields4info])
    allinfos<-subset(allinfos,select=names(allinfos)[! names(allinfos) %in% fields4coun])
    allinfos<-subset(allinfos,select=names(allinfos)[! names(allinfos) %in% fields2keep])
    
    alldata4info<-subset(alldata,select=c(fields4info,"unique"))
    alldata<-subset(alldata,select=names(alldata)[! names(alldata) %in% fields4info])
    
    alldata4coun<-subset(alldata,select=c(fields4coun,"unique"))
    alldata<-subset(alldata,select=names(alldata)[! names(alldata) %in% fields4coun])
    
    measname<-as.matrix(unique(subset(alldata,select=c("meastype","sector_number","measure","variableUID"))))
    #alldata<-subset(alldata,select=-measname)
    
    alldata4uniq<-subset(alldata,select=c(fields2keep,"unique","variableUID"))
    alldata4uniq<-unique(alldata4uniq)
    alldata<-subset(alldata,select=names(alldata)[! names(alldata) %in% fields2keep])
    
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Generate data frame with one columns per year ----
    # and the value in the corresponding column ---
    # zeros in all other columns
    # http://stackoverflow.com/questions/11350537/convert-a-factor-column-to-multiple-boolean-columns
    # Alternative method could be: #http://stackoverflow.com/questions/9084439/r-colsums-by-group
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    temp1<-as.data.frame(model.matrix(~factor(year)-1,data=alldata)*alldata$value)
    temp2<-cbind(alldata,temp1)
    names(temp2)<-gsub("factor\\(year\\)","",as.vector(names(temp2)))
    temp2<-subset(temp2,select=c("unique",allyears[allyears %in% names(temp2)]))
    
    # seems to work better: https://stat.ethz.ch/pipermail/r-help/2008-March/157414.html
    #temp3<-lapply(split(temp1,temp1$category),function(x) colSums(x[,-1]))
    alldatanames<-as.vector(sort(unique(temp2$unique)))
    temp3<-lapply(split(temp2,temp2$unique,drop=TRUE),function(x) colSums(x[,-1],na.rm=TRUE))
    temp4<-as.data.frame(Reduce(cbind,temp3))
    names(temp4)<-alldatanames
    temp4<-as.data.frame(t(temp4))
    
    alldata<-merge(alldata4uniq,temp4,by.x="unique",by.y=0)
    
    #Restrict the years
    years2delete<-as.vector(as.character(allyears[!(allyears %in% years2keep)]))
    allcolumns<-names(alldata)
    alldata<-subset(alldata,select=allcolumns[! allcolumns %in% years2delete])
    
    # Save alldata for later re-use incase of allem or all3 ####
    toremove<-c("temp1","temp2","temp3","temp4")
    rm(toremove)
    stepsdone<-1
    save(stepsdone,alldata,allnotations,allinfos,measname,file=rdatallem)
    write.table(alldata[grepl("^3",alldata$sector_number),],file=paste0(csvfil,"_cat3.csv"),sep=",")
    write.table(alldata[grepl("^4",alldata$sector_number),],file=paste0(csvfil,"_cat4.csv"),sep=",")
    
}
