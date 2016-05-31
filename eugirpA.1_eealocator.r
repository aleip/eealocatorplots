# Jump over the generation of 'alldata' in case this has already been done


if(generatealldata==1){
#     # ---> Read text-file if no RData file exists of if the text file is more recent
#     rdatfile<-paste0(csvfil,".RData")
#     if(!file.exists(rdatfile)){
#         print(paste0("Load ",csvfil,".txt and generate new ",rdatfile))
#         alldata<-read.csv(paste0(csvfil,".txt"),na.string="-999")
#         save(alldata,file=rdatfile)
#     #}else if(file.info(paste0(csvfil,".txt"))$mtime>file.info(rdatfile)$mtime){
#     #    print(paste0("Load updated",csvfil,".txt and generate new ",rdatfile))
#     #    alldata<-read.csv(paste0(csvfil,".txt"),na.string="-999")
#     #    save(alldata,file=rdatfile)
#     }else{
#         print(paste0("Retrieve ",rdatfile))
#         load(rdatfile)
#     }
    source("eealocator_generate_rdata.r")
    
    # --- Eleminate the 'no' texts .... they do not add information ####
    #no<-"no method_|no option_|no source_|no target_|no type_|Additional Information_"
    #no<-c("no method","no option","no source","no target","no type","Additional Information")
    levels(alldata$method)[levels(alldata$method)=="no method"]<-""
    levels(alldata$option)[levels(alldata$option)=="no option"]<-""
    levels(alldata$source)[levels(alldata$source)=="no source"]<-""
    levels(alldata$target)[levels(alldata$target)=="no target"]<-""
    levels(alldata$type)[levels(alldata$type)=="no type"]<-""
    levels(alldata$category)[levels(alldata$category)=="no classification"]<-""

    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Create vectors for categories, years, and unique rows (not considering years) ----
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    print("Create vectors for categories")

    # Select gases ####
    gases2keep<-c("Aggregate GHGs","CH4","no gas","CO2","N2O")
    select<-alldata$gas %in% gases2keep
    alldata<-alldata[select,]
    gases<-as.character(unique(alldata$gas))
    
    # Select years ####
    #years2delete<-as.vector(as.character(allyears[!(allyears %in% years2keep)]))
    select<-alldata$year %in% years2keep
    alldata<-alldata[select,]
    years<-as.character(unique(alldata$year))
    
    # Store method descriptions in different data frame - delete from alldata ####
    print("Store allmethods")
    methods<-as.character(unique(alldata$method))
    allmethods<-alldata[alldata$measure=="Method",]
    allmethods<-simplifytestmatrix(allmethods,"year",years2keep)
    alldata<-alldata[! alldata$measure=="Method",]
    
    print("Store allinfos")
    infos<-c("Documentation box","Emission factor information","Type")
    measures<-as.character(unique(alldata$measure))
    allinfos<-alldata[alldata$measure %in% infos,]
    allinfos<-simplifytestmatrix(allinfos,"year",years2keep)
    alldata<-alldata[! alldata$measure %in% infos,]

    notations<-levels(alldata$notation)
    
    parties<-as.character(unique(alldata$party))
    classifications<-as.character(unique(alldata$classification))
    categories<-as.character(unique(alldata$category))
    sources<-as.character(unique(alldata$source))
    targets<-as.character(unique(alldata$target))
    options<-as.character(unique(alldata$option))
    types<-as.character(unique(alldata$type))
    units<-as.character(unique(alldata$unit))
    sectors<-as.character(unique(alldata$sector_number))
    uids<-as.character(unique(alldata$variableUID))
    
    submission_version<-as.character(unique(alldata$submission_version))
    submission_year<-as.character(unique(alldata$submission_year))
    countries<-unique(alldata[,c("party","country_name")])
    alldata<-subset(alldata,select=(! names(alldata) %in% c("country_name","submission_version","submission_year")))
    
    
    # Store notations in different data frame - delete from alldata ####
    print("Store allnotations")
    notationkeys<-"[CS,NO,NE,IE,NA,D,T1,T2]"
    allnotations<-alldata[grepl(notationkeys,alldata$notation),]
    alldata<-alldata[! grepl(notationkeys,alldata$notation),]
    allnotations<-simplifytestmatrix(allnotations,"year",years2keep)
    alldata<-unique(alldata)
    

    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Generate data frame with one columns per year ----
    # and the value in the corresponding column ---
    # zeros in all other columns
    # http://stackoverflow.com/questions/11350537/convert-a-factor-column-to-multiple-boolean-columns
    # Alternative method could be: #http://stackoverflow.com/questions/9084439/r-colsums-by-group
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    print("Generate data frame with one columns per year")
    # First create a vector of unique strings corresponding to each of the variables (all years) of alldata
    tmpfields<-c("sector_number","category","method","classification","source","target","type","measure","notation","gas","unit","party")
    tmpfields<-names(alldata)[!names(alldata)%in%c("year","value")]
    alldatauniq<-subset(alldata,select=tmpfields)
    alldatauniq<-as.data.frame(Reduce(paste0,alldatauniq))
    row.names(alldatauniq)<-row.names(alldata)
    names(alldatauniq)<-"unique"
    alldata$unique<-alldatauniq$unique
    
    # Second prepare a data frame where the years are in columns not in rows
    newmatrix<-(unique(subset(alldata,select=!names(alldata)%in%c("year","value"))))
    rok<-nrow(newmatrix)
    nok<-length(years2keep)
    rts<-length(levels(alldatauniq$unique))
    if(rok!=rts){stop("nok and nts different")}
    
    # Third create a matrix with unique values temp1 will have nrow(alldata), 
    #              the matrix is split into nrow(newmatrix) and the sum is calculated: 
    #              - in each of the 'sub-matrices' temporarily generated there is exactly one value for each year-column
    # Explanation: the column 'year' is used and for all levels(alldata$year) found in the data one
    #              extra column is created with the 1 if occurring and 0 if not.
    #              Multiplication with adddata$value creates the matrix with the same nrow(alldata)
    temp1<-as.data.frame(model.matrix(~factor(year)-1,data=alldata)*alldata$value)
    temp2<-cbind(alldatauniq,temp1)
    
    alldatanames<-as.vector(sort(unique(temp2$unique)))
    temp3<-lapply(split(temp2,temp2$unique,drop=TRUE),function(x) colSums(x[,-1],na.rm=TRUE))
    
    # Forth: the resulting list is converted back to a data frame and binded to the meta data
    temp4<-as.data.frame(Reduce(cbind,temp3))
    names(temp4)<-alldatanames
    temp4<-as.data.frame(t(temp4))
    alldata<-subset(merge(newmatrix,temp4,by.x="unique",by.y="row.names"),select=-unique)
    names(alldata)[grepl("[12]",names(alldata))]<-gsub("factor\\(year\\)","",names(alldata)[grepl("[12]",names(alldata))])
    
    
    #     temp3<-lapply(split(temp2,temp2$unique,drop=TRUE),function(x) colSums(x[,-1],na.rm=TRUE))
    # 
    #     # Forth: the resulting list is converted back to a data frame and binded to the meta data
    #     temp4<-as.vector(unlist(temp3))
    #     temp5<-as.data.frame(matrix(temp4,nrow=rok,ncol=nok,byrow=TRUE))
    #     names(temp5)<-years2keep
    #     alldata<-cbind(newmatrix,temp5)
    
    rm(list=c("temp1","temp2","temp3","temp4","newmatrix","nok","rok","rts","alldatauniq"))
    
    
    # measureacronyms --------------------------------------------------------------
    # Keep long text with the exception of 'measure' which is needed to identify
    # if it is an activity data, emissions, emission factor or parameter or other
    # alldata$measurelong<-alldata$measure
    #measureacronyms<-read.csv("metadim_row8measure.txt",stringsAsFactors=FALSE)
    #alldata<-subset(temp1,select=-dummy)
    print("measureacronyms")
    measureacronyms<-read.csv("measures_20150731.txt",stringsAsFactors=FALSE)
    alldata<-merge(alldata,measureacronyms,by.x="measure",by.y="measname")
    
    alldatanosector<-alldata[alldata$sector_number=="-",]
    alldata<-alldata[! alldata$sector_number=="-",]
    
    # Save alldata for later re-use incase of allem or all3 ####
    print("Save alldata")
    stepsdone<-1
    savelist<-c("stepsdone","savelist","alldata","allnotations","allinfos","allmethods")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s1~",figdate,".RData"),rdatallem))
    save(measures,parties,years,notations,classifications,categories,sources,methods,
         targets,options,types,gases,units,sectors,uids,file=rdatmeta)
    write.table(alldata[grepl("^3",alldata$sector_number),],file=paste0(csvfil,"_agri_s1.csv"),sep=",")
    write.table(alldata[grepl("^4",alldata$sector_number),],file=paste0(csvfil,"_lulucs_s1.csv"),sep=",")

    source("curplot.r")
    
}
