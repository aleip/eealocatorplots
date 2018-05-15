# Jump over the generation of 'alldata' in case this has already been done


if(generatealldata==1){
    source("eealocator_generate_rdata.r")
    
    # --- Eleminate the 'no' texts .... they do not add information ####

    # Select gases ####
    gases2keep<-c("Aggregate GHGs","CH4","no gas","CO2","N2O","NMVOC")
    select<-alldata$gas %in% gases2keep
    othergases<-read.table("plots_sec2othergases.txt")
    select2<-alldata$variableUID%in%unlist(othergases)
    select<-select | select2
    alldata<-alldata[select,]
    gases<-as.character(unique(alldata$gas))
    
    # Select years ####
    #years2delete<-as.vector(as.character(allyears[!(allyears %in% years2keep)]))
    select<-alldata$year %in% years2keep
    alldata<-alldata[select,]
    years<-as.character(unique(alldata$year))
    years<-sort(years)
    
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
    notationkeys<-"[CS,NO,NE,IRL,NA,D,T1,T2]"
    allnotations<-alldata[grepl(notationkeys,alldata$notation),]
    alldata<-alldata[! grepl(notationkeys,alldata$notation),]
    allnotations<-simplifytestmatrix(allnotations,"year",years2keep)
    alldata<-unique(alldata)
    

    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Generate data frame with one columns per year ----
    # and the value in the corresponding column ---
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    print("Generate data frame with one columns per year")
    #startt<-Sys.time()
    cols2leave<-paste(names(alldata)[!names(alldata)%in%c("year","value")],collapse="+")
    arrange<-as.formula(paste(cols2leave,"~ year"))
    
    #todo!! Convert values immediately to numeric maybe before in the 'value' column
    alldata<-dcast(alldata,arrange,value.var="value")
    #finished<-Sys.time()
    #print(finished-startt)
    
    
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
    savelist<-c("stepsdone","savelist","alldata","allnotations","allinfos","allmethods","alldatanosector", "country4sub")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s1~",figdate,".RData"),rdatallem))
    save(measures,parties,years,notations,classifications,categories,sources,methods,
         targets,options,types,gases,units,sectors,uids,file=rdatmeta)
    write.table(alldata[grepl("^3",alldata$sector_number),],file=paste0(csvfil,"_agri_s1.csv"),sep=",")
    write.table(alldata[grepl("^4",alldata$sector_number),],file=paste0(csvfil,"_lulucs_s1.csv"),sep=",")

    source("curplot.r")
    
}
