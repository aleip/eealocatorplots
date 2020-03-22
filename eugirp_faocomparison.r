library(plyr)


faocategs<-c("Burning_crop_residues","Burning_Savanna","Crop_residues",
             "Cultivated_Organic_Soils","Energy","Enteric_Fermentation",
             "Manure_applied_to_soils","Manure_left_on_pasture",
             "Manure_Management","Rice_Cultivation",
             "Synthetic_Fertilizers")

faocontent<-c("Enteric Fermentation","Manure Management","Rice Cultivation",
              "Synthetic Fertilizers","Manure applied to Soils","Manure left on Pasture",
              "Crop Residues","Cultivation of Organic Soils","Burning - Crop Residues",
              "Agriculture Total")

faosites<-c("GE","GM","GRC","GY","GU","GP","GA","GV","GBK","GT")
#faocountries<-faocountries[faocountries$cAc!="",]
#faocountries<-read.csv(file=paste0(invloc,"/fao/faocountries.csv"))
faocountries<-read.csv("faocountries.csv",header = TRUE)
faocountries<-subset(faocountries,select=-X)
names(faocountries)[1:2] <- c("Area","Area.Code")
faofile<-paste0(faodir,"faodata_",cursubm,".RData")
#print(file.exists(faofile))
recalc_faofile <- 1
if(!file.exists(faofile) | recalc_faofile == 1){
    yr <- as.numeric(years[length(years)]) + 2
    temp = list.files(pattern="Emissions.*.csv",path=paste0(faodir, yr))
    lastdownload<-max(as.numeric(Sys.Date()-as.Date(file.mtime(paste0(faodir,"/",yr,"/",temp)))))
    #print(lastdownload)
    if(is.na(lastdownload) | lastdownload>30){
      for (i in faocontent){
            #print(paste0("http://www.fao.org/faostat/en/#data/",faosites[i]))
            #remDr<-remoteDriver(browserName = "firefox",remoteServerAddr = "localhost", port = 4567)
            #remDr$open(silent = TRUE)
            #remDr$navigate(paste0("http://www.fao.org/faostat/en/#data/",faosites[i]))
            #Sys.sleep(5)
            #readline(prompt=paste0("Download 'Europe' from the Bulk Downloads on the right side.\n",
            #                       "Save the *csv file into the folder: ",faodir,".\n",
            #                       "Then press [enter] to continue."))
            #remDr$close()
        print(i)
        i1 <- gsub(" ", "_", i)
        if(i1 == "Cultivation_of_Organic_Soils") i1 <- "Cultivated_Organic_Soils"
        if(i1 == "Burning_-_Crop_Residues") i1 <- "Burning_crop_residues"
        download.file(paste0("http://fenixservices.fao.org/faostat/static/bulkdownloads/Emissions_Agriculture_", i1, "_E_Europe.zip"),
                      paste0(faodir,"Emissions_Agriculture_", i1, "_E_Europe.zip"))
        unzip(paste0(faodir,"Emissions_Agriculture_", i1, "_E_Europe.zip"), 
              exdir = paste0(faodir,"/", yr), overwrite = TRUE)
        file.remove(paste0(faodir,"Emissions_Agriculture_", i1, "_E_Europe.zip"))
      }
    }
    
    
    # Read all FAO files ####
    # See different options here http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r
    temp = list.files(pattern="Emissions.*.csv",path=paste0(faodir, "/", yr))
    faodata = lapply(paste0(faodir, "/", yr, "/", temp), read.csv,header=TRUE)
    
    # Clean up files so that they have the same number of cols (=> years) ####
    faoyears<-paste0("Y",yearsnum)
    years2eliminate<-c(1950:1989,2030,2050)
    faodata<-lapply(faodata,function(x) x[,!names(x) %in% c(paste0("Y",years2eliminate),paste0("Y",years2eliminate,"F"), paste0("Y",years2eliminate,"N"), paste0("Y",years,"N"))])
    
    fillyears<-function(D){
        lastyears<-c("Y2012","Y2012F","Y2013","Y2013F")
        lastyears<-c(paste0("Y",2012:years[length(years)]),paste0("Y",2012:years[length(years)],"F"))
        for(lastyear in lastyears){
            if(!lastyear%in%names(D)){
                D[,lastyear]<-NA 
            }
        }
        return(D)
    }
    
    #faodata<-lapply(myfiles1,function(x) fillyears(x))
    faodata<-lapply(faodata,function(x) fillyears(x))
    temp2<-faodata
    nrowstotal<-sum(unlist(lapply(temp2,function(x) nrow(x))))
    faodata<-Reduce(rbind.fill,faodata)
    temp3<-faodata
    
    # Clean up faodata (countries) ####
    faodata<-temp3
    #countrycode<-c("Country","Country.Code")
    countrycode<-c("Area","Area.Code")
    #faocountries<-unique(faodata[,countrycode])
    faodata<-merge(faocountries,faodata,by = countrycode)
    newnames<-names(faodata)[!names(faodata)%in%countrycode]
    faodata<-faodata[faodata$party!="",newnames]
    temp4<-faodata
    
    # Create links to UNFCCC ####
    faodata<-temp4
    createlinks<-1
    elementcode<-c("Element","Element.Code")
    itemcode<-c("Item","Item.Code")
    
    # 2018: Addapting "Elements" and "Items"
    revalue(faodata$Item, c("Nutrient nitrogen N (total)" = "Nitrogen Fertilizers (N total nutrients)")) -> faodata$Item
    revalue(faodata$Element, c("Agricultural Use" = "Consumption")) -> faodata$Element
    revalue(faodata$Element, c("Agricultural Use in nutrients" = "Consumption in nutrients")) -> faodata$Element
    temp4<-faodata

    if(createlinks){
        faodata<-temp4
        # Clean up files so that they have the same number of cols (=> elements/measures) 
        faoelements<-(faodata[,elementcode])
        faoelements<-unique(faoelements)
        write.csv(faoelements,file=paste0(faodir,"faoelements.csv"))
        
        agrimets<-unique(allagri[,c("meastype","measure",metafields)])
        o<-order(agrimets$meastype)
        agrimets<-agrimets[o,]
        View(agrimets)
        write.csv(agrimets,file=paste0(faodir,"agrimets.csv"))
        
        # Clean up files so that they have the same number of cols (=> Items/measures)
        faoitems<-(faodata[,itemcode])
        faoitems<-unique(faoitems)
        write.csv(faoitems,file=paste0(faodir,"faoitems.csv"))
        
        agrisects<-unique(allagri[,c(sectfields)])
        o<-order(agrisects$sector_number)
        agrisects<-agrisects[o,]
        View(agrisects)
        write.csv(agrisects,file=paste0(faodir,"agrisects.csv"))
    }
    faocontinue <- 1
    if(faocontinue==1){
        faodata<-temp4
        itemlink<-read.csv(paste0(faodir,"faoitems_link.csv"))
        elementlink<-read.csv(paste0(faodir,"faoelements_link.csv"))
        faodata<-merge(itemlink,faodata,by=itemcode)
        faodata<-merge(elementlink,faodata,by=elementcode)
        # Eliminate un-necessary items and elements - here energy and savanna
        selection<-faodata[,"sector_number.y"]%in%c("x","y","z")
        faodata<-faodata[!selection,]
        # 
        selection<-faodata[,"sector_number.x"]%in%c("")
        faodata<-faodata[!selection,]
        # Remove data in CO2eq 
        selection<-faodata$gas=="" & (faodata$meastype=="EM" | faodata$meastype=="IEF")
        faodata<-faodata[!selection,]
        # Items that are too specific (individual crops for crop residue burning or specific/aggregate animal types)
        selection<-faodata[,"measure"]%in%c("x","y","z")
        faodata<-faodata[!selection,]
        # Applied manure differentiated by animal type - use only 'All animals'
        selection<-faodata[,"sector_number.x"]=="3.D.1.2" & faodata$Item!="All Animals"
        faodata<-faodata[!selection,]
        selection<-faodata[,"sector_number.x"]=="3.D.1.3" & faodata$Item!="All Animals"
        faodata<-faodata[!selection,]
        temp5<-faodata
        
        #sector_number included in both elements and items need to be checked
        faodata<-temp5
        sectnrs<-c("sector_number.x","sector_number.y")
        faodata[,"sector_number.x"]<-as.character(faodata[,"sector_number.x"])
        faodata[,"sector_number.y"]<-as.character(faodata[,"sector_number.y"])
        nn<-c(sectnrs,names(faodata)[!names(faodata)%in%sectnrs])
        
        combinesectoranimal<-function(cursect){
            a<-cursect[1]
            b<-gsub("3.A","",cursect[2])
            c<-paste0(a,b)
            d<-""
            return(list(c,d))
        }
        
        cursect<-"3.B.2"
        selection<-faodata[,"sector_number.x"]!=faodata[,"sector_number.y"] & faodata[,"sector_number.x"]==cursect
        faodata[selection,sectnrs]<-Reduce(rbind,lapply(c(1:sum(selection)),function(x) Reduce(cbind,combinesectoranimal(faodata[selection,sectnrs][x,]))))
        cursect<-"3.B.1"
        selection<-faodata[,"sector_number.x"]!=faodata[,"sector_number.y"] & faodata[,"sector_number.x"]==cursect
        faodata[selection,sectnrs]<-Reduce(rbind,lapply(c(1:sum(selection)),function(x) Reduce(cbind,combinesectoranimal(faodata[selection,sectnrs][x,]))))
        cursect<-"3.A"
        selection<-faodata[,"sector_number.x"]!=faodata[,"sector_number.y"] & faodata[,"sector_number.x"]==cursect
        faodata[selection,"sector_number.x"]<-faodata[selection,"sector_number.y"]
        
        selection<-faodata[,"sector_number.x"]!="3.C" & faodata$Item=="Rice, paddy"
        faodata<-faodata[!selection,]
        
        selection<-(faodata[,"sector_number.x"]=="3.D.2" | grepl("3.B.2.5",faodata[,"sector_number.x"])) & 
            grepl("3.A",faodata[,"sector_number.y"])& faodata$Item!="All Animals"
        faodata<-faodata[!selection,]
        
        selection<-grepl("3.D.1.[234]$",faodata[,"sector_number.x"]) | grepl("3.D.2$",faodata[,"sector_number.x"]) |
            grepl("3.B.2.5",faodata[,"sector_number.x"])
        faodata[selection,"sector_number.y"]<-faodata[selection,"sector_number.x"]
        
        selection<-faodata[,"sector_number.x"]!=faodata[,"sector_number.y"] & faodata[,"sector_number.y"]!=""
        if(sum(selection>0)){
            sect<-subset(faodata[selection,],select=sectnrs)
            stop("Still problems with the sector numbers!")
        }
        temp6<-faodata
        
        
        #category included in both elements and items need to be checked
        faodata<-temp6
        catnrs<-c("category.x","category.y")
        faodata[,"category.x"]<-as.character(faodata[,"category.x"])
        faodata[,"category.y"]<-as.character(faodata[,"category.y"])
        nn<-c(catnrs,names(faodata)[!names(faodata)%in%catnrs])
        
        curcat<-c("Farming","")
        selection<-faodata[,"sector_number.x"]=="3.D.2" & faodata[,"category.x"]%in%curcat
        faodata[selection,"category.y"]<-faodata[selection,"category.x"]
        
        selection<-faodata[,"category.x"]!=faodata[,"category.y"] & faodata[,"category.x"]%in%curcat & 
            faodata[,"category.y"]!="" & grepl("3.[AB]",faodata[,"sector_number.x"])
        faodata[selection,"category.x"]<-faodata[selection,"category.y"]
        #if(sum(selection>0)){stop("Still problems with the categories!")}
        temp7<-faodata
        
        faodata<-temp7
        nn<-names(faodata)
        nn<-gsub(".x","",nn)
        names(faodata)<-nn
        doty<-nn[grepl("\\.y",nn)]
        codes<-nn[grepl("Code",nn)]
        nometa<-metafields[!metafields%in%c("type","source")]
        nnfirst<-c("sector_number","category","Element","Item","type","Unit")
        nn<-nn[!nn %in% c(doty,codes,nometa)]
        nn<-c(nnfirst,nn[!nn%in%nnfirst])
        
        selection<-faodata$category=="Crop Residues" & faodata$Item=="Rice, paddy"
        faodata<-faodata[!selection,]
        itemcat<-unique(faodata[,c("Item","category","target")])
        
        o<-order(faodata$sector_number,faodata$category)
        faodata<-faodata[o,nn]
        faodata$source<-""
        
        selection<-faodata$sector_number=="" | faodata$measure==""
        if(sum(selection)>0) View(faodata[selection,])
        temp8<-faodata
        
        # Sum up indirect emissions from managed soils
        faodata<-temp8
        selection<-faodata$sector_number=="3.D.2"
        agglist<-list(faodata[selection,c("party")])
        aggvals<-faodata[selection,faoyears]
        temp9<-aggregate(x = aggvals ,by = agglist, sum, na.rm=TRUE)
        names(temp9)<-c("party",faoyears)
        
        newfields<-c(faoyears,paste0(faoyears,"F"),"party","Element","Item")
        orifields<-names(faodata)
        keepfilds<-orifields[!orifields%in%newfields]
        beforemer<-orifields[!orifields%in%c(faoyears,"party")]
        keepvalus<-unique(faodata[selection,keepfilds])
        agruemp<-as.data.frame(matrix(rep("",ncol(faodata)),nrow=1,ncol=ncol(faodata)),stringsAsFactors = FALSE)
        names(agruemp)<-names(faodata)
        agruemp[,faoyears]<-rep(0,length(years))
        agruemp[,keepfilds]<-keepvalus
        agruemp$Element<-"Indirect emissions (N2O)"
        agruemp$Item<-"Indirect N2O Emissions From Managed Soils"
        agruemp[,paste0(faoyears,"F")]<-"EUGIRP"
        agruemp<-agruemp[,beforemer]
        agruemp<-merge(temp9,agruemp)[,orifields]
        
        faodata<-faodata[!selection,]
        faodata<-rbind(faodata,agruemp)
        
        # There is no sum of all animals in UNFCCC
        selection<-faodata$meastype=="POP"&faodata$category=="Farming"
        faodata<-faodata[!selection,]
        
        # Do not check 'cattle' (only Dairy and Non-Dairy Cattle
        selection<-faodata$category=="Cattle"
        faodata<-faodata[!selection,]
        temp9<-faodata
        
    }
    
    #faomeasures<-unique(faodata[,-which(names(faodata)%in%faoyears)])
    #faomeasures<-unique(faomeasures[,-which(names(faomeasures)%in%paste0(faoyears,"F"))])
    
    
    fnames<-names(faodata)
    fyears<-which(names(faodata)%in%paste0("Y",years))
    fnames[fyears]<-years
    names(faodata)<-fnames
    
    faodata<-filldf(faodata,names(allagri))
    faodata<-faodata[,names(allagri)]
    faodata<-convert2char(faodata)
    
    # Merge 3.D.1.1. activity data
    # Until 2001 category: Consumption then Consumption in nutrients
    selection<-faodata$sector_number=="3.D.1.1" & faodata$meastype=="AD"
    d111<-faodata[selection,]
    if(nrow(d111)>0){
      faodata<-faodata[!selection,]
      d111agg<-aggregate(d111[,years],by = list(d111$party),sum,na.rm=TRUE)
      d111agg<-unique(merge(d111[,-which(names(d111)%in%years)],d111agg,by.x="party",by.y="Group.1",all.x=TRUE))
      faodata<-rbind(faodata,d111agg[,names(faodata)])
    }
    temp10<-faodata
    
    ### Fill some data in allagri
    curagri<-allagri
    # Generate AD data for Pasture, Range and Paddock ####
    # Remove final 'dot' for 'other live
    selection<-grepl("4\\.$",curagri$sector_number)
    curagri$sector_number[selection]<-gsub("4\\.$","4",curagri$sector_number[selection])
    
    # Select all first level animal types
    selection<-grepl("3.B.2..$",curagri$sector_number)& (!grepl("Option",curagri$option)) & grepl("Pasture",curagri$source)
    selection<-selection & (curagri$category=="Cattle" | curagri$category=="Dairy Cattle" | curagri$category=="Non-Dairy Cattle" | curagri$sector_number!="3.B.2.1")
    selection<-selection & (curagri$category=="Sheep" | curagri$sector_number!="3.B.2.2")
    selection<-selection & (curagri$category=="Swine" | curagri$sector_number!="3.B.2.3")
    seluids<-unique(curagri$variableUID[selection])
    t1<-extractuiddata(curagri,uid = seluids[1],c = allcountries,narm=FALSE,noeu=TRUE)
    t1[is.na(t1)]<-0
    for(i in c(2:length(seluids))){
        t2<-extractuiddata(curagri,uid = seluids[i],c = allcountries,narm=FALSE,noeu=TRUE)
        t2[is.na(t2)]<-0
        t1<-t1+t2
    }
    sec<-"3.B.2.5 N2O Emissions per MMS"
    cat<-"Farming"
    gas<-"no gas"
    unit<-"kt N/year"
    sou<-"Pasture  range and paddock"
    mea<-"NEXC"
    uid1<-newuid(sector = sec,categ = cat,meast = mea,units = unit,metho = "",sourc = sou,targe = "",optio = "",gasun = "")
    addallagriout<-add2allagri(t1,sec=sec,cat=cat,gas=gas,unit=unit,sou=sou,tar="",mea=mea,msr="Nitrogen excretion per MMS",
                               uid1,note="pasture total not reported",DATA=curagri,noeu=TRUE)
    curagri<-addallagriout[[1]]
    rowsadded<-addallagriout[[2]]
    
    tempagri<-curagri
    # Group cattle to 'dairy' and 'non-dairy' for all countries ####
    #source("eugirp_cattle.r")
    
    # AD in 3.F for all crops
    selectionad<-grepl("^3.F..$",curagri$sector_number) & curagri$meastype=="AD"
    seluids<-unique(curagri$variableUID[selectionad])
    adt1<-extractuiddata(curagri,uid = seluids[1],c = allcountries,narm=FALSE,noeu=TRUE)
    adt1[is.na(adt1)]<-0
    for(i in c(2:length(seluids))){
        t2<-extractuiddata(curagri,uid = seluids[i],c = allcountries,narm=FALSE,noeu=TRUE)
        t2[is.na(t2)]<-0
        adt1<-adt1+t2
    }
    sec<-"3.F"
    cat<-"Farming"
    gas<-"no gas"
    unit<-"kt dm"
    sou<-""
    mea<-"AD"
    uid2<-newuid(sector = sec,categ = cat,meast = mea,units = unit,metho = "",sourc = sou,targe = "",optio = "",gasun = "")
    addallagriout<-add2allagri(adt1,sec="3.F",cat="3.F",gas="no gas",unit="kt dm",
                               sou="",tar="",mea="AD",msr="Field Burning of Agricultural Residues",
                               uid2,note="field burning AD only reported by crop",DATA=curagri,noeu=TRUE)
    curagri<-addallagriout[[1]]
    rowsadded<-addallagriout[[2]]
    
    allagri<-curagri
    acountry <- sort(c(curcountries[variable==eusubm & value==1]$code3, "NOR"))
    #View(curagri[selection,])
    
    
    # Adding variableUID
    faouids<-unlist(lapply(c(1:nrow(faodata)),function(x) 
        getvariableUID(sec = faodata$sector_number[x],cat = faodata$category[x],
                       mt = faodata$meastype[x],gas = faodata$gas[x])))
    faodata$variableUID<-faouids
    faodata$variableUID[faodata$sector_number=="3.C"&faodata$meastype=="AREA"]<-"BC643791-5E99-423C-B292-8FA3AD614F77"
    faodata$variableUID[faodata$sector_number=="3.B.2.5"&faodata$meastype=="EM"]<-"8332828A-BBF4-4C8B-B6FD-AE2553118257" #Total indirect emissions
    faodata$variableUID[faodata$sector_number=="3.B.2.5 N2O Emissions per MMS"&faodata$meastype=="NEXC"]<-"158363A3-51A4-4420-8C88-045290B9D0DD" #Total MMS without pasture and burning
    
    # Fill missing metadata
    mismeta<-c("unit","classification","type","target","notation","option","method")
    faodata[,mismeta]<-Reduce(rbind,lapply(c(1:nrow(faodata)),function(x) Reduce(cbind,as.character(fillbyvariableUID(col = mismeta,uid = faodata$variableUID[x])))))
    #x<-faodata[,-which(names(faodata)%in%mismeta)]
    #y<-unique(allagri[,c("variableUID",mismeta)])
    #faodata[,mismeta]<-merge(x,y,by="variableUID",all.x=TRUE)
    
    # Ensure that excludeparty is not deleted
    if("GBK" %in% acountry) faodata$party[faodata$party=="GBE"]<-"GBK"
    if("FRK" %in% acountry) faodata$party[faodata$party=="FM"]<-"FRK"
    if("ISL" %in% acountry) faodata$party[faodata$party=="IC"]<-"ISL"
    if("NOR" %in% acountry) faodata$party[faodata$party=="NO"]<-"NOR"
    
    # Calculate EU28 sums
    faodata<-faodata[!faodata$party%in%eu,]
    faodata<-eu28sums(A = faodata,aeu = eusubm,years=years)
    faodata$datasource<-"fao"
    #faodata[faodata==0]<-NA
    faodata<-faodata[!grepl("0.00011221533645398",faodata$variableUID),]
    
    # Fill missing IEFs for EU28
    faounique<-c("meastype","measure","sector_number","category","gas","variableUID")
    faomeas<-(unique(faodata[,faounique]))
    faoem<-faomeas[faomeas$meastype=="EM",]
    
    euiefs<-function(line){
        cat<-line$category
        sec<-line$sector_number
        admeas<-c("AD","POP","AREA")
        #print(line)
        #View(faodata[faodata$category==cat&faodata$sector_number==sec,])
        aduid<-as.character(faomeas$variableUID[faomeas$category==cat&faomeas$sector_number==sec&faomeas$meastype%in%admeas])
        #iefuid<-as.character(faomeas$variableUID[faomeas$category==cat&faomeas$sector_number==sec&faomeas$meastype=="IEF"])
        iefuid<-as.character(agrimeas$variableUID[agrimeas$category==cat&agrimeas$sector_number==sec&agrimeas$meastype=="IEF"])
        if(length(aduid)==0) {
            aduid<-as.character(faomeas$variableUID[faomeas$category==cat&faomeas$sector_number==gsub("B.[12]","A",sec)&faomeas$meastype%in%admeas])
            if(length(aduid)==0) {
                aduid<-"x"
                iefuid<-"x"
            }
        }else if(length(aduid)>1){
            aduid<-"x"
            iefuid<-"x"
        }
        if(length(iefuid)==0) iefuid<-"x"
        print(paste(aduid,iefuid,length(aduid),length(iefuid),sep=" - "))
        return(list(aduid,iefuid))
    }
    filliefs<-function(line){
        uid<-line$variableUID
        aduid<-as.character(line$aduid)
        iefuid<-as.character(line$iefuid)
        #print(iefuid)
        if (line$measure=="Total N excreted"){
            
        }else{
            eu28new<-unique(allagri[allagri$variableUID==iefuid,-which(names(allagri)%in%c("party",years,"datasource","correction","autocorr"))])
            #print(eu28new)
            eu28new<-convert2char(eu28new)
            #print(eu28new)
            eu28new$party<-eusubm
            eu28new$autocorr<-NA
            eu28new$correction<-1
            eu28new$datasource<-"fao"
            em<-faodata[faodata$party==eusubm&faodata$variableUID==uid,years]
            ad<-faodata[faodata$party==eusubm&faodata$variableUID==aduid,years]
            ad[ad==0]<-NA
            ief<-em/ad
            eu28new[,years]<-ief
            eu28new<-eu28new[,names(faodata)]
            #print(eu28new)
            return(eu28new)
        }
    }
    
    faoem[,c("aduid","iefuid")]<-  Reduce(rbind,lapply(1:nrow(faoem),function(x) Reduce(cbind,euiefs(line = faoem[x,]))))
    faoem<-faoem[faoem$aduid!="x"&faoem$iefuid!="x",]
    faoeuiefs<-Reduce(rbind,lapply(1:nrow(faoem),function(x) Reduce(cbind,filliefs(line = faoem[x,]))))
    faoeuiefs<-as.data.frame(faoeuiefs)
    names(faoeuiefs)<-names(faodata)
    faoeuiefs<-convert2char(DF = faoeuiefs)
    faodata<-rbind(faodata,faoeuiefs)
    
    faodata<-convert2num(DF = faodata,cols = years)
    selection<-faodata$meastype=="POP"
    faodata[selection,years]<-faodata[selection,years]/1000
    selection<-faodata$meastype=="AD"&grepl("3.D.1.",faodata$sector_number)
    faodata[selection,years]<-faodata[selection,years]/1000000
    selection<-faodata$measure=="Total N excreted"&faodata$datasource=="fao"
    faodata[selection,years]<-faodata[selection,years]/1000000
    #Convert FAO rice area [ha] to 1000 km2 --> 100*1000
    selection<-faodata$meastype=="AREA"&faodata$datasource=="fao"&grepl("3.C",faodata$sector_number)
    faodata[selection,years]<-faodata[selection,years]/100000
    
    faodata$measure[faodata$measure=="Implied Emission Factor"]<-"Implied emission factor"
    
    save(faodata,file=paste0(faofile))
}else{
  print("FAO data already exists... Loading faofile")
  load(file=faofile, verbose = TRUE)
}

# EU28 -iefs are not yet fully correct (unit!)
faodata<-faodata[!(faodata$party==eusubm&faodata$meastype=="IEF"),]

## Launch comparison plots
# Print plots for 'summable data' 
adempars<-c("AD","EM") #Note also areas can be included here

# runfocus defines type of plot (so far: value, trend, countries..)
# .. however best tested is only 'value' as the other not recently used
runfocus<-"value"
runfocus<-"compare"

# The vector 'datasource' determines which (and how many) 
datasource<-c("nir","fao")

# rundata defines datatype: adem or ief
# adem: activity data, emissions etc
# ief: implied emission factors and other variables expressed per activity unit
rundata<-"ief"
rundata<-"adem"
runfocus<-"value"
datasource <- c("nir","fao")
faodata<-filldf(faodata,names(allagri))
plotdatagenerated<-generateplotdata(rundata = rundata,datasource = datasource,subcountries = eusubm)
plotdata<-plotdatagenerated[[1]]
plotmeas<-plotdatagenerated[[2]]
adddefault<-plotdatagenerated[[3]]
sharesexist<-plotdatagenerated[[4]]

plotmeas<-unique(plotmeas[,-which(names(plotmeas)%in%c("datasource","imeas"))])
plotmeas$imeas<-sapply(1:nrow(plotmeas),function(x) x)

x1<-97;x2<-nrow(plotmeas)
x1<-1;x2<-2
x1<-1;x2<-nrow(plotmeas)
for(imeas in x1:x2){loopoverplots(imeas = imeas,runfocus = runfocus,eusubm = eusubm)}
plotmeas$imeas<-unlist(lapply(c(1:nrow(plotmeas)),function(x) x))
write.table(data.frame("ID"=rownames(plotmeas),plotmeas),file=paste0(plotsdir,"/",rundata,"plots~",curtime(),".csv",collapse=NULL),row.names=FALSE,sep=";",dec=".")

runfocus<-"compare"
x1<-2;x2<-2
x1<-1;x2<-nrow(plotmeas)
for(imeas in x1:x2){plotcomparison(imeas,plotmeas,plotdata,lyear = years[length(years)])}
plotmeas$imeas<-unlist(lapply(c(1:nrow(plotmeas)),function(x) x))
write.table(data.frame("ID"=rownames(plotmeas),plotmeas),file=paste0(plotsdir,"/",rundata,"plots~",curtime(),".csv",collapse=NULL),row.names=FALSE,sep=";",dec=".")

#source("eugirp_prepareplots.r")
