options(warn=2)
options(error=NULL) 

#addparentvalues<-function(addparentanimal){
allswines<-unique(addparentanimal$category[grepl("^3.A.3",addparentanimal$sector_number)])
allsheeps<-unique(addparentanimal$category[grepl("^3.A.2",addparentanimal$sector_number)])
#if(parent=="Sheep") childs<-allsheeps[!allsheeps%in%parent]

#parent<-"Sheep"
for(parent in sheepswine){
    cat("\n", parent)
    if(parent=="Sheep")            childs<-allsheeps[!allsheeps%in%parent]
    if(parent=="Swine")            childs<-allswines[!allswines%in%parent]
    if(parent=="Cattle")           childs<-allcattle[!allcattle%in%parent]
    if(parent=="Dairy Cattle")     childs<-alldairy[!alldairy%in%parent]
    if(parent=="Non-Dairy Cattle") childs<-allnondairy[!allnondairy%in%parent]
    if(parent=="Sheep")            subcat<-2
    if(parent=="Swine")            subcat<-3
    if(parent=="Cattle")           subcat<-1
    if(parent=="Dairy Cattle")     subcat<-1
    if(parent=="Non-Dairy Cattle") subcat<-1
    #print(paste0("1",parent,paste(childs,collapse="-")))
    
    #curcatego<-"^3.B.1"
    curcategos<-c("^3.A","^3.B.1","^3.B.2")
    for(curcatego in curcategos){
        #cat(" ",curcatego)
        if(curcatego=="^3.A")   {curmeasures<-measta2weight;curgas<-"CH4"}
        if(curcatego=="^3.B.1") {curmeasures<-meastb12weight;curgas<-"CH4"}
        if(curcatego=="^3.B.2") {curmeasures<-meastb22weight;curgas<-"N2O"}
        
        #print(paste0("2",curcatego))
        
        #curmeasty<-"VSEXC"
        for(curmeasty in curmeasures){
            #cat(" ",curmeasty)
            #print(paste0("3",curmeasty))
            targets<-""
            if(curmeasty=="CLIMA") targets<-c("Cool","Temperate","Warm")
            for(targ in targets){
                #cat(" ",targ)
                selectiontar<-addparentanimal$meastype==curmeasty & addparentanimal$target==targ & grepl(paste0(curcatego,".",subcat),addparentanimal$sector_number) 
                tarstart<-addparentanimal[selectiontar,]
                tarstart<-tarstart[!tarstart$party=="EU28",]
                #View(addparentanimal[selection,])
                #save(addparentanimal,file="addparentanimal.RData")
                countrieschilds<-unique(tarstart$party[tarstart$category%in%childs])
                countriesparent<-unique(tarstart$party[tarstart$category%in%parent])
                countriesmissig<-countrieschilds[!countrieschilds%in%countriesparent]
                
                sources<-""
                if(curmeasty=="CLIMA") sources<-c(sources,manureSystems)
                for(sour in sources){                
                    #cat(" ",sour)
                    parentuid<-unique(tarstart$variableUID[tarstart$party%in%countriesparent & tarstart$category==parent & tarstart$source==sour])
                    #if(length(unique(allagri$category[allagri$variableUID%in%parentuid]))) parentuid<-parentuid[!grepl("Other [cC]attle.[DN]",unique(allagri$category[allagri$variableUID%in%parentuid]))]    
                    if(length(parentuid)>1) View(tarstart[tarstart$party%in%countriesparent & tarstart$category==parent & tarstart$source==sour,])
                    if(length(parentuid)==0) parentuid<-newuid(sector = paste0(curcatego,".",subcat),categ = parent,meast = curmeasty,units = "",metho = "",sourc = sour,targe = targ,opti = "",gasun = curgas)
                    #if(length(parentuid)==0) countriesmissig<-""
                    #View(tarstart[tarstart$party%in%countriesparent & tarstart$category==parent,])
                    selection<-tarstart$party%in%countriesmissig & tarstart$category%in%childs & tarstart$source==sour
                    agrimissing<-tarstart[selection,]
                    #View(agrimissing)
                    #ms<-"ISL"
                    for(ms in countriesmissig){
                        #cat(" ",ms)
                        #if(curmeasty=="WEIGHT" & parent=="Non-Dairy Cattle"&ms=="HRV")stop()
                        #print(paste0("4",ms))
                        seltemp<-agrimissing$party==ms
                        if(length(seltemp)>0){
                            curchilds<-agrimissing$category[seltemp]
                            newline<-agrimissing[seltemp & agrimissing$category==curchilds[1],]
                            if(nrow(newline)>0) {
                                # Childs must be all for which population data are availabe
                                # otherwise a bias will be generated
                                # ... but setting back to all 'childs' creates problems if some childs are truly not used
                                # curchilds<-childs
                                tmpp<-vector(length = length(years))
                                tmps<-vector(length = length(years))
                                newline$category<-parent
                                newline$notation<-"eugirp"
                                noparent<-FALSE
                                if(length(parentuid)>0) newline$variableUID<-parentuid
                                #at<-curchilds[1]
                                for(at in curchilds){
                                    #print(paste0("5",at))
                                    tmpval<-agrimissing[seltemp & agrimissing$category==at,years]
                                    tmpval[is.nan(tmpval)]<-NA
                                    if(ms=="POL"&parent=="Non-Dairy Cattle") {curat<-"Non-Dairy Cattle"}else{curat<-at}
                                    tmppop<-unique(addparentanimal[addparentanimal$party==ms & addparentanimal$category==curat & grepl("^3.A",addparentanimal$sector_number) & addparentanimal$meastype=="POP",years])
                                    #tmppop<-unique(addparentanimal[addparentanimal$party==ms & addparentanimal$category==at & grepl("^3.A",addparentanimal$sector_number) & addparentanimal$meastype=="POP",years])
                                    #if(curmeasty=="WEIGHT" & at=="Dairy Cattle"&ms=="GBK")stop()
                                    #if(curmeasty=="WEIGHT" & at=="Non-Dairy Cattle"&ms=="HRV")stop()
                                    
                                    nopop<-nrow(tmppop)==0
                                    noval<-nrow(tmpval)==0
                                    if(!nopop)nopop<-sum(tmppop,na.rm=TRUE)==0
                                    if(!noval)noval<-sum(tmpval,na.rm=TRUE)==0
                                    
                                    if(!nopop & noval){
                                        # Aggregate value for parent cannot be calculated
                                        newline[,years]<-NA
                                        newline$notation<-paste0("NE - values for ",at," are missing")
                                        noparent<-TRUE
                                        #stop(newline$notation)
                                    }else 
                                        if(nopop & !noval){
                                        stop(paste0("There are values for parameter",curmeasty," but no population data of ",at," in ",ms," for ",curcatego))
                                    }else if(nopop & noval){
                                        #... Otherwise this child does not exist and does not have to be considered
                                        #print(paste0("No population data or values for ",curmeasty," of ",at," in ",ms))
                                    }else if(!nopop & !noval){
                                        tmpp<-tmpp + tmpval * tmppop
                                        tmps<-tmps + tmppop
                                        noparent<-FALSE
                                    }
                                    #print(tmpp)
                                    #print(tmps)
                                }
                                if(!noparent){
                                    tmpn<-tmpp/tmps
                                    newline[years]<-tmpn
                                }
                                addparentanimal<-rbind(addparentanimal,newline)
                                #if(ms=="POL"&curmeasty=="IEF"&curcatego=="^3.B.1"&parent=="Non-Dairy Cattle") stop()
                            }
                        }
                    }
                }
            }
        }
    }
}
