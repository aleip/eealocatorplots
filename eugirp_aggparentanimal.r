addparentanimal<-allagri160
options(warn=2)
options(error=recover) 
#addparentvalues<-function(addparentanimal){
allswines<-unique(addparentanimal$category[grepl("^3.A.3",addparentanimal$sector_number)])
allsheeps<-unique(addparentanimal$category[grepl("^3.A.2",addparentanimal$sector_number)])
#if(parent=="Sheep") childs<-allsheeps[!allsheeps%in%parent]

#parent<-"Sheep"
for(parent in sheepswine){
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
    #print(paste0("1",parent))
    
    #curcatego<-"^3.B.1"
    curcategos<-c("^3.A","^3.B.1","^3.B.2")
    for(curcatego in curcategos){
        if(curcatego=="^3.A")   curmeasures<-measta2weight
        if(curcatego=="^3.B.1") curmeasures<-meastb12weight
        if(curcatego=="^3.B.2") curmeasures<-meastb22weight
        #print(paste0("2",curcatego))
        
        #curmeasty<-"VSEXC"
        for(curmeasty in curmeasures){
            #print(paste0("3",curmeasty))
            targets<-""
            if(curmeasty=="CLIMA") targets<-c("Cool","Temperate","Warm")
            for(targ in targets){
                selectiontar<-addparentanimal$meastype==curmeasty & addparentanimal$target==targ & grepl(paste0(curcatego,".",subcat),addparentanimal$sector_number) 
                tarstart<-addparentanimal[selectiontar,]
                tarstart<-tarstart[!tarstart$party=="EU28",]
                #View(addparentanimal[selection,])
                countrieschilds<-unique(tarstart$party[tarstart$category%in%childs])
                countriesparent<-unique(tarstart$party[tarstart$category%in%parent])
                countriesmissig<-countrieschilds[!countrieschilds%in%countriesparent]
                
                sources<-""
                if(curmeasty=="CLIMA") sources<-c(sources,manureSystems)
                for(sour in sources){                
                    parentuid<-unique(tarstart$variableUID[tarstart$party%in%countriesparent & tarstart$category==parent & tarstart$source==sour])
                    if(length(parentuid)>1) View(tarstart[tarstart$party%in%countriesparent & tarstart$category==parent & tarstart$source==sour,])
                    if(length(parentuid)==0) parentuid<-newuid()
                    #if(length(parentuid)==0) countriesmissig<-""
                    #View(tarstart[tarstart$party%in%countriesparent & tarstart$category==parent,])
                    selection<-tarstart$party%in%countriesmissig & tarstart$category%in%childs & tarstart$source==sour
                    agrimissing<-tarstart[selection,]
                    #View(agrimissing)
                    #ms<-"IS"
                    for(ms in countriesmissig){
                        #print(paste0("4",ms))
                        seltemp<-agrimissing$party==ms
                        if(length(seltemp)>0){
                            curchilds<-agrimissing$category[seltemp]
                            tmpp<-vector(length = length(years))
                            tmps<-vector(length = length(years))
                            tmpmin<-rep(NA,length(years))
                            tmpmax<-vector(length = length(years))
                            newline<-agrimissing[seltemp & agrimissing$category==curchilds[1],]
                            if(nrow(newline)>0) {
                                newline$category<-parent
                                if(length(parentuid)>0) newline$variableUID<-parentuid
                                #at<-curchilds[1]
                                for(at in curchilds){
                                    #print(paste0("5",at))
                                    tmp1<-agrimissing[seltemp & agrimissing$category==at,years]
                                    tmp1d<-data.frame(tmp1)
                                    tmp1d[2,]<-tmpmin
                                    tmpmin<-apply(tmp1d,2,min,na.rm=TRUE)
                                    tmpmax<-apply(tmp1d,2,max,na.rm=TRUE)
                                    tmp2<-unique(addparentanimal[addparentanimal$party==ms & addparentanimal$category==at & grepl("^3.A",addparentanimal$sector_number) & addparentanimal$meastype=="POP",years])
                                    if(ms=="PL"&at=="Other Cattle.Non-dairy cattle") tmp2<-unique(addparentanimal[addparentanimal$party==ms & addparentanimal$category=="Non-Dairy Cattle" & grepl("^3.A",addparentanimal$sector_number) & addparentanimal$meastype=="POP",years])
                                    
                                    tmp1[is.nan(tmp1)]<-NA
                                    if(!is.na(sum(tmp1,na.rm=TRUE))){
                                        if(!is.na(sum(tmp2))){
                                            tmpp<-tmpp + tmp1 * tmp2
                                            tmps<-tmps + tmp2
                                        }else{stop(paste0("Population missing ",ms,at,curmeasty,curcatego))}
                                    }else{stop(paste0(curmeasty," missing ",ms,at,curcatego))}
                                }
                                tmpn<-tmpp/tmps
                                newline[years]<-tmpn
                                addparentanimal<-rbind(addparentanimal,newline)
                            }
                        }
                    }
                }
            }
        }
    }
}
