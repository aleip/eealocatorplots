seltype<-"Total (with LULUCF)"
seltype<-"" #with indirect
selclass<-"Total (with LULUCF  with indirect)    " #with indirect
selclass<-"Total (without LULUCF  with indirect)    " #with indirect

selclass<-"Sectors/Totals" #without indirect
seltype<-"Total (without LULUCF)" #without indirect


mytotal<-alltotals[alltotals$party=="EU28" & alltotals$type==seltype &
                       alltotals$classification==selclass &
                       alltotals$gas=="Aggregate GHGs",lastyear]


Enteric fermentation is the largest 
`100*agrigeneu[agrigeneu$sector_number=="3.A",lastyear]/mytotal` of emissions.
