# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# FORMATTING OF THE UNIT ######################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
unitforplot<-function(curunit){
if(curunit=="kg/head/yr"){textunit<-"[ kg " ~~ head^{-1}  ~~ yr^{-1} ~"]"} else
if(curunit=="kg/yr"){textunit<-"[ kg " ~~ yr^{-1} ~"]"} else
if(curunit=="kg/day"){textunit<-"[ kg " ~~ day^{-1} ~"]"} else
if(curunit=="h/day"){textunit<-"[ h " ~~ day^{-1} ~"]"} else
if(curunit=="MJ/day"){textunit<-"[ MJ " ~~ day^{-1} ~"]"} else
if(curunit=="t/yr"){textunit<-"[ t " ~~ yr^{-1} ~"]"} else
if(curunit=="kg/year"){textunit<-"[ kg " ~~ yr^{-1} ~"]"} else
if(curunit=="t/year"){textunit<-"[ t " ~~ yr^{-1} ~"]"} else
if(curunit=="kt/year"){textunit<-"[ kt " ~~ yr^{-1} ~"]"} else
if(curunit=="Mt/year"){textunit<-"[ Mt " ~~ yr^{-1} ~"]"} else
if(curunit=="kg/animal"){textunit<-"[ kg " ~~ animal^{-1} ~"]"} else
if(curunit=="kg N/yr"){textunit<-"[ kg N " ~~ yr^{-1} ~"]"} else
if(curunit=="kg N/year"){textunit<-"[ kg N " ~~ yr^{-1} ~"]"} else
if(curunit=="t N/yr"){textunit<-"[ t N " ~~ yr^{-1} ~"]"} else
if(curunit=="t N/year"){textunit<-"[ t N " ~~ yr^{-1} ~"]"} else
if(curunit=="kt N/year"){textunit<-"[ kt N " ~~ yr^{-1} ~"]"} else
if(curunit=="kg/head/year"){textunit<-"[ kg " ~~ head^{-1}  ~~ yr^{-1} ~"]"} else
if(curunit=="kg N/head/year"){textunit<-"[ kg N " ~~ head^{-1}  ~~ yr^{-1} ~"]"} else
if(curunit=="kg dm/head/day"){textunit<-"[ kg dm " ~~ head^{-1}  ~~ day^{-1} ~"]"} else
if(curunit=="t dm/ha"){textunit<-"[ t dm " ~~ ha^{-1} ~"]"} else
if(curunit=="t CO2-C/t"){textunit<-"[ t CO"[2]*"-C" ~~ t^{-1} ~"]"} else
if(curunit=="MJ/head/day"){textunit<-"[ MJ " ~~ head^{-1}  ~~ day^{-1} ~"]"} else
if(curunit=="kg N2O-N/kg N"){textunit<-expression(" [kg N"[2]*"O (kg N)"^'-1'*']')} else
if(curunit=="kg N2O/kg N handled"){textunit<-expression(" [kg N"[2]*"O (kg N handled)"^'-1'*']')} else
if(curunit=="kg N2O/kg N"){textunit<-expression(" [kg N"[2]*"O (kg N)"^'-1'*']')} else
if(curunit=="kg N2O-N/ha"){textunit<-expression(" [kg N"[2]*"O (ha)"^'-1'*']')} else
if(curunit=="kg N2O/head/year"){textunit<-expression("[ kg N"[2]*"O head"^'-1'*"year"^'-1'*']')} else
if(curunit=="kt C"){textunit<-"[ kt C ]"} else
if(curunit=="Mt C"){textunit<-"[ Mt C ]"} else
if(curunit=="kt DC"){textunit<-"[ kt DC ]"} else
if(curunit=="Mt DC"){textunit<-"[ Mt DC ]"} else
if(curunit=="kg/t dm"){textunit<-expression("[ kg (t dm)"^'-1'*']')} else 
if(curunit=="t/t dm"){textunit<-expression("[ t (t dm)"^'-1'*']')} else
if(curunit=="t/ha"){textunit<-expression("[ t (ha)"^'-1'*']')} else
if(curunit=="kg dm"){textunit<-expression("[ kg dm ]")} else
if(curunit=="t dm"){textunit<-expression("[ t dm ]")} else
if(curunit=="kt dm"){textunit<-expression("[ kt dm ]")} else
if(curunit=="Gg"){textunit<-"[ Gg ]"} else
if(curunit=="kg"){textunit<-"[ kg ]"} else
if(curunit=="t"){textunit<-"[ t ]"} else
if(curunit=="1000 metric t"){textunit<-"[ 1000 metric t ]"} else
if(curunit=="kt"){textunit<-"[ kt ]"} else
if(curunit=="ha"){textunit<-"[ ha ]"} else
if(curunit=="kha"){textunit<-"[ kha ]"} else
if(curunit=="Mio ha"){textunit<-"[ Mio ha ]"} else
if(curunit=="ha/year"){textunit<-"[ ha " ~~ yr^{-1} ~"]"} else
if(curunit=="kha/year"){textunit<-"[ kha " ~~ yr^{-1} ~"]"} else
if(curunit=="Mha/year"){textunit<-"[ Mha " ~~ yr^{-1} ~"]"} else
if(curunit=="1000 m^3"){textunit<-"[ 1000 m"^{3} ~~ "]"} else
if(curunit=="g/m^2"){textunit<-"[ g m"^{2} ~~ "]"} else
if(curunit=="10^6 m^3"){textunit<-"[ 10"^{6} ~ "m"^{3} ~~ "]"} else
if(curunit=="10^9m^2/year"){textunit<-"[ 10"^{9} ~ "m"^{2} ~ "yr"^{-1} ~ "]"} else
if(curunit=="m^3/kg VS"){textunit<-"[ m"^{3} ~ "(kg VS)"^{-1} ~ "]"} else
if(curunit=="Mt"){textunit<-"[ Mt ]"} else
if(curunit=="Mg"){textunit<-"[ Mg ]"} else
if(curunit=="kt CO2 equivalent"){textunit<-"[ kt" ~ CO[2] ~ "eq ]"} else
if(curunit=="Mt CO2 equivalent"){textunit<-"[ Mt" ~ CO[2] ~ "eq ]"} else
if(curunit=="1000s"){textunit<-"[ 1000s" ~"]"} else
if(curunit=="TJ"){textunit<-"[ TJ" ~"]"} else
if(curunit=="1000 TJ"){textunit<-"[ 1000 TJ" ~"]"} else
if(curunit=="kMt"){textunit<-"[ kMt" ~"]"} else
if(curunit=="Mio"){textunit<-"[ Mio" ~"]"} else
if(curunit=="%"){textunit<-"[ %" ~"]"} else
if(curunit==""){textunit<-""} else stop(curunit)
return(textunit)
}