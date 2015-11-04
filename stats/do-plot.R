

interval=paste0(year,"-01-10/",year,"-01-24")

openPDF(paste0(StatsOutFolder,"HTA_",year,"_10-24jan"))
for(HTA_name in HTA_names)
{
  plot(conso_HTA_xts[interval,HTA_name],
       type="l",
       main=paste("Consommation du depart:",HTA_name))
}
closePDF()

openPDF(paste0(StatsOutFolder,"PS_",year,"_10-24jan"))
for(PS_name in PS_names)
{
  plot(conso_HTA_xts[interval,PS_name],
       type="l",
       main=paste("Consommation du poste source:",PS_name))
}
closePDF()


openPDF(paste0(StatsOutFolder,"HTA_",year))
for(HTA_name in HTA_names)
{
  plot(period.apply(conso_HTA_xts[,HTA_name],endpoints(conso_HTA_xts[,1],"days"), mean),
       type="l",
       main=paste("Consommation du depart:",HTA_name))
}
closePDF()

openPDF(paste0(StatsOutFolder,"PS_",year))
for(PS_name in PS_names)
{
  plot(period.apply(conso_HTA_xts[,PS_name],endpoints(conso_HTA_xts[,1],"days"), mean),
       type="l",
       main=paste("Consommation du poste source:",PS_name))
}
closePDF()

