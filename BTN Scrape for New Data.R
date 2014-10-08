library(XML)
library(RCurl)


url=paste("http://www.autoblog.com/category/by-the-numbers/")
doc = htmlTreeParse(url, useInternalNodes = T)
text = xpathSApply(doc, "//*/h2", xmlValue) 
src = xpathApply(doc, "//a[@href]", xmlGetAttr, "href")
links=unlist(src)


links.aolcomments=links[grepl("aol-comments",links)]

links.clean=gsub("#aol-comments","", links.aolcomments)




  url=paste("http://www.autoblog.com",links.clean[1],sep="")
  doc=getURL(url)
  gdrive.dat=substring(doc, regexpr("<iframe frameborder=", doc)[1]+20,regexpr("<iframe frameborder=", doc)[1]+200)


gdrive.dat.1=substring(gdrive.dat,regexpr("key=", gdrive.dat)[1]+4, regexpr(";", gdrive.dat)[1]-1)


unique.keys.1=gsub("amp", "gid=",gdrive.dat.1)
unique.keys.2=paste("https://docs.google.com/spreadsheet/pub?key=",unique.keys.1,sep="")


gdrive.tables=list()
  for(j in 1:20){
    url=paste(unique.keys.2,j,sep="")
    doc=getURL(url)
    tables=readHTMLTable(htmlParse(doc,asText=T))
    gdrive.tables=c(gdrive.tables,tables)
  }




gdrive.tables.1=gdrive.tables[which(names(gdrive.tables)=="tblMain")]

new.car.sales=(gdrive.tables.1[length(gdrive.tables.1)]$tblMain)
colnames(new.car.sales)=as.matrix(new.car.sales[1,])
new.car.sales=new.car.sales[-1,-1]

num.days.sold=round(mean(na.omit(as.numeric(gsub(",","",(as.character(new.car.sales[,3]))))/as.numeric(gsub(",","",(as.character(new.car.sales[,6])))))))
new.car.sales.1=data.frame(new.car.sales$Brand,as.numeric(gsub(",","",(as.character(new.car.sales[,3])))))
colnames(new.car.sales.1)=c("Brand", "Sales")
new.car.sales.2=new.car.sales.1[1:which(new.car.sales.1[,1]=="COMPANIES")-1,]

new.car.sales.2$Brand=gsub(" ", ".", new.car.sales.2$Brand)
new.car.sales.2$Brand=gsub("-", ".", new.car.sales.2$Brand)

new.car.sales.3=t(c(new.car.sales.2[,2], num.days.sold))
colnames(new.car.sales.3)=c(new.car.sales.2$Brand, "Number.of.Days.sold")

##data=read.csv("/Users/sweiss/Google Drive/by the numbers folder/merged.data.7.14.2.csv.csv")
current.month="2014-08-01" ##NEED TO UPDATE
data=read.csv(paste("/Users/sweiss/Google Drive/by the numbers folder/merged.data", current.month,".csv",sep=""))


library(gtools)
merged.data=smartbind(data,new.car.sales.3)

merged.data$new.dates..1.=seq(as.Date("2005-07-01"), by = "month", length = dim(data)[1]+1)
current.month=merged.data$new.dates..1.[length(merged.data$new.dates..1.)]

dead.brands=c("Hummer", "Isuzue", "Mercury", "Pontiac", "Saab", "Saturn", "Suzuki")
merged.data[dim(merged.data)[1], which(colnames(merged.data) %in% dead.brands)]=0

current.month="2014-09-01"
write.csv(merged.data, file=paste("/Users/sweiss/Google Drive/by the numbers folder/merged.data", current.month,".csv",sep="") )


read.csv(paste("/Users/sweiss/Google Drive/by the numbers folder/merged.data", current.month,".csv",sep=""))


