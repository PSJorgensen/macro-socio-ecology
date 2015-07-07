library(reshape2)
wd<-"G:\\Documents\\PostDocKVA\\Data\\UNEP\\melted"
setwd(wd)
csvs<-dir()[grep(".csv",dir(),fixed=TRUE)]

for(i in 1:length(csvs)){

csv.i <- read.csv(paste(csvs[i]), header=T,na.strings = c("..."))

## remove numbers from country column

if(i==1){ csv.df <- melt(csv.i,id.vars = "Country");names(csv.df)[i+2]<-csvs[i] }
if(i>1){ csv.i <- melt(csv.i,id.vars = "Country") }


if(i>1){ csv.df <- merge(csv.df, csv.i, by = c("Country","variable"),all = TRUE);names(csv.df)[i+2]<-csvs[i]}

}
csv.df[,"variable"] <- gsub("X","",as.character(csv.df[,"variable"]))

