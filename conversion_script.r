
library(rmarkdown)

wd<- "G:/Documents/PostDocKVA/Labbook/"
html.output.dir<-"G:/Documents/PostDocKVA/Labbook/psjorgensen.github.com/_posts"

project.title<- "macro-socio-ecology"

docs.vec<-c("macro-socio-ecology-outline","macro-socio-ecology-variables")

htmlpost_switch <- TRUE # TRUE, 
#html_switch <- TRUE # TRUE, FALSE
#md_switch <- TRUE # TRUE, FALSE
#pdf_switch <- TRUE # TRUE, FALSE
#word_switch <- TRUE # TRUE, FALSE

#htmlpost_switch <- FALSE # TRUE, FALSE
html_switch <- FALSE # TRUE, FALSE
md_switch <- FALSE # TRUE, FALSE
pdf_switch <- FALSE # TRUE, FALSE
word_switch <- FALSE # TRUE, FALSE


setwd(paste(wd,"projects/",project.title,sep=""))


for(i in 2){#c(1:6)){#1:length(docs.vec)){

# html
if(htmlpost_switch == TRUE){
  render(
    input=paste(docs.vec[i],".Rmd",sep=""),
    output_format="html_document",
    output_dir=html.output.dir,
    output_file=paste(Sys.Date(),"-",docs.vec[i],".html",sep="")
  )
}

# html
if(html_switch == TRUE){
  render(
    input=paste(docs.vec[i],".Rmd",sep=""),
    output_format="html_document",
    output_dir=getwd(),
    output_file=paste(docs.vec[i],".html",sep="")
  )
}

# word
if(word_switch == TRUE){
  render(
    input=paste(docs.vec[i],".Rmd",sep=""),
    output_format="word_document",
    output_dir=getwd(),
    output_file=paste(docs.vec[i],".docx",sep="")
  )
}

# md
if(md_switch == TRUE){
  render(
    input=paste(docs.vec[i],".Rmd",sep=""),
    output_format="md_document",
    output_dir=getwd(),
    output_file=paste(docs.vec[i],".md",sep="")
  )
}

# pdf
if(pdf_switch == TRUE){
  render(
    input=paste(docs.vec[i],".Rmd",sep=""),
    output_format="pdf_document",
    output_dir=getwd(),
    output_file=paste(docs.vec[i],".pdf",sep="")
  )
}

}

####  END

