library(shiny)
library(ggplot2)

##load("/data/MFSocialMelt.RData")
#dataset <- MF.QoG.melt.df.ggvis.melt
#dataset <- diamonds
dataset<- read.csv("MF.QoG.melt.df.ggvis.melt.csv",header=TRUE)
MF.QoG.melt.df.ggvis.melt <- dataset

shinyUI(fluidPage(
  
  title = "MacroSocioEco TS Explorer",
  
  plotOutput('plot'),
  
  hr(),
  
  fluidRow(
    column(3,
           h4("MacroSocioEco TS Explorer"),
           sliderInput('sampleSize', 'Sample Size', 
                       min=1, max=nrow(dataset),
                       value=nrow(dataset), 
                       step=25, round=0),
           br(),
           checkboxInput('jitter', 'Jitter'),
           checkboxInput('smooth', 'Smooth')
    ),
    column(4, offset = 1,
           selectInput('x', 'X', names(dataset)),
           selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
           selectInput('color', 'Color', c('None', names(dataset)))
    ),
    column(4,
           selectInput('facet_row', 'Facet Row',
                       c(None='.', names(MF.QoG.melt.df.ggvis.melt[sapply(MF.QoG.melt.df.ggvis.melt, is.factor)]))),
           selectInput('facet_col', 'Facet Column',
                       c(None='.', names(MF.QoG.melt.df.ggvis.melt[sapply(MF.QoG.melt.df.ggvis.melt, is.factor)])))
    )
  )
))
