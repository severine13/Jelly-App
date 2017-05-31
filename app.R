# Martini Severine
# deployed in shiny-web 17 November 2016
# training for shiny development using Jellywatch dataset
# objective is to represent the diversity of observations, numbers, spatiality over time...


# Find out more about building applications with Shiny here:
#  http://shiny.rstudio.com/
#

#load the libraries
library(shiny)
library(ggplot2)
library(shinydashboard)
#detach("package:plyr", unload=TRUE) 
library(dplyr)
library(ggmap)
library(htmlwidgets)
library(leaflet)


# set the path
#setwd("C:/Users/smartini/repos/MBARI/small-projects/jellywatch-shiny")
setwd("./") 

# load the data
gexf <- read.table("phpretrievetab.txt",sep="\t",header=T)
# rename the columns
names(gexf)=c("date","lat","long","concept")

# date format for easier representations
H=strptime(gexf$date, format="%Y/%m/%d",tz="gmt")
gexf$year=as.numeric(format(H,format="%Y"))
gexf$month=as.numeric(format(H,format="%m"))

####### TO USE ####
# aggregation nombre d observation par an/mois/concept
dataset=gexf %>% group_by(year,month,concept) %>% summarise(counts=n())
###################

# aggreger les donnees 
data_agg=aggregate(dataset[,c(1,4)],by=list(dataset$year,dataset$concept),sum)
# name the dataset
names(data_agg)=c("year","concept","sumyear","counts")

# Define UI for the shiny application 
ui <- fluidPage(
   # Application title + web link
   headerPanel(tags$div(tags$h1(tags$b("Jelly'app")),tags$h2(tags$a(href="http://www.jellywatch.org/","Jellywatch website")))
  ),
   sidebarLayout(
   sidebarPanel(
  # add a video
  tags$video(src = "jelly.mp4", type = "video/mp4", autoplay = NA, controls = NA,width=350),
  
  # seletc the concept of interest with a select box   
   selectInput("concept","Categories",c("Jellyfish" = "Jellyfish",
                                        "Clean sea" = "Clean sea",
                                        "Man o war" = "Man o war",
                                        "Plastic" = "Plastic",
                                        "Red Tide" = "Red Tide",
                                        "Squid" = "Squid",
                                        "Box Jelly" = "Box Jelly",
                                        "Velella"="Velella",
                                        "Vertebrate"="Vertebrate",
                                        "Other" = "Other")
   ),
  # select the date of interest with a slider   
   sliderInput("daterange", "Date range:",min = 2000,max = 2020,value=c(2010,2016),sep=""),
  
   # select the colors 
  radioButtons("colors","Color choice",c("black"="black","yellow"="yellow","red"="red","purple"="purple4"))
        ),
# add a cute image of jellywatch / logo in the main panel
  mainPanel(img(src='jelly.png', align = "right"),
# define the number of graphs..         
        fluidRow(
         column(6,plotOutput("distPlot"))),
         plotOutput("distPlot2"),
        # plotOutput("distPlot3"),
         plotOutput("distPlot4"),
      leafletOutput("map")
        )
      )
)


# Define server 
server <- function(input, output) {
   
## TO DO ##
  # add the line of TOTAL OBS vs the input$concept
    output$distPlot <- renderPlot({
       ggplot(data_agg[which(data_agg$concept==input$concept),],aes(x=year,y=cumsum(counts)))+
           geom_line(size=3,color=input$colors)+geom_smooth(color="black")+ylab("Total observations")+ theme_minimal(base_size = 18)+
           scale_size_area(max_size = 20)+xlim(input$daterange)
     })
    
### TO DO circular plot
   output$distPlot4 <- renderPlot({
     ggplot(dataset[which(dataset$concept==input$concept&dataset$year>input$daterange[1]&dataset$year<input$daterange[2]),])+
       geom_point(aes(x=month, y=counts,size=year),color=input$colors)+ 
        scale_x_continuous(breaks=seq(0,13,1),labels=c("","January","February","March","April","May","June","July","August","September","October","November","December",""))+geom_smooth(aes(x=month, y=counts),color="black")+coord_polar(theta="x")
       })
###    
    
    output$distPlot2 <- renderPlot({
      ggplot(dataset[which(dataset$concept==input$concept),],aes(x=year,y=month,size=counts,fill=counts))+
        geom_point(shape=21)+ theme_minimal(base_size = 18)+scale_size_area(max_size = 20)+
        scale_y_continuous(breaks=seq(1,12,1))+
        scale_fill_continuous(low = "white", high = input$colors)+xlim(input$daterange)
    })
    
   # output$distPlot3 <- renderPlot({
    #  ggmap(get_map(location = "Monterey", source = "stamen",maptype="terrain",crop=F))+geom_point(data=gexf[which(gexf$concept==input$concept),],aes(long,lat),color=input$colors,alpha=0.4,size=2)
   # ggmap(get_map(location = input$space, maptype="satellite",crop=F))+geom_point(data=gexf[which(gexf$concept==input$concept),],aes(long,lat),color=input$colors,alpha=0.4,size=2)
   #    })
    
      output$map <- renderLeaflet({
        leaflet() %>% 
          addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}") %>% 
          addCircleMarkers(popup = paste(gexf[which(gexf$concept==input$concept&dataset$year>input$daterange[1]&dataset$year<input$daterange[2]),]$Scientificname,gexf[which(gexf$concept==input$concept&dataset$year>input$daterange[1]&dataset$year<input$daterange[2]),]$date),data = data.frame(lat =gexf[which(gexf$concept==input$concept&dataset$year>input$daterange[1]&dataset$year<input$daterange[2]),]$lat, lng =gexf[which(gexf$concept==input$concept&dataset$year>input$daterange[1]&dataset$year<input$daterange[2]),]$lon), radius = 3.5, weight = 0, fillOpacity = 0.5, fillColor = input$colors)
      })


      

####### TO DO
# add     
    

}

# Run the application 
shinyApp(ui = ui, server = server)

