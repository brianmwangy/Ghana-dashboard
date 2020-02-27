library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(tidyverse)
library(RColorBrewer)
library(raster)
library(htmltools)
library(DT)
library(readr)
library(plotly)


#loading data table
var_data<-read_csv("./www/data.csv")

#loading ghana data
#ghn_data<-read_csv("./www/Ghana_final2.csv")

#loading ghana shape file
shp<-readOGR("./www/ghn.shp")
# shp<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Ghana Viz/R_Ghana/www",
#              layer="Ghana",GDAL1_integer64_policy = TRUE)

ghpt<-readOGR("./www/ghana_items.shp")

#filter based on item
# gold<-ghpt[ghpt$Item.Type=="Goldmines",]
# cocoa<-ghpt[ghpt$Item.Type=="Cocoa fields",]
# lab<-ghpt[ghpt$Item.Type=="Laboratory",]
#setting default crs
crs(shp)<- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

ui <-dashboardPage(
  dashboardHeader(
    title = "GHANA DASHBOARD", titleWidth = 400
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "About",
        tabName = "intro",
        icon = icon("info")
      ),
      
      menuItem(
        "Map",
        tabName = "map",
        icon = icon("map")
      )
    )             
    
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }
                              '))),
    tabItems(
      
      tabItem(
        tabName = "intro",
        fluidRow(
          column(6,
                 box(
                   title = HTML("<strong>Background.</strong>"),
                   status = "info",
                   collapsible = TRUE,width = NULL,
                   h4("This dashboard displays the distribution of water and demographic 
                      explanatory variables for each of the 216 districts in Ghana."),
                   h4("It can be used  for both technical and non-technical stakeholders 
                      such as national policy makers, NGOs, district administrators, 
                      development partners and consultants as an advocacy tool to support 
                      informed decision making for planning."),
                   h4("The dashboard can also be used as a diagnostic tool that helps to 
                      easily identify districts in Ghana where service improvements is 
                      needed based on the selected variable.")
                 )
          ),
          
          column(6,
                 box(
                   title = HTML("<strong>Dataset.</strong>"),
                   status = "info",
                   collapsible = TRUE,width = NULL,
                  h4("The Ghana data is obtained from different sources as described in the table. 
                      It is then grouped into water and demographic variables."),
                  p(h4("Water variables include:Distance to water body (Lake, River) in metres,
                        Flood frequency rates,Water coverage levels,Water scarcity levels
                        ")),
                  p(h4("Demographic variables include:% of poor persons per district,Cholera cases per 100,000 people,
                        Distance to the main road in metres,Men and Women literacy levels,Population density,Time to access urban centre in minutes
                      "))
                 )
          )
          
        ),
       fluidRow(
         column(12,
                box(
                  title = HTML("<strong>The table below shows the various explanatory variables, 
                               their description, and their source.</strong>"),
                  status = "info",
                  collapsible = TRUE,width = NULL,
         dataTableOutput("table")
                )
         )
       )#end row
        
        ), #end tabname
      
      tabItem(
        tabName = "map",
        
        fluidRow(
          column(width = 6,
                 selectInput(
                   inputId = "stats",
                   label = h4("Select Indicator"),
                   choices =c(
                     "Cholera cases per 100,000 people"=1,
                     "Men literacy levels(%)"=2,
                     "Women literacy levels(%)"=3,
                     "Time to access urban centre(minutes)"=4,
                     "Distance from the main road(metres)"=5,
                     "Water scarcity levels"=6,
                     "Flood frequency rates"=7
                   ),selected = 1
                   
                 ) 
                 
          ),
        #end column
        column(width = 6,
               checkboxGroupInput(
                 inputId = "pnt",
                 label = "",
                 choices = c("Select Item Types"=8
                 )
               )
          
        )
        
        ),
        
      fluidRow(
        column(8,
               box(
                 title = HTML("<strong>District map</strong>"),
                 status = "info",
                 collapsible = TRUE,width = NULL,height = 700,
                 leafletOutput("map",height = 600)
               )
               ),#end col
        column(4,
               box(
                 status = "info",
                 collapsible = TRUE,width = NULL,
                  height = 650,

                 #slider input for cholera cases 
                 sliderInput(inputId = "cholera",
                             label = "Cholera cases per 100,000 people",
                             min = min(shp@data$ChlrCss,na.rm =T),
                             max = max(shp@data$ChlrCss,na.rm =T),
                             value = c(min(shp@data$ChlrCss,na.rm =T),
                                       max(shp@data$ChlrCss,na.rm =T))
                 ),
                 #slider input for men literacy levels
                 sliderInput(inputId = "men",
                             label = "Men literacy levels(%)",
                             min = min(shp@data$prcmnlt,na.rm =T),
                             max = max(shp@data$prcmnlt,na.rm =T),
                             value = c(min(shp@data$prcmnlt,na.rm =T),
                                       max(shp@data$prcmnlt,na.rm =T))
                 ),
                 
                 #slider input for women literacy levels
                 sliderInput(inputId = "women",
                             label = "Women literacy levels(%)",
                             min = min(shp@data$prcwmnl,na.rm =T),
                             max = max(shp@data$prcwmnl,na.rm =T),
                             value = c(min(shp@data$prcwmnl,na.rm =T),
                                       max(shp@data$prcwmnl,na.rm =T))
                 ),
                 #slider input for time to access urban centre
                 sliderInput(inputId = "time",
                             label = "Time to access urban centre(minutes)",
                             min = min(shp@data$TmTAcUC,na.rm =T),
                             max = max(shp@data$TmTAcUC,na.rm =T),
                             value = c(min(shp@data$TmTAcUC,na.rm =T),
                                       max(shp@data$TmTAcUC,na.rm =T))
                 ),
                 #slider input for distance from the main road
                 sliderInput(inputId = "road",
                             label = "Distance from the main road(metres)",
                             min = min(shp@data$DstnFMR,na.rm =T),
                             max = max(shp@data$DstnFMR,na.rm =T),
                             value = c(min(shp@data$DstnFMR,na.rm =T),
                                       max(shp@data$DstnFMR,na.rm =T))
                 )
               )
               # box(
               #   title = "Top 5 districts",
               #   status = "info",
               #   collapsible = TRUE,width = NULL,
               #   height = 300,
               #   plotlyOutput("top")
               # ),
               # box(
               #   title = "Bottom 5 districts",
               #   status = "info",
               #   collapsible = TRUE,width = NULL,
               #   height = 300,
               #   plotlyOutput("bottom")
               # )
               )#end column
      )
        
      ) #end tab item
      )#end tab items
    
    )
  
  
    ) 

server<-function(input,output,session){
  
  
  #cholera legend
  brk1<-seq(0,300,50)
  chl<-colorBin("YlOrRd", shp$ChlrCss,na.color = "transparent",bins = brk1)
  
  #men lit legend
  brk2<-seq(0,100,20)
  mn<-colorBin("YlOrRd", shp$prcmnlt,na.color = "transparent",bins = brk2)
  
  #wmn lit legend
  brk3<-seq(0,100,20)
  wmn<-colorBin("YlOrRd", shp$prcwmnl,na.color = "transparent",bins = brk3)
  
  #time access urban centre
  brk4<-seq(0,350,50)
  tm<-colorBin("YlOrRd", shp$TmTAcUC,na.color = "transparent",bins = brk4)
  
  #distance from the main road
  brk5<-seq(0,18000,3000)
  dst<-colorBin("YlOrRd", shp$DstnFMR,na.color = "transparent",bins = brk5)
  
  #water scarcity levels
  wsc<-colorFactor(topo.colors(4), shp$wtr_scr)
  wsc2<-colorFactor("YlOrRd", shp$wtr_scr)
  
  #flood frequency
  fld<-colorFactor("YlOrRd", shp$fld_frq)
  
  #item type 
  itm<-colorFactor(topo.colors(3), ghpt$Item.Type)
  
  
  #sliderinput reactive function for all numeric input options
  sld<-reactive({
    subset(shp,shp@data$ChlrCss>=input$cholera[1]&
             shp@data$ChlrCss<=input$cholera[2]&
             shp@data$prcmnlt>=input$men[1]&
             shp@data$prcmnlt<=input$men[2]&
             shp@data$prcwmnl>=input$women[1]&
             shp@data$prcwmnl<=input$women[2]& 
             shp@data$TmTAcUC>=input$time[1]&
             shp@data$TmTAcUC<=input$time[2]&
             shp@data$DstnFMR>=input$road[1]&
             shp@data$DstnFMR<=input$road[2]
           
    )
    
  })
  
  ##########rendering the data table###################
  output$table<-renderDataTable(
    var_data,
    class = 'cell-border stripe',
    editable = TRUE,
    options = list(scrollX = T)
  )
  
  #rendering the basemap
  output$map<-renderLeaflet(
    leaflet(sld()) %>%
      setView(lng=-0.990341,lat=8.213941,zoom = 6.5) %>%
      addPolygons(
        color = ~chl(ChlrCss),
        smoothFactor = 0.5,
        weight = 2, opacity = 1.0,
        fillOpacity = 0.9,
        highlightOptions = highlightOptions(
          weight = 1,
          color = "brown",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = paste(
          "<strong>Region:</strong>",sld()$REGION,
          "<br>",
          "<strong>District:</strong>",sld()$DISTRIC,
          "<br>",
          "<strong>Cholera Cases:</strong>",sld()$ChlrCss
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                  padding = "3px 8px"), 
                                     textsize = "11px", direction = "auto")
      ) %>% addLegend(
        pal = chl, values = shp$ChlrCss, title = "Cholera cases per 100,000 people"
      )
   
  ) #end leaflet
  
  observe({
    clear<-leafletProxy("map") %>% clearControls()
    
    if ("1" %in% input$stats){
      clear %>%
      addPolygons(
        data = sld(),
        color = ~chl(ChlrCss),
        smoothFactor = 0.5,
        weight = 2, opacity = 1.0,
        fillOpacity = 0.9,
        highlightOptions = highlightOptions(
          weight = 1,
          color = "brown",
          fillOpacity = 0.7,
          bringToFront = FALSE
        ),
        label = paste(
          "<strong>Region:</strong>",sld()$REGION,
          "<br>",
          "<strong>District:</strong>",sld()$DISTRIC,
          "<br>",
          "<strong>Cholera Cases:</strong>",sld()$ChlrCss
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                  padding = "3px 8px"), 
                                     textsize = "11px", direction = "auto")
      ) %>% addLegend(
        pal = chl, values = shp$ChlrCss, title = "Cholera cases per 100,000 people"
      )
    }
    ######end#####
    if ("2" %in% input$stats){
      
      clear %>%
        addPolygons(
          data = sld(),
          color = ~mn(prcmnlt),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>Men literacy levels(%):</strong>",sld()$prcmnlt
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = mn, values = shp$prcmnlt, title = "Men literacy levels(%)"
        )
        
    }
    
    ########end#####
    
    if ("3" %in% input$stats){
      
      clear %>%
        
        addPolygons(
          data = sld(),
          color = ~wmn(prcwmnl),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>Women literacy(%):</strong>",sld()$prcwmnl
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = wmn, values = shp$prcwmnl, title = "Womeen literacy levels(%)"
        )
    }
    
    #######end######
    
    if ("4" %in% input$stats){
      
      clear %>%
        
        addPolygons(
          data = sld(),
          color = ~tm(TmTAcUC),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>Time to access urban centre(minutes):</strong>",sld()$TmTAcUC
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = tm, values = shp$TmTAcUC, title = "Time to access urban centre(minutes)"
        )
    }
    
    #########end########
    
    if ("5" %in% input$stats){
      
      clear %>%
        
        addPolygons(
          data = sld(),
          color = ~dst(DstnFMR),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>Distance from main road(metres):</strong>",sld()$DstnFMR
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = dst, values = shp$DstnFMR, title = "Distance from main road(metres)"
        )
    }
    
    ###########end#########
    
    if ("6" %in% input$stats){
      
      clear %>%
        
        addPolygons(
          data = sld(),
          color = ~wsc2(wtr_scr),
          smoothFactor = 0.5,
          weight = 2, 
          opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>Water scarcity levels:</strong>",sld()$wtr_scr
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = wsc2, values = shp$wtr_scr, title = "Water scarcity levels"
        )
    }
    
    ############end#########
    
    if ("7" %in% input$stats){
      
      clear %>%
        
        addPolygons(
          data = sld(),
          color = ~fld(fld_frq),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = paste(
            "<strong>Region:</strong>",sld()$REGION,
            "<br>",
            "<strong>District:</strong>",sld()$DISTRIC,
            "<br>",
            "<strong>Flood frequency rates:</strong>",sld()$fld_frq
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = fld, values = shp$fld_frq, title = "Flood frequency rates"
        )
    }
    
    ################end##############
    if ("8" %in% input$pnt){
      
      leafletProxy("map") %>% 
        
        addCircleMarkers(
          data = ghpt,
          color = ~itm(Item.Type),
          fillOpacity = 1.0,
          radius = 6,
          stroke = FALSE,
          label = paste(
            "<strong>Item Type:</strong>",ghpt$Item.Type,
            "<br>",
            "<strong>Name:</strong>",ghpt$Name
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "11px", direction = "auto")
        ) %>% addLegend(
          pal = itm, values = ghpt$Item.Type, title = "Item Type"
        )
    }
    
    
    
  }) ###end of observer fxn
  
  
  #rendering top districts
  # output$top<-renderPlotly({
  #   if("1" %in% input$stats){
  #     ghn_data %>% select(District,Cholera_Cases) %>%
  #       arrange(desc(Cholera_Cases)) %>%
  #       top_n(5) %>% ggplot(aes(x=reorder(District,Cholera_Cases),y=Cholera_Cases))+
  #       geom_col(fill="#FF6666")+
  #       geom_text(aes(label=Cholera_Cases))+
  #       labs(
  #         y="Cholera cases per 100,000 people",
  #         x="District"
  #       )+
  #       coord_flip()
  #     
  #   }
  #   
  #   if("2" %in% input$stats){
  #     ghn_data %>% select(District,perc_men_lit) %>%
  #       arrange(desc(perc_men_lit)) %>%
  #       top_n(5) %>% ggplot(aes(x=reorder(District,perc_men_lit),y=perc_men_lit))+
  #       geom_col(fill="#FF6666")+
  #       geom_text(aes(label=Cholera_Cases))+
  #       labs(
  #         y="Men literacy(%)",
  #         x="District"
  #       )+
  #       coord_flip()
  #     
  #   }
  #   
  #   
  # }) 
  ###############end top rendering#############
  
  #rendering bottom districts
  # output$bottom<-renderPlotly({
  #   if("1" %in% input$stats){
  #     ghn_data %>% select(District,Cholera_Cases) %>%
  #       arrange(desc(Cholera_Cases)) %>%
  #       top_n(-5) %>% ggplot(aes(x=reorder(District,Cholera_Cases),y=Cholera_Cases))+
  #       geom_col(fill="#FF6666")+
  #       geom_text(aes(label=Cholera_Cases))+
  #       labs(
  #         y="Cholera cases per 100,000 people",
  #         x="District"
  #       )+
  #       coord_flip()
  #     
  #   }
  #   
  #   if("2" %in% input$stats){
  #     ghn_data %>% select(District,perc_men_lit) %>%
  #       arrange(desc(perc_men_lit)) %>%
  #       top_n(-5) %>% ggplot(aes(x=reorder(District,perc_men_lit),y=perc_men_lit))+
  #       geom_col(fill="#FF6666")+
  #       geom_text(aes(label=Cholera_Cases))+
  #       labs(
  #         y="Men literacy(%)",
  #         x="District"
  #       )+
  #       coord_flip()
  #     
  #   }
  #   
  # })
  
  
}

shinyApp(ui,server)


