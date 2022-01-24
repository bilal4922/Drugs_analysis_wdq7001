library(shiny)
library(ggplot2)
library(glue)
library(dplyr) 
library(ggwordcloud)
library(readxl)
library(leaflet)
library(geojsonio)
library(RColorBrewer)
library(viridis)
library(maps)
library(shinydashboard)
library(rintrojs)
library(shinyWidgets)
library(DT)
library(rpivotTable) 



#icon = shiny::icon(verify_fa=FALSE)
drug_by_gender = read.csv("./Data/Number Of Drug Addicts By Sex, Malaysia.csv",header = TRUE)
drug_by_state = read.csv("./Data/Number Of Drug Addicts By State, Malaysia.csv",header = TRUE)
drug_by_gender = read.csv("./Data/Number Of Drug Addicts By Sex, Malaysia.csv",header = TRUE)
drug_by_age = read.csv("./Data/Number Of Drug Addicts By Age Group, Malaysia.csv",header = TRUE)
drug_by_type = read.csv("./Data/Number Of Addicts By Type Of Drugs, Malaysia.csv",header = TRUE)
master <- read_excel("Data/Master List.xlsx")
drug_by_reason = read_excel("./Data/jumlah-penagih-mengikut-sebab-mula-mengguna-dadah-2014-2018.xlsx")
master$Coordnates.Latitude <-  as.numeric(master$Coordnates.Latitude)
master$Coordnates.Longitude <-  as.numeric(master$Coordnates.Longitude)
master$Opiate <-  as.numeric(master$Opiate)
subset = drug_by_reason[3:9]

library(shinythemes)
mybins <- seq(0, 7000, by=100)
mypalette <- colorBin( palette="YlOrBr", domain=master$Value, na.color="transparent", bins=mybins)

mytext <- paste(
  "State: ", master$State, "<br/>", 
  "year: ", master$Year, "<br/>", 
  "Total Addicts: ", master$Value, sep="") %>%
  lapply(htmltools::HTML)


# sidebar <- dashboardSidebar(
#   sidebarMenu(
#     menuItem("Main", tabName = "Main", icon =icon("dashboard")),
#     menuItem("Trend", tabName = "Trend", icon =icon("dashboard")),
#     menuItem("Source", tabName = "Source", icon =icon("dashboard"))
#   )
# )
# header <- dashboardHeader(
#   title = "My Dashboard",
#   dropdownMenu(type = "notification",notificationItem(text = "Source"),notificationItem(text = "Github Page")))
  

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title="My Dashboard", dropdownMenu(type = "notification",notificationItem(text = "Source"), notificationItem(text = "Github Page"))),
    dashboardSidebar(  
      sidebarMenu(
        menuItem("Main", tabName = "Main", icon =icon("dashboard")),
        menuItem("Trend", tabName = "Trend", icon =icon("dashboard")),
        menuItem("Data  View Explorer ", tabName = "d_explorer", icon=icon("file-excel-o")),
        menuItem("Project Documentation", tabName = "about", icon=icon("book")),
        menuItem("Source", tabName = "Source", icon =icon("dashboard"))
    )),
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "Main",
        titlePanel(title = h2("Drug Related Cases In Malaysia", align="center")),
        column(12,box( 
          fluidRow(
            valueBoxOutput("opi",width = 1),
            valueBoxOutput("her",width = 1),
            valueBoxOutput("Ben",width = 1),
            valueBoxOutput("mor",width = 1),
            valueBoxOutput("opiu",width = 1),
            valueBoxOutput("Met",width = 1),
            valueBoxOutput("Tra",width = 1),
            valueBoxOutput("Cod",width = 1),
            valueBoxOutput("Mar",width = 1),
            valueBoxOutput("Amp",width = 1),
            valueBoxOutput("Psy",width = 1),
            valueBoxOutput("Oth",width = 1)
          ),
          fluidRow(
          ),width = 20)
        ),
        box(
          h4("Filters"),width=2,  sliderInput(inputId ="one", label = "Select A Year",value = min(master$Year)+4, min = min(master$Year),max = max(master$Year),width = '100%'),
          selectInput(inputId = "stateselect", label = "Choose a state", choices = c("All",unique(drug_by_state$State)), multiple = FALSE)
        , height = "800px"),
        fluidRow(
        column(12,box(leafletOutput("mymap",width = "100%", height = 800)),width = 9),
        ),
        
        
      fluidRow(
        column(4, 
               box(wellPanel(
                 h3("Number of Drug Addicts by Age Group"),
                 plotOutput("plot"),
                 h5(glue("This chart shows that trend of drug users by age group from  {min(drug_by_state$Year)} until {max(drug_by_state$Year)}")),
               ),width = 25)
        ),
        column(4, 
               box(wellPanel(
                 h3("Number of Drug Addicts by Gender"),
                 plotOutput("bartwo"),
                 h5(glue("This chart shows that trend of drug users by gender from  {min(drug_by_gender$Year)} until {max(drug_by_gender$Year)}")),
               ),width = 25)
        ),
        column(4, 
               box(wellPanel(
                 h3(glue("Number of Drug Addicts Died in {min(drug_by_gender$Year)}")),
                 plotOutput("death"),
                 h5(glue("This chart shows that trend of drug users by gender from  {min(drug_by_gender$Year)} until {max(drug_by_gender$Year)}")),
               ),width = 25)
        )
        
        
      )
    ),
    # Second tab content
    tabItem(tabName = "Trend",
            introBox(
              pickerInput("stateselect",
                          "Choose a State", 
                          choices = unique(drug_by_state$State), 
                          options = list(`actions-box` = TRUE),
                          selected = drug_by_state[2, 1],
                          multiple = T),
              width = 2),
            fluidRow(
              wellPanel(h3("Trend of Drug Addicts By States"),
                        plotOutput("linetrend")),
              introBox(
                pickerInput("reasonselect",
                            "Choose a Reason", 
                            choices = colnames(subset), 
                            options = list(`actions-box` = TRUE),
                            selected = drug_by_state[2, 1]),
                width = 2),
            
              wellPanel(h3("Trend of Reason Using Drugs By States"),
                        plotOutput("linereason")),

              
            ),
            
            
            
            

    ),
    ### about tab content
    tabItem(tabName = "about",
            fluidPage(
              HTML("
              <h1>Documentation</h1>
                <p>Welcome to the documentation section of this application. You are strongly advised to go through this documentation prior to using the application we will explain the functionality and purpose of every tab in this application. Let’s get started</p>
                <br/>
                <div style='background-color:white; padding: 5px 10px; border-radius: 5px; margin-bottom: 50px;'>
                  <h4><i class=' fa fa-globe fa-fw'></i><b>Interactive Map</b></h4>
                  <p>we have designed this tab to support visualization on Malaysia drugs addicts’ hotspots and locations on an interactive map. You are provided with filters such as …. to interact with the map.</p>
                  <br/>
                  <h4><i class=' fa fa-file-excel-o fa-fw'></i><b>Outbreak Analysis</b></h4>
                  <p>We have aggregated Malaysia dengue cases in bar chart and prepared filters such as Year and State for users to visualize the data.</p>
                     <br/>
                  <h4><i class=' fa fa-bar-chart-o fa-fw'></i><b>Data Explorer</b></h4>
                  <p>we have tabulated and aggregated Malaysia dengue cases from 2000 to 2019. You can view the processed data complemented with the Year filter.</p>
                  <br/>
                  <h4><i class=' fa fa-area-chart fa-fw'></i><b>Trend Visualization</b></h4>
                  <p>We built a trend visualisation that contains total dengue cases reported by each state between 2010-2015. It uses the Pivot Table and Pivot Chart that enable custom visualisation and filtration desired by users.</p>
                  <br/>
                  <h4><i class=' fa fa-line-chart fa-fw'></i><b>Scatterplot</b></h3>
                  <p>We build a trend visualization that consists of total drug addicts’ cases reported by each state between 2000 -2019. It uses the Pivot Table and Pivot Chart that enables customs visualization and filtration desired by users</p>
                  <br/>
                  <h4><i class=' fa fa-file-code-o fa-fw'></i><b>Source Code</b></h4>
                  <p>We have plotted the drug addicts’ cases across Malaysian states for you to visualize the supply and demand of this problem. You will be able to see which state needs to be alert and have the authorities dispatch to patrol the areas more strictly. </p>
                  <br/>
                </div>
               ")
            )
    ),
    tabItem(tabName = "d_explorer",
            fluidPage(
              
              titlePanel("Data Explorer"),
              sidebarLayout(
                conditionalPanel(
                  condition = NULL,
                  sidebarPanel(
                    width = 3,
                    helpText("Please choose the year to see list of drugs data"),
                    selectInput("newdatayear", label = "Select year", choices = 2000:2019)
                  )),
                mainPanel(dataTableOutput("newtbl"))
              )
            )
    )
    
  ),

    )
  )
  )

server <- function(input, output,session) {
output$mymap <- renderLeaflet({
  my_year=input$one
  xxxx <-  filter(master, master$Year==my_year)
  leaflet(xxxx) %>% 
    addTiles() %>% 
    fitBounds(~min(Coordnates.Longitude), ~min(Coordnates.Latitude), ~max(Coordnates.Longitude), ~max(Coordnates.Latitude)) %>%
  addCircleMarkers(~Coordnates.Longitude, ~Coordnates.Latitude, 
                   fillColor = ~mypalette(Value),fillOpacity = 0.7, color="white", radius=~Value/250, stroke=FALSE,
                   label = mytext,labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"))
  })

###Bilal - display the data in table form
output$newtbl <- renderDataTable({
  newdata <- master %>% filter (Year == input$newdatayear)
  
  opts <- list(pageLength = 20, lengthChange = FALSE, searching = FALSE,
               info = FALSE,  pagingType = "full")
  
  datatable(newdata, escape = FALSE, rownames = FALSE, options = opts)
})


output$pieone <- renderPlot({
  ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Value, fill=Sex))+ 
    geom_bar(stat = "identity",color="white")+ geom_text(aes(label = Value), vjust = -0.2) + coord_polar("y", start=0)
})

output$death <- renderPlot({
  ggplot(data=master %>% filter(Year == input$one), aes(x=State, y = Death, fill=State))+ 
    geom_bar(stat = "identity",color="white")+ geom_text(aes(label = Death), vjust = -0.2) + 
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
})

output$opi <- renderValueBox({
  dgraf <- as.character(input$stateselect)
  my_year=input$one
  if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
  else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
  }
  valueBox( 
    value = sum(xxxxx$Opiate),
    width = 1,
    subtitle = "Opiate")
})

output$her <- renderValueBox({
  dgraf <- as.character(input$stateselect)
  my_year=input$one
  if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
  else {
    xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
  }
  valueBox( 
    value = sum(xxxxx$Heroin),
    width = 1,
    subtitle = "Heroin")
})

output$mor <- renderValueBox({
  dgraf <- as.character(input$stateselect)
  my_year=input$one
  if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
  else {
    xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
  }
  valueBox( 
    value = sum(xxxxx$Morphine),
    width = 1,
    subtitle = "Morphine")
})

output$opiu <- renderValueBox({
  dgraf <- as.character(input$stateselect)
  my_year=input$one
  if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
  else {
    xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
  }
  valueBox( 
    value = sum(xxxxx$Opium),
    width = 2,
    subtitle = "Opium")
})

output$Cod <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
} 
valueBox(value = sum(xxxxx$Codeine),width = 1,subtitle = "Codeine")})

output$Mar <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
} 
valueBox(value = sum(xxxxx$Marijuana),width = 1,subtitle = "Marijuana")})

output$Amp <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
} 
valueBox(value = sum(xxxxx$Amphetamine-type.stimulants.(ATS)),width = 1,subtitle = "Amphetamine-type stimulants (ATS)")})

output$Ect <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
}
valueBox(value = sum(xxxxx$Ectasy.Pill),width = 1,subtitle = "Ectasy Pill")})

output$Amp <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
}
valueBox(value = sum(xxxxx$Amphetamine),width = 1,subtitle = "Amphetamine")})

output$Met <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
} 
valueBox(value = sum(xxxxx$mymeta),width = 1,subtitle = "Metamphetamine")})

output$Psy <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
} 
valueBox(value = sum(xxxxx$Psychotropic.Pill),width = 1,subtitle = "Psychotropic Pill")})



output$Ben <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
}
valueBox(value = sum(xxxxx$Benzoylecgonine),width = 1,subtitle = "Benzoylecgonine")})

output$Tra <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
} 
valueBox(value = sum(xxxxx$Tramadol),width = 1,subtitle = "Tramadol")})

output$Oth <- renderValueBox({dgraf <- as.character(input$stateselect)
my_year=input$one
if (dgraf == "All"){xxxxx <-  filter(master, master$Year==my_year)}
else {
  xxxxx <-  filter(master, master$Year==my_year & master$State %in% dgraf)
} 
valueBox(value = sum(xxxxx$Others),width = 1,subtitle = "Others")})


output$bartwo <- renderPlot({
  dgraf <- as.character(input$stateselect)  

  if (dgraf == "Johor")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Johor, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) + coord_polar("y", start=0) + geom_text(aes(label = Johor), vjust = -0.2)
  
  else if (dgraf == "Kedah")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Kedah, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Kedah), vjust = -0.2)
  
  else if (dgraf == "Kelantan")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Kelantan, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Kelantan), vjust = -0.2)
  
  else if (dgraf == "Melaka")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Melaka, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Melaka), vjust = -0.2)
  
  else if (dgraf == "Negeri Sembilan")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Negeri.Sembilan, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Negeri.Sembilan), vjust = -0.2)
  
  else if (dgraf == "Pahang")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Pahang, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Pahang), vjust = -0.2)
  
  else if (dgraf == "Perak")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Perak, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Perak), vjust = -0.2)
  
  else if (dgraf == "Perlis")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Perlis, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Perlis), vjust = -0.2)
  else if (dgraf == "Sabah")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Sabah, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Sabah), vjust = -0.2)
  
  else if (dgraf == "Sarawak")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Sarawak, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Sarawak), vjust = -0.2)
  
  else if (dgraf == "Terengganu")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Terengganu, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Terengganu), vjust = -0.2)
  
  else if (dgraf == "Kuala Lumpur")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Kuala.Lumpur, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Kuala.Lumpur), vjust = -0.2)
  
  else if (dgraf == "Labuan")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Labuan, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Labuan), vjust = -0.2)
  
  else if (dgraf == "Selangor")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Selangor, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Selangor), vjust = -0.2)
  
  else if (dgraf == "Putrajaya")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Putrajaya, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Putrajaya), vjust = -0.2)
  
  else if (dgraf == "All")
    ggplot(data=drug_by_gender %>% filter(Year == input$one), aes(x=Sex, y = Value, fill=Sex))+ geom_bar(stat = "identity",  width=0.2)+
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+ coord_polar("y", start=0)+ geom_text(aes(label = Value), vjust = -0.2)
  
  
})


output$plot <- renderPlot({
  
  dgraf <- as.character(input$stateselect)  
  if (dgraf == "Johor")
  ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Johor ,fill=Age.group))+ geom_bar(stat = "identity", width=0.7)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()+ geom_text(aes(label = Johor, vjust = -0.2))
  
  else if (dgraf == "Kedah")
  ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Kedah ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Kelantan")
  ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Kelantan ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Melaka")
  ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Melaka ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Negeri Sembilan")
  ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Negeri.Sembilan ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()

  else if (dgraf == "Pahang")
  ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Pahang ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Perak")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Perak ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Selangor")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Selangor ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Perlis")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y = Perlis ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()

  else if (dgraf == "Sabah")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Sabah ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Sarawak")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Sarawak ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Pulau Pinang")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Pulau.Pinang ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Terengganu")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Terengganu ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Kuala Lumpur")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Kuala.Lumpur ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Labuan")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Labuan ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "Putrajaya")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Putrajaya ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  else if (dgraf == "All")
    ggplot(data=drug_by_age %>% filter(Year %in% input$one), aes(x=Age.group,  y =Value ,fill=Age.group))+ geom_bar(stat = "identity",  width=0.7)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ coord_flip()
  
  
  })

  output$linetrend <- renderPlot({
    ggplot(data=drug_by_state %>% filter(State %in% input$stateselect), aes(x=Year, y=Value, fill=State)) +
      geom_line(aes(color=State))+
      geom_point(aes(color=State))+
      scale_x_continuous(n.breaks = 20)
  })
  
 
  output$linereason <- renderPlot({
    reason <- as.character(input$reasonselect)
    ggplot(data=drug_by_reason %>% filter(State %in% input$stateselect), aes_string(x="Year", y=input$reasonselect, fill="State")) +
      geom_line(aes(color=State))+
      geom_point(aes(color=State))+
      scale_x_continuous(n.breaks = 20)
  })
  # output$linereason <- renderPlot({
  #   select_quo <- quo(input$reasonselect)
  #   
  #   drug_by_reason %>%
  #     mutate(user_input = !!select_quo) %>%
  #     ggplot(data=drug_by_reason %>% filter(State %in% input$stateselect), aes(fill=State,  y=user_input, x=Year)) + 
  #     geom_line(aes(color=State))+
  #     geom_point(aes(color=State))+
  #     scale_x_continuous(n.breaks = 20)
  # 
  # })
}
shinyApp(ui, server)