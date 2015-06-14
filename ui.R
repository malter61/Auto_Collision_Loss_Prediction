library(shiny)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ROCR)
shinyUI(fluidPage(
  titlePanel("Zebra Presentation- Claims Adjuster Automation"),
  headerPanel(h5("by Mark Malter")),
  sidebarLayout(
    sidebarPanel(
      selectInput("Safely_Operable", "Asset safely operable:",
                  choices=c('Yes','No','Unknown')),
      
      selectInput("Detailed_Loss_Type", "Detailed Loss Type:",
                  choices=c('Changing lanes','Head-on collision','Insured drove into standing water',
                            'Insured hit a fixed object','Insured Hit a Pedestrian/Bicycle',
                            'Intersection accident','Making a turn','Parked Vehicle - insured hit parked vehicle.',
                            'Parked Vehicle - Other Party hit parked Insured','Rear-end accident - insured rear-ended other party',
                            'Rear-end accident - multiple cars','Rear-end accident - Other party rear-ended insured',
                            'Sideswipe accident','Unknown loss facts','Other'),selected='Insured hit a fixed object'),
      
      selectInput("Asset_Towed_From_Scene", "Asset towed from scene:",
                  choices=c('Yes','No','Unknown'),selected='No'),
      
      selectInput("Front_HOOD", "Front hood damaged:",
                  choices=c('Yes','No'),selected='No'),
      
      selectInput("Doors_open_freely", "Doors open freely:",
                  choices=c('Yes','No','Unknown')),
      
      selectInput("Interior_AIR_BAGS", "Interior Air Bags Deployed:",
                  choices=c('Yes','No'),selected='No'),
      
      selectInput("Rear_TRUNK_TAILGATE_HATCH", "Rear Trunk Tailgate Hatch:",
                  choices=c('Yes','No'),selected='No'),
            
      selectInput("Passenger_Side_Exterior_IND", "Passenger side exterior ind.:",
                  choices=c('Yes','No'),selected='No'),
      
      selectInput("Driver_Side_Exterior_IND", "Driver side exterior ind.:",
                  choices=c('Yes','No'),selected='No'),
      
      selectInput("Passenger_Side_Exterior_REAR_DOOR", "Passenger side exterior rear door:",
                  choices=c('Yes','No'),selected='No'),
      
      selectInput("Driver_Side_Exterior_REAR_DOOR", "Driver side exterior rear door:",
                  choices=c('Yes','No'),selected='No'),
      
      selectInput("Was_a_police_report_filed", "Police report filed:",
                  choices=c('Yes','No'),selected='No'),
            
      selectInput("Did_this_loss_occur_in_a_parking_lot", "Loss occur in parking lot:",
                  choices=c('Yes','No'),selected='No'),
      
      selectInput("luxury.car", "Luxury car:",
                  choices=c('Yes','No'),selected='No'),
      
      selectInput("old.car", "Old car (> 20 years):",
                  choices=c('Yes','No')),
      
      textInput(inputId="vehicleAge",label="Vehicle age:",value="3"),
      textInput(inputId="BasePrice",label="Base Price:",value="25000"),
      textInput(inputId="CurbWeight",label="Curb Weight:",value="3500"),
      textInput(inputId="FrontFrac",label="Front Frac:",value="0.05"),
      textInput(inputId="RearFrac",label="Rear Frac:",value="0.05"),
      textInput(inputId="PassengerSideExteriorFrac",label="Passenger Side Exterior Frac:",value="0.05"),
      textInput(inputId="DriverSideExteriorFrac",label="Driver Side Exterior Frac:",value="0.05")

    
      ),
      
    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel("Estimated collision cost ",
                  #dataTableOutput("predcost"))
                  textOutput("predcost"))
                  
                  
      )  
    )
  )))