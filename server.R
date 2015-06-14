library(shiny)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ROCR)

shinyServer(function(input,output){

  
  ## To print the heading in the main panel for the structure of the dsataset. 
  #output$predcost <- renderDataTable({
  output$predcost <- renderText({
    #collisions <- collisions.df
    intercept = 4.137e+00
    if (input$Safely_Operable=='Unknown'){
      ASO = -1.139e-01
    }else if (input$Safely_Operable=='Yes'){
      ASO = -4.290e-01
    }else ASO <- 0
    
    if(input$Detailed_Loss_Type=='Changing lanes'){
      DLT = 1.523e-01
    }else if(input$Detailed_Loss_Type=='Head-on collision'){
      DLT = -2.013e-01
    }else if(input$Detailed_Loss_Type=='Insured drove into standing water'){
      DLT = 4.994e-01
    }else if(input$Detailed_Loss_Type=='Insured hit a fixed object'){
      DLT = 2.465e-01
    }else if(input$Detailed_Loss_Type=='Insured Hit a Pedestrian/Bicycle'){
      DLT = -1.059e-01
    }else if(input$Detailed_Loss_Type=='Intersection accident'){
      DLT = 2.778e-01
    }else if(input$Detailed_Loss_Type=='Making a turn'){
      DLT = 2.520e-01
    }else if(input$Detailed_Loss_Type=='Parked Vehicle - insured hit parked vehicle.'){
      DLT = 1.118e-01
    }else if(input$Detailed_Loss_Type=='Parked Vehicle - Other Party hit parked Insured'){
      DLT = 2.539e-02
    }else if(input$Detailed_Loss_Type=='Rear-end accident - insured rear-ended other party'){
      DLT = 2.481e-01
    }else if(input$Detailed_Loss_Type=='Rear-end accident - multiple cars'){
      DLT = 2.987e-01
    }else if(input$Detailed_Loss_Type=='Rear-end accident - Other party rear-ended insured'){
      DLT = 1.727e-01
    }else if(input$Detailed_Loss_Type=='Sideswipe accident'){
      DLT = 1.694e-01
    }else if(input$Detailed_Loss_Type=='Unknown loss facts'){
      DLT = 2.600e-01
    }else DLT = 0
    
    
    if (input$Asset_Towed_From_Scene=='Unknown'){
        ATFS = 7.891e-02
    }else if (input$Asset_Towed_From_Scene=='Yes'){
        ATFS = 2.737e-01
    }else ATFS = 0

    FF = 1.358e+00*as.numeric(input$FrontFrac)

    if (input$Front_HOOD == 'Yes'){
        FH = 1.439e-01
    }else FH = 0

    if (input$Doors_open_freely == 'Unknown'){
        DOF = 4.121e-02
    }else if (input$Doors_open_freely == 'Yes'){
        DOF = -1.419e-01
    } else DOF = 0

    if(input$Interior_AIR_BAGS == 'Yes'){
        IAB = 4.139e-01
    }else IAB = 0

      VA = -4.100e-02*as.numeric(input$vehicleAge)
      BP = 4.521e-06*as.numeric(input$BasePrice) + 3.941e-01*log(as.numeric(input$BasePrice))
      CW = -3.090e-05*as.numeric(input$CurbWeight)

    if (input$Rear_TRUNK_TAILGATE_HATCH == 'Yes'){
        RTTH = 2.152e-01
    }else RTTH = 0

    PSEF = 5.494e-01*as.numeric(input$PassengerSideExteriorFrac)
    RF = 6.789e-01*as.numeric(input$RearFrac)
    DSEF = 4.395e-0*as.numeric(input$DriverSideExteriorFrac)

    if(input$Driver_Side_Exterior_IND == 'Yes'){
        DSEI = 2.143e-02
    }else DSEI = 0

    if(input$Passenger_Side_Exterior_IND == 'Yes'){
        PSEI = 1.670e-02
    }else PSEI = 0

    if(input$Did_this_loss_occur_in_a_parking_lot == 'Yes'){
        PL = -1.895e-01
    }else PL = 0

    if(input$Passenger_Side_Exterior_REAR_DOOR == 'Yes'){
        PSERD = 1.599e-01
    }else PSERD = 0

    if(input$Was_a_police_report_filed == 'Yes'){
        PRF = 9.592e-02
    }else PRF = 0

    if(input$Driver_Side_Exterior_REAR_DOOR == 'Yes'){
        DSERD = 1.645e-01
    }else DSERD = 0

    if(input$old.car == 'Yes'){
      OC = 1.119e-01
    }else OC = 0
    #OC = 3.324e-02*as.numeric(input$OldCar)

    if(input$luxury.car == 'Yes'){
        LC = 1.822e-01
    }else LC = 0

    cost <- intercept+ASO+DLT+ATFS+FF+FH+DOF+IAB+VA+BP+CW+RTTH+PSEF+RF+DSEF+DSEI+PSEI+PL+PSERD+PRF+DSERD+OC+LC
    cost <- as.data.frame(cost,rownames=FALSE)
    cost <- round(exp(cost))
    colnames(cost) <- 'Estimated_collision_cost'
    cost[1] <- paste('$',cost$Estimated_collision_cost[1],sep='')
    collision.cost <- as.character(cost[1])
    collision.cost <- paste('Estimated collision cost: ',collision.cost,sep='')
    collision.cost
  })

    
})