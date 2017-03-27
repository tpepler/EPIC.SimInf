#********************************************************************
# Function to ensure correct 'select' column values for SIR model

.events2SIR <- function(eventdata){
  eventdata$select[eventdata$event == 0] <- 2
  eventdata$select[eventdata$event == 1] <- 1
  eventdata <- eventdata[eventdata$event != 2, ]
  eventdata$select[eventdata$event == 3] <- 2
  return(eventdata)
}

#********************************************************************

