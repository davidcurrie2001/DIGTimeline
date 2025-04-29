library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(timevis)

# Define the groups
groups <- data.frame(
  id = c("ices", "policy","group","platform","metadata","technique","distribution","format","standard","use","rescue"),
  content = c("ICES History", "Data Policy","ICES Group","Data Collection Platforms", "Metadata", "Data Collection Techniques", "Data Distribution", "Data Formats", "Standards", "Data Use", "Data Rescue")
)

### UI ####
ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "lumen"),
  # Application title
  titlePanel("Celebrating 100 years of data management at ICES"),
  fluidRow(column(1,offset = 10, "Light/dark mode:"),
           column(1, 
                  input_dark_mode(id = "mode")
                  )
           ),
  hr(),
      tabsetPanel(
        tabPanel("Introduction",
                 p("Go to the 'Events' tab to explore the timeline of events"),
                 p("Add text to describe the app here")
        ),
        tabPanel("Events",
                 fluidRow(
                   column(2,
                          radioButtons("EventTypeRadioButton",
                                       label = "Show events:",
                                       choices = c("Key events","All events"),
                                       selected = "Key events",
                                       inline = TRUE)
                   ),
                   column(6,
                          selectInput("GroupSelect",
                                      label="Event type:",
                                      choices=groups$content,
                                      selected=groups$content,
                                      multiple = TRUE)
                   )
                 ),
                 tabsetPanel(
                   tabPanel("Timeline",
                            timevisOutput("timeline")
                   ),
                   tabPanel("Table",
                            fluidRow(
                              column(dataTableOutput("timelineTable"), width = 12)
                            )
                  )
                )
        )
      )
)

### server ###
server <- function(input, output, session) {
  
  ### LOAD AND PREPARE DATA ###
  
  # read the file data/DIGTimeline_data.txt which is tab delimited
  timelineData <- read.csv("data/DIGTimeline_data.txt", sep = "\t")
  #timelineData <- timelineData[1:14,]
  
  # Function to format the HTML to be displayed for timeline entries
  templateDIG <- function(title, body, link) {
    
    shortBody <- body
    if (nchar(shortBody)>20){
      shortBody <- substr(shortBody, 1, 20) 
      shortBody <- paste0(shortBody, "...") 
    }

    #print(title)
    #print(shortBody)
    #print(link)
    
    titleCode <- ""
    if (link == ""){
      titleCode <- '<tr><td>%s<em>%s</em></td></tr>'
    } else {
      titleCode <- '<tr><td><em><a href="%s" target="_blank">%s</a></em></td></tr>'
    }
    
    bodyCode <- ""
    if (shortBody == ""){
      bodyCode <- '%s'
    } else {
      bodyCode <- '<tr><td>%s</td></tr>'
    }
    
    allCode <- paste0('<table><tbody>',
                      titleCode,
                      bodyCode,
                      '</tbody></table>')
      
      sprintf(allCode,
              link, title, shortBody)

  }
  
  # Create a new column in the data frame with the template
  # use sapply to apply the templateDIG function to each row of the data frame
  timelineData$content <- sapply(1:nrow(timelineData), function(i) {
    templateDIG(timelineData$title[i],
                timelineData$description[i],
                timelineData$link[i])
  })
  
  ### END OF DATA PREPARATION ###
  
  
  # Filter the input data using the widget values
  FilterEntries <- reactive({

    # Filter using the type and severity inputs
    myEntriesFiltered <- timelineData
    
    # If required, just include the "key events"
    if (input$EventTypeRadioButton == "Key events") {
      myEntriesFiltered <- myEntriesFiltered[myEntriesFiltered$keyEvent == TRUE,]
    }
    
    groupsTemp <- groups %>%
      dplyr::rename(groupDescription = content)
    
    myEntriesFiltered <- myEntriesFiltered %>%
      dplyr::left_join(groupsTemp, by = c("group"="id"))

    # If we have something in the group type input then filter using that
    if (length(input$GroupSelect) == 0 & is.null(input$GroupSelect)){
      myEntriesFiltered <- myEntriesFiltered[0==1,]
    } else {
      # Filter by the group types
      myEntriesFiltered <- myEntriesFiltered[tolower(myEntriesFiltered$groupDescription)  %in% tolower(input$GroupSelect),]
    }
    
    myEntriesFiltered

  })
  
  # Filter the groups using the widget values
  FilterGroups <- reactive({

    myGroupsFiltered <- groups
    
    if(length(input$GroupSelect) == 0 & is.null(input$GroupSelect)){
      myGroupsFiltered <- myGroupsFiltered[0==1,]
    } else {
      myGroupsFiltered <- myGroupsFiltered[tolower(myGroupsFiltered$content) %in%  tolower(input$GroupSelect),]
    }
    
    myGroupsFiltered
    
  })

  # Create the timeline using the timevis package
  output$timeline <- renderTimevis({
    timevis(data = FilterEntries(),
            groups = FilterGroups(),
            showZoom = TRUE,
            fit = TRUE)
  })

  #Display the timeline data in a table
  output$timelineTable <- DT::renderDataTable({

    data <- FilterEntries() %>%
      dplyr::mutate(htmlLink =  ifelse(link == "", link , paste0("<a href='",link, "'  target='_blank'>Link</a>"))) %>%
      dplyr::arrange(start) %>%
      dplyr::mutate(startYear = substr(start, 1, 4))   %>%
      dplyr::select(title,description,htmlLink, startYear, groupDescription)  %>%
      dplyr::rename(Title = title, Description = description, Link = htmlLink, Year = startYear, Type = groupDescription)
  }, rownames= FALSE, escape = FALSE, options = list(dom = 'tp', pageLength = 25))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
