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
  titlePanel("100 years of data management at ICES"),
  hr(),
      tabsetPanel(
        tabPanel("Introduction",
                 "Add text to describe the app here"
        ),
        tabPanel("Selected Events",
                 selectInput("GroupSelect",
                             label="Type",
                             choices=groups$content,
                             selected=groups$content,
                             multiple = TRUE),
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

    # if (link == "") {
    #   sprintf(
    #     '<table><tbody>
    #   <tr><td><em>%s</em></td></tr>
    #   <tr>
    #     <td>%s</td>
    #   </tr>
    # </tbody></table>',
    #     title, shortBody)
    # } else {
    #   sprintf(
    #     '<table><tbody>
    #   <tr><td><em><a href="%s" target="_blank">%s</a></em></td></tr>
    #   <tr>
    #     <td>%s</td>
    #   </tr>
    # </tbody></table>',
    #     link, title, shortBody)
      
      sprintf(allCode,
              link, title, shortBody)

  }
  
  # Filter the input data using the widget values
  FilterEntries <- reactive({

    # Filter using the type and severity inputs
    myEntriesFiltered <- timelineData
    
    groupsTemp <- groups %>%
      dplyr::rename(groupDescription = content)
    
    myEntriesFiltered <- myEntriesFiltered %>%
      dplyr::left_join(groupsTemp, by = c("group"="id"))

    # If we have soemthing in the issue input then filter using that
    if (length(input$GroupSelect) == 0 & is.null(input$GroupSelect)){
      myEntriesFiltered <- myEntriesFiltered[0==1,]
    } else {
      # Filter by the issue numbers
      myEntriesFiltered <- myEntriesFiltered[tolower(myEntriesFiltered$groupDescription)  %in% tolower(input$GroupSelect),]
    }

    myEntriesFiltered

  })

  # Filter and fromat data for the time line
  FormatDataForTimeline <- reactive({

    myData <- FilterEntries()

    #print(myData)

    # Create a new column in the data frame with the template
    # use sapply to apply the templateDIG function to each row of the data frame
    myData$content <- sapply(1:nrow(myData), function(i) {
      templateDIG(myData$title[i],
                  myData$description[i],
                  myData$link[i])
    })

    myData

  })


  output$timeline <- renderTimevis({
    timevis(data = FormatDataForTimeline(),
            groups = groups,
            showZoom = TRUE,
            fit = TRUE)
  })

  #Display the timeline in a table
  output$timelineTable <- DT::renderDataTable({

    data <- FilterEntries() %>%
      dplyr::mutate(htmlLink =  paste0("<a href='",link, "'  target='_blank'>Link</a>")) %>%
      dplyr::arrange(start) %>%
      dplyr::select(title,description,htmlLink,start, groupDescription)  %>%
      dplyr::rename(Title = title, Description = description, Link = htmlLink, Date = start, Type = groupDescription)
  }, rownames= FALSE, escape = FALSE, options = list(dom = 'tp', pageLength = 25))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
