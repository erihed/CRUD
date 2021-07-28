#Databasen Digital_instrumentloggbok ligger på här: Mtc07d1.mta.karolinska.se
#GibHub CRUD

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyFeedback)
library(shinyjs)
library(thematic)
library(DBI)
library(pool)
library(odbc)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)
library(validate)
library(uuid)

#Database connection with pool!
pool <- dbPool(
    drv = odbc::odbc(),
    Driver   = "SQL Server",
    Server   = "mtc07d1.mta.karolinska.se",
    Database = "Digital_instrumentloggbok",
    trusted_connection = TRUE,
    Port     = 1433)
onStop(function() {
    poolClose(pool)
})

#Testing pool! Visar även om scriptet skriver till MSInstruments eller inte.
pool %>% tbl("MSInstruments") %>% head(15)
destDir <- "H:\\R\\Projects\\test"

ui <- fluidPage(
    theme = shinytheme("darkly"),
    shinyFeedback::useShinyFeedback(),
    
           # Application title
    titlePanel(
        h1("Digital Instrument logbook", align = "center")),
        sidebarLayout(
            sidebarPanel(
            dateInput(inputId = "date",
                      label = "Date",
                      weekstart = 1),
            
            textInput(inputId = "ID",
                      label = "HSAId",
                      value = ""),
            
            prettyRadioButtons(inputId = "instr",
                         label = "Instrument",
                         choices = c("Asterix",
                                     "Obelix", 
                                     "Pluto", 
                                     "Langben", 
                                     "Mimmi", 
                                     "Fido", 
                                     "Mymlan", 
                                     "Too-Ticki"),
                         icon = icon("check"),
                         status = "success",
                         bigger = TRUE,
                         selected = character(0)),
            
            
            textAreaInput(inputId = "event",
                          label = "Event",
                          value = "What has happened?"),
            
            textAreaInput(inputId = "solution",
                          label = "Action",
                          value = "What did you do?"),
            
            checkboxInput(inputId = "addfile", 
                          label = "Add file (names without spaces!)", 
                          value = FALSE, 
                          width = NULL),
        
            
            conditionalPanel(
                condition = "input.addfile == true",
                fileInput(inputId = "file_input", 
                          label = "Appendix (.pdf format only)",
                          multiple = TRUE,
                          accept = c(".pdf"))
            ),
            
            actionButton(inputId = "submit", 
                         label = "Send",
                         class = "btn-success",
                         style = "color: #fff;",
                         icon = icon('plus'),
                         width = '100%')
        ),
    #Kopplat till att se ev. uppladdad PDF som ej ännu fungerar.
        mainPanel(
              tabsetPanel(
                  tabPanel("LogBookTable", DT::dataTableOutput("tbl"),
                           fluidRow(
                               actionButton(inputId = "edit_button", label = "Edit", icon("edit")),
                               actionButton(inputId = "delete_button", label = "Delete", icon("trash-alt"))
                           ),
                           br(),),
                  tabPanel("Analysis", plotOutput("plot1"), plotOutput("plot2"), plotOutput("plot3")),
                  tabPanel("File Preview", uiOutput("pdfview"))        
                    
              )
    )
))

server <- function(input, output, session) {
    
    responses_df <- reactive({
        
        #make reactive to
        input$edit_button
        input$delete_button
        
        dbReadTable(pool, "MSInstruments")
        
    })  
    
    observeEvent((input$ID), {
        req(input$ID)
        if (nchar(input$ID) != 4 ) {
            showFeedbackDanger(
                inputId = "ID",
                text = "wrong number of characters!",
                color = "#F89406",
                icon = shiny::icon("exlamationmark", lib = "glyphicon")
                
            )
        } else {
            hideFeedback("ID")
        }
        
    })
    
    output$tbl <- DT::renderDataTable({
        
        #Hämtar hela SQL-databasen med pool connection
        outp <- dbGetQuery(pool, "SELECT * from MSInstruments")
        
        #Gör directory xxx tillgängligt
        addResourcePath("pdf", "H:/R/Projects/test")
        
        #Skapar hyperlänkar i DT för kolumnen "Appendix"
        outp$Appendix <- paste0("<a href=pdf/",outp$Appendix ,">",outp$Appendix,"</a>")
        
        outp <- outp[order(outp$Date),]
        
        #Säkerställer att DT läser htmlkoden ovan
       datatable(outp, 
                  class = 'cell-border stripe',
                  callback = JS('table.page("last").draw(false);'),
                  editable = FALSE, #Lek med denna för att kunna ändra i databasen?
                  selection = "single",
                  escape = FALSE,
                  style = "bootstrap")
        
        # Obs PDFer namngivna med mellanslag genererar felmeddelande när hyperlänken klickas!
    })
    # Skriver data till databasen MSInstruments
    # Adderar all inputdata till dataframe.
    
    analysisPlot <- dbGetQuery(pool, "SELECT * from MSInstruments")
    analysisPlot <- data.frame(analysisPlot)
    
    output$plot1 <- renderPlot({
        ggplot(data = analysisPlot, aes(x = Instrument)) +
            geom_histogram(stat = "count", fill = "#06ff8f")
    })
    output$plot2 <- renderPlot({
        ggplot(data = analysisPlot, aes(x = HSAId)) +
            geom_histogram(stat = "count", fill = "#06ff8f")    
        
    })
    
    observeEvent(input$submit,{pool
        req(input$ID, input$event, input$instr)
        req(nchar(input$ID) == 4)
        
        #Skapar df1 från input i GUI.
        
        
        if(input$addfile == FALSE) {
        
        
        df1 <- data.frame("RowID" = UUIDgenerate(),
                          "Date" = input$date,
                          "HSAId" = input$ID,
                          "Instrument" = input$instr,
                          "Event" = input$event,
                          "Action" = input$solution,
                          "Appendix" = "NA")
        
        #Skriver df1 till SQL-databasen
        dbWriteTable(pool, "MSInstruments", df1, append = TRUE)}
        
        #Aktiverar skapandet av df2 ifall checkboxen addfile är checkad.
        else {
        
            df2 <- data.frame("RowID" = UUIDgenerate(),
                         "Date" = input$date,
                         "HSAId" = input$ID,
                         "Instrument" = input$instr,
                         "Event"= input$event,
                         "Action"= input$solution,
                         "Appendix" = input$file_input$name)
        #Skriver df2 till SQL-databasen.
        dbWriteTable(pool, "MSInstruments", df2, append = TRUE)
            
        }
        output$tbl <- DT::renderDataTable({
            req(input$ID, input$event, input$instr)
            feedbackWarning("input$ID", show = nchar(input$ID) < 4,"Please add ID!")
            
            #Hämtar hela SQL-databasen med pool connection
            outp <- dbGetQuery(pool, "SELECT * from MSInstruments")
            
            #Gör directory xxx tillgängligt
            addResourcePath("pdf", "H:/R/Projects/test")
            
            #Skapar hyperlänkar i DT för kolumnen "Appendix"
            outp$Appendix <- paste0("<a href=pdf/",outp$Appendix ,">",outp$Appendix,"</a>")
            
            outp <- outp[order(outp$Date),]
            
            #Säkerställer att DT läser htmlkoden ovan
            datatable(outp, 
                      class = 'cell-border stripe',
                      callback = JS('table.page("last").draw(false);'),
                      escape = FALSE,
                      editable = FALSE,
                      selection = "single",
                      style = "bootstrap") 
            
            # Obs!! PDFer namngivna med mellanslag genererar felmeddelande när hyperlänken klickas!
            
        })
        
        analysisPlot <- dbGetQuery(pool, "SELECT * from MSInstruments")
        analysisPlot <- data.frame(analysisPlot)
        
        output$plot1 <- renderPlot({
            ggplot(data = analysisPlot, aes(x = Instrument)) +
                geom_histogram(stat = "count", fill = "#06ff8f")
        })
        output$plot2 <- renderPlot({
            ggplot(data = analysisPlot, aes(x = HSAId)) +
                geom_histogram(stat = "count", fill = "#06ff8f")    
            
        })
    })
    
    observe({
        req(input$file_input)
        
        #Kopierar pdf-filen från c:/temp till destDir som definierats utanför appen.
        file.copy(input$file_input$datapath,
                   file.path(destDir, input$file_input$name),
                  overwrite = F)
        
        #Fungerar ej ännu att visa den uppladdade PDFen!
        output$pdfview <- renderUI({
            tags$iframe(style = "height:600px; width:100%; scrolling = yes", src = "Anställningsunderlag_.pdf")
        })
        
    })
    entry_form <- function(button_id){
        
        showModal(modalDialog(title = "Edit data",
                              dateInput(paste0("date_add", input$add_button), "Date:", value = input$date),
                              textInput(paste0("ID_add", input$add_button), "HSAId:"),
                              radioButtons(paste0("instrument_add", input$add_button), "Instrument:", choices = c("Asterix",
                                                                                                               "Obelix", 
                                                                                                               "Pluto", 
                                                                                                               "Langben", 
                                                                                                               "Mimmi", 
                                                                                                               "Fido", 
                                                                                                               "Mymlan", 
                                                                                                               "Too-Ticki")),
                              textAreaInput(paste0("event_add", input$add_button), "Event:"),  
                              textAreaInput(paste0("solution_add", input$add_button), "Solution:"),
                              fileInput(paste0("file_add", input$add_button), "Appendix:"), 
                              actionButton(inputId = "edit_button", label = "Add item"),
                              easyClose = TRUE, footer = NULL ))
        }
    formData <- reactive({
        
        formData <- data.frame("RowID" = UUIDgenerate(),
                               "Date" = input$date_add,
                               "HSAId" = input$ID_add,
                               "Instrument" = input$instrument_add,
                               "Event" = input$event_add,
                               "Action" = input$solution_add,
                               "Appendix" = input$file_add)
        return(formData)
        dbWriteTable(pool, "MSInstruments", formData, append = TRUE)
    })
    
    appendData <- function(data){
        quary <- sqlAppendTable(pool, "MSInstruments", data, row.names = FALSE)
        dbExecute(pool, quary)
    }
    
    deleteData <- reactive({
        
        SQL_df <- dbReadTable(pool, "MSInstruments")
        row_selection <- SQL_df[input$tbl_rows_selected, "RowID"]
        
        quary <- lapply(row_selection, function(nr){
            dbExecute(pool, sprintf('DELETE FROM "MSInstruments" WHERE "RowID" == ("%s")', nr))
        })
    })
    observeEvent(input$delete_button, priority = 20,{
        
        if(length(input$tbl_rows_selected)>=1 ){
            deleteData()
        }
        
        showModal(
            
            if(length(input$tbl_rows_selected) < 1 ){
                modalDialog(
                    title = "Warning",
                    paste("Please select row(s)." ),easyClose = TRUE
                )
            })
    })
    
    observeEvent(input$edit_button, priority = 20,{
        
        SQL_df <- dbReadTable(pool, "MSInstruments")
        
        showModal(
            if(length(input$tbl_rows_selected) > 1 ){
                modalDialog(
                    title = "Warning",
                    paste("Please select only one row." ), easyClose = TRUE)
            } else if(length(input$tbl_rows_selected) < 1){
                modalDialog(
                    title = "Warning",
                    paste("Please select a row." ), easyClose = TRUE)
            })  
        
        if(length(input$tbl_rows_selected) == 1 ){
            
            entry_form("edit_button")
            
            updateDateInput(session, inputId = "date_add", value = SQL_df[input$tbl_rows_selected, "Date"])
            updateTextInput(session, inputId = "ID_add", value = SQL_df[input$tbl_rows_selected, "HSAId"])
            updateRadioButtons(session, inputId = "instrument_add", selected = SQL_df[input$tbl_rows_selected, "Instrument"])
            updateTextAreaInput(session, inputId = "event_add", value = SQL_df[input$tbl_rows_selected, "Event"])
            updateTextAreaInput(session, inputId = "solution_add", value = SQL_df[input$tbl_rows_selected, "Action"])
            
        }
        
    })
    observeEvent(input$submit_edit, priority = 10, {
        
        SQL_df <- dbReadTable(pool, "MSInstruments")
        row_selection <- SQL_df[input$tbl_row_last_clicked, "RowID"] 
        dbExecute(pool, sprintf('UPDATE "MSInstruments" SET "Date" = ?, "HSAId" = ?, "Event" = ?,
                          "Action" = ? "Appendix" = ? WHERE "RowID" = ("%s")', row_selection), 
                  param = list(input$date,
                               input$ID,
                               input$instr,
                               input$event,
                               input$solution,
                               input$addfile))
        removeModal()
        
    })
    output$tbl <- DT::renderDataTable({
        #Hämtar hela SQL-databasen med pool connection
        outp <- dbGetQuery(pool, "SELECT * from MSInstruments")
        
        #Gör directory xxx tillgängligt
        addResourcePath("pdf", "H:/R/Projects/test")
        
        #Skapar hyperlänkar i DT för kolumnen "Appendix"
        outp$Appendix <- paste0("<a href=pdf/",outp$Appendix ,">",outp$Appendix,"</a>")
        
        outp <- outp[order(outp$Date),]
        
        #Säkerställer att DT läser htmlkoden ovan
        datatable(outp, 
                  class = 'cell-border stripe',
                  callback = JS('table.page("last").draw(false);'),
                  editable = FALSE, #Lek med denna för att kunna ändra i databasen?
                  selection = "single",
                  escape = FALSE,
                  style = "bootstrap")
    
        # Obs PDFer namngivna med mellanslag genererar felmeddelande när hyperlänken klickas!
    })
    
}

thematic_shiny()
shinyApp(ui, server) 

# dbExecute(pool, "DELETE FROM MSInstruments") för att tömma MSInstruments
# dbExecute(pool, "ALTER TABLE MSInstruments DROP COLUMN Appendix") för att ta bort kolumnen kallad Appendix
