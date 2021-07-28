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
                         label = ("Send"),
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
                               actionButton("add_button", "Add", icon("plus")),
                               actionButton("edit_button", "Edit", icon("edit")),
                               actionButton("copy_button", "Copy", icon("copy")),
                               actionButton("delete_button", "Delete", icon("trash-alt"))
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
        input$submit
        input$submit_edit
        input$copy_button
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
        
        showModal(
            modalDialog(
                div(id=("entry_form"),
                    tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
                    tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
                    fluidPage(
                        fluidRow(
                            splitLayout(
                                cellWidths = c("250px", "100px"),
                                cellArgs = list(style = "vertical-align: top"),
                                dateInput("date", "Date"),
                                textInput("ID", labelMandatory("HSAId"), placeholder = ""),
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
                                textAreaInput("event", "Event"),
                                textAreaInput("solution", "Action", placeholder = "", height = 100, width = "354px"),
                                helpText(labelMandatory(""), paste("Mandatory field.")),
                                actionButton(button_id, "Send")
                                
                        ),
                        easyClose = TRUE
                    )
                )
            )
        )
        )}
    formData <- reactive({
        
        formData <- data.frame("RowID" = UUIDgenerate(),
                               "Date" = input$date,
                               "HSAId" = input$ID,
                               "Instrument" = input$instr,
                               "Event" = input$event,
                               "Action" = input$solution,
                               "Appendix" = "NA")
        return(formData)
        dbWriteTable(pool, "MSInstruments", formData, append = TRUE)
    })
    
    appendData <- function(data){
        quary <- sqlAppendTable(pool, "MSInstruments", data, row.names = FALSE)
        dbExecute(pool, quary)
    }
    
    observeEvent(input$add_button, priority = 20,{
        
        entry_form("submit")
        
    })
    observeEvent(input$submit, priority = 20,{
        
        appendData(formData())
        shinyjs::reset("entry_form")
        removeModal()
        
    })
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
    
    unique_id <- function(data){
        replicate(nrow(data), UUIDgenerate())
    }
    
    copyData <- reactive({
        
        SQL_df <- dbReadTable(pool, "MSInstruments")
        row_selection <- SQL_df[input$tbl_rows_selected, "RowID"] 
        SQL_df <- SQL_df %>% filter(RowID %in% row_selection)
        SQL_df$RowID <- unique_id(SQL_df)
        
        quary <- sqlAppendTable(pool, "MSInstruments", SQL_df, row.names = FALSE)
        dbExecute(pool, quary)
    })
    observeEvent(input$copy_button, priority = 20,{
        
        if(length(input$tbl_rows_selected)>=1 ){
            copyData()
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
                    paste("Please select a row NOW!" ), easyClose = TRUE)
            })  
        
        if(length(input$tbl_rows_selected) == 1 ){
            
            entry_form("submit_edit")
            
            updateDateInput(session, "date", value = SQL_df[input$tbl_rows_selected, "Date"])
            updateTextInput(session, "ID", value = SQL_df[input$tbl_rows_selected, "HSAId"])
            updatePrettyRadioButtons(session, "instr", selected = SQL_df[input$tbl_rows_selected, "Instrument"])
            updateTextAreaInput(session, "event", value = SQL_df[input$tbl_rows_selected, "Event"])
            updateTextAreaInput(session, "solution", value = SQL_df[input$tbl_rows_selected, "Action"])
            
        }
        
    })
    observeEvent(input$submit_edit, priority = 20, {
        
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
