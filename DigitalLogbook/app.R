#Databasen Digital_instrumentloggbok ligger på här: Mtc07d1.mta.karolinska.se
#GibHub CRUD

library(shiny)
library(shinyWidgets)
library(DBI)
library(pool)
library(odbc)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)

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
    setBackgroundColor(
        color = c("#68c2ff", "#2171B5"),
        gradient = "linear",
        direction = "bottom"),
    
    
        
        # Application title
        titlePanel("Digital Instrument logbook"),
        sidebarLayout(
            sidebarPanel(
            dateInput(inputId = "date",
                      label = "Date",
                      weekstart = 1),
            
            textInput(inputId = "ID",
                      label = "HSAId",
                      value = "XXXX"),
            
            radioButtons(inputId = "instr",
                         label = "Instrument",
                         choices = c("Asterix",
                                     "Obelix", 
                                     "Pluto", 
                                     "Langben", 
                                     "Mimmi", 
                                     "Fido", 
                                     "Mymlan", 
                                     "Too-Ticki"),
                         selected = character(0)),
            
            textAreaInput(inputId = "event",
                          label = "Event",
                          value = "Problem"),
            
            textAreaInput(inputId = "solution",
                          label = "Action",
                          value = "Solution"),
            
            checkboxInput(inputId = "addfile", 
                          label = "Add file", 
                          value = FALSE, 
                          width = NULL),
            
            conditionalPanel(
                condition = "input.addfile == true",
                fileInput(inputId = "file_input", 
                          label = "Appendix (.pdf format only)", 
                          accept = c(".pdf"))
            ),
            
            actionButton(inputId = "submit", 
                         label = ("Add"),
                         class = "btn-success",
                         style = "color: #fff;",
                         icon = icon('plus'),
                         width = '100%')
        ),
    #Kopplat till att se ev. uppladdad PDF som ej ännu fungerar.
        mainPanel(uiOutput("pdfview"),
              tabsetPanel(
                  tabPanel(DT::dataTableOutput("tbl"))
              )
    )
))

server <- function(input, output, session) {
    
    output$tbl <- DT::renderDataTable({
        
        #Hämtar hela SQL-databasen med pool connection
        outp <- dbGetQuery(pool, "SELECT * from MSInstruments")
        
        #Gör directory xxx tillgängligt
        addResourcePath("pdf", "H:/R/Projects/test")
        
        #Skapar hyperlänkar i DT för kolumnen "Appendix"
        outp$Appendix <- paste0("<a href=pdf/",outp$Appendix ,">",outp$Appendix,"</a>")
        
        #Säkerställer att DT läser htmlkoden ovan
        datatable(outp, 
                  class = 'cell-border stripe',
                  callback = JS('table.page("last").draw(false);'),
                  escape = FALSE) 
                  
        # Obs PDFer namngivna med mellanslag genererar felmeddelande när hyperlänken klickas!
        
    })
    # Skriver data till databasen MSInstruments
    # Adderar all inputdata till dataframe.
    observeEvent(input$submit,{pool
        
        #Skapar df1 från input i GUI.
        if(input$addfile == FALSE) {
        df1 <- data.frame("Date" = input$date,
                                  "HSAId" = input$ID,
                                  "Instrument" = input$instr,
                                  "Event"= input$event,
                                 "Action"= input$solution)
        
        #Skriver df1 till SQL-databasen
        dbWriteTable(pool, "MSInstruments", df1, append = TRUE)}
        
        #Aktiverar skapandet av df2 ifall checkboxen addfile är checkad.
        if(input$addfile == TRUE) {
            df2 <- data.frame("Date" = input$date,
                         "HSAId" = input$ID,
                         "Instrument" = input$instr,
                         "Event"= input$event,
                         "Action"= input$solution,
                         "Appendix" = input$file_input$name)
        #Skriver df2 till SQL-databasen.
        dbWriteTable(pool, "MSInstruments", df2, append = TRUE)
            
        }
        output$tbl <- DT::renderDataTable({
            
            #Hämtar hela SQL-databasen med pool connection
            outp <- dbGetQuery(pool, "SELECT * from MSInstruments")
            
            #Gör directory xxx tillgängligt
            addResourcePath("pdf", "H:/R/Projects/test")
            
            #Skapar hyperlänkar i DT för kolumnen "Appendix"
            outp$Appendix <- paste0("<a href=pdf/",outp$Appendix ,">",outp$Appendix,"</a>")
            
            #Säkerställer att DT läser htmlkoden ovan
            datatable(outp, 
                      class = 'cell-border stripe',
                      callback = JS('table.page("last").draw(false);'),
                      escape = FALSE) 
            
            # Obs PDFer namngivna med mellanslag genererar felmeddelande när hyperlänken klickas!
            
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
            tags$iframe(style = "height:600px; width:100%; scrolling = yes", src = "0.pdf")
        })
        
    })
}

shinyApp(ui, server) 

# dbExecute(pool, "DELETE FROM MSInstruments") för att tömma MSInstruments
# dbExecute(pool, "ALTER TABLE MSInstruments DROP COLUMN Appendix") för att ta bort kolumnen kallad Appendix
