


classify <- function(sentinel2, df1, id , apath, myrastername, created){

  fluidPage <- shiny::fluidPage
  sidebarLayout <- shiny::sidebarLayout
  useShinyalert <- shinyalert::useShinyalert
  selectInput <- shiny::selectInput
  sliderInput <- shiny::sliderInput
  actionButton <- shiny::actionButton
  onclick  <- shinyjs::onclick
  mainPanel <- shiny::mainPanel
  plotlyOutput <- plotly::plotlyOutput
  verbatimTextOutput <- shiny::verbatimTextOutput
  tableOutput <- shiny::tableOutput
  renderPlotly <- plotly::renderPlotly
  geom_text <- ggplot2::geom_text
  theme <- ggplot2::theme
  ggplotly <- plotly::ggplotly
  renderPrint <- shiny::renderPrint
  observeEvent <- shiny::observeEvent
  mutate  <- dplyr::mutate
  rename <- dplyr::rename
  select <- dplyr::select
  shinyalert <- shinyalert::shinyalert
  shinyApp <- shiny::shinyApp
  sidebarPanel <- shiny::sidebarPanel
  tags <- shiny::tags
  event_data <- plotly::event_data
  layout <- plotly::layout
  aes <- ggplot2::aes



  ui <- fluidPage(
    ######################################################
    sidebarLayout(position = 'left',
                  ################## SIDEPANEL
                  sidebarPanel(
                    useShinyalert()
                    ,selectInput('index'
                                 ,'Pick Index'
                                 ,c('b_ndvi', 'b_npcri', 'b_si'
                                    , 'b_ndsi', 'b_ndwi', 'b_mndwi', 'b_bsi', 'b_nbai', 'b_ndbi')
                    )
                    ,sliderInput("min", "min", value = 0, min = 0, max = 99)
                    ,sliderInput("max", "max", value = 99, min = 0, max = 99)
                    ,selectInput("xclass", "Pick Class", c("tree", "grass", "soil",'built','water','cloud'))
                    ,useShinyalert()
                    ,actionButton('saveData','Save')
                    ,tags$button(
                      id = 'close',
                      type = "button",
                      class = "btn action-button",
                      onclick = "setTimeout(function(){window.close();},500);",  # close browser
                      "Close window"

                    )
                  ),
                  ##############################################
                  mainPanel(
                    plotlyOutput("plot",height='700px',width='700px'),
                    verbatimTextOutput("brush")
                    ,tableOutput("data")
                    ,width=6
                    ,tags$style(type="text/css",
                                ".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }")
                  )
    ))
  ####################################################################
  ####################################################################

  server <- function(input, output, session) {
    output$plot <- renderPlotly({

      p <- sentinel2 +
        geom_text(
          data = filter(df1
                        , get(input$index) > input$min & get(input$index) < input$max)
          , aes(x = x, y = y
                , label = get(input$index)
                , color = as.factor(get(input$index)))
          ,size = 2.5
        ) +
        theme(legend.position = "none")

      ggplotly(p) %>% layout(dragmode = "select")
    })

    # SELECT POINTS WITH BRUSH
    output$brush <- renderPrint({
      d <<- event_data("plotly_selected") %>% mutate(catx = input$xclass)
      req(d)
      print(select(d,-curveNumber,-pointNumber))
    })
    #####
    observeEvent(input$saveData,{
      zclass <<- as.character(distinct(d,catx))
      foo <<- select(d,x,y,catx) %>% mutate(id = id) %>% rename(xclass = catx)
      filename <- paste0(apath,'raster_',myrastername,'_id_', id, '_class_', zclass,'_created_',created,'.rds')
      saveRDS(foo,filename)

      shinyalert('okay, it is saved')


    })
    session$onSessionEnded(function() { stopApp() })
  }
  ################################################################
  ################################################################
  shinyApp(ui, server)
}
