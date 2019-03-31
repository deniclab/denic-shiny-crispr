# load dataset, libraries, and helper scripts
crispr_df <- readRDS('combined_spread.rds')
require(shiny)
require(ggplot2)
require(stringr)
require(DT)
source('helpers.R')

  #-----------------------------------------------------------------------------#
  #--------<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-------#
  #------------------------------------- UI ------------------------------------#
  #--------<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-------#
  #-----------------------------------------------------------------------------#
ui <- fluidPage(
  #-----------------------------------------------------------------------------#
  #------------------------------- HTML/css tags -------------------------------#
  #-----------------------------------------------------------------------------#
  tags$head(
    tags$style(
      HTML('
         #attr_footer {background-color:#FFFFFF;}
         hr {border-top: 1px solid #888888;
             margin: 0px;
             padding: 0px;}
         ')
    )
  ),
  #-----------------------------------------------------------------------------#
  #--------------------------------- Main body ---------------------------------#
  #-----------------------------------------------------------------------------#
  absolutePanel(top=0, bottom=125, left=0, width='100%',
                fluidRow(
                  titlePanel(h3("Shoemaker et al, 2017 - K562 CRISPR KO screens"),
                             windowTitle = "Shoemaker et al, 2017 - K562 CRISPR KO screens"), align='center'
                ),
                tabsetPanel(
  #------------------------------ first tab: plot ------------------------------#
                  tabPanel(
                    'Plot',
                    fluidRow(style='padding-left:20px;
                             padding-right: 20px;
                             margin-top: -5px;
                             text-align:justify;',
                             h3("Instructions"),
                             p(paste0("Using the radio buttons on the left, select plot type: single experiment (rank-ordered) ",
                                      "or two-experiment scatter. Use the ",
                                      "dropdown menus to select experiments to plot. Hover over points to see ",
                                      "which genes they correspond to, click on points to see source data, or highlight ",
                                      "genes of interest by entering them in the box on the left. Hovering is ",
                                      "disabled when a gene has been typed or clicked on. Reset selection using the Reset ",
                                      "button. Source data for highlighted genes is shown in the table below. Use the ",
                                      "radio buttons to the left of the table to control what data is displayed."))
                    ), # end instructions fluidRow
                    fluidRow(
                      column(3,
                             wellPanel( # equivalent to sidebarPanel, but using fluidRow instead of sidebarLayout
                               # implement radio button to switch between single experiment vs
                               # two-experiment plots
                               radioButtons(
                                 "plot_type", h3("Plot type"),
                                 choices = list("Single experiment rank-ordered " = 1, 
                                                "Two-experiment scatter" = 2),
                                 selected = 1),
                               # next line reactively displays dropdown menus for expt options
                               # depending upon the plot_type: one dropdown for one expt, two
                               # for two-expt scatter
                               htmlOutput('expt_type'),
                               textInput("highlighted_gene",
                                         label=h5('Enter comma-separated gene names to highlight in the plot'),
                                         value='',
                                         placeholder='Optional: Enter gene names for source data'),
                               span(textOutput('gene_warnings'), style="color:red"),
                               actionButton('reset', 'Reset')
                             ) # close wellPanel
                      ), # close column
                      column(9,
                             # display plot; see server() for logic describing what is shown
                             plotOutput("plot1", hover = "plot_hover", click = "plot_click",
                                        height='550px'),
                             br(),
                             br()
                      ) # end column 2
                    ), # end fluidRow
                    fluidRow( # these are accessed for plot_get_cols to help select columns
                      column(3,
                             h3('Data table values'),
                             radioButtons("plot_samples", # which samples should be shown in tbl?
                                          h4('Samples:'),
                                          choices=list(
                                            'Replicate means' = 1,
                                            'Individual replicates' = 2,
                                            'Both' = 3
                                          ),
                                          selected = 1),
                             radioButtons('table_expts', # which experiments should be shown in tbl?
                                          h4('Show data for:'),
                                          choices=list(
                                            'Only plotted experiments' = 1,
                                            'All experiments' = 2
                                          ),
                                          selected = 1),
                             radioButtons('plot_data_format', # do you want to show beta scores or rank?
                                          h4('Score format:'),
                                          choices=list(
                                            'Beta scores' = 1,
                                            'Rank within experiment' = 2,
                                            'Both' = 3
                                          ),
                                          selected=3)
                      ), # end 1st column
                      column(9,
                             p(em("Table column names are formatted "),
                               strong("reporter _ (replicate # or mean) _ (beta score or rank)") 
                             ),
                             tableOutput('plot_table')
                      ) # end column 2
                    ) # end fluidRow
                  ), # end tabPanel Plot
  #----------------------------- second tab: table -----------------------------#
                  tabPanel('Table', # 2nd page: show tabular data
                           fluidRow(
                             style='padding:20px; text-align:justify;',
                             h3("Instructions"),
                             p(paste0("Using the checkboxes on the left, select experiments to include in the table. ",
                                      "Use the radio buttons to indicate whether experiment means or replicates should ",
                                      "be shown, as well as the score format. Column names are formatted"),
                               strong("reporter _ (replicate # or mean) _ (beta score or rank)."),
                               paste0("Search for individual genes using the search box at the top. Sort by a column by ",
                                      "clicking on the column name. Filter rows based on a column by clicking on the box ",
                                      "below the column name, then use the slider to control the filter. "))
                           ), # end instructions fluidRow
                           fluidRow(
                             column(
                               3,
                               wellPanel( # sidebar with radio buttons and checkboxes for options
                                 h3('Data table values'),
                                 checkboxGroupInput( # which experiments should be shown in the table?
                                   'main_table_cols', label=h4('Experiments to show'),
                                   choices=list(
                                     "LC3" = "LC3", "SQSTM1" = 'SQSTM1', "NBR1" = "NBR1",
                                     "TAX1BP1" = "TAX1BP1", "NDP52" = "NDP52"),
                                   selected= c('LC3','SQSTM1','NBR1','TAX1BP1','NDP52')),
                                 radioButtons(
                                   "table_samples", h4('Samples:'), # show means, replicates, both?
                                   choices=list(
                                     'Replicate means' = 1,
                                     'Individual replicates' = 2,
                                     'Both' = 3
                                   ),
                                   selected = 1),
                                 radioButtons(
                                   'table_data_format', h4('Score format:'), # show beta scores, rank, both?
                                   choices=list(
                                     'Beta scores' = 1,
                                     'Rank within experiment' = 2,
                                     'Both' = 3
                                   ),
                                   selected=1)
                               ) # end wellPanel
                             ), # end 1st column 
                             column(9,
                                    DT::dataTableOutput('main_table')
                             ) # end 2nd column
                           ) # end fluidRow
                  ) # end tabPanel Table
                ), # end tabsetPanel
                fluidRow(
                br(),
                br(),
                br()
                )
  ),
  #-----------------------------------------------------------------------------#
  #------------------------------- Fixed footer --------------------------------#
  #-----------------------------------------------------------------------------#
  fixedPanel(id = 'attr_footer', bottom=0, left=0, height=50, width='100%',
             hr(style='color: 222222'),
             column(width=3,
                    a(img(src='deniclab-with-crest.png', height='40px'), href='http://www.deniclab.com'),
                    align='center'),
             column(width=3,
                    a(img(src='bioRxiv_MS_logo.png', height='40px'), href='https://www.biorxiv.org/content/early/2017/12/06/229732'), align='center', style='margin-top: 5px'),
             column(width=3,
                    br(),
                    downloadButton('downloadData', "Download source data",
                                   style='color: #ce0000; margin-top: -15px'),
                    align='center'),
             column(width=3,
                    HTML(
                      '<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a>'), align='center', style='margin-top: 7px')
  )
) # end fluidPage


#-----------------------------------------------------------------------------#
#--------<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-------#
#----------------------------------- Server ----------------------------------#
#--------<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-------#
#-----------------------------------------------------------------------------#
server <- function(input, output, session) {
  
  #-----------------------------------------------------------------------------#
  #---------- reactives to get dynamic values from helper.R functions ----------#
  #-----------------------------------------------------------------------------#
  reactive_split_genes <- reactive({
    split_genes(crispr_df$Gene, input$highlighted_gene)
  })
  
  axes_cols <- reactive({
    get_axes(var1=input$var1, var2=input$var2, var3=input$var3, plot_type=input$plot_type)
  })
  
  plot_table_cols <- reactive({
    plot_get_cols(crispr_df, var1 = input$var1, var2 = input$var2, 
                  var3 = input$var3, plot_type = input$plot_type, 
                  sample_rb = input$plot_samples, expts_rb = input$table_expts,
                  format_rb = input$plot_data_format)
  })
  
  main_table_cols <- reactive({
    main_table_get_cols(crispr_df, expt_vec = input$main_table_cols,
                        sample_rb = input$table_samples, 
                        format_rb = input$table_data_format)
  })
  
  merged_hl_genes <- reactive({
    vector_to_hl_string(nearPoints(
      crispr_df, input$plot_click,
      xvar=axes_cols()$x_ax,
      yvar=axes_cols()$y_ax))
  })
  
  #-----------------------------------------------------------------------------#
  #------ output attributes to generate plots, tables, and input widgets -------#
  #-----------------------------------------------------------------------------#
  
  # generate initial plot
  output$plot1 <- renderPlot({ # see helpers.R for the plot_expts fxn
    req(input$var3)
    plot_expts(crispr_df, input$var3)
  })
  
  # make table to show beneath plot
  output$plot_table <- renderTable({
    if (input$highlighted_gene == ''){
      nearPoints(crispr_df, input$plot_hover,
                 xvar=axes_cols()$x_ax,
                 yvar=axes_cols()$y_ax)[,c(1,plot_table_cols())]
    }else{
      subset(crispr_df, 
             Gene %in% reactive_split_genes()$present)[,c(1,plot_table_cols())]
    }
  }, spacing='xs', striped=TRUE)
  
  # make main table to show in the Table tabPanel
  output$main_table <- DT::renderDataTable({
    crispr_df[,c(1,main_table_cols())]
  }, class=c('compact','hover','cell-border', 'order-column'),
  rownames = FALSE, filter='top'
  )
  # TODOS FOR output$main_table:
  # 1. implement sorting, either here or upstream somewhere, so top hits shown 1st
  # (could have "sort" buttons at the top of each row to sort by that row?)
  # 2. add sliders for each sample to show data within a specific range - should
  # react to changes from beta score to rank, etc.
  # 3. add reset button to reset to defaults
  
  # renderUI to produce experiment dropdown lists reactive on radio buttons
  output$expt_type <- renderUI({ # displays dropdown menu listing axis options
    if (input$plot_type == 2){ # if showing two-experiment scatter
      tagList(
        helpText("Select two reporters to compare"),
        plot_ax_select('var1', 'Choose x-axis variable'), # see helpers.R for fxn
        plot_ax_select('var2', 'Choose y-axis variable', initial='SQSTM1')
      ) # close tagList
    }else{ # if showing one-experiment rank vs. beta
      tagList(
        helpText("Select a reporter to plot by rank"),
        plot_ax_select('var3', 'Choose experiment to plot')  # see helpers.R
      ) # close tagList
    } # close else
  }) # close renderUI - for displaying dropdown menus listing axis options
  
  # download handler for saving csv-formatted source data
  output$downloadData <- downloadHandler(
    filename = "Shoemaker_2017_CRISPR_screens.csv",
    content = function(file) {
      write.csv(crispr_df, file, row.names = FALSE)
    })
  
  #-----------------------------------------------------------------------------#
  #--------------------------------- observers ---------------------------------#
  #-----------------------------------------------------------------------------#
  
  # following object is primary observer for re-plotting data
  observe({
    if (identical(reactive_split_genes()$absent, character(0))){
      print_genes = ''
    }else if (reactive_split_genes()$absent == ''){ # if gene_list$absent is empty
      print_genes = ''
    }else{ # if there are genes in the input list that aren't in crispr_df$Gene
      print_genes = c('WARNING: gene(s)', reactive_split_genes()$absent, 'not in dataset')
      print_genes = paste(print_genes, collapse=' ')
    }
    output$gene_warnings <- renderText({print_genes})
    if (input$plot_type == 1){ # if doing a one-experiment rank vs. beta plot
      req(input$var3)
      output$plot1 <- renderPlot({ # see helpers.R for the plot_expts fxn
        plot_expts(crispr_df, input$var3,
                   gene_vec=reactive_split_genes()$present)
      })
    }else{ # if doing a two-experiment scatter plot
      req(input$var1, input$var2)
      output$plot1 <- renderPlot({
        plot_expts(crispr_df, input$var1, input$var2,
                   gene_vec=reactive_split_genes()$present)
      })
    }
  }) # close observe
  
  # observer to watch for plot clicks
  observeEvent(input$plot_click, {
    # update highlighted_gene text input with the gene name you just clicked
    updateTextInput(session, 'highlighted_gene', value=merged_hl_genes())
  })
  
  # watch for reset button to be pressed, when it is,
  # reset the highlighted_gene input to ''
  observeEvent(input$reset, {
    updateTextInput(session, 'highlighted_gene', value='')
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)
