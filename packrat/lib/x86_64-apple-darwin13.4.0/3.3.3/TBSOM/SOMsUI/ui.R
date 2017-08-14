# skin = "red",
dashboardPage(skin = 'red',
    dashboardHeader( 
      title = 'SOM'
    ),# end dashboardHeader
    dashboardSidebar(
      sidebarMenu(
        menuItem("Welcome", tabName = "welcome", icon = icon("info")),
        menuItem("Import Data", tabName = "import", icon = icon("upload")),
        menuItem("Profiling", tabName = "profiling", icon = icon("search")),
        menuItem("SOM", icon = icon("th"), tabName = "SOM",
          menuSubItem('Train SOM', tabName = 'trainSOM', icon = icon("gears")),
          menuSubItem('Eval SOM', tabName = 'evalSOM', icon = icon("check-square")),
          menuSubItem('Clustering', tabName = 'clusterSOM', icon = icon("gears")),
          menuSubItem('Eval Clustering', tabName = 'evalClustering', icon = icon("check-square"))#,
        #  menuSubItem('Compare Clustering', tabName = 'compareClustering', icon = icon("dashboard"))
        ),
        menuItem("Download", tabName = "download", icon = icon("download")),
        menuItem("Help", tabName = "help", icon = icon("question"))
      )
    ),# end dashboardSidebar
    
    ## Body content
    dashboardBody(
      tabItems(
        tabItem(tabName = "welcome",
                includeMarkdown("Intro.md")
                ),
        tabItem(tabName = "import",
                  fluidPage(
                    fluidRow(
                      box(width=4, 
                          title = "Data Import",
                          #status = "primary", 
                          solidHeader = FALSE,
                              fileInput("inFile", label="Choose File",  accept = c(
                                   'text/csv',
                                   'text/comma-separated-values',
                                   'text/tab-separated-values',
                                   'text/plain',
                                   '.csv',
                                   '.tsv')),
                               selectInput("inFile_header", "Header:",
                                           c("Auto"="auto","Yes" = TRUE, "No" = FALSE), 'auto'),
                               selectInput("inFile_rownames", "Row Names:",
                                           c("Yes"='TRUE', "No"='FALSE'), 'FALSE'),
                               selectInput("inFile_sep", "Separator:",
                                           c("Auto","Comma", "Semicolon", "Tab", "Space"), 'auto'),
                               selectInput("inFile_dec", "Decimal mark:",
                                           c("Dot" = '.', "Comma" = ','), '.'),
                               actionButton("loadTaFeng", "Load Demo Data", width = '100%')
                             ),# end box
                      box(width=8, 
                          title = "Data Preview",
                          #status = "primary", 
                          solidHeader = FALSE,
                          rHandsontableOutput("preview"))
                    )# end fluidRow
                  )# end fluidPage
                ),# end tabItem
        tabItem(tabName = "profiling",
                fluidPage(
                  fluidRow(
                    box(width=12, 
                        title = "Data Overview & Feature Selection",
                        #status = "primary", 
                        solidHeader = FALSE,
                        br(),
                        rHandsontableOutput("overview")
                    )# end box
                  )# end fluidRow
                )# fluidPage
        ),# tabItem
        tabItem(tabName = "trainSOM",
                fluidPage(
                  fluidRow(
                    box(width = 4,
                        bsButton("som_train", label = " Select at least 2 features", disabled = TRUE, style = "danger", icon("ban"), block = TRUE),
                        hr(),
                        numericInput("som_xdim", label = "Map dimension - x:", value = 5, min = 1),
                        numericInput("som_ydim", label = "Map dimension - y:", value = 5, min = 1),
                        numericInput("som_rlen", label = "Iterations:", value = 100, min = 1),
                        selectInput("som_topo", "Topology:",
                                    c("Hexagonal" = "hexagonal",
                                      "Rectangular" = "rectangular")),
                        selectInput("som_torodial", "Torodial map:",
                                    c("False" = 'FALSE',
                                      "True" = 'TRUE')),
                        # in a hexagonal toroidal map, the number of rows must be even
                        numericInput("som_seed", label = "Seed:", value = 42),
                        title = "SOM Options",
                        #status = "primary", 
                        solidHeader = FALSE
                             
                    ),# box
                    tabBox(
                      width = 8,
                      title = "SOM Visualisation",
                      #status = "primary", 
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "SOMtrainingTabBox", height = "250px", 
                      tabPanel("fan",     plotOutput("SOMtrainedFan")),
                      tabPanel("changes", plotOutput("SOMtrainedChanges")),
                      tabPanel("counts",  plotOutput("SOMtrainedCounts")),
                      tabPanel("quality", plotOutput("SOMtrainedQuality")),
                      tabPanel("dist",    plotOutput("SOMtrainedDist"))
                    )# end tabBox
                  )# end fluidRow
                )# end fluidPage
        ),# end tabItem
        tabItem(tabName = "evalSOM",
                fluidPage(
                  fluidRow(
                    box(
                      width = 12,
                      title = "Feature Heatmaps",
                      #status = "primary", 
                      solidHeader = FALSE,
                      uiOutput("SOMevalGallery")

                    ),
                    bsModal("bsmodal1", "Zoomtest", "plot_click1", #size = "large",
                            plotOutput("zoomplot1"))
                    # TODO (DKO 13.07.2016) add bsModal for every figure
                    
                  )# end fluidRow
                )# end fluidPage
        ),# end tabItem
        tabItem(tabName = "clusterSOM",
                fluidPage(
                  fluidRow(
                    box(width=4, 
                        title = "Cluster Options",
                        #status = "primary", 
                        solidHeader = FALSE,
                        actionButton("som_cluster", label = "Cluster SOM"),
                        hr(),
                        selectInput("cluster_type", "Cluster algorithm:",
                                    c("ward.D", "ward.D2", "ward.adjacent", "single", "complete", "average", "mcquitty", "median", "centroid"), 'ward.D'),
                        sliderInput("cluster_num", "Number of clusters:",
                                    min = 2, max = 10, value = 3, step = 1),
                        hr(),
                        actionButton("som_cluster_reset", label = "Reset selection"),
                        plotOutput("som_cluster_num")
                    ),# end box
                    box(width=8, 
                        title = "SOM Clusters",
                        #status = "primary", 
                        solidHeader = FALSE,
                        plotOutput("som_clustered", click = "click_SOMcluster")
                    )# end box
                  )# end fluidRow
                )# end fluidPage
        ),# end tabItem
        tabItem(tabName = "evalClustering",
                fluidPage(
                  fluidRow(
                    box(
                      width = 12,
                      title = "Cluster Characteristics",
                      #status = "primary", 
                      solidHeader = FALSE,
                      uiOutput("SOMdensityGallery")
                      
                    )# end box
                  )# end fluidRow
                )# end fluidPage
        ),#t end abItem
        tabItem(tabName = "download",
                fluidPage(
                  fluidRow(
                    box(
                      width = 12,
                      title = "Download Results",
                      #status = "primary", 
                      solidHeader = FALSE
                      #uiOutput("SOMevalGallery")
                      )# end box
                ),# fluidRow
                fluidRow(
                  box(
                   width = 12,
                   fluidPage(
                     fluidRow(
                       column( width=4,
                               #disabled=TRUE if no results
                   downloadButton("downall", label = "Download all")),
                   column(width=4,
                   downloadButton("downfig", label = "Download figures")),
                   column(width=4,
                   downloadButton("downdata", label = "Download Models"))
                   )
                  )
                  )
                )
                )# fluidPage
        )#tabItem
      )# end tabItems
    )# end dashboardBody
)# end dashboardPage
