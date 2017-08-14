# http://stackoverflow.com/questions/33722757/update-handsontable-by-editing-table-and-or-eventreactive

# TODO: update save & som training button

shinyServer(function(input, output, session) {
  
  # Import data -------------------------------------------------------------

  # The user can upload a data set or use the TaFeng Grocery Demo data set.
  # All import parameters for an uploaded data set are identified atumoatically
  # but they can also be changed manually.
  
  # container for reactive dataset
  dataSet <- reactiveValues()
  trigger <- reactiveValues()
  trigger[['selectFeatures']] <- 0
  
  # switch to demo mode if "Load Demo Data" Button is clicked
  observeEvent(input$loadTaFeng, {
    dataSet[['demo']] <- 1 # demo mode: on
    dataSet[["dfSummary"]] <- NULL
    trigger[['selectFeatures']] <- 0
    dataInput()
    updateButton(session ,"som_train", label = " Select at least 2 features", disabled = TRUE, style = "danger", icon("ban"), block = TRUE)
  })
  
  # switch to uploaded data mode if a data set is uploaded
  observeEvent(input$inFile, {
    dataSet[['demo']] <- 0 # demo mode: off
    dataSet[["dfSummary"]] <- NULL
    trigger[['selectFeatures']] <- 0
    dataInput()
    updateButton(session ,"som_train", label = " Select at least 2 features", disabled = TRUE, style = "danger", icon("ban"), block = TRUE)
  })
  
  dataInput <- reactive({
    
    # check whether demo or uploaded data should be used
    if(dataSet[['demo']] == 0){

      # use uploaded dataset
      if (is.null(input$inFile)){
        # stop if no dataset is entered
        return(NULL)
      } else {
        # workaround for "auto" | TRUE | FALSE fread() parameters
        inFile_header <- input$inFile_header
        if(inFile_header != "auto") inFile_header <- as.logical(inFile_header)
        
        inFile_sep <- switch(input$inFile_sep,
                             "Auto"      = "auto",
                             "Comma"     = ",",
                             "Semicolon" = ";",
                             "Tab"       = "\t",
                             "Space"     = " ")
        
        if (input$inFile_rownames == "TRUE") {
          df <- data.frame(fread(input$inFile$datapath, header=inFile_header, 
                                 sep=inFile_sep,
                                 dec=input$inFile_dec), row.names=1)
        } else {
          df <- data.frame(fread(input$inFile$datapath, header=inFile_header, 
                                 sep=inFile_sep, 
                                 dec=input$inFile_dec), row.names=NULL)
        }
      }
    } else {
      # use TaFeng Grocery Demo data
      #load("../../data/taFengGrocery.Rda")
      data("taFeng")
      df <- as.data.frame(taFeng)
      rm(taFeng)
    }
    
    # update reactive dataset (add a column for selected features)
    dataSet[["dfSummary"]] <- TBSOM::Data.Availability(df) %>% 
                            dplyr::mutate(use = FALSE) %>% 
                            dplyr::select(use,everything())

    # winsorize data to shrink outliers back to normal values
    val.numeric <- sapply(df, is.numeric)
    df[,val.numeric] <- sapply(df[,val.numeric], winsorize)
    
    df
  }) # end reactive
    
  # Import preview ----------------------------------------------------------
  
  # Preview the first 15 rows of the recently imported data for adhoc checks.
  
  output$preview <- renderRHandsontable({
    if(is.null(input$inFile) & input$loadTaFeng == 0){
      return(NULL)
    } else {
      DF <- dataInput() %>% filter(row_number() <= 15)
      rhandsontable(DF, readOnly = TRUE)
    }
  })
  
  # Overview and feature selection ------------------------------------------
  
  # Show some summary statistics for all available features. This overview
  # is also used for selecting the features that are used for the SOM training.
  
  output$overview <- renderRHandsontable({
    if(is.null(input$inFile) & input$loadTaFeng == 0){
      return(NULL)
    } else {
      trigger[['selectFeatures']] <- 1
      DF <- dataSet[["dfSummary"]]
      rhandsontable(DF, readOnly = TRUE, selectCallback = FALSE) %>% 
        hot_col("use", readOnly = FALSE) # change 'use' column to readable
    }
  })
  
  # Update selected features ------------------------------------------------
  
  # After (de-)selecting a feature the data set has to be updated. At least
  # 2 features are needed for the SOM training. Therefore the "Train SOM" 
  # button will be disabled if there are less than 2 features selected.
  
  observeEvent(input$overview, { 
    if (!is.null(input$overview) & trigger[['selectFeatures']] > 0) {
      dataSet[["dfSummary"]] <- hot_to_r(input$overview)
    }

    if(sum(dataSet[["dfSummary"]]$use) > 1){
      updateButton(session ,"som_train", label = " Train SOM", disabled = FALSE, style = "default", icon("check"), block = TRUE)
    } else {
      updateButton(session ,"som_train", label = " Select at least 2 features", disabled = TRUE, style = "danger", icon("ban"), block = TRUE)
    }
  })
  
  
  # Training SOM ------------------------------------------------------------
  
  # The SOM will be trained on all selected features in the overview tabset.
  
  # Train SOM after button is pressed
  SOMtrained <- eventReactive(input$som_train, {
    
    # Reproducible
    set.seed(input$som_seed)
    
    # Select only 'selected' features
    df <- dataInput()[,dataSet[["dfSummary"]]$use]
    
    # replacing facotrs with dummy variables
    df <- TBSOM::RFunction_Factor2Dummies(df)
    
    # Normalization
    df <- scale(df)
    
    # SOM training
    som(as.matrix(df),
        grid = somgrid(xdim = input$som_xdim, ydim = input$som_ydim, topo = input$som_topo), 
        rlen = input$som_rlen,
        toroidal = as.logical(input$som_torodial))

  })
  
  # SOM Evaluation Plots ----------------------------------------------------
  
  # Different plots will be generated which help the user to identify the
  # quality of the created SOM.
  
  output$SOMtrainedFan <- renderPlot({
    plot(SOMtrained(), type = "codes")
  })
  output$SOMtrainedChanges <- renderPlot({
    plot(SOMtrained(), type = "changes")
  })
  output$SOMtrainedCounts <- renderPlot({
    #plot(SOMtrained(), type = "counts", palette.name=coolBlueHotRed)
    TBSOM::hexagonalPlotEval(SOMtrained(), type = "counts")
  })
  output$SOMtrainedQuality <- renderPlot({
    #plot(SOMtrained(), type = "quality", palette.name=coolBlueHotRed)
    TBSOM::hexagonalPlotEval(SOMtrained(), type = "quality")
  })
  output$SOMtrainedDist <- renderPlot({
    #plot(SOMtrained(), type = "dist.neighbours", palette.name=grey.colors)
    TBSOM::hexagonalPlotEval(SOMtrained(), type = "dist.neighbours")
  })
  
  # Feature Heatmaps --------------------------------------------------------
  
  # For every selected feature a heatmap will be created to visualize the
  # distribution over the SOM. All heatmaps will be added to a reactive
  # gallery that automatically changes the number of plots per row depending
  # on the screen size.
  
  # http://stackoverflow.com/questions/32292547/shiny-switching-between-reactive-data-sets-with-rhandsontable
  
  # create all heatmaps
  observe({
    lapply(1:length(attributes(SOMtrained()$data)$dimnames[[2]]), function(i){
      output[[paste("plot", i, sep="") ]] <- renderPlot({
        hexagonalPlotEval(SOMtrained(), type = "heatmap", feature = i,
                          main=attributes(SOMtrained()$data)$dimnames[[2]][i])
      }, height = 250, width = 250)
    })
  })
  
  # CURRENTLY ONLY IMPLIMENTED FOR THE FIRST HEATMAP
  # create heatmaps again for zoomed view
  observe({
    lapply(1:length(attributes(SOMtrained()$data)$dimnames[[2]]), function(i){
      output[[paste("zoomplot", i, sep="") ]] <- renderPlot({
        hexagonalPlotEval(SOMtrained(), type = "heatmap", feature = i,
                          main=attributes(SOMtrained()$data)$dimnames[[2]][i])
        #plot(SOMtrained(), type = "property", property = SOMtrained()$codes[,i], 
        #     main=attributes(SOMtrained()$data)$dimnames[[2]][i], palette.name=coolBlueHotRed)
      }, height = 450, width = 450)
    })
  })
  
  # create taglist (= gallery) of all heatmaps
  output$SOMevalGallery <- renderUI({
    numfigures <- length(attributes(SOMtrained()$data)$dimnames[[2]])
    plot_output_list <- lapply(1:numfigures, function(i) {
      plotname <- paste("plot", i, sep="")
      plotclick <- paste("plot_click", i, sep="")
      plotOutput(plotname, inline = TRUE, click =plotclick, brush = "asd")
    })
    
    # Convert the list to a tagList - this is necessary for the list of items to display properly.
    do.call(tagList, plot_output_list)
  })
  
  # Open zoomed bsModal after click on plot
  observeEvent(input$plot_click1, {
    toggleModal(session = session , modalId = "bsmodal1" , toggle = "open")
  })
  
  # TODO (DKO 13.07.2016) add observeEvent for every figure
  
  # Cluster SOM -------------------------------------------------------------
  
  # Provide the user the ability to choose between several cluster algorithms
  # and also the number of created clusters can be adjusted. For identifying a
  # good number of clusters there is a help plot available.
  
  # help plot for identifying number of cluster
  output$som_cluster_num <- renderPlot({
    mydata <- SOMtrained()$codes
    wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
    for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                         centers=i)$withinss)
    plot(1:15, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")
  })
  
  # cluster SOM after button is pressed
  SOMclustered <- eventReactive(input$som_cluster, {
    # reset selection
    Nodes[["selected"]] <- NULL
    Clusters[["selected"]] <- NULL
    # cluster SOM
    if(input$cluster_type == "ward.adjacent"){
      adjacentWard(SOMtrained(), toroidal = FALSE)[[input$cluster_num]]
    } else {
      cutree(hclust(dist(SOMtrained()$codes), method = input$cluster_type), k = input$cluster_num)
    }
    
  })
  
  # plot clustered SOM
  output$som_clustered <- renderPlot({
    if(length(Clusters[["selected"]]) == 0){
      TBSOM::hexagonalPlot(SOMtrained(), values = SOMclustered(), addlegend = FALSE, segment = TRUE, colors = pretty_palette.alpha[SOMclustered()])
      add.cluster.boundaries(SOMtrained(), SOMclustered())
      
      #plot(SOMtrained(), type="mapping", bgcol = pretty_palette.alpha[SOMclustered()], main = "Clusters")
      #add.cluster.boundaries(SOMtrained(), SOMclustered())
    } else {
      highlight.color <- pretty_palette.alpha
      highlight.color[Clusters[["selected"]]] <- pretty_palette[Clusters[["selected"]]]
      hexagonalPlot(SOMtrained(), values = SOMclustered(), addlegend = FALSE, segment = TRUE, colors = highlight.color[SOMclustered()])
      #plot(SOMtrained(), type="mapping", bgcol = highlight.color[SOMclustered()], main = "Clusters")
      add.cluster.boundaries(SOMtrained(), SOMclustered())
    }
    
  })
  
  # Cluster SOM Selection ---------------------------------------------------

  # It is possible to select clusters with a click inside the clustered plot.
  # These selections have to be saved in order to update the evalution plots.
  # If you click on an already selected cluster you will deselect the cluster.
  # All selected clusters will be highlighted.
  
  Clusters <- reactiveValues()
  Nodes <- reactiveValues()
  
  # updated reactive values for selected clusters and nodes
  observeEvent(input$click_SOMcluster$x,{
    selectedNode <- nearestNode(SOMtrained(),c(input$click_SOMcluster$x,input$click_SOMcluster$y))
    selectedCluster <- SOMclustered()[selectedNode]

    # add/remove Cluster from selected Clusters
    if(!selectedCluster %in% Clusters[["selected"]]){
      Clusters[["selected"]] <- c(Clusters[["selected"]],selectedCluster)
    } else {
      Clusters[["selected"]] <- Clusters[["selected"]][selectedCluster != Clusters[["selected"]]]
    }

    # add/remove Node from selected Nodes
    if(!selectedNode %in% Nodes[["selected"]]){
      Nodes[["selected"]] <- c(Nodes[["selected"]],selectedNode)
    } else {
      Nodes[["selected"]] <- Nodes[["selected"]][selectedNode != Nodes[["selected"]]]
    }
  })
  
  # reset selection after pressing the button
  observeEvent(input$som_cluster_reset,{
    Nodes[["selected"]] <- NULL
    Clusters[["selected"]] <- NULL
  })
  
  # Cluter Evaluation -------------------------------------------------------
  
  # Create density plots for all chosen features grouped by the clusters. Only
  # highlighted clusters will be plotted. Similar to the headmaps the density
  # plots will be added to a responsive gallery.
  # If no cluster is selected all density lines are shown.
  
  # create all density plots
  observe({
    lapply(1:length(attributes(SOMtrained()$data)$dimnames[[2]]), function(i){
      output[[paste("densityPlot", i, sep="") ]] <- renderPlot({
        df <- cbind(dataInput(),node = SOMtrained()$unit.classif,cluster = SOMclustered()[SOMtrained()$unit.classif])
        
        colorPalette <- pretty_palette
        # show only density lines for "selected" clusters
        if(length(Clusters[["selected"]]) > 0){
          df <- df[df$cluster %in% Clusters[["selected"]], ]
          colorPalette <- pretty_palette[sort(Clusters[["selected"]])]
        }
        
        ggplot(df, aes_string(attributes(SOMtrained()$data)$dimnames[[2]][i], colour = "as.factor(cluster)")) +
          geom_density() +
          theme(legend.position = "bottom") +
          scale_colour_manual(name = "clusters", values = colorPalette)
        
      }, height = 250, width = 250)
    })
  })
  
  # create taglist (= gallery) of all density plots
  output$SOMdensityGallery <- renderUI({
    numfigures <- length(attributes(SOMtrained()$data)$dimnames[[2]])
    plot_output_list <- lapply(1:numfigures, function(i) {
      plotname <- paste("densityPlot", i, sep="")
      plotOutput(plotname, inline = TRUE)
    })

    # Convert the list to a tagList - this is necessary for the list of items to display properly.
    do.call(tagList, plot_output_list)
  })

  
  # Download ----------------------------------------------------------------
  
  # The user will have different options to download the clustered data or
  # some of the created plots.
  
  output$downall <- downloadHandler(
    filename = function(){
      paste("SOM_",input$inFile,format(Sys.time(),"%y-%m-%d_%H:%M:%S"),".zip",sep="")
    },
    content = function(file){
      #profiled data
      write.csv(dataSet[["dfSummary"]],"profiled_data.txt", row.names = FALSE)
      #original data
      write.csv(dataInput(),"dataset.csv", row.names = FALSE)
      #training data (kohonen class)
      f3 <- SOMtrained()
      saveRDS(f3,file="model.rds")
      #clusters
      f4 <-SOMclustered() 
      saveRDS(f4,file="clusters.rds")
      
      #pictures
      jpeg(file = "SOM.jpg")
      plot(SOMtrained())
      dev.off()
      jpeg(file = "SOM_changes.jpg")
      plot(SOMtrained(), type = "changes")
      dev.off()
      jpeg(file = "SOM_quality.jpg")
      #plot(SOMtrained(), type = "quality")
      TBSOM::hexagonalPlotEval(SOMtrained(), type = "quality")
      dev.off()
      jpeg(file = "SOM_counts.jpg")
      TBSOM::hexagonalPlotEval(SOMtrained(), type = "counts")
      #plot(SOMtrained(), type = "counts")
      dev.off()
      jpeg(file = "SOM_dist_neighbours.jpg")
      #plot(SOMtrained(), type = "dist.neighbours")
      TBSOM::hexagonalPlotEval(SOMtrained(), type = "dist.neighbours")
      dev.off()
      jpeg(file = "SOM_codes.jpg")
      plot(SOMtrained(), type = "codes")
      dev.off()
      
      #clusters
      jpeg(file = "SOM_clusters.jpg")
      if(length(Clusters[["selected"]]) == 0){
        TBSOM::hexagonalPlot(SOMtrained(), values = SOMclustered(), addlegend = FALSE, segment = TRUE, colors = pretty_palette.alpha[SOMclustered()])
        add.cluster.boundaries(SOMtrained(), SOMclustered())
        dev.off()
      } else {
        highlight.color <- pretty_palette.alpha
        highlight.color[Clusters[["selected"]]] <- pretty_palette[Clusters[["selected"]]]
        TBSOM::hexagonalPlot(SOMtrained(), values = SOMclustered(), addlegend = FALSE, segment = TRUE, colors = highlight.color[SOMclustered()])
        #plot(SOMtrained(), type="mapping", bgcol = highlight.color[SOMclustered()], main = "Clusters")
        add.cluster.boundaries(SOMtrained(), SOMclustered())
        dev.off()
      }
      



      files <- c("profiled_data.txt","dataset.csv","model.rds","clusters.rds",
                 "SOM.jpg","SOM_changes.jpg","SOM_quality.jpg","SOM_counts.jpg",
                 "SOM_dist_neighbours.jpg","SOM_codes.jpg","SOM_clusters.jpg")
      
      zip(zipfile=file,files=files)
      unlink(files)
    }
  )

  output$downfig <- downloadHandler(
    filename = function(){
      paste("SOM_figures",input$inFile,format(Sys.time(),"%y-%m-%d_%H:%M:%S"),".zip",sep="")
    },
    content = function(file){
      #pictures
      jpeg(file = "SOM.jpg")
      plot(SOMtrained())
      dev.off()
      jpeg(file = "SOM_changes.jpg")
      plot(SOMtrained(), type = "changes")
      dev.off()
      jpeg(file = "SOM_quality.jpg")
      #plot(SOMtrained(), type = "quality")
      TBSOM::hexagonalPlotEval(SOMtrained(), type = "quality")
      dev.off()
      jpeg(file = "SOM_counts.jpg")
      #plot(SOMtrained(), type = "counts")
      TBSOM::hexagonalPlotEval(SOMtrained(), type = "counts")
      dev.off()
      jpeg(file = "SOM_dist_neighbours.jpg")
      #plot(SOMtrained(), type = "dist.neighbours")
      TBSOM::hexagonalPlotEval(SOMtrained(), type = "dist.neighbours")
      dev.off()
      jpeg(file = "SOM_codes.jpg")
      plot(SOMtrained(), type = "codes")
      dev.off()
      
      #clusters
      jpeg(file = "SOM_clusters.jpg")
      if(length(Clusters[["selected"]]) == 0){
        hexagonalPlot(SOMtrained(), values = SOMclustered(), addlegend = FALSE, segment = TRUE, colors = pretty_palette.alpha[SOMclustered()])
        add.cluster.boundaries(SOMtrained(), SOMclustered())
        dev.off()
      } else {
        highlight.color <- pretty_palette.alpha
        highlight.color[Clusters[["selected"]]] <- pretty_palette[Clusters[["selected"]]]
        hexagonalPlot(SOMtrained(), values = SOMclustered(), addlegend = FALSE, segment = TRUE, colors = highlight.color[SOMclustered()])
        #plot(SOMtrained(), type="mapping", bgcol = highlight.color[SOMclustered()], main = "Clusters")
        add.cluster.boundaries(SOMtrained(), SOMclustered())
        dev.off()
      }
      
      
      files <- c("SOM.jpg","SOM_changes.jpg","SOM_quality.jpg","SOM_counts.jpg",
                 "SOM_dist_neighbours.jpg","SOM_codes.jpg","SOM_clusters.jpg")
      
      zip(zipfile=file,files=files)
      unlink(files)
    }
  )
  
  output$downdata <- downloadHandler(
    filename = function(){
      paste("SOM_data",input$inFile,format(Sys.time(),"%y-%m-%d_%H:%M:%S"),".zip",sep="")
    },
    content = function(file){
      #profiled data
      write.csv(dataSet[["dfSummary"]],"profiled_data.txt", row.names = FALSE)
      #original data
      write.csv(dataInput(),"dataset.csv", row.names = FALSE)
      #training data (kohonen class)
      f3 <- SOMtrained()
      saveRDS(f3,file="model.rds")
      #clusters
      f4 <-SOMclustered() 
      saveRDS(f4,file="clusters.rds")

      files <- c("profiled_data.txt","dataset.csv","model.rds","clusters.rds")
      
      zip(zipfile=file,files=files)
      unlink(files)
    }
  )
  
})




