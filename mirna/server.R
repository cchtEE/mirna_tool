#
# This is the server logic of a Shiny web application. You can run the
# application by clicking "Run App" above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(R.utils)
library(RCurl)
#library(limma)
#library(edgeR)
library(DT)
#library(dplyr)

#library(multiMiR)
#library(miRBaseConverter)
library(ggplot2)
library(ggrepel)
library(graphics)
library(tidyverse)
library(highcharter)
library(clusterProfiler)
#library(purrr)

library(shinyBS)
library(shinyjs)
library(shinydashboard)

library(survival)
library(survminer)
library(RColorBrewer)
library(igraph)
library(visNetwork)
library(hpar)

#############################################
############# survival plot function ###############
surv_plot <- function(mat1, mat2, ID, sep = "median"){
  colnames(mat1) <- substr(colnames(mat1),1,12)
  cc <- intersect(colnames(mat1), mat2$bcr_patient_barcode)
  clinic_sub <- subset(mat2, mat2$bcr_patient_barcode %in% cc)
  clinic_unique <- clinic_sub[!duplicated(clinic_sub$bcr_patient_barcode),]
  order_stage <- match(colnames(mat1), clinic_unique$bcr_patient_barcode)
  clinic_unique_order <- clinic_unique[order_stage,]
  id_ <- ID
  # get the expression values for the selected mirna and transpose the expression matrix
  c_mat <- t(mat1)
  ## add expression values to clinical data
  clinic_unique_order$ID_exp = c_mat[, id_]
  
  # find the median value of the gene and print it
  #thresh = summary(clinic_unique_order$ID_exp)
  #
  thresh <- as.numeric(summary(clinic_unique_order$ID_exp)[3])

  clinic_unique_order$ID_regulation = ifelse(clinic_unique_order$ID_exp >= thresh, "UP", "DOWN")
  
  clinic_unique_order$deceased = clinic_unique_order$vital_status == "Dead"
  clinic_unique_order$overall_survival = ifelse(clinic_unique_order$deceased,
                                                clinic_unique_order$days_to_death,
                                                clinic_unique_order$days_to_last_followup)
  
  ##fit the survival model
  fit = survfit(Surv(overall_survival, deceased) ~ ID_regulation, data=clinic_unique_order)
  # we can extract the survival p-value and print it
  #pval = surv_pvalue(fit, data=clinic_unique_order)$pval
  
  # and finally, we produce a Kaplan-Meier plot
  k <- ggsurvplot(fit, data=clinic_unique_order, pval=T, risk.table=T, title=paste(id_), surv.median.line = "hv", 
                  # Change legends: title & labels
                  legend.title = ID,
                  legend.labs = c("UP", "Down"), xlab = "Time (Days)")
  return(k)
}


##function to return pvalue < 0.05
cox_km_surv_func <- function(file){
  file <- file
  file <- file[file$pValue < 0.05,]
  return(file)
}


##function to plot GO plots
GO_func <- function(GO_file){
  GO_file <- dotplot(GO_file, split="ONTOLOGY", showCategory = 10,
                     title = "Gene Ontology", font.size = 15, orderBy = "x")+ facet_grid(ONTOLOGY~., scale="free")
  return(GO_file)
}

## #function to plot KEGG plots
kegg_plot_func <-  function(kegg_file){
  kegg_file <- dotplot(kegg_file, showCategory = 20, font.size = 15, orderBy = "x")
  return(kegg_file)
}


# This function will create the buttons for the datatable, they will be unique
## for mirna
shinyInput <- function(FUN, len, id, ...) {inputs <- character(len)
for (i in seq_len(len)) {
  inputs[i] <- as.character(FUN(paste0(id, i), ...))}
inputs
}


## hyperlink to The Human protein Atlas page
createLink <- function(val, val1) {
  sprintf("<a href='https://www.proteinatlas.org/%s/pathology/%s' target='_blank' class='btn btn-primary'>Info</a>",val, val1)
  #https://www.proteinatlas.org/ENSG00000000938-FGR/pathology/cervical+cancer
}

##function to get deg from file
deg_func <- function(file){
  file <- file[abs(file$logFC) > 2 & file$FDR < 0.05,]
  colnames(file)[1] <- "Genes"
  return(file)
}


### volcanoplot function for miRNA
vol_func <- function(DE_data, p_val, logfc){
  x <- DE_data
  x$sig <- ifelse(x$FDR <= p_val & abs(x$logFC) >= logfc, "DEmiRNA", "ns")
  hc <- highchart() %>%
    hc_add_series(x, "scatter", hcaes(logFC, -log10(FDR), group = sig, value = miRNA),
                  color = c("#FF0000", "#00FFFF"), 
                  enableMouseTracking = c(TRUE, TRUE),
                  showInLegend = TRUE, marker = list(radius = 4)) %>%
    hc_tooltip(pointFormat = "{point.value}",  headerFormat = "") %>%
    hc_xAxis(title = list(text = "Log fold change"), gridLineWidth = 1,
             tickLength = 0, startOnTick = "true", endOnTick = "true", min = -6, max = 6) %>%
    hc_yAxis(title = list(text = "-Log10(adj.p-value)")) %>%
    hc_chart(zoomType = "xy", width=700) #%>%
    #hc_exporting(enabled = TRUE, filename = "volcano")
  hc
  return(hc)
}

### volcanoplot function for mRNA and lncRNAs
vol_func_G <- function(DE_data, p_val, logfc){
  x <- DE_data
  x$sig <- ifelse(x$FDR <= p_val & abs(x$logFC) >= logfc, "DEGs", "ns")
  hc <- highchart() %>%
    hc_add_series(x, "scatter", hcaes(logFC, -log10(FDR), group = sig, value = Ensembl_ID),
                  color = c("#FF0000", "#00FFFF"), 
                  enableMouseTracking = c(TRUE, TRUE),
                  showInLegend = TRUE, marker = list(radius = 4)) %>%
    hc_tooltip(pointFormat = "{point.value}",  headerFormat = "") %>%
    hc_xAxis(title = list(text = "Log fold change"), gridLineWidth = 1,
             tickLength = 0, startOnTick = "true", endOnTick = "true", min = -6, max = 6) %>%
    hc_yAxis(title = list(text = "-Log10(adj.p-value)")) %>%
    hc_chart(zoomType = "xy", width=500) 
  hc
  return(hc)
}
    

## function to rename colnames of targte file and remove repating entries

target_func <- function(targetdata){
  targetdata <- unique(targetdata)
  targetdata <- targetdata[,c(1,2,3,4,6,7,8,9)]
  colnames(targetdata) <- c("Database", "Mature miRNA Acc.", "Mature miRNA ID", "Target Symbol", "Target Ensembl", "Experiment", "Support Type", "Pubmed ID")
  return(targetdata)
}

##change colnames of miRNA-lnc interactions predicted data
targte_int_func <- function(pred_int){
  pred_int <- pred_int[,-c(5,8)]
  colnames(pred_int) <- c("Database", "Mature miRNA Acc.", "Mature miRNA ID", "Target Symbol", "Target Ensembl", "Prediction Score")
  return(pred_int)
  }


#############################################################################################################
##############################################################################################################
# Define server logic required to perform the DEmiRNA analysis

server <- function(input, output, session) {
  
#################### Home Page ####
  output$image <- renderImage({
    list(src = "www/workflow_app.png",
         alt = "Workflow Overview"
    )
  }, deleteFile = FALSE)
  #################
  
  output$keepAlive3 <- renderText({
    req(input$count3)
    paste("", input$count3)
  })
  
  output$keepAlive4 <- renderText({
    req(input$count4)
    paste("", input$count4)
  })
  
  output$keepAlive5 <- renderText({
    req(input$count5)
    paste("", input$count5)
  })
  
  output$keepAlive6 <- renderText({
    req(input$count6)
    paste("", input$count6)
  })
  
  output$keepAlive7 <- renderText({
    req(input$count7)
    paste("", input$count7)
  })
  
  
  #### DEmiRNA analysis for Normal vs satge I #############
  #read counts matrix
  filedata1 <- reactive({		
    raw_counts <- load(input$cancerN)
    if(raw_counts == "comb_tcga"){
      raw_counts <- comb_tcga$normdata
    }
    }) 
  
  #read phenodata
  filedata2 <- reactive({		
    pheno_data <- load(input$cancerN)
    if(pheno_data == "comb_tcga"){
      pheno_data <- comb_tcga$pheno
      return(pheno_data)
    }
    }) 
  
  
  topmir <- reactive({		
    top_mir <- load(input$cancerN)
    if(top_mir == "comb_tcga"){
      top_mir <- comb_tcga$topdeg
    }
  })

  
  de_mirna <- reactive({
    withProgress(message = "Calculating DEmiRNAs, please wait...", value = NULL, {
    de = topmir()
    de = datatable(de, rownames = FALSE)
    })
  }) %>%
    bindCache(input$cancerN, topmir())
  
  ## add action button to table
  output$DEmiRNA <- renderDataTable(de_mirna())
    
  ## boxplot or violine plot of miRNA expression
  #getting miRNA from the de_mirna
  mir_id <- reactive({
    ids = filedata1()
    ids = rownames(ids)
    ids = as.character(ids)
  })

  
  observeEvent(input$tabs,{
    ids = mir_id()
    if (is.null(ids))
      return(NULL)
    ids = as.character(ids)
    updateSelectizeInput(session, "mirnaN",selected=ids[1], choices = ids, server=TRUE)
  })
  
  ##violine plot ###
  vio_plot <- reactive({
    mir_mat <- filedata1()
    #mir_mat <- mir_mat$voomObj$E
    #extract the miRNA expression from the mir_mat
    miID_exp <- mir_mat[input$mirnaN,]
    
    ##remove the mir_mat data now
    rm(mir_mat)
    
    ##add this expression of mirna to phenodata file
    #get phenodata file
    p_data = filedata2()
    p_data = cbind(p_data, "miRNA" = miID_exp)
    p_data = p_data
    }) %>%
    bindCache(filedata1(), filedata2(), input$mirnaN)
  
  output$vioPlotN <- renderPlot({
    if (input$plotsN == "BoxPlot") {
      v = vio_plot()
      v = data.frame(v)
      v$group = as.factor(v$group)
      ggplot(v, aes(group, miRNA, fill=group)) + geom_violin(width=1.0) + 
        labs(x="Sample Type", y = input$mirnaN) +
        geom_boxplot(width=0.1, fill="white") +
        guides(fill = guide_legend(title = "Type"))
    }else{return("null")}
  }) 
  
  output$volcanoPlotN <- renderHighchart({
    if(input$plotsN == "VolcanoPlot") {
      vol_func(topmir(), p_val = 0.05, logfc = 0.56)
    }else{return("null")}
    }) 
  
  #############################################################################################################
  ########################  for DEmiRNA analysis between all stages   ##########################################
  
  s_data1 <- reactive({		
    raw_counts <- load(input$cancer)
    if(raw_counts == "comb_tcga"){
      raw_counts <- comb_tcga$normdata
    }
  })
  
  #read phenodata
  s_data2 <- reactive({		
    pheno_data <- load(input$cancer)
    if(pheno_data == "comb_tcga"){
      pheno_data <- comb_tcga$pheno
      return(pheno_data)
    }
  })
  
  # Load the "clinical" object from the Rdata file defined in Ui.R  
  s_data3 <- reactive({	
    clinical_data <- load(input$cancer)
    if(clinical_data == "comb_tcga"){
      clinical_data <- comb_tcga$clinical 
      }
    })
  
  s_topmir <- reactive({		
    top_mir <- load(input$cancer)
    if(top_mir == "comb_tcga"){
      top_mir <- comb_tcga$topdeg
    }
  })
  
  
  ############## reset action button ###############
  #v <- reactiveValues(doPlot = FALSE)
  
  s_de_mirna <- reactive({
    #if(input$tabs == 1){}
      withProgress(message = "Calculating DEmiRNAs, please wait...", value = NULL, {
        de = s_topmir()
        de = as.data.frame(cbind(KMPlot = shinyInput(actionButton, nrow(de),"button_", label = "View", onclick = "Shiny.onInputChange(\"select_button\",  this.id)" ),de))
      })
  })
  

  ## add action button to table
  output$DEmiRNA_stage <- renderDataTable(s_de_mirna(),selection = "single",options = list(searching = FALSE,pageLength = 10),
                                          server = FALSE, escape = FALSE,rownames= FALSE)


  #### KM-Plot : popup plot #################
  
  # Here I created a reactive to save which row was clicked which can be stored for further analysis
  SelectedRow <- eventReactive(input$select_button,{
    aa <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    mirna_name <- s_de_mirna()[aa,2]
    })
 
  
  # This is needed so that the button is clicked once for modal to show, a bug reported here
  # https://github.com/ebailey78/shinyBS/issues/57
  observeEvent(input$select_button, {
    toggleModal(session, "modalmirna", "open")
  })
  
  
  output$popup <- renderUI({
    print(input$select_button)
    bsModal("modalmirna", "Kaplan-Meier Plot", trigger = "select_button", size = "large",
            column(12,renderPlot(surv_plot(s_data1(), s_data3(), ID = SelectedRow()))))
  })
  
  
  ##################################################################################################################################
  
  ## boxplot or violine plot of miRNA expression
  #getting miRNA from the de_mirna
  s_mir_id <- reactive({
    ids = s_data1()
    ids = rownames(ids)
    ids = as.character(ids)
  })
  
  
  observeEvent(input$tabs,{
    ids = s_mir_id()
    if (is.null(ids))
      return(NULL)
    ids = as.character(ids)
    updateSelectizeInput(session, "mirna",selected=ids[1], choices = ids, server=TRUE)
  })
  
  ##violine plot ###
  s_vio_plot <- reactive({
    mir_mat <- s_data1()
    #extract the miRNA expression from the mir_mat
    miID_exp <- mir_mat[input$mirna,]
    
    ##remove the mir_mat data now
    rm(mir_mat)
    
    ##add this expression of mirna to phenodata file
    #get phenodata file
    p_data = s_data2()
    p_data = cbind(p_data, "miRNA" = miID_exp)
    p_data = p_data
  })%>%
    bindCache(s_data1(), input$mirna, s_data2())
  
  output$vioPlot <- renderPlot({
    if (input$plots == "BoxPlot") {
      v = s_vio_plot()
      v = data.frame(v)
      v$group = as.factor(v$group)
      ggplot(v, aes(group, miRNA, fill=group)) + geom_violin(width=1.0) + 
        labs(x="Sample Type", y = input$mirna) +
        geom_boxplot(width=0.1, fill="white") +
        guides(fill = guide_legend(title = "Type"))
    }else{return("null")}
  })
  
  output$volcanoPlot <- renderHighchart({
    if(input$plots == "VolcanoPlot") {
      vol_func(s_topmir(), p_val = 0.05, logfc = 0.56)
    }else{return("null")}
  })
  
  
  
  ####################################################################
  
  #############################################################################
  ############################### miRNA survival analysis ########################
  
  #read counts matrix
  surv1 <- reactive({		
    cox_data <- load(input$cancer3)
    if(cox_data == "surv_coxph"){
      cox_data <- surv_coxph
      cox_data <- cox_km_surv_func(cox_data)
    }
  })
  
  surv_analysis_cox <-  reactive({
    if(input$surv == "coxph"){
      model <- surv1()
      model <- cbind("miRNA" = rownames(model), model)
      model
    }
  })%>%bindCache(input$surv, surv1())
  
  ## add action button to table
  output$survival_cox <- renderDataTable({
    datatable(surv_analysis_cox(),
              options = list(
                "pageLength" = 25), rownames = FALSE)
  })
  
  
  surv2 <- reactive({		
    km_data <- load(input$cancer_median)
    if(km_data == "surv_km_log"){
      km_data <- surv_km_log
      km_data <- cox_km_surv_func(km_data)
    }
  })
  

  ####################################################
  
  # Load the "clinical" object from the Rdata file defined in Ui.R  
  surv3 <- reactive({		
    km_data <- load(input$cancer_mean)
    if(km_data == "surv_km_log"){
      km_data <- surv_km_log
      km_data <- cox_km_surv_func(km_data)
    }
  })
  
  surv4 <- reactive({		
    km_data <- load(input$cancer_1stQu)
    if(km_data == "surv_km_log"){
      km_data <- surv_km_log
      km_data <- cox_km_surv_func(km_data)
    }
  })
  
  surv5 <- reactive({		
    km_data <- load(input$cancer_3rdQu)
    if(km_data == "surv_km_log"){
      km_data <- surv_km_log
      km_data <- cox_km_surv_func(km_data)
    }
  })
  
 
  surv_analysis_KM <-  reactive({
    if(input$surv == "km"){
      if(input$sep == "median"){
        model <- surv2()
        model <- cbind("miRNA" = rownames(model), model)
      } else if(input$sep == "mean"){
        model <- surv3()
        model <- cbind("miRNA" = rownames(model), model)
      }else if(input$sep == "1stQu"){
        model <- surv4()
        model <- cbind("miRNA" = rownames(model), model)
      }else if(input$sep == "3rdQu"){
        model <- surv5()
        model <- cbind("miRNA" = rownames(model), model)
      }
    }
  })%>%
    bindCache(input$surv, input$sep, surv2(), surv3(), surv4(), surv5())
  
  output$survival_KM <- renderDataTable({
    datatable(surv_analysis_KM(),
              options = list(
                "pageLength" = 25), rownames = FALSE)
  })
  

  
  #######################################################
  
  
  ####### miRNA target ###############
  ####################################
  #read target files and miRNA-mRNA intercation files
  filedata4 <- reactive({		
    target_file <- load(input$cancer1)
    if(target_file == "mirna_targets"){
      target_file <- mirna_targets
      target_file <- target_func(target_file)
    }
  })

  select_db1 <- reactive({
    db <- filedata4()
    db <- db[db$Database == "mirtarbase",]
    })%>%
    bindCache(input$cancer1, filedata4())
  
  select_db2 <- reactive({
    db <- filedata4()
    db <- db[db$Database == "mirecords",]
    })%>%
    bindCache(input$cancer1, filedata4())
    
  
  select_db3 <- reactive({
    db <- filedata4()
    db <- db[db$Database == "tarbase",]
    })%>%
    bindCache(input$cancer1, filedata4())
    

  output$miRTarget <- renderDataTable({
    withProgress(message = "Reteriving validated miRNA targets..", value = NULL, {
      if(input$db == "mirtarbase"){
        t_mirna <- select_db1()
        t_mirna <- datatable(t_mirna, rownames = FALSE)
      }else if(input$db == "mirecords"){
        t_mirna <- select_db2()
        t_mirna <- datatable(t_mirna, rownames = FALSE)
      }else if(input$db == "tarbase"){
        t_mirna <- select_db3()
        t_mirna <- datatable(t_mirna, rownames = FALSE)
      }
      })
    })
  
  
  output$hpa <- renderDataTable({
    t_mirna <- filedata4()
    t_mirna <- t_mirna[[5]]
    
    example <- getHpa(id = t_mirna, hpadata = "hpaCancer")
    
    
    #example$link <- createLink(example$Gene)
    if(input$cancertype == "breast cancer"){
      example$link <- createLink(example$Gene, "breast cancer")
      example <- subset(example, example$Cancer == "breast cancer")
      rownames(example) <- 1:nrow(example)
      example
      #example$link <- createLink(example$Gene)
    }else if(input$cancertype == "endometrial cancer"){
      example$link <- createLink(example$Gene, "endometrial cancer")
      example <- subset(example, example$Cancer == "endometrial cancer")
      rownames(example) <- 1:nrow(example)
      example
      #example$link <- createLink(example$Gene)
    }else if(input$cancertype == "ovarian cancer"){
      example$link <- createLink(example$Gene, "ovarian cancer")
      example <- subset(example, example$Cancer == "ovarian cancer")
      rownames(example) <- 1:nrow(example)
      example
      #example$link <- createLink(example$Gene)
    }else{
      example$link <- createLink(example$Gene, "cervical cancer")
      example <- subset(example, example$Cancer == "cervical cancer")
      rownames(example) <- 1:nrow(example)
      example
    }
    
  }, escape = FALSE)
  
  
  ### mRNA targets
  
  filedata6 <- reactive({
    file <- load(input$mrna_mat1)
    if(file == "mi_mrna_int"){
      file <- mi_mrna_int
      if(nrow(file) == 0){
        file
      }else if(nrow(file)!= 0){
        file <- target_func(file)
      }
    }
  })%>%
    bindCache(input$mrna_mat1)
  
  
  output$ui_mrna_val <- renderUI({
    withProgress(message = "Extracting miRNA-mRNA interactions..", value = NULL, {
      if(nrow(filedata6()) == 0)
        return("Not found miRNA-mRNA validated ineractions")
      dataTableOutput("miR_mRNA")
    })
  })
  
  output$miR_mRNA <- renderDataTable({
      miR_val <- filedata6()
      miR_val <- datatable(miR_val, rownames = FALSE)
  }) 
  
  ### lncRNA targets
  
  filedata7 <- reactive({
    file <- load(input$val_int)
    if(file == "mi_lnc_int_val"){
      file <- mi_lnc_int_val
      if(nrow(file) == 0){
        file
      }else if(nrow(file)!= 0){
        file <- target_func(file)
      }
      }
  })%>%bindCache(input$val_int)
  
  output$ui_val <- renderUI({
    withProgress(message = "Extracting miRNA-lncRNA interactions..", value = NULL, {
      if(nrow(filedata7()) == 0)
        return("Not found miRNA-lncRNA validated ineractions")
      dataTableOutput("miR_lnc_val")
    })
  }) 
  
  output$miR_lnc_val <- renderDataTable({
      miR_val <- filedata7()
      miR_val <- datatable(miR_val, rownames = FALSE)
     }) 
  
  
  filedata8 <- reactive({
    file <- load(input$pred_int)
    if(file == "mi_lnc_int_pred"){
      file <- mi_lnc_int_pred
      if(nrow(file) == 0){
        file
      }else if(nrow(file)!= 0){
        file <- targte_int_func(file)
      }
    }
  })%>%
    bindCache(input$pred_int)
  
  
  output$ui_pred <- renderUI({
    withProgress(message = "Searching miRNA-lncRNA interactions..", value = NULL, {
      if(nrow(filedata8()) == 0)
        return("Not found miRNA-lncRNA predicted ineractions")
      dataTableOutput("miR_lnc_pred")
    })
  }) 
  
  output$miR_lnc_pred <- renderDataTable({
    miR_val <- filedata8()
    miR_val <- datatable(miR_val, rownames = FALSE)
  }) 
  
  ### GO analysis
  load_plot <- reactive({
    plot <- load(input$go_file)
    if(plot == "GO"){
      plot <- GO
      plot <- GO_func(plot)
    }
  })%>%
    bindCache(input$go_file)
  

  output$GO <- renderPlot({
    p <- load_plot()
    p
    })
 
  ## KEGG analysis
  kegg_plot <- reactive({
    plot <- load(input$kegg_file)
    if(plot == "kegg"){
      plot <- kegg
      plot <- kegg_plot_func(plot)
    }
  })%>%bindCache(input$kegg_file)
  
  
  
  output$KEGG <- renderPlot({
    p <- kegg_plot()
    p
  })
  
  ## miRNA-mRNA correlation analaysis and network creation
  
  ##read the files again for network generation
  #read counts matrix
  file1 <- reactive({
    cor_net <- load(input$cancer2)
    if(cor_net == "cor_deg"){
      cor_net <- cor_deg$cor_mim
      return(cor_net)
    }
  })
  
  
  
  ## load the gene symbol and biotype file 
  gene_sym <- reactive({
    file_sym <- load("Data/GeneSymbol.rda") 
    if(file_sym == "map_sym"){
      file_sym <- map_sym
    }
    
  })
  
  ##
  NetM <- reactive({
    withProgress(message = "Generating Network, please wait...", value = NULL, {
      
      cor_data <- file1()
      ## keep the high correlated connection 
      cor_data[abs(cor_data) < input$cutoff] <- 0
      
      #create netwrok object
      net_mim <- graph_from_incidence_matrix(cor_data, weighted = "correlation")
      rm(cor_data)
      
      ## map the biotype for each gene, miRNA
      map_sym <- gene_sym()
      #map_sym$gene_biotype[match(V(net_mim)$name, map_sym$ensembl_gene_id)]
      V(net_mim)$biotype <- map_sym$gene_biotype[match(V(net_mim)$name, map_sym$ensembl_gene_id)]
      V(net_mim)$biotype[is.na(V(net_mim)$biotype)] <- "miRNA"
      
      rm(map_sym)
  
      
      ## delete the nodes with zeor degree
      Isolated = which(degree(net_mim)==0)
      net_mim = delete.vertices(net_mim, Isolated)
      })
    }) %>%
    bindCache(file1(), input$cutoff, gene_sym())
  
  
  
  output$visnetwork <- renderVisNetwork({
    # Plot igraph object with visNetwork. 
    net_mim <- NetM()
    
    ##assortativity of network
    #assort_corr <- assortativity(net_mim, V(net_mim), directed=F)
    
    fc <- fastgreedy.community(net_mim)
    V(net_mim)$community <- fc$membership
    
    #stop(safeError("Select smaller cut-off value to show the network"))
    #remove fc
    rm(fc)
    
    nodes <- data.frame(id = V(net_mim)$name, title = V(net_mim)$name, group = V(net_mim)$community, 
                        biotype = V(net_mim)$biotype)
    
    ## add shape of the nodes
    nodes <- cbind(shape = ifelse(grepl("ENSG", nodes$title, ignore.case = T), "circle", "square"), nodes)
    nodes$shape[nodes$biotype == "lncRNA"] <- "triangle"
    
    nodes <- nodes[order(nodes$id, decreasing = F),]
    
    nodes <- unique(nodes)
    edges <- get.data.frame(net_mim, what="edges")[1:3]
    
    #stop(safeError("Select smaller correlation cut-off value."))
    
    visNetwork(nodes, edges) %>% visIgraphLayout(layout = "layout_with_fr") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      #allow for long click to select additional nodes
      visInteraction(multiselect = TRUE) %>%
      visIgraphLayout() %>% 
      #Use visEvents to turn set input$current_node_selection to list of selected nodes
      visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_selection', nodes.nodes);
                ;}") 
    })
  
  
  output$assort <- renderText({
    net_mim <- NetM()
    ##assortativity of network
    assort_corr <- assortativity(net_mim, V(net_mim), directed=F)
    
    })
  
  output$edge_tbl <- renderDT({
    net_mim <- NetM()
    #save(net_mim, file = "network_test.rda")
    
    fc <- fastgreedy.community(net_mim)
    V(net_mim)$community <- fc$membership
    
    ##remove fc
    rm(fc)
    
    nodes <- data.frame(id = V(net_mim)$name, title = V(net_mim)$name, group = V(net_mim)$community, 
                        biotype = V(net_mim)$biotype)
    
    nodes <- cbind(shape = ifelse(grepl("ENSG", nodes$title, ignore.case = T), "circle", "square"), nodes)
    nodes$shape[nodes$biotype == "lncRNA"] <- "triangle"
    
    nodes <- nodes[order(nodes$id, decreasing = F),]
    edges <- get.data.frame(net_mim, what="edges")[1:3]
    edges %>%
      filter((to %in% input$current_node_selection)|(from %in% input$current_node_selection))
    })
  

## save list of gene ids to submit to stringDB or metascape
  
  ## get genelist from the network
  geneList <- reactive({
    net_mim <- NetM()
    #igraph::V(net_mim)$biotype[is.na(igraph::V(net_mim)$biotype)] <- "miRNA"
    genelist <- V(net_mim)$name[V(net_mim)$biotype != "miRNA"]
    
  })
  
  
  output$download_button <- downloadHandler(
    filename = function(){
      paste("genelist-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      genelist <- geneList()
      #writeLines(paste(genelist), file)
      write.table(paste(genelist), file, col.names = FALSE, 
                  row.names = FALSE, quote = FALSE)
    }
  )
  
  ##########################################################################################
  ###########################################################################################
  ####################### mRNA analysis ######################################################
  ###########################################################################################
  
  
  #### DEG analysis for Normal vs satge I #############
  #read counts matrix
  filedata1_mrna <- reactive({		
    counts <- load(input$cancerN_mrna)
    if(counts == "pro_coding"){
      counts <- pro_coding$norm
    }
  }) 
  
  #read phenodata
  filedata2_mrna <- reactive({		
    pheno_data <- load(input$cancerN_mrna)
    if(pheno_data == "pro_coding"){
      pheno_data <- pro_coding$pheno
    }
  }) 
  
  
  topdeg <- reactive({		
    top <- load(input$cancerN_mrna)
    if(top == "pro_coding"){
      top <- pro_coding$deg
    }
  })
  
  
  de_mrna <- reactive({
    withProgress(message = "Calculating DEGs, please wait...", value = NULL, {
      de = topdeg()
      de <- deg_func(de)
      de = datatable(de, rownames = FALSE)
    })
  }) %>%
    bindCache(input$cancerN_mrna, topdeg())
  
  ## add action button to table
  output$DEG <- renderDataTable(de_mrna())
  
  ## boxplot or violine plot of miRNA expression
  #getting miRNA from the de_mirna
  mrna_id <- reactive({
    ids = filedata1_mrna()
    ids = rownames(ids)
    ids = as.character(ids)
  })
  
  
  observeEvent(input$tabs_mrna,{
    ids = mrna_id()
    if (is.null(ids))
      return(NULL)
    ids = as.character(ids)
    updateSelectizeInput(session, "mrnaN",selected=ids[1], choices = ids, server=TRUE)
  })
  
  ##violine plot ###
  vio_plot_mrna <- reactive({
    m_mat <- filedata1_mrna()
    #mir_mat <- mir_mat$voomObj$E
    #extract the miRNA expression from the mir_mat
    mID_exp <- m_mat[input$mrnaN,]
    
    ##remove the mir_mat data now
    rm(m_mat)
    
    ##add this expression of mirna to phenodata file
    #get phenodata file
    p_data = filedata2_mrna()
    p_data = cbind(p_data, "mRNA" = mID_exp)
    p_data = p_data
  }) %>%
    bindCache(filedata1_mrna(), filedata2_mrna(), input$mrnaN)
  
  output$vioPlotN_mrna <- renderPlot({
    if (input$plotsN_mrna == "BoxPlot") {
      v = vio_plot_mrna()
      v = data.frame(v)
      v$group = as.factor(v$group)
      ggplot(v, aes(group, mRNA, fill=group)) + geom_violin(width=1.0) + 
        labs(x="Sample Type", y = input$mrnaN) +
        geom_boxplot(width=0.1, fill="white") +
        guides(fill = guide_legend(title = "Type"))
    }else{return("null")}
  }) 
  
 
  
  output$volcanoPlotN_mrna <- renderHighchart({
    if(input$plotsN_mrna == "VolcanoPlot") {
      vol_func_G(topdeg(), p_val = 0.05, logfc = 2)
      }else{return("null")}
  })%>%bindCache(topdeg())
  
  
  #############################################################################################################
  ########################  for DEG analysis between all stages   ##########################################
  
  s_data1_mrna <- reactive({		
    counts <- load(input$cancer_mrna)
    if(counts == "pro_coding"){
      counts <- pro_coding$norm
    }
  })
  
  #read phenodata
  s_data2_mrna <- reactive({		
    pheno_data <- load(input$cancer_mrna)
    if(pheno_data == "pro_coding"){
      pheno_data <- pro_coding$pheno
    }
  })
  
  # Load the "clinical" object from the Rdata file defined in Ui.R  
  s_data3_mrna <- reactive({	
    clinical_data <- load(input$cancer_mrna)
    if(clinical_data == "pro_coding"){
      clinical_data <- pro_coding$clinical 
    }
  })
  
  s_topdeg <- reactive({		
    top_deg <- load(input$cancer_mrna)
    if(top_deg == "pro_coding"){
      top_deg <- pro_coding$deg
    }
  })
  
  
  s_de_mrna <- reactive({
    withProgress(message = "Calculating DEGs, please wait...", value = NULL, {
      de = s_topdeg()
      de <- deg_func(de)
      de = as.data.frame(cbind(KMPlot = shinyInput(actionButton, nrow(de),"button_", label = "View", onclick = "Shiny.onInputChange(\"select_button2\",  this.id)" ),de))
    })
  })
  
  
  ## add action button to table
  output$DEG_stage <- renderDataTable(s_de_mrna(),selection = "single",options = list(searching = FALSE,pageLength = 10),
                                      server = FALSE, escape = FALSE,rownames= FALSE) 
  
  #### KM-Plot : popup plot #################
  
  # Here I created a reactive to save which row was clicked which can be stored for further analysis
  SelectedRow2 <- eventReactive(input$select_button2,{
    aa <- as.numeric(strsplit(input$select_button2, "_")[[1]][2])
    mrna_name <- s_de_mrna()[aa,2]
  })
  
  
  # This is needed so that the button is clicked once for modal to show, a bug reported here
  # https://github.com/ebailey78/shinyBS/issues/57
  observeEvent(input$select_button2, {
    toggleModal(session, "modalmrna", "open")
  })
  
  
  output$popup_mrna <- renderUI({
    print(input$select_button2)
    bsModal("modalmrna", "Kaplan-Meier Plot", trigger = "select_button2", size = "large",
            column(12,renderPlot(surv_plot(s_data1_mrna(), s_data3_mrna(), ID = SelectedRow2()))))
  })
  
  
  
  
  ##################################################################################################################################
  
  ## boxplot or violine plot of miRNA expression
  #getting miRNA from the de_mirna
  s_m_id <- reactive({
    ids = s_data1_mrna()
    ids = rownames(ids)
    ids = as.character(ids)
  })
  
  
  observeEvent(input$tabs_mrna,{
    ids = s_m_id()
    if (is.null(ids))
      return(NULL)
    ids = as.character(ids)
    updateSelectizeInput(session, "mrna",selected=ids[1], choices = ids, server=TRUE)
  })
  
  ##violine plot ###
  s_vio_plot_mrna <- reactive({
    m_mat <- s_data1_mrna()
    #extract the miRNA expression from the mir_mat
    mID_exp <- m_mat[input$mrna,]
    
    ##remove the m_mat data now
    rm(m_mat)
    
    ##add this expression of mirna to phenodata file
    #get phenodata file
    p_data = s_data2_mrna()
    p_data = cbind(p_data, "mRNA" = mID_exp)
    p_data = p_data
  })%>%
    bindCache(s_data1_mrna(), input$mrna, s_data2_mrna())
  
  output$vioPlot_mrna <- renderPlot({
    if (input$plots_mrna == "BoxPlot") {
      v = s_vio_plot_mrna()
      v = data.frame(v)
      v$group = as.factor(v$group)
      ggplot(v, aes(group, mRNA, fill=group)) + geom_violin(width=1.0) + 
        labs(x="Sample Type", y = input$mrna) +
        geom_boxplot(width=0.1, fill="white") +
        guides(fill = guide_legend(title = "Type"))
    }else{return("null")}
  })
  
  
  output$volcanoPlot_mrna <- renderHighchart({
    if(input$plots_mrna == "VolcanoPlot"){
      vol_func_G(s_topdeg(), p_val = 0.05, logfc = 2)
      }else{retrun("null")}
  })%>%
    bindCache(s_topdeg())
  
  ####################################################################
  
  #############################################################################
  ############################### mRNA survival analysis ########################
  
  #read counts matrix
  surv1_mrna <- reactive({		
    cox_data <- load(input$cancer3_mrna)
    if(cox_data == "surv_coxph_mrna"){
      cox_data <- surv_coxph_mrna
      cox_data <- cox_km_surv_func(cox_data)
    }
  })
  
  surv_analysis_cox_mrna <-  reactive({
    if(input$surv_mrna == "coxph_mrna"){
      model <- surv1_mrna()
      model <- cbind("Genes" = rownames(model), model)
      model
    }
  })%>%bindCache(input$surv_mrna, surv1_mrna())
  
  ## add action button to table
  output$survival_cox_mrna <- renderDataTable({
    datatable(surv_analysis_cox_mrna(),
              options = list(
                "pageLength" = 25), rownames = FALSE)
  })
  
  ###################################################################################
  ########################## logrank analysis #################################

  surv2_mrna <- reactive({		
    km_data <- load(input$cancer_median_mrna)
    if(km_data == "surv_kmLR_mrna"){
      km_data <- surv_kmLR_mrna
      km_data <- cox_km_surv_func(km_data)
    }
  })
  
  
  surv3_mrna <- reactive({		
    km_data <- load(input$cancer_mean_mrna)
    if(km_data == "surv_kmLR_mrna"){
      km_data <- surv_kmLR_mrna
      km_data <- cox_km_surv_func(km_data)
    }
  })
  
  surv4_mrna <- reactive({		
    km_data <- load(input$cancer_1stQu_mrna)
    if(km_data == "surv_kmLR_mrna"){
      km_data <- surv_kmLR_mrna
      km_data <- cox_km_surv_func(km_data)
    }
  })
  
  surv5_mrna <- reactive({		
    km_data <- load(input$cancer_3rdQu_mrna)
    if(km_data == "surv_kmLR_mrna"){
      km_data <- surv_kmLR_mrna
      km_data <- cox_km_surv_func(km_data)
    }
  })
  
  
  surv_analysis_KM_mrna <-  reactive({
    if(input$surv_mrna == "km_mrna"){
      if(input$sep_mrna == "median_mrna"){
        model <- surv2_mrna()
        model <- cbind("Genes" = rownames(model), model)
      } else if(input$sep_mrna == "mean_mrna"){
        model <- surv3_mrna()
        model <- cbind("Genes" = rownames(model), model)
      }else if(input$sep_mrna == "1stQu_mrna"){
        model <- surv4_mrna()
        model <- cbind("Genes" = rownames(model), model)
      }else if(input$sep_mrna == "3rdQu_mrna"){
        model <- surv5_mrna()
        model <- cbind("Genes" = rownames(model), model)
      }
    }
  })%>%
    bindCache(input$surv_mrna, input$sep_mrna, surv2_mrna(), surv3_mrna(), surv4_mrna(), surv5_mrna())
  
  output$survival_KM_mrna <- renderDataTable({
    datatable(surv_analysis_KM_mrna(),
              options = list(
                "pageLength" = 25), rownames = FALSE)
  })
  
  
  
  #################################################################################################
  #################################################################################################
  ################################ lncRNA analysis ################################################
  #################################################################################################
  
  #### DEG analysis for Normal vs satge I #############
  #read counts matrix
  filedata1_lnc <- reactive({		
    counts <- load(input$cancerN_lnc)
    if(counts == "RNA_Seq_lnc"){
      counts <- RNA_Seq_lnc$count
    }
  }) 
  
  #read phenodata
  filedata2_lnc <- reactive({		
    pheno_data <- load(input$cancerN_lnc)
    if(pheno_data == "RNA_Seq_lnc"){
      pheno_data <- RNA_Seq_lnc$pheno
    }
  }) 
  
  
  topdeg_lnc <- reactive({		
    top <- load(input$cancerN_lnc)
    if(top == "RNA_Seq_lnc"){
      top <- RNA_Seq_lnc$deg
    }
  })
  
  
  de_lnc <- reactive({
    withProgress(message = "Calculating DEGs, please wait...", value = NULL, {
      de = topdeg_lnc()
      de = deg_func(de)
      de = datatable(de, rownames = FALSE)
    })
  }) %>%
    bindCache(input$cancerN_lnc, topdeg_lnc())
  
  ## add action button to table
  output$DEG_lnc <- renderDataTable(de_lnc())
  
  ## boxplot or violine plot of miRNA expression
  #getting miRNA from the de_mirna
  lnc_id <- reactive({
    ids = filedata1_lnc()
    ids = rownames(ids)
    ids = as.character(ids)
  })
  
  
  observeEvent(input$tabs_lnc,{
    ids = lnc_id()
    if (is.null(ids))
      return(NULL)
    ids = as.character(ids)
    updateSelectizeInput(session, "lncN",selected=ids[1], choices = ids, server=TRUE)
  })
  
  ##violine plot ###
  vio_plot_lnc <- reactive({
    lnc_mat <- filedata1_lnc()
    #extract the miRNA expression from the mir_mat
    lncID_exp <- lnc_mat[input$lncN,]
    
    ##remove the mir_mat data now
    rm(lnc_mat)
    
    ##add this expression of mirna to phenodata file
    #get phenodata file
    p_data = filedata2_lnc()
    p_data = cbind(p_data, "lncRNA" = lncID_exp)
    p_data = p_data
  }) %>%
    bindCache(filedata1_lnc(), filedata2_lnc(), input$lncN)
  
  output$vioPlotN_lnc <- renderPlot({
    if (input$plotsN_lnc == "BoxPlot") {
      v = vio_plot_lnc()
      v = data.frame(v)
      v$group = as.factor(v$group)
      ggplot(v, aes(group, lncRNA, fill=group)) + geom_violin(width=1.0) + 
        labs(x="Sample Type", y = input$lncN) +
        geom_boxplot(width=0.1, fill="white") +
        guides(fill = guide_legend(title = "Type"))
    }else{return("null")}
  }) 
  
  
  ## volcano plot
  
  output$volcanoPlotN_lnc <- renderHighchart({
    if(input$plotsN_lnc == "VolcanoPlot") {
      x <- topdeg_lnc()
      x$lnc <- rownames(x)
      x$sig <- ifelse(x$PValue < 0.05 & abs(x$logFC) > 2, "DE_lncRNA", "Not Regulated")
      hc <- highchart() %>%
        hc_add_series(x, "scatter", hcaes(logFC, -log10(PValue), group = sig, value = lnc),
                      color = c("#FF0000", "#00FFFF"), 
                      enableMouseTracking = c(TRUE, TRUE),
                      showInLegend = TRUE, marker = list(radius = 4)) %>%
        hc_tooltip(pointFormat = "{point.value}",  headerFormat = "") %>%
        hc_xAxis(title = list(text = "Log fold change"), gridLineWidth = 1,
                 tickLength = 0, startOnTick = "true", endOnTick = "true", min = -6, max = 6) %>%
        hc_yAxis(title = list(text = "-Log10(p-value)")) %>%
        hc_chart(zoomType = "xy", width=700) %>%
        hc_exporting(enabled = TRUE, filename = "volcano")
      hc
    }else{return("null")}
  }) 
  
  
  
  #############################################################################################################
  ########################  for DEG analysis between all stages   ##########################################
  
  s_data1_lnc <- reactive({		
    counts <- load(input$cancer_lnc)
    if(counts == "RNA_Seq_lnc"){
      counts <- RNA_Seq_lnc$count
    }
  })
  
  #read phenodata
  s_data2_lnc <- reactive({		
    pheno_data <- load(input$cancer_lnc)
    if(pheno_data == "RNA_Seq_lnc"){
      pheno_data <- RNA_Seq_lnc$pheno
    }
  })
  
  # Load the "clinical" object from the Rdata file defined in Ui.R  
  s_data3_lnc <- reactive({	
    clinical_data <- load(input$cancer_lnc)
    if(clinical_data == "RNA_Seq_lnc"){
      clinical_data <- RNA_Seq_lnc$clinical 
    }
  })
  
  s_topdeg_lnc <- reactive({		
    top_deg <- load(input$cancer_lnc)
    if(top_deg == "RNA_Seq_lnc"){
      top_deg <- RNA_Seq_lnc$deg
    }
  })
  

  
  s_de_lnc <- reactive({
    withProgress(message = "Calculating DEGs, please wait...", value = NULL, {
     de = s_topdeg_lnc()
     de = deg_func(de)
      de = as.data.frame(cbind(KMPlot = shinyInput(actionButton, nrow(de),"button_", label = "View", onclick = "Shiny.onInputChange(\"select_button3\",  this.id)" ),de))
    })
  })
  
  
  ## add action button to table
  output$DEG_lnc_stage <- renderDataTable(s_de_lnc(),selection = "single",options = list(searching = FALSE,pageLength = 10),
                                          server = FALSE, escape = FALSE,rownames= FALSE)
  
  # Here I created a reactive to save which row was clicked which can be stored for further analysis
  SelectedRow3 <- eventReactive(input$select_button3,{
    aa <- as.numeric(strsplit(input$select_button3, "_")[[1]][2])
    lnc_name <- s_de_lnc()[aa,2]
  })
  
  
  # This is needed so that the button is clicked once for modal to show, a bug reported here
  # https://github.com/ebailey78/shinyBS/issues/57
  observeEvent(input$select_button3, {
    toggleModal(session, "modallnc", "open")
  })
  
  
  output$popup_lnc <- renderUI({
    print(input$select_button3)
    bsModal("modallnc", "Kaplan-Meier Plot", trigger = "select_button3", size = "large",
            column(12,renderPlot(surv_plot(s_data1_lnc(), s_data3_lnc(), ID = SelectedRow3()))))
  })
  
  ##################################################################################################################################
  
  ## boxplot or violine plot of miRNA expression
  #getting miRNA from the de_mirna
  s_lnc_id <- reactive({
    ids = s_data1_lnc()
    ids = rownames(ids)
    ids = as.character(ids)
  })
  
  
  observeEvent(input$tabs_lnc,{
    ids = s_lnc_id()
    if (is.null(ids))
      return(NULL)
    ids = as.character(ids)
    updateSelectizeInput(session, "lnc",selected=ids[1], choices = ids, server=TRUE)
  })
  
  ##violine plot ###
  s_vio_plot_lnc <- reactive({
    lnc_mat <- s_data1_lnc()
    #extract the miRNA expression from the mir_mat
    lncID_exp <- lnc_mat[input$lnc,]
    
    ##remove the m_mat data now
    rm(lnc_mat)
    
    ##add this expression of mirna to phenodata file
    #get phenodata file
    p_data = s_data2_lnc()
    p_data = cbind(p_data, "lncRNA" = lncID_exp)
    p_data = p_data
  })%>%
    bindCache(s_data1_lnc(), input$lnc, s_data2_lnc())
  
  output$vioPlot_lnc <- renderPlot({
    if (input$plots_lnc == "BoxPlot") {
      v = s_vio_plot_lnc()
      v = data.frame(v)
      v$group = as.factor(v$group)
      ggplot(v, aes(group, lncRNA, fill=group)) + geom_violin(width=1.0) + 
        labs(x="Sample Type", y = input$lnc) +
        geom_boxplot(width=0.1, fill="white") +
        guides(fill = guide_legend(title = "Type"))
    }else{return("null")}
  })
  
  
  output$volcanoPlot_lnc <- renderHighchart({
    if(input$plots_lnc == "VolcanoPlot") {
      x <- s_topdeg_lnc()
      x$lnc <- rownames(x)
      x$sig <- ifelse(x$PValue < 0.05 & abs(x$logFC) > 2, "DE_lncRNA", "Not Regulated")
      hc <- highchart() %>%
        hc_add_series(x, "scatter", hcaes(logFC, -log10(PValue), group = sig, value = lnc),
                      color = c("#FF0000", "#00FFFF"), 
                      enableMouseTracking = c(TRUE, TRUE),
                      showInLegend = TRUE, marker = list(radius = 4)) %>%
        hc_tooltip(pointFormat = "{point.value}",  headerFormat = "") %>%
        hc_xAxis(title = list(text = "Log fold change"), gridLineWidth = 1,
                 tickLength = 0, startOnTick = "true", endOnTick = "true", min = -6, max = 6) %>%
        hc_yAxis(title = list(text = "-Log10(p-value)")) %>%
        hc_chart(zoomType = "xy", width=700) %>%
        hc_exporting(enabled = TRUE, filename = "volcano")
      hc
    }else{return("null")}
  }) 
  

  
  ####################################################################
  
  #############################################################################
  ############################### mRNA survival analysis ########################
  
  #read counts matrix
  surv1_lnc <- reactive({		
    cox_data <- load(input$cancer3_lnc)
    if(cox_data == "surv_coxph_lnc"){
      cox_data <- surv_coxph_lnc
      cox_data <- cox_km_surv_func(cox_data)
    }
  })
  
  surv_analysis_cox_lnc <-  reactive({
    if(input$surv_lnc == "coxph_lnc"){
      model <- surv1_lnc()
      model <- cbind("Genes" = rownames(model), model)
      model
    }
  })%>%bindCache(input$surv_lnc, surv1_lnc())
  
  ## add action button to table
  output$survival_cox_lnc <- renderDataTable({
    datatable(surv_analysis_cox_lnc(),
              options = list(
                "pageLength" = 25), rownames = FALSE)
  })
  
  ###################################################################################
  ########################## logrank analysis #################################
  
  surv2_lnc <- reactive({		
    km_data <- load(input$cancer_median_lnc)
    if(km_data == "surv_kmLR_lnc"){
      km_data <- surv_kmLR_lnc
      km_data <- cox_km_surv_func(km_data)
    }
  })
  
  
  surv3_lnc <- reactive({		
    km_data <- load(input$cancer_mean_lnc)
    if(km_data == "surv_kmLR_lnc"){
      km_data <- surv_kmLR_lnc
      km_data <- cox_km_surv_func(km_data)
    }
  })
  
  surv4_lnc <- reactive({		
    km_data <- load(input$cancer_1stQu_lnc)
    if(km_data == "surv_kmLR_lnc"){
      km_data <- surv_kmLR_lnc
      km_data <- cox_km_surv_func(km_data)
    }
  })
  
  surv5_lnc <- reactive({		
    km_data <- load(input$cancer_3rdQu_lnc)
    if(km_data == "surv_kmLR_lnc"){
      km_data <- surv_kmLR_lnc
      km_data <- cox_km_surv_func(km_data)
    }
  })
  
  
  surv_analysis_KM_lnc <-  reactive({
    if(input$surv_lnc == "km_lnc"){
      if(input$sep_lnc == "median_lnc"){
        model <- surv2_lnc()
        model <- cbind("Genes" = rownames(model), model)
      } else if(input$sep_lnc == "mean_lnc"){
        model <- surv3_lnc()
        model <- cbind("Genes" = rownames(model), model)
      }else if(input$sep_lnc == "1stQu_lnc"){
        model <- surv4_lnc()
        model <- cbind("Genes" = rownames(model), model)
      }else if(input$sep_lnc == "3rdQu_lnc"){
        model <- surv5_lnc()
        model <- cbind("Genes" = rownames(model), model)
      }
    }
  })%>%
    bindCache(input$surv_lnc, input$sep_lnc, surv2_lnc(), surv3_lnc(), surv4_lnc(), surv5_lnc())
  
  output$survival_KM_lnc <- renderDataTable({
    datatable(surv_analysis_KM_lnc(),
              options = list(
                "pageLength" = 25), rownames = FALSE)
  })
  
  options(shiny.sanitize.errors = FALSE)
 
  ##################### Tutorial page ###########
  #output$tutorialP <- renderUI({
    #includeHTML("Data/www/tutorial_page.html")
  #})
  
  
  }
 
