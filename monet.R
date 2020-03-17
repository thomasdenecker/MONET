################################################################################
# Title : MONET
# Organism : All 
# Omics area : Proteomic
# Users : Thomas Denecker
# Email : thomas.denecker@gmail.com
# Date : Mar, 2020
################################################################################

################################################################################
###                                Library                                   ###
################################################################################

library(dplyr)
library(igraph)
library(shiny)
library(shinyjs)
library(shinyalert)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
require(visNetwork)

################################################################################
###                                URL                                       ###
################################################################################

string_api_url = "https://string-db.org/api"
output_format = "tsv"

limitsNodes = 10

################################################################################
###                                Functions                                 ###
################################################################################

idMappingUniprot <- function(from, to, query, format='tab'){
  r = GET("https://www.uniprot.org/uploadlists/", 
          query = list("from" = from, "to" = to ,'format' = 'tab', 'query'= query))
  return(readr:::read_tsv(r$content))
}

################################################################################
###                                   JS                                     ###
################################################################################

jsCode <- '
  shinyjs.Visu3D = function(params) {
    // Create NGL Stage object
    const viewportDiv = document.getElementById("viewport");
    viewportDiv.innerHTML = "";
    var stage = new NGL.Stage( "viewport" );
    str1 = "rcsb://" ;

    // Handle window resizing
    window.addEventListener( "resize", function( event ){
        stage.handleResize();
    }, false );
    
    // Load PDB
    stage.loadFile( str1.concat("", params) , { defaultRepresentation: true } );

  }
'

################################################################################
###                                   UI                                     ###
################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "MONET", 
                  dropdownMenu(icon = icon("question-circle"),badgeStatus =NULL,headerText = "Global information",
                               messageItem(
                                 from = "Find our project?",
                                 message = "Visit our Github!",
                                 icon = icon("github"),
                                 href = "https://github.com/thomasdenecker/MONET"
                               ),
                               messageItem(
                                 from = "New User?",
                                 message = "Read the docs!",
                                 icon = icon("book"),
                                 href = "https://github.com/thomasdenecker/MONET/wiki"
                               ),
                               messageItem(
                                 from = "A bug with app?",
                                 message = "Declare an issue!",
                                 icon = icon("exclamation-triangle"),
                                 href = "https://github.com/thomasdenecker/MONET/issues"
                               )
                  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import data", tabName = "import"),
      menuItem("Graph", tabName = "graph"),
      menuItem("Enrichissement", tabName = "enrichissement"),
      menuItem("Raw data", tabName = "rawdata")
    )
  ),
  dashboardBody(
    tags$head(tags$link(href = "img/icon_MONET.png",
                        rel ="icon", type="image/png")),
    tags$head(tags$script( src="https://cdn.rawgit.com/arose/ngl/v2.0.0-dev.32/dist/ngl.js")), 
    tags$head(tags$style(type = "text/css", "
               canvas{height:100% !important; width:100% !important;background-color: rgba(255, 255, 255,0) !important}
               ")),
    useShinyjs(),
    extendShinyjs(text = jsCode,functions = "Visu3D"),
    tabItems(
      tabItem("import",
              img(src="img/logo_MONET.svg", height = "150px", style="display: block; margin-left: auto; margin-right: auto;"), 
              
              withSpinner(uiOutput("import"), color = getOption("spinner.color", default = "blue"))
              
      ),
      tabItem("graph",
              fluidRow(
                column(6,div(id = "networkDiv", visNetworkOutput("network", height = "100%"))),
                column(6,div(id = "networkSidebar",
                             h3("Network style"),
                             selectInput("layout", "Layout :", selected = "layout_nicely", 
                                         choices = c("Bipartite" = "layout_as_bipartite", 
                                                     "Star"  ="layout_as_star", 
                                                     "Tree" = "layout_as_tree", 
                                                     "Circle" = "layout_in_circle", 
                                                     "Nicely" = "layout_nicely", 
                                                     "Grid" = "layout_on_grid", 
                                                     "Sphere" = "layout_on_sphere", 
                                                     "Randomly" = "layout_randomly", 
                                                     "DH" = "layout_with_dh", 
                                                     "FR" = "layout_with_fr", 
                                                     "Gem" = "layout_with_gem", 
                                                     "Graphopt" = "layout_with_graphopt", 
                                                     "Kk" = "layout_with_kk", 
                                                     "Lgl" = "layout_with_lgl", 
                                                     "Mds" = "layout_with_mds", 
                                                     "Sugiyama" = "layout_with_sugiyama")),
                             selectizeInput("colo","Enrichissement coloration", choices = NULL, 
                                            selected = NULL, multiple = FALSE), 
                             selectizeInput("coloL2",NULL, choices = NULL, 
                                            selected = NULL, multiple = FALSE), 
                             h3("Node information"), 
                             helpText("Click on node to have information"),
                             h4("General information"), 
                             htmlOutput("selected_var_gene"), 
                             htmlOutput("selected_var_description"),
                             htmlOutput("selected_var_species"),
                             div(id= "PDBDIV", 
                                 htmlOutput("PDB_title"),
                                 selectizeInput("PDBSelector_ID", "PDB ID", choices = NULL, 
                                                selected = NULL, multiple = FALSE),
                                 div(id="viewport",style="width:100%; height:400px;" ),
                             ),
                             h4("Functionnal annotation"),
                             htmlOutput("FA_component_title"),
                             DTOutput('FA_component'),
                             htmlOutput("FA_Function_title"),
                             DTOutput('FA_Function'),
                             htmlOutput("FA_Process_title"),
                             DTOutput('FA_Process'), 
                             htmlOutput("FA_KEGG_title"),
                             DTOutput('FA_KEGG'),
                             htmlOutput("FA_InterPro_title"),
                             DTOutput('FA_InterPro'),
                             htmlOutput("FA_Keyword_title"),
                             DTOutput('FA_Keyword'),
                             htmlOutput("FA_PMID_title"),
                             DTOutput('FA_PMID'),
                             htmlOutput("FA_RCTM_title"),
                             DTOutput('FA_RCTM'),
                             htmlOutput("FA_SMART_title"),
                             DTOutput('FA_SMART'),
                             htmlOutput("FA_Pfam_title"),
                             DTOutput('FA_Pfam')
                )))
      ),
      tabItem("enrichissement",
              h1("Enrichissement"), 
              h2("Go terms"),
              h3("Cellular component"),
              DTOutput('CC'),
              h3("Biological Process"),
              DTOutput('BP'),
              h3("Molecular Function"),
              DTOutput('MF'),
              h2("INTERPRO Protein Domains and Features"), 
              DTOutput('INTERPRO'),
              h2("UniProt Keywords"), 
              DTOutput('UniProt'),
              h2("KEGG Pathways"), 
              DTOutput('KEGG'),
              h2("Reference publications"), 
              DTOutput('PMID')
      ),
      tabItem("rawdata",
              h1("Rawdata"), 
              h2("Proteins to genes"), 
              DTOutput('PI'),
              h2("Information for all nodes"), 
              DTOutput('DT_infoAll'),
              h2("Network table"),
              DTOutput('DT_network'),
              h2("Interaction"),
              DTOutput('DT_Interaction')
      )
    ),
    tags$style(type = "text/css", "#networkDiv {height: calc(100vh - 115px) !important;}
               #networkSidebar {height: calc(100vh - 85px) !important; overflow-y: auto;}
               ")
  )
)

################################################################################
###                                 Server                                   ###
################################################################################

server <- function(input, output, session) {
  si <- sessionInfo()
  
  if(! file.exists("www/data/STRINGspecies.txt")){
    speciesSTRING = download.file("https://stringdb-static.org/download/species.v11.0.txt", 
                                  destfile = "www/data/STRINGspecies.txt"
    )
  }
  
  speciesSTRING = read.csv2("www/data/STRINGspecies.txt", 
                            sep ="\t", stringsAsFactors = F)
  updateSelectInput(session, "Species",
                    choices = setNames(speciesSTRING[,1],speciesSTRING[,3] ) , 
                    
  )
  
  Level_GO = read.csv("www/data/Level_GO.txt", sep = "\t", header = T)
  rownames(Level_GO) = Level_GO$GO_ID
  
  #=============================================================================
  # Reactive Values
  #=============================================================================
  
  STRING <-reactiveValues()
  
  output$import <- renderUI({
    div(
      h1("Import data"),
      textAreaInput("protList", "Protein list", placeholder = "FTR1,FET3,..."),
      selectizeInput("Species", "Species", choices = NULL, 
                     selected = NULL, multiple = FALSE,
                     options = list(
                       placeholder = 'Search a species',
                       maxOptions = 100,
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      actionButton("Search", "Search"),
      tags$br(), 
      tags$br(), 
      fluidRow(valueBoxOutput("sizeQuery", width = 3),
               valueBoxOutput("unmatchedProtein", width = 3),
               valueBoxOutput("nbNodesFinal", width = 3),
               valueBoxOutput("connection", width = 3))
      
    )
  })
  
  
  #=============================================================================
  # Import
  #=============================================================================
  
  observeEvent(input$Search, {
    m = 8
    withProgress(message = 'Extraction in progress', value = 0, {
      
      incProgress(1/m, detail = "Initilization")
      
      STRING$initProt = unlist(strsplit(x =input$protList,split = '[ \r\n]' ) )
      STRING$ourSpecies = input$Species
      
      STRING$associationProtGenes = setNames(rep(NA, length(STRING$initProt)), STRING$initProt)
      
      parameters = paste0( "identifiers=", paste0(STRING$initProt,collapse = "%0d"), 
                           "&species=",STRING$ourSpecies,
                           collapse = "")
      
      #-------------------------------------------------------------------------------
      # Association prot to gene par STRING
      #-------------------------------------------------------------------------------
      incProgress(1/m, detail = "Protein to gene")
      method = "get_string_ids"
      parametersInfo = paste0( "identifiers=", paste0(STRING$initProt,collapse = "%0d"), 
                               "&species=",STRING$ourSpecies,
                               collapse = "")
      
      request_url = paste0(string_api_url, "/", output_format ,"/", method, "?", collapse = "")
      request_url = paste0(request_url, parametersInfo, collapse = "")
      STRING$dataInfo = read.csv2(request_url, sep ="\t", header = T, stringsAsFactors = F)
      
      STRING$associationProtGenes[(STRING$dataInfo$queryIndex + 1) ] = STRING$dataInfo$preferredName
      STRING$listUnknow = names(STRING$associationProtGenes)[is.na(STRING$associationProtGenes)]
      STRING$associationProtGenes[is.na(STRING$associationProtGenes)] = names(STRING$associationProtGenes)[is.na(STRING$associationProtGenes)]
      
      #-------------------------------------------------------------------------------
      # Nouvelles connexions
      #-------------------------------------------------------------------------------
      incProgress(1/m, detail = "Add new proteins")
      method = "interaction_partners"
      parametersGraph = paste0(parameters, "&limit=",limitsNodes)
      
      request_url = paste0(string_api_url, "/", output_format ,"/", method, "?", collapse = "")
      request_url = paste0(request_url, parametersGraph, collapse = "")
      STRING$dataInteraction = read.csv2(request_url, sep ="\t", header = T, 
                                         stringsAsFactors = F)
      
      #-------------------------------------------------------------------------------
      # Connexions entre les nouvelles 
      #-------------------------------------------------------------------------------
      incProgress(1/m, detail = "Generate new interactions")
      listProtInteraction = unique(c(STRING$dataInteraction[,"preferredName_A"], 
                                     STRING$dataInteraction[,"preferredName_B"]))
      method = "network"
      parametersNetworks = paste0( "identifiers=", paste0(listProtInteraction,collapse = "%0d"), 
                                   "&species=",STRING$ourSpecies,
                                   collapse = "")
      
      
      request_url = paste0(string_api_url, "/", output_format ,"/", method, "?", collapse = "")
      request_url = paste0(request_url, parametersNetworks, collapse = "")
      STRING$dataNetwork = read.csv2(request_url, sep ="\t", header = T)
      
      #-------------------------------------------------------------------------------
      # Récupération de l'information pour les noeuds 
      #-------------------------------------------------------------------------------
      incProgress(1/m, detail = "Get information")
      method = "get_string_ids"
      parametersInfo = paste0( "identifiers=", paste0(listProtInteraction,collapse = "%0d"), 
                               "&species=",STRING$ourSpecies,
                               collapse = "")
      
      request_url = paste0(string_api_url, "/", output_format ,"/", method, "?", collapse = "")
      request_url = paste0(request_url, parametersInfo, collapse = "")
      STRING$dataInfoAll = read.csv2(request_url, sep ="\t", header = T, stringsAsFactors = F)
      STRING$dataInfoAll = STRING$dataInfoAll %>% 
        filter(preferredName %in% listProtInteraction)
      
      #---------------------------------------------------------------------------
      # Enrichissement
      #---------------------------------------------------------------------------
      incProgress(1/m, detail = "Calculate enrichissement")
      method = "enrichment"
      parametersEnrichment = paste0( "identifiers=", paste0(listProtInteraction,collapse = "%0d"), 
                                     "&species=",STRING$ourSpecies,
                                     collapse = "")
      
      request_url = paste0(string_api_url, "/", output_format ,"/", method, "?", collapse = "")
      request_url = paste0(request_url, parametersEnrichment, collapse = "")
      STRING$dataEnrichissement = read.csv2(request_url, sep ="\t", header = T)
      
      #---------------------------------------------------------------------------
      # Component
      #---------------------------------------------------------------------------
      incProgress(1/m, detail = "Component analysis")
      
      updateSelectInput(session, "colo",
                        choices = c("None" = "None", 
                                    setNames(as.character(unique(STRING$dataEnrichissement$category)), 
                                             as.character(unique(STRING$dataEnrichissement$category)))) ,
                        selected = "None"
      )
      
      #---------------------------------------------------------------------------
      # Graph creation
      #---------------------------------------------------------------------------
      incProgress(1/m, detail = "Graph creation")
      
      if(nrow(STRING$dataInfoAll) !=0 ){
        STRING$nodes  = STRING$dataInfoAll[,c("preferredName","annotation")] %>% distinct() %>%
          mutate(annotation = preferredName,
                 type = case_when(preferredName %in% STRING$associationProtGenes ~ "square",
                                  T ~ "circle"),
                 color = "orange")
        
      } else {
        STRING$nodes  = STRING$dataInfoAll[,c("preferredName","annotation")] %>% distinct() %>%
          mutate(annotation = preferredName,
                 type = case_when(preferredName %in% STRING$associationProtGenes ~ "square",
                                  T ~ "circle")) %>%
          mutate(color = "orange")
      }
      
      colnames(STRING$nodes) = c("id", "label", "shape", "color")
      
      if(length(STRING$listUnknow) != 0){
        STRING$nodes  = rbind(nodes, cbind("id" = STRING$listUnknow, 
                                           "label"= STRING$listUnknow, 
                                           "shape" = "rectangle", 
                                           "color" = "gray")) %>% distinct()
      }
      
      STRING$links = STRING$dataNetwork[, c("preferredName_A", "preferredName_B")]
      colnames(STRING$links) = c("from", "to")
      
    })
  })
  
  observeEvent(input$colo, {
    if(!is.null(STRING$dataEnrichissement)){
      
      updateSelectInput(session, "coloL2",
                        choices = STRING$dataEnrichissement %>% filter(category == input$colo) %>% pull(description) , 
      )
    }
  })
  
  observeEvent(input$coloL2, {
    if(input$coloL2 != ""){
      inter = STRING$dataEnrichissement %>% filter(description == input$coloL2) %>% pull(preferredNames)
      inter = unlist(strsplit(as.character(inter), ","))
      STRING$nodes$color = "orange"
      STRING$nodes$color[STRING$nodes$id %in% inter ] = "red" 
    } else {
      STRING$nodes$color = "orange"
    }
  })
  
  output$PI = renderDT(
    STRING$dataInfo, options = list(lengthChange = FALSE)
  )
  
  output$DT_enrichissement = renderDT(
    STRING$dataEnrichissement, options = list(lengthChange = FALSE)
  )
  
  output$DT_infoAll = renderDT(
    STRING$dataInfoAll, options = list(lengthChange = FALSE)
  )
  
  output$DT_network = renderDT(
    STRING$dataNetwork, options = list(lengthChange = FALSE)
  )
  
  output$DT_Interaction = renderDT(
    STRING$dataInteraction, options = list(lengthChange = FALSE)
  )
  
  #=============================================================================
  # Création du graphe 
  #=============================================================================
  
  output$network <- renderVisNetwork({
    if(!is.null(STRING$nodes)){
      STRING$network = visNetwork(as.data.frame(STRING$nodes), as.data.frame(STRING$links)) %>%
        visExport() %>%
        visOptions(nodesIdSelection = TRUE,
                   highlightNearest = TRUE) %>%
        visIgraphLayout(layout = input$layout, randomSeed = 123) %>%
        visInteraction(navigationButtons = TRUE, 
                       dragNodes = FALSE,
                       dragView = FALSE, zoomView = FALSE)
      
      STRING$network
    } else {
      NULL
    }
    
  })
  
  output$selected_var_gene <- renderUI({
    HTML(paste("<b>Gene name </b>:", input$network_selected))
  })
  
  output$selected_var_description <- renderText({ 
    HTML(paste("<b>Description </b>:", unique(STRING$dataInfoAll$annotation[STRING$dataInfoAll$preferredName == input$network_selected])))
  })
  
  output$selected_var_species <- renderText({ 
    HTML(paste("<b>Species </b>:<i>", unique(STRING$dataInfoAll$taxonName[STRING$dataInfoAll$preferredName == input$network_selected]), "</i>"))
  })
  
  #=============================================================================
  # Gene selected Information
  #=============================================================================
  
  # Get information
  observeEvent(input$network_selected, {
    if(input$network_selected != ""){
      method = "functional_annotation"
      parametersEnrichment = paste0( "identifiers=", input$network_selected, 
                                     "&species=",STRING$ourSpecies,
                                     collapse = "")
      
      request_url = paste0(string_api_url, "/", output_format ,"/", method, "?", collapse = "")
      request_url = paste0(request_url, parametersEnrichment, collapse = "")
      STRING$annotation = read.csv2(request_url, sep ="\t", header = T)
      
      STRING$ID = unique(STRING$dataInfoAll$stringId[STRING$dataInfoAll$preferredName == input$network_selected])
      STRING$UniprotID = as.matrix(idMappingUniprot("STRING_ID", "ID", STRING$ID, "tab"))
      STRING$UniprotID = as.character(STRING$UniprotID[1,2])
      if(!is.na(STRING$UniprotID)){
        STRING$PDB = as.matrix(idMappingUniprot("ID", "PDB_ID", STRING$UniprotID, "tab"))
        final = NULL
        for(i in STRING$PDB[,2]){
          inter = read.csv(paste0("http://www.rcsb.org/pdb/rest/customReport.csv?pdbids=",i,"&reportName=StructureSummary&service=wsfile&format=csv"))
          if(nrow(inter) != 0){
            final = c(final, i)
          }
        }
        STRING$PDB = final
      } else {
        STRING$PDB = NULL
        updateSelectInput(session, "PDBSelector_ID",
                          choices = NULL)
      }
    } else {
      STRING$annotation = NULL
      STRING$PDB = NULL
      shinyjs::hide(id = "PDBDIV")
      updateSelectInput(session, "PDBSelector_ID",
                        choices = "")
    }
    
  })
  
  observeEvent(STRING$PDB, {
    if(!is.null(STRING$PDB) && length(STRING$PDB) != 0){
      updateSelectInput(session, "PDBSelector_ID",
                        choices = setNames(as.character(STRING$PDB),
                                           as.character(STRING$PDB)))
      shinyjs::show(id = "PDBDIV")
    } else {
      updateSelectInput(session, "PDBSelector_ID",
                        choices = NULL)
      shinyjs::hide(id = "PDBDIV")
    }
  })
  
  # PDB
  
  observeEvent(input$PDBSelector_ID, {
    if(!is.null(input$PDBSelector_ID) && input$PDBSelector_ID != ""){
      js$Visu3D(input$PDBSelector_ID)
    } 
    
  })
  
  output$PDB_title <- renderText({ 
    if(!is.null(STRING$PDB) && length(STRING$PDB) != 0){
      HTML('<b><u>PDB</u></b>') 
    } else {
      NULL
    }
  })
  
  # output$PDB_zone <- renderText({ 
  #   if(!is.null(STRING$PDB) && nrow(STRING$PDB) != 0){
  #     HTML('<div id="viewport" style="width:100%; height:400px;"></div>') 
  #   } else {
  #     NULL
  #   }
  # })
  
  
  # Go terms : Component
  output$FA_component_title <- renderText({ 
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "Component")) != 0){
      HTML("<b><u>Cellular component</u></b>") 
    } else {
      NULL
    }
  })
  
  output$FA_component = renderDT({
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "Component")) != 0){
      STRING$annotation %>% filter(category == "Component") %>% 
        mutate(term =  gsub("\\.", ":", term),
               term = paste0('<a href="http://amigo.geneontology.org/amigo/term/',term,'" target="_blank">',term,"</a>" )) %>%
        dplyr::select(term, description) %>%
        dplyr::rename('GO term' = term,
                      'Description' = description
        ) 
    } else {
      NULL
    }
  }, selection = 'none', escape = FALSE,
  options = list(pageLength = 5, scrollX = TRUE)
  )
  
  # Go terms : Function
  output$FA_Function_title <- renderText({ 
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "Function")) != 0){
      HTML("<b><u>Molecular Function</u></b>") 
    } else {
      NULL
    }
  })
  
  output$FA_Function = renderDT({
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "Function")) != 0){
      STRING$annotation %>% filter(category == "Function") %>% 
        mutate(term =  gsub("\\.", ":", term),
               term = paste0('<a href="http://amigo.geneontology.org/amigo/term/',term,'" target="_blank">',term,"</a>" )) %>%
        dplyr::select(term, description) %>%
        dplyr::rename('GO term' = term,
                      'Description' = description
        ) 
    } else {
      NULL
    }
  }, selection = 'none', escape = FALSE,
  options = list(pageLength = 5, scrollX = TRUE)
  )
  
  # Go terms : Process
  output$FA_Process_title <- renderText({ 
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "Process")) != 0){
      HTML("<b><u>Biological process</u></b>") 
    } else {
      NULL
    }
  })
  
  output$FA_Process = renderDT({
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "Process")) != 0){
      STRING$annotation %>% filter(category == "Process") %>% 
        mutate(term =  gsub("\\.", ":", term),
               term = paste0('<a href="http://amigo.geneontology.org/amigo/term/',term,'" target="_blank">',term,"</a>" )) %>%
        dplyr::select(term, description) %>%
        dplyr::rename('GO term' = term,
                      'Description' = description
        ) 
    } else {
      NULL
    }
  }, selection = 'none', escape = FALSE,
  options = list(pageLength = 5, scrollX = TRUE)
  ) 
  
  
  # Pfam
  output$FA_Pfam_title <- renderText({ 
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "Pfam")) != 0){
      HTML("<b><u>Pfam</u></b>") 
    } else {
      NULL
    }
  })
  
  output$FA_Pfam = renderDT({
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "Pfam")) != 0 ){
      STRING$annotation %>% filter(category == "Pfam") %>% 
        mutate(term =  gsub("\\.", ":", term),
               term = paste0('<a href="http://pfam.xfam.org/family/',term,'" target="_blank">',term,"</a>")) %>%
        dplyr::select(term, description) %>%
        dplyr::rename('GO term' = term,
                      'Description' = description
        ) 
    } else {
      NULL
    }
  }, selection = 'none', escape = FALSE,
  options = list(pageLength = 5, scrollX = TRUE)
  )
  
  # SMART
  output$FA_SMART_title <- renderText({ 
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "SMART")) != 0){
      HTML("<b><u>SMART</u></b>") 
    } else {
      NULL
    }
  })
  
  output$FA_SMART = renderDT({
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "SMART")) != 0){
      STRING$annotation %>% filter(category == "SMART") %>% 
        mutate(term =  gsub("\\.", ":", term),
               term = paste0('<a href="http://smart.embl.de/smart/do_annotation.pl?DOMAIN=',term,'" target="_blank">',term,"</a>" )) %>%
        dplyr::select(term, description) %>%
        dplyr::rename('GO term' = term,
                      'Description' = description
        ) 
    } else {
      NULL
    }
  }, selection = 'none', escape = FALSE,
  options = list(pageLength = 5, scrollX = TRUE)
  )
  
  # Reactome
  output$FA_PMID_title <- renderText({ 
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "RCTM")) != 0){
      HTML("<b><u>Reactome</u></b>") 
    } else {
      NULL
    }
  })
  
  output$FA_RCTM = renderDT({
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "RCTM")) != 0){
      STRING$annotation %>% filter(category == "RCTM") %>% 
        mutate(term =  gsub("\\.", ":", term),
               term = paste0('<a href="https://reactome.org/content/detail/R-',term,'" target="_blank">',term,"</a>" )) %>%
        dplyr::select(term, description) %>%
        dplyr::rename('GO term' = term,
                      'Description' = description
        ) 
    } else {
      NULL
    }
  }, selection = 'none', escape = FALSE,
  options = list(pageLength = 5, scrollX = TRUE)
  )
  
  # PMID
  output$FA_RCTM_title <- renderText({ 
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "PMID")) != 0){
      HTML("<b><u>Pubmed</u></b>") 
    } else {
      NULL
    }
  })
  
  output$FA_PMID = renderDT({
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "PMID")) != 0){
      STRING$annotation %>% filter(category == "PMID") %>% 
        mutate(term =  gsub("\\.", ":", term),
               term = paste0('<a href="https://www-ncbi-nlm-nih-gov.insb.bib.cnrs.fr/pubmed?term=',gsub("PMID.", "", term),'%5Buid" target="_blank">',term,"</a>" )) %>%
        dplyr::select(term, description) %>%
        dplyr::rename('GO term' = term,
                      'Description' = description
        ) 
    } else {
      NULL
    }
  }, selection = 'none', escape = FALSE,
  options = list(pageLength = 5, scrollX = TRUE)
  )
  
  # KEGG
  output$FA_KEGG_title <- renderText({ 
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "KEGG")) != 0){
      HTML("<b><u>KEGG</u></b>") 
    } else {
      NULL
    }
  })
  
  output$FA_KEGG = renderDT({
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "KEGG")) != 0){
      STRING$annotation %>% filter(category == "KEGG") %>% 
        mutate(term =  gsub("\\.", ":", term),
               term = paste0('<a href="https://www.genome.jp/kegg-bin/show_pathway?',term,'" target="_blank">',term,"</a>")) %>%
        dplyr::select(term, description) %>%
        dplyr::rename('GO term' = term,
                      'Description' = description
        ) 
    } else {
      NULL
    }
  }, selection = 'none', escape = FALSE,
  options = list(pageLength = 5, scrollX = TRUE)
  )
  
  # InterPro
  output$FA_InterPro_title <- renderText({ 
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "InterPro")) != 0){
      HTML("<b><u>InterPro</u></b>") 
    } else {
      NULL
    }
  })
  
  output$FA_InterPro = renderDT({
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "InterPro")) != 0){
      STRING$annotation %>% filter(category == "InterPro") %>% 
        mutate(term =  gsub("\\.", ":", term),
               term = paste0('<a href="https://www.ebi.ac.uk/interpro/entry/InterPro/',term,'/" target="_blank">',term,"</a>")) %>%
        dplyr::select(term, description) %>%
        dplyr::rename('GO term' = term,
                      'Description' = description
        ) 
    } else {
      NULL
    }
  }, selection = 'none', escape = FALSE,
  options = list(pageLength = 5, scrollX = TRUE)
  )
  
  # Keyword
  output$FA_Keyword_title <- renderText({ 
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "Keyword")) != 0){
      HTML("<b><u>UniProt</u></b>") 
    } else {
      NULL
    }
  })
  
  output$FA_Keyword = renderDT({
    if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "Keyword")) != 0){
      STRING$annotation %>% filter(category == "Keyword") %>% 
        mutate(term =  gsub("\\.", ":", term),
               term = paste0('<a href="https://www.uniprot.org/keywords/',term,'" target="_blank">',term,"</a>" )) %>%
        dplyr::select(term, description) %>%
        dplyr::rename('GO term' = term,
                      'Description' = description
        ) 
    } else {
      NULL
    }
  }, selection = 'none', escape = FALSE,
  options = list(pageLength = 5, scrollX = TRUE)
  )
  
  #-----------------------------------------------------------------------------
  # Enrichissement
  #-----------------------------------------------------------------------------
  
  output$CC = renderDataTable(
    STRING$dataEnrichissement %>% filter(category == "Component") %>% 
      mutate(term =  gsub("\\.", ":", term),
             term = paste0('<a href="http://amigo.geneontology.org/amigo/term/',term,'" target="_blank">',term,"</a>" ),
             number_of_genes = paste0(number_of_genes , " of ", number_of_genes_in_background)) %>%
      dplyr::select(term, description, number_of_genes, fdr) %>%
      dplyr::rename('GO term' = term,
                    'Description' = description, 
                    'Count in gene set' = number_of_genes, 
                    'false dicovery rate' = fdr
      ), 
    selection = 'none', escape = FALSE,
    options = list(scrollX = TRUE)
  )
  
  output$BP = renderDataTable(
    STRING$dataEnrichissement %>% filter(category == "Process") %>% 
      mutate(term =  gsub("\\.", ":", term),
             term = paste0('<a href="http://amigo.geneontology.org/amigo/term/',term,'" target="_blank">',term,"</a>" ),
             number_of_genes = paste0(number_of_genes , " of ", number_of_genes_in_background)) %>%
      dplyr::select(term, description, number_of_genes, fdr) %>%
      dplyr::rename('GO term' = term,
                    'Description' = description, 
                    'Count in gene set' = number_of_genes, 
                    'false dicovery rate' = fdr
      ), 
    selection = 'none', escape = FALSE,
    options = list(scrollX = TRUE)
  )
  
  output$MF = renderDataTable(
    STRING$dataEnrichissement %>% filter(category == "Function") %>% 
      mutate(term =  gsub("\\.", ":", term),
             term = paste0('<a href="http://amigo.geneontology.org/amigo/term/',term,'" target="_blank">',term,"</a>" ),
             number_of_genes = paste0(number_of_genes , " of ", number_of_genes_in_background)) %>%
      dplyr::select(term, description, number_of_genes, fdr) %>%
      dplyr::rename('GO term' = term,
                    'Description' = description, 
                    'Count in gene set' = number_of_genes, 
                    'false dicovery rate' = fdr
      ), 
    selection = 'none', escape = FALSE,
    options = list(scrollX = TRUE)
  )
  
  output$INTERPRO = DT::renderDataTable({
    if(!is.null(STRING$dataEnrichissement ) & nrow(STRING$dataEnrichissement ) != 0){
      STRING$dataEnrichissement %>% filter(category == "InterPro") %>% 
        mutate(term = paste0('<a href="https://www.ebi.ac.uk/interpro/entry/InterPro/',term,'/" target="_blank">',term,"</a>" ),
               number_of_genes = paste0(number_of_genes , " of ", number_of_genes_in_background)) %>%
        dplyr::select(term, description, number_of_genes, fdr) %>%
        dplyr::rename('GO term' = term,
                      'Description' = description, 
                      'Count in gene set' = number_of_genes, 
                      'false dicovery rate' = fdr
        )
      
    } else {
      NULL
    }
    
  }, 
  selection = 'none', escape = FALSE, 
  options = list(scrollX = TRUE)
  )
  
  output$UniProt = renderDT(
    STRING$dataEnrichissement %>% filter(category == "Keyword") %>% 
      mutate(term = paste0('<a href="https://www.uniprot.org/keywords/',term,'" target="_blank">',term,"</a>" ),
             number_of_genes = paste0(number_of_genes , " of ", number_of_genes_in_background)) %>%
      dplyr::select(term, description, number_of_genes, fdr) %>%
      dplyr::rename('GO term' = term,
                    'Description' = description, 
                    'Count in gene set' = number_of_genes, 
                    'false dicovery rate' = fdr
      ),
    selection = 'none', escape = FALSE,
    options = list(scrollX = TRUE)
  )
  
  output$KEGG = renderDataTable(
    STRING$dataEnrichissement %>% filter(category == "KEGG") %>% 
      mutate(term = paste0('<a href="https://www.genome.jp/kegg-bin/show_pathway?',term,'" target="_blank">',term,"</a>" ),
             number_of_genes = paste0(number_of_genes , " of ", number_of_genes_in_background)) %>%
      dplyr::select(term, description, number_of_genes, fdr) %>%
      dplyr::rename('GO term' = term,
                    'Description' = description, 
                    'Count in gene set' = number_of_genes, 
                    'false dicovery rate' = fdr
      ), 
    selection = 'none', escape = FALSE,
    options = list(scrollX = TRUE)
  )
  
  output$PMID = renderDataTable(
    STRING$dataEnrichissement %>% filter(category == "PMID") %>% 
      mutate(term = paste0('<a href="https://www-ncbi-nlm-nih-gov.insb.bib.cnrs.fr/pubmed?term=',gsub("PMID.", "", term),'%5Buid" target="_blank">',term,"</a>" ),
             number_of_genes = paste0(number_of_genes , " of ", number_of_genes_in_background)) %>%
      dplyr::select(term, description, number_of_genes, fdr) %>%
      dplyr::rename('GO term' = term,
                    'Description' = description, 
                    'Count in gene set' = number_of_genes, 
                    'false dicovery rate' = fdr
      ), 
    selection = 'none', escape = FALSE,
    options = list(scrollX = TRUE )
  )
  
  output$sizeQuery <- renderValueBox({
    valueBox(
      length(STRING$initProt), "# protein in query", icon = icon("list"),
      color = "green"
    )
  })
  
  output$unmatchedProtein <- renderValueBox({
    valueBox(
      length(STRING$listUnknow), "# Unmatched", icon = icon("question-circle"),
      color = "red"
    )
  })
  
  output$nbNodesFinal <- renderValueBox({
    if(is.null(nrow(STRING$nodes))){
      inter = 0
    } else {
      inter = nrow(STRING$nodes)
    }
    valueBox(
      inter, "# nodes", icon = icon("circle"),
      color = "blue"
    )
  })
  
  output$connection <- renderValueBox({
    if(is.null(STRING$links)){
      inter = 0
    } else {
      inter = nrow(STRING$links)
    }
    valueBox(
      inter, "# edges", icon = icon("arrows-alt-h"),
      color = "purple"
    )
  })
  
}

shinyApp(ui, server)
