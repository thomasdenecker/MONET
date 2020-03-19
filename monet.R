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
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(visNetwork)
library(httr)
library(plotly)
library(reshape2)

################################################################################
###                                URL                                       ###
################################################################################

string_api_url = "https://string-db.org/api"
output_format = "tsv"

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

ui <- dashboardPagePlus(
  dashboardHeaderPlus(title = tagList(
    span(class = "logo-lg", img(src="img/logo_MONET_white.svg", height = "40px") ), 
    img(src = "img/icon_MONET_white.png")),
    dropdownMenu(icon = icon("question-circle"),badgeStatus =NULL,headerText = "Global information",
                 messageItem(
                   from = "Find our project?",
                   message = "Visit our Github!",
                   icon = icon("github", class = "fa"),
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
    ), 
    left_menu = tagList(dropdownBlock(
      id = "networkDropdown",
      title = HTML("<i class='fa fa-gear'></i>  Network settings"),
      numericInput(inputId = "sizeNodes", label = "Node size",value = 20,min = 1,
                   max=50),
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
                     selected = NULL, multiple = FALSE)
    ))
  ),
  dashboardSidebar(
    uiOutput('sidebar')
  ),
  dashboardBody(
    tags$head(tags$link(href = "img/icon_MONET.png",
                        rel ="icon", type="image/png")),
    tags$head(tags$script(HTML("document.title = 'MONET';"))),
    tags$head(tags$script( src="https://cdn.rawgit.com/arose/ngl/v2.0.0-dev.32/dist/ngl.js")), 
    tags$head(tags$style(type = "text/css", "
               canvas{height:100% !important; width:100% !important;background-color: rgba(255, 255, 255,0) !important}
               #networkDropdown .label { display : none;}
               #networkDropdown .menu { padding-inline-start:0px;}
               ")),
    tags$head(HTML('<link rel="stylesheet" type="text/css"
                                     href="style.css" />')), 
    useShinyjs(),
    useShinyalert(),
    extendShinyjs(text = jsCode,functions = "Visu3D"),
    tabItems(
      tabItem("import",
              h1("Import data"),
              helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do 
               eiusmod tempor incididunt ut labore et dolore magna aliqua. Elit 
               sed vulputate mi sit amet. Interdum posuere lorem ipsum dolor sit 
               amet consectetur. Massa eget egestas purus viverra accumsan in. 
               Dolor sit amet consectetur adipiscing elit ut. Fames ac turpis 
               egestas integer. Hac habitasse platea dictumst quisque sagittis 
               purus sit amet. Integer malesuada nunc vel risus commodo viverra 
               maecenas accumsan. Tempus egestas sed sed risus pretium quam. 
               Nibh tortor id aliquet lectus proin nibh nisl condimentum. Mauris 
               commodo quis imperdiet massa tincidunt nunc pulvinar sapien et. 
               Sollicitudin aliquam ultrices sagittis orci a. Platea dictumst 
               quisque sagittis purus sit amet. Nulla at volutpat diam ut venenatis 
               tellus in metus. Amet porttitor eget dolor morbi non arcu risus.", style = "text-align: justify;"),
              h4(class = "infoGene","1- Select an import method"),
              
              pickerInput(
                inputId = "dataInputType", 
                width = "500px",
                label = NULL, 
                choices = c("From a list (carriage return)" = "list", 
                            "From a file" = "file"), 
                selected = NULL, multiple = FALSE, 
                choicesOpt = list(
                  icon = c("glyphicon-list", 
                           "glyphicon-file")
                  # glyphicon-align-justify
                )),
              
              textAreaInput("protList", NULL, placeholder = "FTR1\nFET3\n...",
                            width = "500px", resize = "vertical", height = "200px"),
              
              div(id= "importFileDiv",
                  fluidRow(
                    column(3,
                           h4("Parameters"),
                           fileInput("file",label = NULL,
                                     buttonLabel = "Browse...",
                                     placeholder = "No file selected"),align = "center",
                           # Input: Checkbox if file has header
                           radioButtons("header", "Header",
                                        choices = c("Yes" = TRUE,
                                                    "No" = FALSE),
                                        selected = TRUE, inline=T),
                           
                           # Input: Select separator ----
                           radioButtons("sep", "Separator",
                                        choices = c(Comma = ",",
                                                    Semicolon = ";",
                                                    Tab = "\t"),
                                        selected = "\t", inline=T),
                           
                           # Input: Select quotes ----
                           radioButtons("quote", "Quote",
                                        choices = c(None = "",
                                                    "Double Quote" = '"',
                                                    "Single Quote" = "'"),
                                        selected = "", inline=T)
                    ), 
                    column(9, 
                           h4("File preview"),
                           dataTableOutput(outputId = "contents"))
                  ),
                  selectizeInput("protColumn", "Select the protein column", choices = NULL, 
                                 selected = NULL, multiple = FALSE),
                  
                  selectizeInput("colCoExpression", "Select columns to calculate co-expression", choices = NULL, 
                                 selected = NULL, multiple = T),
                  numericInput(inputId = "topSelected", label = "Percent distance selected (if co-expression is calculated)", min = 1, max = 100,
                               value = 10, width = "500px"),
                  selectizeInput("distance", "Co-expression calculation", choices = c("classic", "correlation"),
                                 selected = "classic",  multiple =F)
              ),
              
              h4(class = "infoGene","2- Select a species"),
              selectizeInput("Species", NULL, width = "500px", choices = NULL, 
                             selected = NULL, multiple = FALSE,
                             options = list(
                               placeholder = 'Search a species',
                               maxOptions = 100,
                               onInitialize = I('function() { this.setValue(""); }')
                             )),
              h4(class = "infoGene", "3- Select a limits"),
              helpText("Limits the number of interaction partners retrieved per protein (most confident interactions come first)"),
              numericInput(inputId = "limitsNodes", label = NULL, min = 0,
                           value = 5, width = "500px"),
              
              actionButton("Search", "Search", icon = icon("upload"))
              
              
      ),tabItem("overview",
                h1("Overview"),
                fluidRow(valueBoxOutput("sizeQuery", width = 3),
                         valueBoxOutput("unmatchedProtein", width = 3),
                         valueBoxOutput("nbNodesFinal", width = 3),
                         valueBoxOutput("connection", width = 3)),
                div(id="DistriDiv",
                    fluidRow(box(width =6,title = 'Distance distribution', solidHeader =T,status = "primary", height = 507,
                                 plotlyOutput("TopSelectedhist")
                    ),
                    box(width = 6,title = 'Value distributions', solidHeader =T, status = "primary", 
                        plotlyOutput("HistoValues"),
                        tags$style("#divSelect .selectize-control {margin-bottom: 0px !important;}
                                #divSelect .form-group {margin-bottom: 3px !important;margin-top: 3px !important;}"), 
                        div(id = "divSelect", selectizeInput(inputId = "SelectHisto", NULL, width = "100%", choices = NULL, 
                                                             selected = NULL, multiple = FALSE) )
                    )
                    )
                    
                ),
                plotOutput("igraph")
                
      ),
      tabItem("graph",
              fluidRow(
                column(6,div(id = "networkDiv", 
                             visNetworkOutput("network", height = "100%"))),
                column(6,div(id = "networkSidebar",
                             h3("Node information"), 
                             helpText("Click on node to have information"),
                             div(id= "geneZone",
                                 HTML('<p class="infoGene">General information</p>'),
                                 htmlOutput("selected_var_gene"), 
                                 htmlOutput("selected_var_description"),
                                 htmlOutput("selected_var_species"),
                                 tags$br(),
                                 HTML('<p class="infoGene">Link external databases</p>'),
                                 htmlOutput("selected_var_refseq", style="display: inline-block;"),
                                 htmlOutput("selected_var_KEGG", style="display: inline-block; padding-left: 12px; padding-right: 12px;"),
                                 htmlOutput("selected_var_uniprot", style="display: inline-block; padding-left: 12px; padding-right: 12px;"),
                                 htmlOutput("selected_var_GeneCard", style="display: inline-block; padding-left: 12px; padding-right: 12px;"),
                                 htmlOutput("selected_var_ensembl", style="display: inline-block; padding-left: 12px; padding-right: 12px;"),
                                 htmlOutput("selected_var_nextprot", style="display: inline-block; padding-left: 12px; padding-right: 12px;"),
                                 htmlOutput("selected_var_CGD", style="display: inline-block; padding-left: 12px; padding-right: 12px;"),
                                 htmlOutput("selected_var_SGD", style="display: inline-block; padding-left: 12px; padding-right: 12px;"),
                                 tags$br(),
                                 tags$br(),
                                 div(id= "PDBDIV", 
                                     htmlOutput("PDB_title"),
                                     selectizeInput("PDBSelector_ID", "PDB ID", choices = NULL, 
                                                    selected = NULL, multiple = FALSE),
                                     div(id="viewport",style="width:100%; height:400px;" ),
                                 ),
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
                                 DTOutput('FA_Pfam'),
                                 HTML('<p class="infoGene">Generate a report</p>'),
                                 helpText("All the information available on the page is gathered in an HTML page. This page is downloaded when you click on the button below."),
                                 downloadButton("reportBTN", "Generate", icon = icon("file-export"))
                             ))
                ))
      ),
      tabItem("enrichissement",
              helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do 
               eiusmod tempor incididunt ut labore et dolore magna aliqua. Elit 
               sed vulputate mi sit amet. Interdum posuere lorem ipsum dolor sit 
               amet consectetur. Massa eget egestas purus viverra accumsan in. 
               Dolor sit amet consectetur adipiscing elit ut. Fames ac turpis 
               egestas integer. Hac habitasse platea dictumst quisque sagittis 
               purus sit amet. Integer malesuada nunc vel risus commodo viverra 
               maecenas accumsan. Tempus egestas sed sed risus pretium quam. 
               Nibh tortor id aliquet lectus proin nibh nisl condimentum. Mauris 
               commodo quis imperdiet massa tincidunt nunc pulvinar sapien et. 
               Sollicitudin aliquam ultrices sagittis orci a. Platea dictumst 
               quisque sagittis purus sit amet. Nulla at volutpat diam ut venenatis 
               tellus in metus. Amet porttitor eget dolor morbi non arcu risus.", style = "text-align: justify;"),
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
              h1("Raw data"), 
              helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do 
               eiusmod tempor incididunt ut labore et dolore magna aliqua. Elit 
               sed vulputate mi sit amet. Interdum posuere lorem ipsum dolor sit 
               amet consectetur. Massa eget egestas purus viverra accumsan in. 
               Dolor sit amet consectetur adipiscing elit ut. Fames ac turpis 
               egestas integer. Hac habitasse platea dictumst quisque sagittis 
               purus sit amet. Integer malesuada nunc vel risus commodo viverra 
               maecenas accumsan. Tempus egestas sed sed risus pretium quam. 
               Nibh tortor id aliquet lectus proin nibh nisl condimentum. Mauris 
               commodo quis imperdiet massa tincidunt nunc pulvinar sapien et. 
               Sollicitudin aliquam ultrices sagittis orci a. Platea dictumst 
               quisque sagittis purus sit amet. Nulla at volutpat diam ut venenatis 
               tellus in metus. Amet porttitor eget dolor morbi non arcu risus.", style = "text-align: justify;"),
              h2("Proteins to genes"), 
              DTOutput('PI'),
              h2("Information for all nodes"), 
              DTOutput('DT_infoAll'),
              h2("Network table"),
              DTOutput('DT_network'),
              h2("Interaction"),
              DTOutput('DT_Interaction')
      ),
      
      tabItem("about",
              h1("Session Information"), 
              helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do 
               eiusmod tempor incididunt ut labore et dolore magna aliqua. Elit 
               sed vulputate mi sit amet. Interdum posuere lorem ipsum dolor sit 
               amet consectetur. Massa eget egestas purus viverra accumsan in. 
               Dolor sit amet consectetur adipiscing elit ut. Fames ac turpis 
               egestas integer. Hac habitasse platea dictumst quisque sagittis 
               purus sit amet. Integer malesuada nunc vel risus commodo viverra 
               maecenas accumsan. Tempus egestas sed sed risus pretium quam. 
               Nibh tortor id aliquet lectus proin nibh nisl condimentum. Mauris 
               commodo quis imperdiet massa tincidunt nunc pulvinar sapien et. 
               Sollicitudin aliquam ultrices sagittis orci a. Platea dictumst 
               quisque sagittis purus sit amet. Nulla at volutpat diam ut venenatis 
               tellus in metus. Amet porttitor eget dolor morbi non arcu risus.", style = "text-align: justify;"),
              h2("R session information and parameters"),
              p("The versions of the R software and Bioconductor packages used for this analysis are listed below. 
              It is important to save them if one wants to re-perform the analysis in the same conditions."),
              uiOutput("sessionText"),
              h2("Database and API version"),
              h4("STRING"), 
              p(HTML("<b>Version </b>: 11.0")),
              p(HTML("<b>Documentation </b>: <a href='http://version11.string-db.org/help/api/'> http://version11.string-db.org/help/api/</a>")),
              h4("Uniprot"), 
              p(HTML("<b>Last modified page </b>: June 28, 2019")),
              p(HTML("<b>Documentation </b>: <a href='https://www.uniprot.org/help/api_idmapping'> https://www.uniprot.org/help/api_idmapping</a>"))
      ),
      
      tabItem("ref",
              h1("References"), 
              helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do 
               eiusmod tempor incididunt ut labore et dolore magna aliqua. Elit 
               sed vulputate mi sit amet. Interdum posuere lorem ipsum dolor sit 
               amet consectetur. Massa eget egestas purus viverra accumsan in. 
               Dolor sit amet consectetur adipiscing elit ut. Fames ac turpis 
               egestas integer. Hac habitasse platea dictumst quisque sagittis 
               purus sit amet. Integer malesuada nunc vel risus commodo viverra 
               maecenas accumsan. Tempus egestas sed sed risus pretium quam. 
               Nibh tortor id aliquet lectus proin nibh nisl condimentum. Mauris 
               commodo quis imperdiet massa tincidunt nunc pulvinar sapien et. 
               Sollicitudin aliquam ultrices sagittis orci a. Platea dictumst 
               quisque sagittis purus sit amet. Nulla at volutpat diam ut venenatis 
               tellus in metus. Amet porttitor eget dolor morbi non arcu risus.", style = "text-align: justify;"),
              h3("STRING"), 
              p(HTML('Szklarczyk D, Gable AL, Lyon D, Junge A, Wyder S, Huerta-Cepas J, Simonovic M, Doncheva NT, Morris JH, Bork P, Jensen LJ, von Mering C.',
                     '<br>',
                     '<b>STRING v11: protein-protein association networks with increased coverage, supporting functional discovery in genome-wide experimental datasets.</b>', 
                     '<br>',
                     'Nucleic Acids Res. 2019 Jan; 47:D607-613.',
                     '<br>',
                     '<a href="https://www-ncbi-nlm-nih-gov.insb.bib.cnrs.fr/pmc/articles/PMC6323986/" target="_blank">PubMed</a>')),
              
              h3("NGL"), 
              p(HTML('S Rose, AR Bradley, Y Valasatava, JM Duarte, A Prlić and PW Rose.',
                     '<br>',
                     '<b>A NGL viewer: web-based molecular graphics for large complexes.</b>', 
                     '<br>',
                     'Bioinformatics: bty419, 2018.',
                     '<br>',
                     '<a href="http://dx.doi.org/10.1093/bioinformatics/bty419" target="_blank">doi:10.1093/bioinformatics/bty419</a>')),
              
              p(HTML('AS Rose and PW Hildebrand.',
                     '<br>',
                     '<b>NGL Viewer: a web application for molecular visualization.</b>', 
                     '<br>',
                     'Nucl Acids Res (1 July 2015) 43 (W1): W576-W579 first published online April 29, 2015.',
                     '<br>',
                     '<a href="https://doi.org/10.1093/nar/gkv402" target="_blank">doi:10.1093/nar/gkv402</a>'))
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
    download.file("https://stringdb-static.org/download/species.v11.0.txt", 
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
  rvEnvent = reactiveValues()
  rvEnvent$search = F
  
  #=============================================================================
  # Hide zone 
  #=============================================================================
  shinyjs::hide(id = "geneZone")
  shinyjs::hide(id = "PDBDIV")
  
  #=============================================================================
  # Menu
  #=============================================================================
  
  observeEvent(rvEnvent$search, {
    if(rvEnvent$search){
      output$sidebar <- renderUI({
        sidebarMenu( id = "tabs",
                     menuItem("Import data", tabName = "import", icon = icon("file-import")),
                     menuItem("Overview", tabName = "overview", icon = icon("file-import")),
                     menuItem("Graph", tabName = "graph", icon = icon("project-diagram") ),
                     menuItem("Enrichissement", tabName = "enrichissement", icon = icon("search")),
                     menuItem("Raw data", tabName = "rawdata", icon = icon("file")),
                     menuItem("About", tabName = "about", icon = icon("cubes")),
                     menuItem("References", tabName = "ref", icon = icon("book"))
        )
      })
      
    } else {
      output$sidebar <- renderUI({
        sidebarMenu(id = "tabs",
                    menuItem("Import data", tabName = "import", icon = icon("file-import")),
                    menuItem("About", tabName = "about", icon = icon("cubes")),
                    menuItem("References", tabName = "ref", icon = icon("book"))
        )
      })
      updateTabItems(session, "tabs", selected = "import")
      shinyjs::runjs("window.scrollTo(0, 0)")
    }
    
    updateTextAreaInput(session, "protList", value = "")
    updateSelectInput(session, "Species", selected = "")
    
  })
  
  observeEvent(input$dataInputType, {
    if(input$dataInputType == "list"){
      shinyjs::hide(id = "importFileDiv")
      shinyjs::show(id = "protList")
      updateNumericInput(session, "limitsNodes", value = 5)
      
    } else if (input$dataInputType == "file") {
      shinyjs::show(id = "importFileDiv")
      shinyjs::hide(id = "colCoExpression")
      shinyjs::hide(id = "contents")
      shinyjs::hide(id = "protColumn")
      shinyjs::hide(id = "protList")
      reset("file")
      updateNumericInput(session, "limitsNodes", value = 1)
    }
  })
  
  output$contents <-  renderDataTable({
    
    req(input$file)
    
    STRING$df <- read.csv(input$file$datapath,
                          header = as.logical(input$header),
                          sep = input$sep,
                          quote = input$quote,
                          nrows=5, stringsAsFactors = F
    )
    
    STRING$df %>% dplyr::mutate_if(is.character, function(x){
      inter = substr(x,start = 1, stop = 20)
      inter[nchar(x)>20] = paste0(inter[nchar(x)>20], "[...]")
      inter
    })
    
  }, selection = 'none', options = list(scrollX = TRUE))
  
  observeEvent(input$file,{
    shinyjs::show(id = "colCoExpression")
    shinyjs::show(id = "contents")
    shinyjs::show(id = "protColumn")
    
    updateSelectizeInput(session, "colCoExpression", 
                         selected = "" )
  })
  
  observeEvent(STRING$df, {
    
    updateSelectizeInput(session, "protColumn", 
                         choices =  setNames(colnames(STRING$df) , colnames(STRING$df)),
                         selected = colnames(STRING$df)[1])
    
    updateSelectizeInput(session, "colCoExpression", 
                         choices =  setNames(colnames(STRING$df) , colnames(STRING$df)))
    
  })
  
  observeEvent(input$protColumn, {
    inter = setNames(colnames(STRING$df) , colnames(STRING$df))
    inter = inter[- which(inter == input$protColumn)]
    updateSelectizeInput(session, "colCoExpression", 
                         choices = inter )
  })
  
  #=============================================================================
  # About
  #=============================================================================
  
  output$sessionText = renderUI({
    HTML(paste(si[[1]]$version.string,",", si[[1]]$platform, "<br>","<br>",
               "<b>Locale</b><br>", paste(si[[3]], collapse = " , "), "<br>","<br>",
               "<b>Attached base packages</b><br>", paste(si[[5]], collapse = " , "),"<br>","<br>",
               "<b>Other attached packages</b><br>", paste(unlist(lapply(si$otherPkgs, function(x){paste(x$Package, x$Version)})), collapse = " , "), "<br>","<br>",
               "<b>Loaded via a namespace (and not attached)</b><br>" ,paste(unlist(lapply(si$loadedOnly, function(x){paste(x$Package, x$Version)})), collapse = " , ") 
    ))
  })
  
  
  #=============================================================================
  # Overview
  #=============================================================================
  output$TopSelectedhist <- renderPlotly({
    # hist of distances
    if(!is.null(input$SelectHisto) && input$SelectHisto != ""){
      threshold = STRING$seuil
      p<- ggplot(melt(STRING$triangMatrix), aes(x=value)) +
        geom_histogram(bins = 30, colour="#FFFFFF00", fill="#0073b7") +
        geom_vline(aes(xintercept=threshold),
                   color="red", linetype="dashed", size=1) + 
        theme(
          panel.background = element_rect(fill = "#FFFFFF00", colour = "black",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"),
          plot.background = element_rect(fill = "#FFFFFF00")
        ) + xlab("Distance values") + ylab("")
      
      ggplotly(p)
      
    } else {
      NULL
    }
    
  })
  
  output$HistoValues <- renderPlotly({
    if(!is.null(input$SelectHisto) && input$SelectHisto != ""){
      p<- ggplot(melt(as.numeric(STRING$importFile[, input$SelectHisto])), aes(x=value)) +
        geom_histogram(bins = 30 , colour="#FFFFFF00", fill="#0073b7") +
        theme(
          panel.background = element_rect(fill = "#FFFFFF00", colour = "black",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"),
          plot.background = element_rect(fill = "#FFFFFF00")
        ) + xlab("Values") + ylab("")
      
      ggplotly(p)
      
    } else {
      NULL
    }
    
  })
  
  output$igraph <- renderPlot({
    g = graph_from_data_frame(STRING$lien, directed = FALSE)
    V(g)$size = 0.5
    l<- layout_nicely(g)
    
    plot(g,vertex.label=NA, main = "Test", 
         layout = l, cex.main= 1.5, cex.lab=1.5, cex.axis=1.5)
  })
  
  
  
  
  #=============================================================================
  # Import
  #=============================================================================
  
  observeEvent(input$Search, {
    
    if(input$Species== ""){
      shinyalert("Oops!", "You must select a species !", type = "error")
    } else {
      m = 8
      rvEnvent$clean = F
      STRING$lien = NULL
      withProgress(message = 'Extraction in progress', value = 0, {
        
        incProgress(1/m, detail = "Initilization")
        if(input$dataInputType == "list"){
          STRING$initProt = unlist(strsplit(x =input$protList,split = '[ \r\n]' ) )
          shinyjs::hide(id = "DistriDiv")
        } else if(input$dataInputType == "file"){
          shinyjs::show(id = "DistriDiv")
          STRING$importFile <- read.csv2(input$file$datapath,
                                         header = as.logical(input$header),
                                         sep = input$sep,
                                         quote = input$quote, 
                                         stringsAsFactors = F
          )
          
          
          STRING$initProt = STRING$importFile[, input$protColumn]
          
          if(! is.null(input$colCoExpression) && length(input$colCoExpression) != 0){
            
            updateSelectizeInput(session, "SelectHisto", 
                                 choices = input$colCoExpression, 
                                 selected = input$colCoExpression[1])
            
            STRING$colCoExpression = input$colCoExpression
            data = STRING$importFile
            rownames(data) = data[, input$protColumn]
            data = data[,  input$colCoExpression]
            
            if(input$distance == "classic"){
              d = dist(data[,input$colCoExpression])
            } else if(input$distance == "correlation") {
              d = as.dist(1 - cor(t(data.matrix(data[,input$colCoExpression]))))
            }
            
            d = as.data.frame(as.matrix(d))
            
            # Get triangular matrix
            triangMatrix = d[lower.tri(d, diag=FALSE)]
            triangMatrix = sort(triangMatrix)
            STRING$triangMatrix = triangMatrix
            
            #-------------------------------------------------------------------------------
            # Definition of a threshold
            #-------------------------------------------------------------------------------
            
            # Find top threshold
            TopThreshold = round(input$topSelected * length(triangMatrix) / 100)
            seuil = triangMatrix[TopThreshold]
            STRING$seuil = seuil
            
            #-------------------------------------------------------------------------------
            # Search for link between genes (if distance is lower than threshold)
            #-------------------------------------------------------------------------------
            
            lien = cbind(from = rownames(d)[which(d < seuil, arr.ind=TRUE)[,1]], 
                         to = colnames(d)[which(d < seuil, arr.ind=TRUE)[,2]])
            
            lien = t(apply(lien, 1, function(x){
              sort(x)
            }))
            
            lien = as.data.frame(lien) %>%
              distinct()
            
            lien = lien[-which(lien[,1] == lien[,2]), ]
            colnames(lien) = c("from", "to")
            
            STRING$lien = lien
            data = STRING$importFile
            
          }
          updatePickerInput(session, "dataInputType", selected = "list")
        }
        
        if(length(STRING$initProt) > 500){
          
          shinyalert("Oops!", "The graph cannot contain more than 500 nodes. Reduce the list to be searched", type = "error")
          rvEnvent$search = F
          
        } else {
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
          request_url = paste0(request_url, parametersInfo,"&limit=1", collapse = "")
          STRING$dataInfo = read.csv2(request_url, sep ="\t", header = T, stringsAsFactors = F)
          STRING$dataInfoAll = STRING$dataInfo 
          STRING$associationProtGenes[(STRING$dataInfo$queryIndex + 1) ] = STRING$dataInfo$preferredName
          STRING$listUnknow = names(STRING$associationProtGenes)[is.na(STRING$associationProtGenes)]
          STRING$associationProtGenes[is.na(STRING$associationProtGenes)] = names(STRING$associationProtGenes)[is.na(STRING$associationProtGenes)]
          
          #-------------------------------------------------------------------------------
          # Nouvelles connexions
          #-------------------------------------------------------------------------------
          incProgress(1/m, detail = "Add new proteins")
          method = "interaction_partners"
          if(input$limitsNodes == 0) {
            STRING$dataInteraction = NULL
          } else {
            parametersGraph = paste0(parameters, "&limit=",input$limitsNodes)
            
            request_url = paste0(string_api_url, "/", output_format ,"/", method, "?", collapse = "")
            request_url = paste0(request_url, parametersGraph, collapse = "")
            STRING$dataInteraction = read.csv2(request_url, sep ="\t", header = T, 
                                               stringsAsFactors = F)
          }

          if(input$limitsNodes != 0 && nrow(STRING$dataInteraction) > 500){
            
            shinyalert("Oops!", paste0("The graph cannot contain more than 500 nodes. Reduce limits or reduce the list to be searched (actually size = ",nrow(STRING$dataInteraction),")"), type = "error")
            rvEnvent$search = F
            
          } else {
            if(input$limitsNodes != 0){
              #-------------------------------------------------------------------------------
              # Connexions entre les nouvelles 
              #-------------------------------------------------------------------------------
              incProgress(1/m, detail = "Generate new interactions")
              listProtInteraction = unique(c(STRING$dataInteraction[,"preferredName_A"], 
                                             STRING$dataInteraction[,"preferredName_B"]))
              method = "network"
              parametersNetworks = paste0( "identifiers=", paste0(listProtInteraction,collapse = "%0d"), 
                                           "&species=",STRING$ourSpecies, "&add_nodes=0",
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
              # Category enrichissement
              #---------------------------------------------------------------------------
              incProgress(1/m, detail = "Category enrichissement")
              
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
              
              if(nrow(STRING$dataInfoAll) != 0 ){
                nodes  = STRING$dataInfoAll[,c("preferredName","annotation")] %>% distinct() %>%
                  mutate(annotation = preferredName,
                         type = case_when(preferredName %in% STRING$associationProtGenes ~ "square",
                                          T ~ "dot"),
                         color = "orange")
              } else {
                nodes  = STRING$dataInfoAll[,c("preferredName","annotation")] %>% distinct() %>%
                  mutate(annotation = preferredName,
                         type = case_when(preferredName %in% STRING$associationProtGenes ~ "square",
                                          T ~ "dot")) %>%
                  mutate(color = "orange")
              }
              
              colnames(nodes) = c("id", "label", "shape", "color")
              
              if(length(STRING$listUnknow) != 0){
                nodes  = rbind(nodes, cbind("id" = STRING$listUnknow, 
                                            "label"= STRING$listUnknow, 
                                            "shape" = "square", 
                                            "color" = "gray")) %>% distinct()
              }
              
              addNodesNew = STRING$associationProtGenes[! STRING$associationProtGenes %in% nodes$id]
              if(length(addNodesNew) != 0){
                nodes  = rbind(nodes, cbind("id" = addNodesNew, 
                                            "label"= addNodesNew, 
                                            "shape" = "square", 
                                            "color" = "gray")) %>% distinct()
              }
              
              STRING$nodes = nodes 
              
              links = data.frame(STRING$dataNetwork[, c("preferredName_A", "preferredName_B")])
              
              links = t(apply(links, 1, function(x){
                sort(x)
              }))
              
              links = data.frame(links, stringsAsFactors=FALSE)
              
              links = as.data.frame(links, stringsAsFactors = F) %>%
                distinct()
              links = cbind(links, "blue")
              colnames(links) = c("from", "to", "color")
              
              if(exists("lien") && nrow(lien) != 0){
                convertLink = NULL
                for(i in 1:nrow(lien)){
                  convertLink = rbind(convertLink,
                                      c(
                                        as.character(STRING$dataInfo$preferredName)[as.numeric(STRING$dataInfo$queryIndex) == (which(as.character(STRING$initProt) == as.character(lien[i,1]))-1)],
                                        as.character(STRING$dataInfo$preferredName)[as.numeric(STRING$dataInfo$queryIndex) == (which(as.character(STRING$initProt) == as.character(lien[i,2]))-1)]
                                      )
                  )
                  
                }
                convertLink = cbind(convertLink, "green")
                colnames(convertLink) = c("from", "to", "color")
                links = rbind(links, convertLink)
              }
              
              links = as.data.frame(links, stringsAsFactors = F) %>%
                distinct()
              
              linksPaste = as.character(setNames(paste0(links[,1], links[,2]),
                                    paste0(links[,1], links[,2])))
              namesDupli = as.character(linksPaste[duplicated(linksPaste)])
              posDuppli = as.numeric(which(linksPaste %in% namesDupli))
              colorLinks = as.character(links[,3])
              colorLinks[posDuppli] = "black"
              links[,3] = as.character(colorLinks)

              links = as.data.frame(links, stringsAsFactors = F) %>%
                distinct()
              
              STRING$links = links

            } else {
              if(exists("lien") && nrow(lien) != 0){

                STRING$nodes =  cbind("id" = STRING$associationProtGenes[STRING$initProt], 
                                      "label"= STRING$initProt, 
                                      "shape" = "square", 
                                      "color" = "orange")
                lien = as.data.frame(lien, stringsAsFactors = F)
                STRING$links = as.data.frame(cbind(from = STRING$associationProtGenes[lien[, 1]], 
                                                   to = STRING$associationProtGenes[lien[, 2]]), stringsAsFactors = F)
                
              }
            }
            
            rvEnvent$search = T
            
            updateTabItems (session, "tabs", selected = "overview")
            sendSweetAlert(
              session = session,
              title = "Ready to explore !",
              text = "Data are imported !", 
              type = "success"
            )
            shinyjs::runjs("window.scrollTo(0, 0)")
          }
        }
      })
    }
    
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
        if(!is.null(STRING$links)){
          STRING$links
        } else {
          NULL
        }
        
        # STRING$dataInteraction, options = list(lengthChange = FALSE)
      )
      
      #=============================================================================
      # Création du graphe 
      #=============================================================================
      
      output$network <- renderVisNetwork({
        if(!is.null(STRING$nodes)){
          STRING$network = visNetwork(as.data.frame(STRING$nodes), as.data.frame(STRING$links)) %>%
            visExport() %>%
            visNodes(size = input$sizeNodes) %>%
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
      # Link external databases
      #=============================================================================
      output$selected_var_refseq <- renderUI({
        if(!is.null(STRING$refseq) && length(STRING$refseq[,2]) != 0){
          if(length(STRING$refseq[,2]) != 1){
            HTML(paste0('<div class="dropdown">
        <button class="btn btn-secondary dropdown-toggle" type="button" id="dropdownMenuButton" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
          <img src="img/refseq_logo_small.png" height="20px">
        </button>
        <div class="dropdown-menu" aria-labelledby="dropdownMenuButton">
        ',paste0('<a class="dropdown-item" href="https://www.ncbi.nlm.nih.gov/protein/',unique(STRING$refseq[,2]),'" target="_blank">',unique(STRING$refseq[,2]),'</a>', collapse="<br>"),'
        </div>
      </div>', collapse=""))
          }else {
            HTML(paste0('<a class="dropdown-item" href="https://www.genome.jp/dbget-bin/www_bget?cgr:',unique(STRING$refseq[,2]),'" target="_blank"><img src="img/refseq_logo_small.png" height="20px"></a>'))
          }
        } else {
          NULL
        }
        
      })
      
      output$selected_var_KEGG <- renderUI({
        if(!is.null(STRING$linkKEGG) && length(STRING$linkKEGG[,2]) != 0){
          if(length(STRING$linkKEGG[,2]) != 1){
            HTML(paste0('<div class="dropdown">
        <button class="btn btn-secondary dropdown-toggle" type="button" id="dropdownMenuButton" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
          <img src="img/kegg_logo_small.png" height="20px">
        </button>
        <div class="dropdown-menu" aria-labelledby="dropdownMenuButton">
        ',paste0('<a class="dropdown-item" href="https://www.genome.jp/dbget-bin/www_bget?',unique(STRING$linkKEGG[,2]),'" target="_blank">',unique(STRING$linkKEGG[,2]),'</a>', collapse="<br>"),'
        </div>
      </div>', collapse=""))
          }else {
            HTML(paste0('<a class="dropdown-item" href="https://www.genome.jp/dbget-bin/www_bget?',unique(STRING$linkKEGG[,2]),'" target="_blank"><img src="img/kegg_logo_small.png" height="20px"></a>'))
          }
        } else {
          NULL
        }
        
      })
      
      output$selected_var_uniprot<- renderUI({
        
        if(!is.null(STRING$UniprotID) && length(STRING$UniprotID) != 0){
          if(length(STRING$UniprotID) != 1){
            HTML(paste0('<div class="dropdown">
        <button class="btn btn-secondary dropdown-toggle" type="button" id="dropdownMenuButton" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
          <img src="img/uniprot_logo_small.png" height="20px">
        </button>
        <div class="dropdown-menu" aria-labelledby="dropdownMenuButton">
        ',paste0('<a class="dropdown-item" href="https://www.uniprot.org/uniprot/',unique(STRING$UniprotID),'" target="_blank">',unique(STRING$UniprotID),'</a>', collapse="<br>"),'
        </div>
      </div>', collapse=""))
          }else {
            HTML(paste0('<a class="dropdown-item" href="https://www.uniprot.org/uniprot/',unique(STRING$UniprotID),'" target="_blank"><img src="img/uniprot_logo_small.png" height="20px"></a>'))
          }
        } else {
          NULL
        }
        
      })
      
      output$selected_var_GeneCard<- renderUI({
        
        if(!is.null(STRING$linkGeneCard) && length(STRING$linkGeneCard[,2]) != 0){
          if(length(STRING$linkGeneCard[,2]) != 1){
            HTML(paste0('<div class="dropdown">
        <button class="btn btn-secondary dropdown-toggle" type="button" id="dropdownMenuButton" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
          <img src="img/genecards_logo_small.png" height="20px">
        </button>
        <div class="dropdown-menu" aria-labelledby="dropdownMenuButton">
        ',paste0('<a class="dropdown-item" href="https://www.genecards.org/cgi-bin/carddisp.pl?gene=',unique(STRING$linkGeneCard[,2]),'" target="_blank">',unique(STRING$linkGeneCard[,2]),'</a>', collapse="<br>"),'
        </div>
      </div>', collapse=""))
          }else {
            HTML(paste0('<a class="dropdown-item" href="https://www.genecards.org/cgi-bin/carddisp.pl?gene=',unique(STRING$linkGeneCard[,2]),'" target="_blank"><img src="img/genecards_logo_small.png" height="20px"></a>'))
          }
        } else {
          shinyjs::hide(id = "selected_var_GeneCard")
          NULL
          
        }
      })
      
      output$selected_var_ensembl<- renderUI({
        
        if(!is.null(STRING$linkensembl) && length(STRING$linkensembl[,2]) != 0){
          if(length(STRING$linkensembl[,2]) != 1){
            HTML(paste0('<div class="dropdown">
        <button class="btn btn-secondary dropdown-toggle" type="button" id="dropdownMenuButton" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
          <img src="img/ensembl_logo_small.png" height="20px">
        </button>
        <div class="dropdown-menu" aria-labelledby="dropdownMenuButton">
        ',paste0('<a class="dropdown-item" href="http://oct2014.archive.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=',unique(STRING$linkensembl[,2]),'" target="_blank">',unique(STRING$linkensembl[,2]),'</a>', collapse="<br>"),'
        </div>
      </div>', collapse=""))
          }else {
            HTML(paste0('<a class="dropdown-item" href="http://oct2014.archive.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=',unique(STRING$linkensembl[,2]),'" target="_blank"><img src="img/ensembl_logo_small.png" height="20px"></a>'))
          }
        } else {
          shinyjs::hide(id = "selected_var_ensembl")
          NULL
        }
      })
      
      output$selected_var_nextprot<- renderUI({
        if(!is.null(STRING$linknextprot) && length(STRING$linknextprot[,2]) != 0){
          if(length(STRING$linknextprot[,2]) != 1){
            HTML(paste0('<div class="dropdown">
        <button class="btn btn-secondary dropdown-toggle" type="button" id="dropdownMenuButton" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
          <img src="img/nextprot_logo_small.png" height="20px">
        </button>
        <div class="dropdown-menu" aria-labelledby="dropdownMenuButton">
        ',paste0('<a class="dropdown-item" href="https://www.nextprot.org/entry/',unique(STRING$linknextprot[,2]),'/" target="_blank">',unique(STRING$linknextprot[,2]),'</a>', collapse="<br>"),'
        </div>
      </div>', collapse=""))
          }else {
            HTML(paste0('<a class="dropdown-item" href="https://www.nextprot.org/entry/NX_P06493/',unique(STRING$linknextprot[,2]),'/" target="_blank"><img src="img/nextprot_logo_small.png" height="20px"></a>'))
          }
        } else {
          shinyjs::hide(id = "selected_var_nextprot")
          NULL
        }
      })
      
      output$selected_var_CGD<- renderUI({
        if(!is.null(STRING$linkCGD) && length(STRING$linkCGD[,2]) != 0){
          if(length(STRING$linkCGD[,2]) != 1){
            HTML(paste0('<div class="dropdown">
        <button class="btn btn-secondary dropdown-toggle" type="button" id="dropdownMenuButton" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
          <img src="img/CGD_logo_small.png" height="20px">
        </button>
        <div class="dropdown-menu" aria-labelledby="dropdownMenuButton">
        ',paste0('<a class="dropdown-item" href="http://www.candidagenome.org/cgi-bin/locus.pl?dbid=',unique(STRING$linkCGD[,2]),'" target="_blank">',unique(STRING$linkCGD[,2]),'</a>', collapse="<br>"),'
        </div>
      </div>', collapse=""))
          }else {
            HTML(paste0('<a class="dropdown-item" href="http://www.candidagenome.org/cgi-bin/locus.pl?dbid=',unique(STRING$linkCGD[,2]),'" target="_blank"><img src="img/CGD_logo_small.png" height="20px"></a>'))
          }
        } else {
          shinyjs::hide(id = "selected_var_CGD")
          NULL
        }
      })
      
      output$selected_var_SGD<- renderUI({
        
        if(!is.null(STRING$linkSGD) && length(STRING$linkSGD[,2]) != 0){
          if(length(STRING$linkSGD[,2]) != 1){
            HTML(paste0('<div class="dropdown">
        <button class="btn btn-secondary dropdown-toggle" type="button" id="dropdownMenuButton" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
          <img src="img/SGD_logo_small.png" height="20px">
        </button>
        <div class="dropdown-menu" aria-labelledby="dropdownMenuButton">
        ',paste0('<a class="dropdown-item" href="https://www.yeastgenome.org/locus/',unique(STRING$linkSGD[,2]),'" target="_blank">',unique(STRING$linkSGD[,2]),'</a>', collapse="<br>"),'
        </div>
      </div>', collapse=""))
          }else {
            HTML(paste0('<a class="dropdown-item" href="https://www.yeastgenome.org/locus/',unique(STRING$linkSGD[,2]),'" target="_blank"><img src="img/SGD_logo_small.png" height="20px"></a>'))
          }
        } else {
          shinyjs::hide(id = "selected_var_SGD")
          NULL
        }
      })
      
      
      #=============================================================================
      # Gene selected Information
      #=============================================================================
      
      # Get information
      observeEvent(input$network_selected, {
        #---------------------------------------------------------------------------
        # Clean step 
        #---------------------------------------------------------------------------
        
        STRING$refseq = NULL 
        STRING$linkKEGG = NULL 
        STRING$UniprotID = NULL 
        STRING$linkGeneCard = NULL 
        STRING$linkensembl = NULL 
        STRING$linknextprot = NULL
        STRING$linkCGD = NULL
        STRING$linkSGD = NULL
        
        #---------------------------------------------------------------------------
        # Show step 
        #---------------------------------------------------------------------------
        shinyjs::show(id = "FA_InterPro_title") 
        shinyjs::show(id = "FA_InterPro") 
        shinyjs::show(id = "FA_Keyword_title") 
        shinyjs::show(id = "FA_Keyword")
        shinyjs::show(id = "FA_RCTM_title")
        shinyjs::show(id = "FA_RCTM") 
        shinyjs::show(id = "FA_PMID_title") 
        shinyjs::show(id = "FA_PMID")       
        shinyjs::show(id = "FA_KEGG_title") 
        shinyjs::show(id = "FA_KEGG") 
        shinyjs::show(id = "FA_component_title") 
        shinyjs::show(id = "FA_component") 
        shinyjs::show(id = "FA_Function_title") 
        shinyjs::show(id = "FA_Function")       
        shinyjs::show(id = "FA_Process_title")
        shinyjs::show(id = "FA_Process") 
        shinyjs::show(id = "FA_Pfam_title") 
        shinyjs::show(id = "FA_Pfam") 
        shinyjs::show(id = "FA_SMART_title") 
        shinyjs::show(id = "FA_SMART")
        
        #---------------------------------------------------------------------------
        # clean step 
        #---------------------------------------------------------------------------
        
        if(input$network_selected != ""){
          STRING$date = format(Sys.time(), "%Y_%m_%d__%H_%M_%S")
          m = 4
          withProgress(message = 'Extraction in progress', value = 0, {
            
            incProgress(1/m, detail = "Functionnal annotation")
            method = "functional_annotation"
            parametersEnrichment = paste0( "identifiers=", input$network_selected, 
                                           "&species=",STRING$ourSpecies,
                                           collapse = "")
            
            request_url = paste0(string_api_url, "/", output_format ,"/", method, "?", collapse = "")
            request_url = paste0(request_url, parametersEnrichment, collapse = "")
            STRING$annotation = read.csv2(request_url, sep ="\t", header = T)
            
            incProgress(1/m, detail = "Print zone")
            if(!is.null(STRING$annotation) && nrow(STRING$annotation) != 0){
              shinyjs::show(id = "geneZone")
            } else {
              shinyjs::hide(id = "geneZone")
            }
            
            incProgress(1/m, detail = "Get Uniprot ID")
            
            STRING$ID = unique(STRING$dataInfoAll$stringId[STRING$dataInfoAll$preferredName == input$network_selected])
            STRING$UniprotID = as.matrix(idMappingUniprot("STRING_ID", "ID", STRING$ID, "tab"))
            STRING$UniprotID = as.character(STRING$UniprotID[1,2])
            
            incProgress(1/m, detail = "Get structure files")
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
              
              STRING$linkKEGG = as.matrix(idMappingUniprot("ID", "KEGG_ID", unique(STRING$UniprotID), "tab"))
              STRING$refseq = as.matrix(idMappingUniprot("ID", "P_REFSEQ_AC", unique(STRING$UniprotID), "tab"))
              
              speciesInter = unique(STRING$dataInfoAll$taxonName[STRING$dataInfoAll$preferredName == input$network_selected]) 
              
              if(speciesInter == "Homo sapiens"){
                STRING$linknextprot = as.matrix(idMappingUniprot("ID", "GENECARDS_ID", unique(STRING$UniprotID), "tab"))
                STRING$linkensembl = as.matrix(idMappingUniprot("ID", "GENECARDS_ID", unique(STRING$UniprotID), "tab"))
                STRING$linkGeneCard = as.matrix(idMappingUniprot("ID", "GENECARDS_ID", unique(STRING$UniprotID), "tab"))
                shinyjs::show(id = "selected_var_nextprot")  
                shinyjs::show(id = "selected_var_ensembl")
                shinyjs::show(id = "selected_var_GeneCard") 
              } else if (grepl('Candida ', speciesInter)){
                STRING$linkCGD = as.matrix(idMappingUniprot("ID", "CGD", unique(STRING$UniprotID), "tab"))
                shinyjs::show(id = "selected_var_CGD") 
              } else if (grepl('Saccharomyces cerevisiae', speciesInter)){
                STRING$linkSGD = as.matrix(idMappingUniprot("ID", "SGD_ID", unique(STRING$UniprotID), "tab"))
                shinyjs::show(id = "selected_var_SGD") 
              } else {
                STRING$linkGeneCard = NULL 
                STRING$linkensembl = NULL 
                STRING$linknextprot = NULL
                STRING$linkCGD = NULL
                STRING$linkSGD = NULL
              }
              
            } else {
              STRING$PDB = NULL
              updateSelectInput(session, "PDBSelector_ID",
                                choices = NULL)
            }
            
          })
          
        } else {
          STRING$annotation = NULL
          STRING$PDB = NULL
          shinyjs::hide(id = "geneZone")
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
          HTML('<p class="infoGene">PDB</p>') 
        } else {
          NULL
        }
      })
      
      # Go terms : Component
      output$FA_component_title <- renderText({ 
        if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "Component")) != 0){
          HTML('<p class="infoGene">Cellular component</p>') 
        } else {
          shinyjs::hide(id = "FA_component_title")
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
          shinyjs::hide(id = "FA_component")
          NULL
        }
      }, selection = 'none', escape = FALSE,
      options = list(pageLength = 5, scrollX = TRUE)
      )
      
      # Go terms : Function
      output$FA_Function_title <- renderText({ 
        if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "Function")) != 0){
          HTML('<p class="infoGene">Molecular Function</p>') 
        } else {
          shinyjs::hide(id = "FA_Function_title")
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
          shinyjs::hide(id = "FA_Function")
          NULL
        }
      }, selection = 'none', escape = FALSE,
      options = list(pageLength = 5, scrollX = TRUE)
      )
      
      # Go terms : Process
      output$FA_Process_title <- renderText({ 
        if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "Process")) != 0){
          HTML('<p class="infoGene">Biological process</p>') 
        } else {
          shinyjs::hide(id = "FA_Process_title")
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
          shinyjs::hide(id = "FA_Process")
          NULL
        }
      }, selection = 'none', escape = FALSE,
      options = list(pageLength = 5, scrollX = TRUE)
      ) 
      
      
      # Pfam
      output$FA_Pfam_title <- renderText({ 
        if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "Pfam")) != 0){
          HTML('<p class="infoGene">Pfam</p>')
        } else {
          shinyjs::hide(id = "FA_Pfam_title")
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
          shinyjs::hide(id = "FA_Pfam")
          NULL
        }
      }, selection = 'none', escape = FALSE,
      options = list(pageLength = 5, scrollX = TRUE)
      )
      
      # SMART
      output$FA_SMART_title <- renderText({ 
        if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "SMART")) != 0){
          HTML('<p class="infoGene">SMART</p>')
        } else {
          shinyjs::hide(id = "FA_SMART_title")
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
          shinyjs::hide(id = "FA_SMART")
          NULL
        }
      }, selection = 'none', escape = FALSE,
      options = list(pageLength = 5, scrollX = TRUE)
      )
      
      # Reactome
      output$FA_RCTM_title <- renderText({ 
        if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "RCTM")) != 0){
          HTML('<p class="infoGene">Reactome</p>')
        } else {
          shinyjs::hide(id = "FA_RCTM_title")
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
          shinyjs::hide(id = "FA_RCTM")
          NULL
        }
      }, selection = 'none', escape = FALSE,
      options = list(pageLength = 5, scrollX = TRUE)
      )
      
      # PMID
      output$FA_PMID_title <- renderText({ 
        if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "PMID")) != 0){
          HTML('<p class="infoGene">Pubmed</p>')
        } else {
          shinyjs::hide(id = "FA_PMID_title")
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
          shinyjs::hide(id = "FA_PMID")
          NULL
        }
      }, selection = 'none', escape = FALSE,
      options = list(pageLength = 5, scrollX = TRUE)
      )
      
      # KEGG
      output$FA_KEGG_title <- renderText({ 
        if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "KEGG")) != 0){
          HTML('<p class="infoGene">KEGG</p>')
        } else {
          shinyjs::hide(id = "FA_KEGG_title")
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
          shinyjs::hide(id = "FA_KEGG")
          NULL
        }
      }, selection = 'none', escape = FALSE,
      options = list(pageLength = 5, scrollX = TRUE)
      )
      
      # InterPro
      output$FA_InterPro_title <- renderText({ 
        if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "InterPro")) != 0){
          HTML('<p class="infoGene">InterPro</p>')
        } else {
          shinyjs::hide(id = "FA_InterPro_title")
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
          shinyjs::hide(id = "FA_InterPro")
          NULL
        }
      }, selection = 'none', escape = FALSE,
      options = list(pageLength = 5, scrollX = TRUE)
      )
      
      # Keyword
      output$FA_Keyword_title <- renderText({ 
        if(!is.null(STRING$annotation) && nrow(STRING$annotation %>% filter(category == "Keyword")) != 0){
          HTML('<p class="infoGene">UniProt</p>')
        } else {
          shinyjs::hide(id = "FA_Keyword_title")
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
          shinyjs::hide(id = "FA_Keyword")
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
      
      #-----------------------------------------------------------------------------
      # Gene report
      #-----------------------------------------------------------------------------
      
      output$reportBTN <- downloadHandler(
        filename = paste0("report_",STRING$date,"_",input$network_selected,".html"),
        content = function(file) {
          params <- list(si = si,
                         date = STRING$date,
                         annotation = STRING$annotation,
                         gene = input$network_selected,
                         description = unique(STRING$dataInfoAll$annotation[STRING$dataInfoAll$preferredName == input$network_selected]),
                         species = unique(STRING$dataInfoAll$taxonName[STRING$dataInfoAll$preferredName == input$network_selected])
          )
          rmarkdown::render("report.Rmd", output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
      )
      
}

shinyApp(ui, server)
