#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#if (!requireNamespace("BiocManager", quietly = TRUE))
  #install.packages("BiocManager")

library(BiocManager)

options(repos = BiocManager::repositories())

library(shiny)
library(shinyFeedback)
library(shinyjs)
library(shinythemes)
library(genomics)
library(Biostrings)


ui <- fluidPage(theme = shinytheme("united"),
      shinyjs::useShinyjs(),
      
      tabsetPanel(
        
        tabPanel(tags$strong("Main page"),
         # Application description
         h4(tags$b(div('This app translates open reading frames (ORF) of 
           randomly generated DNA sequences.')), style = 'color: black;',
                    align = 'center', style = 'background-color: white;'),
                 
         h6(tags$i(tags$p(tags$strong("Disclaimer: This is a beta version of the app. 
          It uses the R packages, genomics (Tiago Olivoto, 2021) and Biostrings 
                          (Pages et al., 2021)."), 
                                  align = 'center', style = 'color:red;'))),
                 
       tags$img(src = "AGRILOGO.jpg", height = 55, width = 50, 
                align = 'right'),
       
       tags$img(src = "KNUST_logo.jpg", height = 60, width = 50, 
                align = 'right'),
       
       h2(tags$strong("GenCoder"), style = 'color: green', align = 'center'),
       
       hr(),
       
       fluidRow(
         div(
           column(5,
                  
        numericInput("size_bp", "Enter length of DNA:", min = 1, 
                     max = 1000, value = 100),
        helpText(tags$strong("DNA seq length should not exceed 1 kb."), 
                 style = 'color: red;'),
        
        tags$head(
          tags$style(HTML('#Generate{background-color: green}'))),
        actionButton("Generate", tags$strong("Generate DNA seq")),
                  
        br(), br(),
        tags$head(tags$style("#FDNA{color: blue;
         font-size: 12px;
         font-style: bold;
         font-family: Verdana;
         word-wrap: break-word;
      }"
      
                  )),
        
      h5(tags$strong("Forward DNA seq:")),
      textOutput("FDNA"),
      
      br(),
      
      tags$head(tags$style("#RCDNA{color: blue;
         font-size: 12px;
         font-style: bold;
         font-family: Verdana;
         word-wrap: break-word;
      }"
      
      )),
      
      h5(tags$strong("Reverse complement seq:")),
      textOutput("RCDNA"),
      
      br(),
      
      tags$head(tags$style("#Frames{color: blue;
         font-size: 12px;
         font-style: bold;
         font-family: Verdana;
         word-wrap: break-word;
      }"
      
      )),
      
      h5(tags$strong("Reading frames:")),
      verbatimTextOutput("Frames")
          )
       ),
      
       div(id = "my_text",
           
           tags$style(type = "text/css", "#my_text{
         color: black;
         font-size: 14px;
         font-family: Verdana;
         text-align: center;
         
        "),
        
         column(2,
          div(checkboxInput("seed", tags$strong("CHECK TO SET SEED"), 
              value = FALSE), style = 'background-color: powderblue;'),
          
          br(),
          tags$head(
          tags$style(HTML('#RFrames{background-color: green}'))),
          actionButton("RFrames", tags$strong("View reading frames")),
                )
       ),
      
       div(
         column(5,
                
          tags$head(
           tags$style(HTML('#Protein{background-color: green}'))),
            actionButton("Protein", tags$strong("Translate reading frame(s)")),
          helpText(tags$strong("Translation speed depends on DNA seq length."), 
                   style = 'color: red'),
          
          br(),
          tags$head(
            tags$style(HTML('#Protein{background-color: green}'))),
          selectInput("type", tags$strong("Select type of aa output:"), 
              c("OneLetter" = "olc", "ThreeLetter" = "tlc", "Name" = "name")),
          helpText(tags$strong("Select type of amino acid (aa) output."), 
                   style = 'color: red'),
          
          br(),
          tags$head(tags$style("#amino_chain{color: blue;
         font-size: 12px;
         font-style: bold;
         font-family: Verdana;
         white-space: pre-line;
      }"
      
                )),
      
      h5(tags$strong("Translated reading frame:")),
      verbatimTextOutput("amino_chain")     
                )
       )
         
       ),
      
      br(), br(), br(), br(),
      hr(),
      
      tags$h6("Copyright ", HTML("&copy"),"2021 | Alexander Wireko Kena, PhD. | 
           Chris Antwi, PhD. | Stephen Amoah, PhD. | Richard Akromah, PhD.",  
           align = 'center', style = 'color: blue')
       
                 ),
        
        tabPanel(tags$strong("Standard Genetic Code"),
         fluidRow( 
           div(
           column(5,
          br(),          
          textInput("Codon", "Enter codon to see amino acid :", value = "AUG"),
          helpText("Enter any of the 64 valid codons in RNA nucleotides."),
          helpText("STOP codons will appear as asterisk."),
                  
            br(),     
             tags$head(
             tags$style(HTML('#Triplet{background-color: green}'))),
           actionButton("Triplet", tags$strong("Decode Codon"), icon("search")),
          
    br(), br(),
          tags$head(tags$style("#AAcid{color: blue;
    font-size: 15px;
    font-style: bold;
    font-family: Verdana;
    }"
          )),
    h5(tags$strong("Specified codon encodes:")),
    verbatimTextOutput("AAcid"),
            )
                  ),
          
          div(
            column(7,
                   
                   br(), br(),     
                 tags$head(
                  tags$style(HTML('#All{background-color: green}'))),
                 actionButton("All", tags$strong("View standard genetic code")),   
                   
                   tags$br(), tags$br(),
                   h5(tags$strong("The standard genetic code table:")),
                   tableOutput("SG_code")   
                   )
          )
                 
                )
                 ),
        
tabPanel(tags$strong("Help"),
 tags$strong(h3("Instructions to users:", style = 'color: black;')),
 
 hr(),

 div(id = 'alex1', 
     tags$head(tags$style(HTML('#alex1 {
       background: #ffe5e5;
       padding: 15px;
       margin-left: 10px;
       margin-right: 500px;
     }'))),
     
tags$ol(type = '1',
        
 tags$li("Enter the length of DNA to generate randomly. Enter integers ONLY."),
 
 tags$li("Click on the Generate DNA seq button to output DNA."),
 
 tags$li("To see reading frames, click on the View reading frames button."),
 
 tags$li("The Translate button will translate any open reading frame with both 
         START and STOP codons."),
 
 tags$li("To view the entire standard genetic code comprising all 64 
codons, click on the View Standard Genetic Code button."),
 
 tags$li("Otherwise, to see an amino acid encoded by any codon, specify 
the codon in RNA nucleotides, and click on the Decode Codon button.")
 
)
),
         
   br(),
   
   tags$strong(h4("Users should note the following:", style = 'color: black;')),
   
   hr(),
   
div(id = 'alex2', 
    tags$head(tags$style(HTML('#alex2 {
       background: #ffe5e5;
       padding: 15px;
       margin-left: 10px;
       margin-right: 500px;
     }'))),
   tags$ul(type = 'none',
           
tags$li("The app, by default, generates DNA seq RANDOMLY. Hence, for any inputed
DNA length, a different seq will be outputed whenever the Generate DNA 
button is clicked. However, if the CHECK TO SET SEED box is checked, it 
overrides the randomness, and outputs the SAME DNA seq for any inputed DNA length.")
   )
),
  
  br(), br(), br(), br(), br(), br(), br(), br(), br(),
  
  hr(),
  
  tags$h6("Copyright ", HTML("&copy"),"2021 | Alexander Wireko Kena, PhD. | 
   Chris Antwi, PhD. | Stephen Amoah, PhD. | Richard Akromah, PhD.",  
   align = 'center', style = 'color: blue')
  
         ),

      )
      
)
                           
     
# Server
server <- function(input, output){
  
  # Function to find reading frames of generated DNA
  read_frm <- function(x){
    
    seqq <- RNAString(gsub("T", "U", x))
    
    res <- vector(mode = "list", length = 3)
    
    
    rf_f1 <- unlist(strsplit(as.character(seqq), "(?<=.{3})", perl = TRUE))
    res[[1]] <- rf_f1
    
    rf_f2 <- unlist(strsplit(as.character(seqq[-1]), "(?<=.{3})", perl = TRUE))
    res[[2]] <- c(as.character(seqq[1]), rf_f2)
    
    rf_f3 <- unlist(strsplit(as.character(seqq[-c(1:2)]), "(?<=.{3})", 
                             perl = TRUE))
    res[[3]] <- c(as.character(seqq[1:2]), rf_f3)
    
    names(res) <- paste0("reading_frame_", 1:3)
    res <- lapply(res, noquote)
    return(res)
  }
  
  # Function to translate open reading frames
  # Input should be a list output of the read_frm() function
  
  find_orf <- function(x){
    
    mat_orf <- vector(mode = "list", length = length(x))
    
    names(mat_orf) <- names(x)
    
    for(nn in names(x)){
      
      rf <- x[[nn]]
      
      BB1 <- rf[nchar(rf) == 3]
      
      # Get index for first START codon
      st <- which(BB1 == "AUG")[1]
      
      # Set condition for translation (start codon present)
      
      if(!is.na(st)){
        
        aa <- genomics::translate(paste(BB1[st:length(BB1)], collapse = ""), 
                                  type = input$type, sep = " ", no_stop = TRUE)
        aa1 <- unlist(strsplit(aa, ""))
        
        if('*' %in% aa1){
          en <- grep("*", aa1, fixed = TRUE)[1]
          mat_orf[[nn]] <- paste0(aa1[1:en], collapse = "")
        }
      } 
      
    }
    
    mat_orf <- mat_orf[!lengths(mat_orf) == 0]
    
    mat_orf <- lapply(mat_orf, noquote)
    
    if(length(mat_orf) > 0 ){
      return(mat_orf)
    }
  }
  ## Generate random DNA sequence
  dna <- reactive({
    
    req(input$size_bp <= 1000, input$Generate, cancelOutput = TRUE)
    
    input$Generate
    
    if(input$seed == FALSE){
      isolate({genomics::rand_nuc_seq(n = input$size_bp, type = "dna")
      })} else(isolate({genomics::rand_nuc_seq(n = input$size_bp, type = "dna", 
                                                seed = 1234)
      })) 
  })
  
  ## Generate reverse complement of DNA seq
  rc_dna <- reactive({
    
    input$Generate
    
    dna1 <- isolate({DNAString(dna())})
    isolate({Biostrings::reverseComplement(dna1)})
    
  })
  
  ## Generate reading frames for forward seq
  fo_rf <- reactive({
    
    req(input$Generate, cancelOutput = TRUE)
    
    input$RFrames
    
    isolate({read_frm(dna())})
    
  })
  
  ## Generate reading frames for reverse complement seq
  rc_rf <- reactive({
    
    req(input$Generate, cancelOutput = TRUE)
    
    input$RFrames
    
    isolate({read_frm(rc_dna())})
    
  })
  
  # Output Forward sequence
  observeEvent(input$Generate,{
    
    req(dna(), input$Generate, cancelOutput = TRUE)
    
    output$FDNA <- renderText({
      
      isolate({return(dna())})
      
    })
    
  })
  
  # Output Reverse complement seq
  observeEvent(input$Generate,{
    
    req(dna(), input$Generate, cancelOutput = TRUE)
    
    output$RCDNA <- renderText({
      
      isolate({as.character(rc_dna())})
      
    })
    
  })
  
  # Generate reading frames
  
  observeEvent(input$RFrames,{
    
    req(input$Generate, dna(), cancelOutput = TRUE)
    
    output$Frames <- renderPrint({
      
      rf <- isolate({list(Forward_seq = fo_rf(), Reverse_complement = rc_rf())})
      
      isolate({return(rf)})
      
    })
    
  })
  
  # Translating reading frames
  
  observeEvent(input$Protein,{
    
    output$amino_chain <- renderPrint({
      
      fo_orf <- isolate({find_orf(fo_rf())})
      
      rc_orf <- isolate({find_orf(rc_rf())})
      
      aa <- isolate({list(Forward_seq = fo_orf, Reverse_complement = rc_orf,
            NOTE = paste("A NULL output indicates no ORF present."))})
      
      isolate({return(aa)})
      
    })
    
  })
  
  # Tab 2 -- standard genetic code
  observeEvent(input$Triplet,{
    
    req(input$Codon)
    
    output$AAcid <- renderPrint({
      AA <- isolate({genomics::aminoacid(input$Codon, transl_table = 1,
                                         type = "name")})
      
      if(AA == "Tyrosisne"){AA <- "Tyrosine"
      } else(AA == AA)
      
      cat(AA)
      
    })
  })
  
  
  observeEvent(input$All,{
    
    output$SG_code <- renderTable({
      SG <- isolate({genomics::gen_code()})
      
      SG[SG$name == "Tyrosisne",]$name <- "Tyrosine" 
      
      return(SG)
      
    }, rownames = TRUE, bordered = TRUE, hover = TRUE)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
