packages<-c("shiny", "tidyverse","readxl","writexl","shinyjs","crayon")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))

bold = function(x) strong(x, .noWS = "outside")
ital = function(x) em(x, .noWS = "outside")
link = function(s, href = s) a(s, href = href, .noWS = "outside")

ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs
  
  tags$head(
    tags$style(type = "text/css", "
      .body {font-size: small}
      .well {padding-top: 10px;}
      .selectize-dropdown {width: 250px !important;}
      .fa-check { font-size:xx-large; color:Lime}
      
  ")),
  
  # Application title
  h2(id = "title-h2", "Arlequin Converter - ChrY"),
  tags$style(HTML("#title-h2 {background-color: gray; color: white; padding: 15px}")),
  
  p(bold("Objetivo: "), "Transformar automaticamente una base de datos de Cromosoma Y para su incorporacion al software Arlequin"),
  p(bold("Ingresar una tabla con los haplotipos con las siguientes aracteristicas:  ")),
  
  tableOutput("tabla_ejemplo"),

  p("    - Las microvariantes se pueden introducir como ',' o como '.'"),
  p("    - La ausencia de informacion respecto a un marcador debe quedar vacio"),
  p("    - El programa acepta archivos .tsv, .csv, .txt y .xls o .xlsx (Excel)"),
 
  p(bold("El programa va a modificar la tabla para que sea compatible con el formato requerido por Arlequin:")),
  p("    - Reemplazara los casilleros vacios por '0'"),
  p("    - A todas las muestras se les asigna un '1' indicando que se refiere a una sola de ellas; Si hay dos muestras con mismo haplotipo se representaran dos veces"),
  p("    - Se puede optar por mantener el nombre orginal de las muestras, o generares un codigo para que resulten anonimas"), 
  p("    - Se quitan los marcadores DYS385 y  DYF387S1"),
  p("    - El marcador DYS389II se transforma en la resta de repeticiones del DYS389II con el DYS389I"),
  
  p(bold("El botón 'Generar archivo .arp', generará el archivo de texto que se ingresa al Arlequin, con la tabla ingresada ahi.")),
  p("Dicho formato requiere ingresar un Titulo del Proyecto y uno para el conjunto de Muestras"),
  
  #Inputs 
    fileInput("file", "Suba un archivo", accept = c(".xlsx",".xls",".txt",".tsv",".csv")),
    selectInput("anonimate", "Modificar ID de muestras?", choices = c("ORIGINAL", "ANONIMO")),
    textInput("Title", "Title Project"),
    textInput("SampleName", "Sample Name"),
    downloadButton("download_table", "Descargar Tabla"),
    downloadButton("download_arp", "Descargar Arp"),
    tableOutput("tabla")
  
)

server <- function(input, output) {
  
  #Tabla de ejemplo para imprimir en instrucciones
  output$tabla_ejemplo<-renderTable(read_xlsx("tabla_ejemplo_Y.xlsx")%>%head(n=3))
  
#funcion principal: convertir la tabla
  to_arlequin_Y<-reactive({
    req(input$file)
    #Segun en que formato esta, otra funcion de lectura
    if(grepl("(txt|tsv)$",input$file$datapath)) {df<-read_tsv(input$file$datapath)} else 
      if(grepl("xlsx$",input$file$datapath)) {df<-read_xlsx(input$file$datapath)} else
        if (grepl("xls$",input$file$datapath)) {df<-read_xls(input$file$datapath)} else
          if (grepl("csv$", input$file$datapath)){df<-read_csv<-read_csv(input$fle$datapath)} 
    #si quierohacerlo anonimo
    if(input$anonimate=="ANONIMO"){df[,1]=paste0("sample",1000:(999+nrow(df)))}
    names(df)[1]<-"sample_id"
    df<-df%>%mutate(val=1)
    df<-select(df, -DYS385, -DYF387S1 )    
    df<-mutate(df,DYS389II=as.numeric(DYS389II),DYS389I=as.numeric(DYS389I), DYS389II=DYS389II-DYS389I)
    df<-df%>%mutate(across(cols=evenrything(), .fns=~as.character(.x)))
    #reemplazo NA por 0
    df[is.na(df)]<-0
    df=df[,c(1,ncol(df),seq(2,ncol(df)-1))]
    #sacar los . o , de las microvariantes
    df<-lapply(df, function(x) gsub("\\.|\\,|\\s", "", x))
    df<-as_tibble(df)
    #Elimino valores
    names(df)[2]=""
    return(df)
  })
  
  #Imprimo tabla
  output$tabla<-renderTable(head(to_arlequin_Y()))
 #Descargar TABLA (mismo filename, agregandole temrinacion _arlequin.xlsx)
  output$download_table <- downloadHandler(
    filename = function() {
      paste0(str_replace(input$file, "\\..*",""),"_", "arlequin.xlsx")
    },
    content = function(file) {
      write_xlsx(to_arlequin_Y(), file)
    }
  )

  # #Generar .arp
  formato = "../Formato_vacio.txt"
  
  #Tabla a formato texto adecuado
  df_to_text<-reactive({
    text<-paste("{",paste(names(to_arlequin_Y()), collapse = "\t"), collapse = "")
    for (i in 1:nrow(to_arlequin_Y())){
      text<- paste(text,"\n", paste(to_arlequin_Y()[i,],collapse ="\t"), collapse="")
    }
  })
  
  SampleSize=reactive(length(unique(to_arlequin_Y()$sample_id)))
  
  #Descargar arp!
  #Para el content va buscando las lineas del file "formato" y les agrega la info necesaria (con gsub). El deparse es para poner comillas al titulo y samplename
  output$download_arp<-downloadHandler(
    filename=function(){
      paste0(str_replace(input$file, "\\..*",""), ".arp")
    },
    content=function(file){
      readLines(formato)%>%gsub("Title\\=", paste("Title\\=",deparse(input$Title), collapse = ""),.)%>%writeLines(con=file)
      readLines(file)%>%gsub("SampleName\\=", paste("SampleName\\=",deparse(input$SampleName), collapse = ""),.)%>%writeLines(con=file)
      readLines(file)%>%gsub("SampleSize\\=", paste("SampleSize\\=",SampleSize(), collapse = ""),.)%>%writeLines(con=file)
      readLines(file)%>%gsub("\\{",df_to_text(),.)%>%writeLines(con=file)
    }
  )
}

shinyApp(ui = ui, server = server)
