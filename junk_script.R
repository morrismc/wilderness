#################################### SECTION TITLE ####################################
g1 <- ggplot() +
  
  geom_tile(x = ncol(stateArea),
    y = stateArea$State,
            color = stateArea$`Area (acre)`)

g1

#################################### Conditional table format ####################################
library(viridis)
library(formattable)




formattable(stateData,
            list(Count = color_tile('white','orange'),
                 `Area (acre)` = color_tile('white','green')))






library(knitr)
library(rmarkdown)

table<-kable(html_table, format="markdown")
cat(table, sep="\n", file="Table.Rmd")
render("table.Rmd",output_format = "pdf_document")


write(paste(html_header, html_table, sep=""), "./Supplementary_table_2.html")

export_formattable(html_table,"./Supplementary_table_2_CrypticMAPSs.pdf")


install.packages('htmltools')
install.packages('webshot')
library(htmltools)
library(webshot)
webshot::install_phantomjs()
install.packages("knitr")
library(knitr)

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}


f <- (formattable(stateData,
                  list(Count = color_tile('white','orange'),
                       `Area (acre)` = color_tile('white','green'))))

html_header="
<head> 
<meta charset=\"utf-8\"> 
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"> 
<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css\">
</head>
<body>
"