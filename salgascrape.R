###<include libraries>
library(rvest)
library(stringr)
library(dplyr)
library(pdftools)
library(httr)
source("C:/DEV/R/R Scripts/CommonFunctions/loadComnFncs.R")
fncs <- c("getMaxVal.R")
loaded <- loadComnFncs(fncs)

Salga_home_URL        <- "https://www.salga.org.za/Municipalities%20MCD.html"
Salga_home_html       <- read_html(Salga_home_URL)
Salga_prov_urls_text  <- Salga_home_html %>% html_nodes("area") %>% html_attr("href")
Salga_prov_urls       <- Salga_prov_urls_text[grepl("^Municipalities",Salga_prov_urls_text)]
    
DF_prov_muni_urls     <- data.frame(
                                prov_name = Salga_prov_urls,
                                prov_url  = Salga_prov_urls
                                )
DF_prov_muni_urls$prov_name <- sub("\\.[^.]*$", "",sub("^Municipalities MCD ","",DF_prov_muni_urls$prov_name))
DF_prov_muni_urls$prov_url  <- paste0("https://www.salga.org.za/",DF_prov_muni_urls$prov_url)
DF_prov_muni_urls$prov_url  <- gsub(" ", "%20", DF_prov_muni_urls$prov_url)

xend   <- nrow(DF_prov_muni_urls)
xstart <- 1    #loop overide start
x = 1 
#xend   <- 1    #loop overide stop 
                              
DF_muni_all                           <- data.frame(
  province = character(),                                 
  municipality = character(),
  address = character(),
  website = character(),
  mayor = character(),
  mayor_pa = character(),
  deputy_mayor = character(),
  chief_financial_officer = character(),
  chief_whip = character(),
  municipal_manager = character(),
  speaker = character())
for (x in xstart:xend)
{
  DF_muni                           <- data.frame(
    province = character(),                                 
    municipality = character(),
    address = character(),
    website = character(), 
    mayor = character(),
    mayor_pa = character(),
    deputy_mayor = character(),
    chief_financial_officer = character(),
    chief_whip = character(),
    municipal_manager = character(),
    speaker = character())
  row                         <- 0 
  col                         <- 0
  prov_html                   <- read_html(DF_prov_muni_urls$prov_url[x]) %>% html_nodes("td[width='579']")
  prov_name                   <- DF_prov_muni_urls$prov_name[x]
  muni_name                   <- prov_html %>%  html_nodes("div.CollapsiblePanelTab") %>% html_text() 
  muni_html                   <- prov_html %>%  html_nodes("div.CollapsiblePanelContent") 
  mxno_muni_html              <- getMaxVal((muni_html)) 
  mxno_muni_name              <- getMaxVal((muni_name))
#  if (mxno_muni_html == 0)
#  { muni_html <- muni_html[-x] }
  if (mxno_muni_html != mxno_muni_name)
    { 
    message(" error on province ", prov_name, " muni names max =>", mxno_muni_name, " mxno_muni_html",mxno_muni_html, " index is ", x     )
    }
  a                           <- 1 
  a_a                         <- 1 
  a_z                         <- getMaxVal(muni_name)
  for (a in a_a:a_z)
  {
    message("muncipality name ", muni_name[a])
    row                         <-  row + 1 
    DF_muni[row, "province"]       <- prov_name
    DF_muni[row, "municipality"]       <- muni_name[a]
    
    colm_nams                   <- c("municipality","province")
    muni_ptags_char             <- muni_html[a] %>% html_nodes("p") %>% as.character
    muni_ptags_html             <- muni_html[a] %>% html_nodes("p") %>% html_text
    
    muni_ptags                  <- gsub("<p>","",muni_ptags_char)
    muni_ptags                  <- gsub("</p>","",muni_ptags)
    muni_ptags                  <- gsub("<br>","",muni_ptags)
    muni_ptags                  <- gsub("<strong>","",muni_ptags)
    muni_ptags                  <- gsub("</strong>","",muni_ptags)
    muni_ptags                  <- muni_ptags %>% str_squish()
    empty_tags                  <- muni_ptags == "" 
    empty_tags                  <- !empty_tags
    muni_ptags                  <- muni_ptags[which(empty_tags)]
    b                           <- 1 
    b_a                         <- 1 
    b_z                         <- getMaxVal(muni_ptags)
    if (b_z != 0)
    {  
      col <- 9 
      for (b in b_a:b_z)
      {
        col                       <- col + 1
#        message("muni+tags", muni_ptags[b])
        if (grepl("postal|street",muni_ptags[b], ignore.case = TRUE) )
          {
            DF_muni[row, "address"]     <- trimws(muni_ptags[b])
            next 
          }
        if  (grepl("mayor",muni_ptags[b], ignore.case = TRUE))
          {
            if  (grepl("deputy",muni_ptags[b], ignore.case = TRUE))
              {
                DF_muni[row, "deputy_mayor"]     <- trimws(muni_ptags[b])
                next
              }
             else 
              {
                DF_muni[row, "mayor"]     <- trimws(muni_ptags[b])
                next     
              } 
            }
        if  (grepl("municipal",muni_ptags[b], ignore.case = TRUE) && grepl("manager",muni_ptags[b], ignore.case = TRUE))
          {
            DF_muni[row, "municipal_manager"]     <- trimws(muni_ptags[b])
            next 
          }
        if  (grepl("speaker",muni_ptags[b], ignore.case = TRUE))
        {
          DF_muni[row, "speaker"]     <- trimws(muni_ptags[b])
          next 
        }
        if  (grepl("chief",muni_ptags[b], ignore.case = TRUE) &&  grepl("whip",muni_ptags[b], ignore.case = TRUE))
        {
          DF_muni[row, "chief_whip"]     <- trimws(muni_ptags[b])
          next 
        }
        if  (grepl("financial",muni_ptags[b], ignore.case = TRUE) &&  grepl("officer",muni_ptags[b], ignore.case = TRUE))
        {
          DF_muni[row, "chief_financial_officer"]     <- trimws(muni_ptags[b])
          next 
        }
        if  (grepl("PA:",muni_ptags[b], ignore.case = TRUE) )
        {
          DF_muni[row, "mayor_pa"]     <- trimws(muni_ptags[b])
          next 
        }
        
        if  (grepl("website",muni_ptags[b], ignore.case = TRUE) )
        {
          DF_muni[row, "website"]     <- trimws(muni_ptags[b])
          next 
        }
        message("missed missed", muni_ptags[b] )       
    }
    }
  }
  DF_muni_all <- rbind(DF_muni_all, DF_muni)
}
write.csv(DF_muni_all,"municipality_contacts.csv", row.names = FALSE)



