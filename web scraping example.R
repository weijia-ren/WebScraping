# Web Scraping Example

###########################################################################
#############                   Example 1                     #############  
#############         Scrape table from static page         ###############
###########################################################################

## step 1: inspect page
library(rvest)
url <- "https://www.ama.org/publications/MarketingNews/Pages/the-ama-gold-report-2017-top-50-market-research-firms.aspx"
browseURL(url)

## step 2: retrieve links
url_parsed <- read_html(url)
table<-html_table(url_parsed, header = FALSE, fill = TRUE) 
table <- table[[1]]

##  step 3: Clean up and manipulate the dataset
library(stringr)
var <- paste("X", 2:19, sep="")
a <- table[var]
df <- a[c(1,8,15,22,31,40,46,52,61,69,80,86,93,99,105,111,117,123,129,138,
          144,150,156,162,168,174,180,186,195,201,207,216,222,228,238,248,254,260,266,
          272,281,288,294,300,306,313,319,325,331,340),]
df[df==""]<-NA
df=as.data.frame(t(apply(df,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} ))) 
df$V6[df$V6 == "Founded"] <- "Founded:"
df$V7[df$V6 != "Founded:"] <- "NA"
df$V6[df$V6 != "Founded:"] <- "NA"
df=as.data.frame(t(apply(df,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))

df$V1<-word(df$V1,1,sep = "\\:") 
df$V1<-gsub(" ","",df$V1) 
df$V1<-gsub("U.S.\nHeadquarters","",df$V1)
df$Name<-word(df$V1,2,sep = "\\.")
df$ID<-word(df$V1,1,sep = "\\.")

df$V11<-gsub(" ","",df$V11) 
df$V11[df$V11 == "$3.6\nB"] <- "$3600 M"
df$V11[df$V11 == "$1.4\nB"] <- "$1400 M"
df$V11 = (gsub("[A-Z]", "", df$V11))
df$U.S.Revenue = as.numeric((gsub("\\$", "", df$V11)))
df$V15<-gsub(" ","",df$V15)
df$V15[df$V15 == "$1.4\nB"] <- "$1400 M"
df$V15[df$V15 == "$1.9\nB"] <- "$1900 M"
df$V15[df$V15 == "$2.7\nB"] <- "$2700 M"
df$V15[df$V15 == "$2.9\nB"] <- "$2900 M"
df$V15 = (gsub("[A-Z]", "", df$V15))
df$Non.US.Revenue = as.numeric((gsub("\\$", "", df$V15)))
df$size = as.numeric((gsub("\\,", "", df$V9)))
df$V7<-as.character(df$V7)
df$year = as.numeric(df$V7)
df$V3[df$V3 == "Chicago"]<-"Chicago, IL"
df$V3<-gsub(" ","",df$V3) 
df$V3[df$V3 == "New\nYork"]<-"New York, NY"
df<- df [c("ID","Name","V3","V5","year","size","U.S.Revenue","Non.US.Revenue")]


library (ggplot2)
ggplot(df,aes(x=year))+geom_histogram(color="blue", fill="blue", alpha=0.2,bins = 50)+
  labs(x="Found Year",y="Counts")+theme(legend.position="bottom")
ggplot(df,aes(x=year,y=U.S.Revenue,size=size,label=ID))+geom_point(alpha=0.4, color="blue")+
  scale_size(range=c(0,30))+geom_text(aes(label=ID),hjust=1, vjust=-0.5,size=3)+
  labs(x="Founded year",y="U.S. Revenue", size="Employee #")+
  theme(legend.position="bottom",legend.box = "vertical",legend.title = element_blank())
ggplot(df,aes(x=year,y=Non.US.Revenue,size=size,label=ID))+geom_point(alpha=0.4, color="blue")+
  scale_size(range=c(0,30))+geom_text(aes(label=ID),hjust=1, vjust=-0.5,size=3)+
  labs(x="Founded year",y="Non-U.S. Revenue", size="Employee #")+
  theme(legend.position="bottom",legend.box = "vertical",legend.title = element_blank())

library(nominatim)
city <- df[['V3']]
city<-gsub("[\r\n]", "", city)
city
b1 <- osm_geocode(city, key = Sys.getenv("nominatimkey"))
b1[c("lat", "lon")]
b1 <- cbind(ID = rownames(b1), b1)

library(leaflet)
leaflet(data = b1) %>% addTiles() %>% addCircleMarkers(~lon, ~lat , popup = ~as.character(ID))


###########################################################################
#############                   Example 2                       ###########  
#############      Scrape text from multiple static pages       ###########
###########################################################################

## step 1: inspect page
url <- "https://en.wikipedia.org/wiki/List_of_statisticians"
browseURL(url)

## step 2: retrieve links
html <- read_html(url)
anchors <- html_nodes(html, xpath = "//ul/li/a[1]")
links <- html_attr(anchors, "href")

links <- links[!is.na(links)]
links_iffer <-
  seq_along(links) >=
  seq_along(links)[str_detect(links, "Odd_Aalen")] &
  seq_along(links) <=
  seq_along(links)[str_detect(links, "George_Kingsley_Zipf")] &
  str_detect(links, "/wiki/")
links_index <- seq_along(links)[links_iffer]
links <- links[links_iffer]
length(links)


##  step 3: extract names
names <- links %>% basename %>% sapply(., URLdecode)  %>% str_replace_all("_", " ") %>% str_replace_all(" \\(.*\\)", "") %>% str_trim


## step 4: fetch personal wiki pages
baseurl <- "http://en.wikipedia.org"
HTML <- list()
Fname <- str_c(basename(links), ".html")
URL <- str_c(baseurl, links)
# loop
for ( i in seq_along(links) ){
  # url
  url <- URL[i]
  # fname
  fname <- Fname[i]
  # download
  if ( !file.exists(fname) ) download.file(url, fname)
  # read in files
  HTML[[i]] <- read_html(fname)
}


## step 5: identify links between statisticians
# loop preparation
connections <- data.frame(from=NULL, to=NULL)
# loop
for (i in seq_along(HTML)) {
  pslinks <- html_attr(
    html_nodes(HTML[[i]], xpath="//p//a"), # note: only look for links in p sections; otherwise too many links collected
    "href")
  links_in_pslinks <- seq_along(links)[links %in% pslinks]
  links_in_pslinks <- links_in_pslinks[links_in_pslinks!=i]
  connections <- rbind(
    connections,
    data.frame(
      from=rep(i-1, length(links_in_pslinks)), # -1 for zero-indexing
      to=links_in_pslinks-1 # here too
    )
  )
}

# results
names(connections) <- c("from", "to")
head(connections)

# make symmetrical
connections <- rbind(
  connections,
  data.frame(from=connections$to,
             to=connections$from)
)
connections <- connections[!duplicated(connections),]


## step 6: visualize connections
connections$value <- 1
nodesDF <- data.frame(name = names, group = 1)

library(networkD3)
network_out <- forceNetwork(Links = connections, Nodes = nodesDF, Source = "from", Target = "to", Value = "value", NodeID = "name", Group = "group", zoom = TRUE, fontSize = 14, opacityNoHover = 3)

saveNetwork(network_out, file = 'connections.html')
browseURL("connections.html")


## step 7: identify top nodes in data frame
nodesDF$id <- as.numeric(rownames(nodesDF)) - 1
connections_df <- merge(connections, nodesDF, by.x = "to", by.y = "id", all = TRUE)
library(dplyr)
to_count_df <- count(connections_df, name)
arrange(to_count_df, desc(n))


###########################################################################
#############                   Example 3                     #############  
#############             Scrape dynamic webpage            ###############
###########################################################################  

library(RSelenium)

rD <- rsDriver()
remDr <- rD[["client"]]

url<- "https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml"
remDr$navigate(url)

xpath <- '//*[@id="leftcol-mp"]/div[3]/ul/li[7]/a[2]'
a<-remDr$findElement(using = 'xpath', value = xpath)  
a$clickElement()


xpath <- '//*[@id="yui-rec1"]/td[3]/div/a'
a <- remDr$findElement(using = 'xpath', value = xpath)
a$clickElement() # click on button

output <- remDr$getPageSource(header = TRUE)
write(output[[1]], file = "AmericanFactFinderTest.html")

# close connection
remDr$closeServer()

content <- read_html("AmericanFactFinderTest.html", encoding = "utf8") 
tabs <- html_table(content, fill = TRUE)
tab <- tabs[[2]]




