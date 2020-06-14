library(visNetwork)
library(shiny)
library(igraph)
library(RColorBrewer)
library(googleVis)

options(shiny.error = function() {
  stop()
})


## Extract nodes and edges from Output file
raw_results <- read.csv("DummyNetworkData.csv")
varDescriptions <- read.csv("DummyLabelFile.csv")

allVariables <- raw_results[is.na(raw_results$Data)==FALSE,]

## Various options available for plotting different aggregations - comment out desired
# Treat Variables in different AGMs as different nodes
# allVariables <- cbind(allVariables,depName=paste0(allVariables$Dependent,allVariables$AGM),varName=paste0(allVariables$Independent,allVariables$AGM))

#Treat Variables in different AGMS the same node (take off the APL suffix)
allVariables <- cbind(allVariables,depName=paste0(substr(allVariables$Dependent,1,43)),varName=paste0(substr(allVariables$Independent,1,43)))

#FIlter by AGM (call out specific AGM by eg. filterByAGM$`AUSTRALIAN CAPITAL TERRITORY`)
# allVar <- split(allVariables,allVariables$Dependent_Demography)

#filter by Demographic
allVar <- allVariables[allVariables$Dependent_Demography=="GNX",]

#filter by AGM
# allVar <- allVariables[allVariables$AGM=="NEW SOUTH WALES - METRO",]

#No filter
# allVar <- allVariables

nodeID <- unique(rbind(as.matrix(allVar$depName),as.matrix(allVar$varName)))

dat_nodes <- NA
for (i in 1:length(nodeID)){
    dat_nodes <-na.omit(rbind(dat_nodes, data.frame(varDescriptions[varDescriptions$Variable %in% substr(nodeID[i],1,43),])))
}

# Fix up data type to numeric
dat_nodes$spend.3.yrs <- as.character(dat_nodes$spend.3.yrs)
dat_nodes$spend.3.yrs[dat_nodes$spend.3.yrs=="#N/A"] <- 0
dat_nodes$spend1 <- gsub(",", "", dat_nodes$spend.3.yrs, fixed = TRUE)
dat_nodes$spend2 <- gsub("(", "", dat_nodes$spend1, fixed = TRUE)
dat_nodes$spend <- as.numeric(gsub(")", "", dat_nodes$spend2, fixed = TRUE))

# Change NAT and LOC to Marketing type
dat_nodes$Data <- as.character(dat_nodes$Data)
dat_nodes$Data[dat_nodes$Data=="NAT"] <- "Marketing"
dat_nodes$Data[dat_nodes$Data=="LOC"] <- "Marketing"

#### If variable code starts with CRL- control, PFM- performance, LOC/NAT- marketing ###
dat_nodes$Data <- as.character(dat_nodes$Data)
dat_nodes$Data[substr(dat_nodes$Variable,1,3) %in% c("CRL","ECO")] <- "Control"
dat_nodes$Data[substr(dat_nodes$Variable,1,3) %in% c("LOC","NAT")] <- "Marketing"
dat_nodes$Data[substr(dat_nodes$Variable,1,3) %in% c("PFM")] <- "Performance"
dat_nodes$Data[dat_nodes$L_TYPE %in% "BHL"] <- "BrandHealth"

#Get Marketing spend only
marketingSpend <- dat_nodes$spend[dat_nodes$Data=="Marketing"]


#Coerce to range (marketing spend only)
OldRange <-  max(marketingSpend)-min(marketingSpend)
NewRange <-  70
dat_nodes$Size <-  (((dat_nodes$spend - min(marketingSpend)) * NewRange) / OldRange) + 20
 
#If not marketing spend, make size 20
dat_nodes$Size[dat_nodes$Data!="Marketing"] <- 30

#Put dollar sign in front
dat_nodes$spend.3.yrs <- paste0("$",dat_nodes$spend.3.yrs)

#Change row names to something more consistent
nodeDetail <- dat_nodes
colnames(nodeDetail)[which(names(nodeDetail) == "BRAND")] <- "Brand"
colnames(nodeDetail)[which(names(nodeDetail) == "MARKETINGSEGMENT")] <- "Marketing Segment"
colnames(nodeDetail)[which(names(nodeDetail) == "CAMPAIGN")] <- "Campaign"
colnames(nodeDetail)[which(names(nodeDetail) == "PROGRAM")] <- "Program"
colnames(nodeDetail)[which(names(nodeDetail) == "PROGRAMCATEGORY")] <- "Program Category"
colnames(nodeDetail)[which(names(nodeDetail) == "ACTIVITY")] <- "Activity"
colnames(nodeDetail)[which(names(nodeDetail) == "spend.3.yrs")] <- "3 Year Spend"
colnames(nodeDetail)[which(names(nodeDetail) == "METRIC")] <- "Metric"

row.names(nodeDetail) <- NULL

# Distinguish same activity name but different data type
activities <- as.character(unique(paste0(dat_nodes$ACTIVITY,' - ',dat_nodes$Data)))
dat_nodes$ACTIVITY1 <- as.character(paste0(dat_nodes$ACTIVITY,' - ',dat_nodes$Data))

#Nodes List
nodes <- data.frame(id=dat_nodes$Variable,group=dat_nodes$ACTIVITY1,Activity=dat_nodes$ACTIVITY,
                    title=paste0("Campaign: ",dat_nodes$CAMPAIGN,"<br>","Activity: ",dat_nodes$ACTIVITY,"<br>","Metric: ",dat_nodes$METRIC,
                                 "<br>","Demographic: ",dat_nodes$DEMOGRAPHIC,"<br>","3 year spend: ",dat_nodes$spend.3.yrs),label="",size=dat_nodes$Size)

# Make list of edges
dat_edges <- unique(data.frame(from=allVar$varName,to=allVar$depName,width=allVar$t.value))
edges <- dat_edges


n <- length(activities)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = (unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))))[1:n]

visAllGroups <- function(out,activities,col_vector){
  for (i in 1:length(activities)){
    ##Different shape for Control variables
    ##Assumes that each activity can only be part of one group of Control/Marketing/PFM
    if ((dat_nodes$Data[dat_nodes$ACTIVITY1==activities[i]][1]=="Performance") ){
      out <- visGroups(out,groupname = activities[i],color=col_vector[i],shape='star')
    }
    else if (dat_nodes$Data[dat_nodes$ACTIVITY1==activities[i]][1] %in% c("Control","BrandHealth")){
      out <- visGroups(out,groupname = activities[i],color=col_vector[i],shape='diamond')
    }
    else{
      out <- visGroups(out,groupname = activities[i],color=col_vector[i],shape='dot')
    }
    
  }
  return(out)
}


rownames <- c("Data","Type","Brand","Marketing Segment","Campaign","Program","Program Category","Activity","Metric","3 Year Spend")


server <- function(input, output) {
  output$network_id <- renderVisNetwork({
visNetwork(nodes, edges) %>%
  visIgraphLayout(randomSeed=5) %>%
  visOptions(highlightNearest = list(enabled=TRUE,degree=list(from=0,to=10),algorithm="hierarchical"),
             nodesIdSelection=list(enabled = TRUE, style = 'width: 300px; height: 26px;
                                   background: #f8f8f8;
                                   color: black;
                                   border:none;
                                   outline:none;'),
             selectedBy =  list(variable = "Activity", style = 'width: 300px; height: 26px;
                                background: #f8f8f8;
                                color: black;
                                border:none;
                                outline:none;')) %>%
  visInteraction(keyboard = TRUE,hover=TRUE,navigationButtons=TRUE) %>%
  visEdges(arrows = list(to=list(enabled = TRUE, scaleFactor= 2))) %>%
  
  visAllGroups(activities,col_vector) #%>%
  
  })
  
  output$view_id <- renderTable({
    if (input$network_id_selected==""){}
    else{
      as.data.frame(t(nodeDetail[nodeDetail$Variable==input$network_id_selected,c("Type","Brand","Marketing Segment","Campaign","Program","Program Category","Activity","Metric","3 Year Spend")]))}
  },

  include.colnames=FALSE)
  
}

ui <-  fluidPage(
  titlePanel("Bayesian Network Visualisation"),
  fluidRow(
    column(width=7,
           visNetworkOutput("network_id",height = "1000px")),
    
    column(width = 5,
           h4("Selected Node Details"),
           tableOutput("view_id")
    )
    
  )
)


shinyApp(ui = ui, server = server)
