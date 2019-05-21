library(shiny)
library(shinyjs)
library(plotrix)

ui <- fluidPage(
  
  titlePanel("Probabilistic Productivity-Susceptibility Analysis"),
  strong("       "),
  fluidRow(column(6,
                  h5("This shiny application is designed to perform a Productivity-Susceptibility 
                     risk analysis. It extends the existing methodology of Patrick et al 
                     (2009) by providing the ability to assign a probability to the score for each 
                     attribute. These score probabilities are combined to define a vulnerability 
                     likelihood function. This alternate methodology and shiny application were 
                     developed by Dr Nathan Vaughan of NOAA's Southeast Fisheries Science Center in 
                     collaboration with Dr Jason Cope of NOAA's Northwest Fisheries Science Center. 
                     Please contact nathan.vaughan@noaa.gov with any questions.")
                  ),
           column(3,
                  fluidRow(
                    column(12,selectInput("chooseAssessment",label="load existing assessment",choices=list("Choose an existing assessment"="DNU")))
                  ),
                  fluidRow(
                    column(12,downloadButton("downloadAssessment",label="Download Assessement"))
                  )
           ),
           column(3,
                  fluidRow(
                    column(12,textInput("AssessmentName",label="Species name",value=""))
                  ),
                  fluidRow(
                    column(12,actionButton("saveAssessment",label="Save Assessment"))
                  )
           )
                  ),
  strong("       "),
  
  tabsetPanel(
    tabPanel("Productivity",
             fluidRow(
               column(2,strong("Attribute description")),
               column(2,strong("")),
               column(2,strong("Score weights")),
               column(2,strong("")),
               column(2,strong("Attribute weight")),
               column(2,strong("Comments"))
             ),
             fluidRow(
               column(2,strong("")),
               column(2,strong("High(3)")),
               column(2,strong("Moderate(2)")),
               column(2,strong("Low(1)")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2,h5("r (intrinsic increase)"),
                      style='margin-bottom:0px;'),
               column(2,h5(">0.5")),
               column(2,h5("0.5-0.16(mid-point 0.1)")),
               column(2,h5("<0.16")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbPA13","weight",min=0,value=1)),
               column(2,numericInput("ProbPA12","weight",min=0,value=1)),
               column(2,numericInput("ProbPA11","weight",min=0,value=1)),
               column(2,numericInput("ProbPA14","",min=0,value=1)),
               column(2,textInput("ProbPA15",label=""))
             ),
             fluidRow(
               column(2,h5("Maximum age")),
               column(2,h5("<20 years")),
               column(2,h5("20-40 years")),
               column(2,h5(">40 years")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbPA23","weight",min=0,value=1)),
               column(2,numericInput("ProbPA22","weight",min=0,value=1)),
               column(2,numericInput("ProbPA21","weight",min=0,value=1)),
               column(2,numericInput("ProbPA24","",min=0,value=1)),
               column(2,textInput("ProbPA25",label=""))
             ),
             fluidRow(
               column(2,h5("Maximum size")),
               column(2,h5("<40 cm")),
               column(2,h5("40-80 cm")),
               column(2,h5(">80 cm")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbPA33","weight",min=0,value=1)),
               column(2,numericInput("ProbPA32","weight",min=0,value=1)),
               column(2,numericInput("ProbPA31","weight",min=0,value=1)),
               column(2,numericInput("ProbPA34","",min=0,value=1)),
               column(2,textInput("ProbPA35",label=""))
             ),
             fluidRow(
               column(2,h5("VonBert (k)")),
               column(2,h5(">0.2")),
               column(2,h5("0.1-0.2")),
               column(2,h5("<0.1")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbPA43","weight",min=0,value=1)),
               column(2,numericInput("ProbPA42","weight",min=0,value=1)),
               column(2,numericInput("ProbPA41","weight",min=0,value=1)),
               column(2,numericInput("ProbPA44","",min=0,value=1)),
               column(2,textInput("ProbPA45",label=""))
             ),
             fluidRow(
               column(2,h5("Natural mortality")),
               column(2,h5(">0.2")),
               column(2,h5("0.1-0.2")),
               column(2,h5("<0.1")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbPA53","weight",min=0,value=1)),
               column(2,numericInput("ProbPA52","weight",min=0,value=1)),
               column(2,numericInput("ProbPA51","weight",min=0,value=1)),
               column(2,numericInput("ProbPA54","",min=0,value=1)),
               column(2,textInput("ProbPA55",label=""))
             ),
             fluidRow(
               column(2,h5("Measured fecundity")),
               column(2,h5(">10e4")),
               column(2,h5("10e2-10e4")),
               column(2,h5("<10e2")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbPA63","weight",min=0,value=1)),
               column(2,numericInput("ProbPA62","weight",min=0,value=1)),
               column(2,numericInput("ProbPA61","weight",min=0,value=1)),
               column(2,numericInput("ProbPA64","",min=0,value=1)),
               column(2,textInput("ProbPA65",label=""))
             ),
             fluidRow(
               column(2,h5("Breeding strategy")),
               column(2,h5("0")),
               column(2,h5("1-3")),
               column(2,h5(">4")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbPA73","weight",min=0,value=1)),
               column(2,numericInput("ProbPA72","weight",min=0,value=1)),
               column(2,numericInput("ProbPA71","weight",min=0,value=1)),
               column(2,numericInput("ProbPA74","",min=0,value=1)),
               column(2,textInput("ProbPA75",label=""))
             ),
             fluidRow(
               column(2,h5("Recruitment")),
               column(2,h5("Highly frequent")),
               column(2,h5("Moderately frequent")),
               column(2,h5("infrequent")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbPA83","weight",min=0,value=1)),
               column(2,numericInput("ProbPA82","weight",min=0,value=1)),
               column(2,numericInput("ProbPA81","weight",min=0,value=1)),
               column(2,numericInput("ProbPA84","",min=0,value=1)),
               column(2,textInput("ProbPA85",label=""))
             ),
             fluidRow(
               column(2,h5("Age at maturity")),
               column(2,h5("<2 years")),
               column(2,h5("2-4 years")),
               column(2,h5(">4 years")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbPA93","weight",min=0,value=1)),
               column(2,numericInput("ProbPA92","weight",min=0,value=1)),
               column(2,numericInput("ProbPA91","weight",min=0,value=1)),
               column(2,numericInput("ProbPA94","",min=0,value=1)),
               column(2,textInput("ProbPA95",label=""))
             ),
             fluidRow(
               column(2,h5("Mean trophic level")),
               column(2,h5("<2.5")),
               column(2,h5("2.5-3.5")),
               column(2,h5(">3.5")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbPA103","weight",min=0,value=1)),
               column(2,numericInput("ProbPA102","weight",min=0,value=1)),
               column(2,numericInput("ProbPA101","weight",min=0,value=1)),
               column(2,numericInput("ProbPA104","",min=0,value=1)),
               column(2,textInput("ProbPA105",label=""))
             )
    ), 
    tabPanel("Susceptibility", 
             fluidRow(
               column(2,strong("Attribute description")),
               column(2,strong("")),
               column(2,strong("Score weights")),
               column(2,strong("")),
               column(2,strong("Attribute weight")),
               column(2,strong("Comments"))
             ),
             fluidRow(
               column(2,strong("")),
               column(2,strong("Low(1)")),
               column(2,strong("Moderate(2)")),
               column(2,strong("High(3)")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2,h5("Management strategy")),
               column(2,h5("Targeted stocks have catch limits and proactive accountability measures")),
               column(2,h5("Targeted stocks have catch limits and reactive accountability measures")),
               column(2,h5("Targeted stocks do not have catch limits or accountability measures")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbSA13","weight",min=0,value=1)),
               column(2,numericInput("ProbSA12","weight",min=0,value=1)),
               column(2,numericInput("ProbSA11","weight",min=0,value=1)),
               column(2,numericInput("ProbSA14","",min=0,value=1)),
               column(2,textInput("ProbSA15",label=""))
             ),
             fluidRow(
               column(2,h5("Areal overlap")),
               column(2,h5("<25% of the stock occures in the area fished")),
               column(2,h5("25-50% of the stock occures in the area fished")),
               column(2,h5(">50 of the stock occures in the area fished")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbSA23","weight",min=0,value=1)),
               column(2,numericInput("ProbSA22","weight",min=0,value=1)),
               column(2,numericInput("ProbSA21","weight",min=0,value=1)),
               column(2,numericInput("ProbSA24","",min=0,value=1)),
               column(2,textInput("ProbSA25",label=""))
             ),
             fluidRow(
               column(2,h5("Geographic concentration")),
               column(2,h5("Stock distributed in <25% of its range")),
               column(2,h5("Stock distributed in 25-50% of its range")),
               column(2,h5("Stock distributed in >50% of its range")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbSA33","weight",min=0,value=1)),
               column(2,numericInput("ProbSA32","weight",min=0,value=1)),
               column(2,numericInput("ProbSA31","weight",min=0,value=1)),
               column(2,numericInput("ProbSA34","",min=0,value=1)),
               column(2,textInput("ProbSA35",label=""))
             ),
             fluidRow(
               column(2,h5("Vertical overlap")),
               column(2,h5("<25% of stock occurs in the depths fished")),
               column(2,h5("25-50% of the stock occurs in the the depths fished")),
               column(2,h5(">50% of the stock occurs in the the depths fished")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbSA43","weight",min=0,value=1)),
               column(2,numericInput("ProbSA42","weight",min=0,value=1)),
               column(2,numericInput("ProbSA41","weight",min=0,value=1)),
               column(2,numericInput("ProbSA44","",min=0,value=1)),
               column(2,textInput("ProbSA45",label=""))
             ),
             fluidRow(
               column(2,h5("Spawning stock biomass")),
               column(2,h5("B is >40% of B0")),
               column(2,h5("B is 25-40% of B0")),
               column(2,h5("B is <25% of B0")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbSA53","weight",min=0,value=1)),
               column(2,numericInput("ProbSA52","weight",min=0,value=1)),
               column(2,numericInput("ProbSA51","weight",min=0,value=1)),
               column(2,numericInput("ProbSA54","",min=0,value=1)),
               column(2,textInput("ProbSA55",label=""))
             ),
             fluidRow(
               column(2,h5("Fishing rate relative to M")),
               column(2,h5("<0.5")),
               column(2,h5("0.5-1")),
               column(2,h5(">1")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbSA63","weight",min=0,value=1)),
               column(2,numericInput("ProbSA62","weight",min=0,value=1)),
               column(2,numericInput("ProbSA61","weight",min=0,value=1)),
               column(2,numericInput("ProbSA64","",min=0,value=1)),
               column(2,textInput("ProbSA65",label=""))
             ),
             fluidRow(
               column(2,h5("Seasonal Migrations")),
               column(2,h5("Seasonal migrations decrease overlap with the fishery")),
               column(2,h5("Seasonal migrations do not substantially affect overlap with the fishery")),
               column(2,h5("Seasonal migrations increase overlap with the fishery")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbSA73","weight",min=0,value=1)),
               column(2,numericInput("ProbSA72","weight",min=0,value=1)),
               column(2,numericInput("ProbSA71","weight",min=0,value=1)),
               column(2,numericInput("ProbSA74","",min=0,value=1)),
               column(2,textInput("ProbSA75",label=""))
             ),
             fluidRow(
               column(2,h5("Schooling/Aggregation and other behavioral responses")),
               column(2,h5("Behavioral resposes decrease the catchability of the gear")),
               column(2,h5("Behavioral resposes do not substantially affect the catchability of the gear")),
               column(2,h5("Behavioral resposes increase the catchability of the gear (i.e. hyperstability of CPUE with schooling behavior")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbSA83","weight",min=0,value=1)),
               column(2,numericInput("ProbSA82","weight",min=0,value=1)),
               column(2,numericInput("ProbSA81","weight",min=0,value=1)),
               column(2,numericInput("ProbSA84","",min=0,value=1)),
               column(2,textInput("ProbSA85",label=""))
             ),
             fluidRow(
               column(2,h5("Morphology affecting capture")),
               column(2,h5("Low selectivity to the fishing gear")),
               column(2,h5("Moderate selectivity to the fishing gear")),
               column(2,h5("High selectivity to the fishing gear")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbSA93","weight",min=0,value=1)),
               column(2,numericInput("ProbSA92","weight",min=0,value=1)),
               column(2,numericInput("ProbSA91","weight",min=0,value=1)),
               column(2,numericInput("ProbSA94","",min=0,value=1)),
               column(2,textInput("ProbSA95",label=""))
             ),
             fluidRow(
               column(2,h5("Survival after capture and release")),
               column(2,h5("Probability of survival >67%")),
               column(2,h5("Probability of survival 33-67%")),
               column(2,h5("Probability of survival <33%")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbSA103","weight",min=0,value=1)),
               column(2,numericInput("ProbSA102","weight",min=0,value=1)),
               column(2,numericInput("ProbSA101","weight",min=0,value=1)),
               column(2,numericInput("ProbSA104","",min=0,value=1)),
               column(2,textInput("ProbSA105",label=""))
             ),
             fluidRow(
               column(2,h5("Desirability/Value of the fishery")),
               column(2,h5("Stock is not highly valued or desired by the fishery")),
               column(2,h5("Stock is moderately valued or desired by the fishery")),
               column(2,h5("Stock is highly valued or desired by the fishery")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbSA113","weight",min=0,value=1)),
               column(2,numericInput("ProbSA112","weight",min=0,value=1)),
               column(2,numericInput("ProbSA111","weight",min=0,value=1)),
               column(2,numericInput("ProbSA114","",min=0,value=1)),
               column(2,textInput("ProbSA115",label=""))
             ),
             fluidRow(
               column(2,h5("Fishery impact to essential fish habitat or habitat in general for non-targets")),
               column(2,h5("Adverse effects absent, minimal, or temporary")),
               column(2,h5("Significant and persistent adverse effects that are mitigated")),
               column(2,h5("Significant and persistent adverse effects that are not mitigated")),
               column(2,strong("")),
               column(2,strong(""))
             ),
             fluidRow(
               column(2),
               column(2,numericInput("ProbSA123","weight",min=0,value=1)),
               column(2,numericInput("ProbSA122","weight",min=0,value=1)),
               column(2,numericInput("ProbSA121","weight",min=0,value=1)),
               column(2,numericInput("ProbSA124","",min=0,value=1)),
               column(2,textInput("ProbSA125",label=""))
             )), 
    tabPanel("Results", 
             fluidRow(column(3,radioButtons("plotType","Display type",choiceNames=c("Bi-plot","Vulnerability Density","Attribute Uncertainty"),choiceValues = c(1,2,3),selected=1)),
                      column(3,numericInput("LowVuln","Low vulnerability threshold",max=2.828427,min=0,value=1.8),
                             numericInput("ModVuln","Moderate vulnerability threshold",max=2.828427,min=0,value=2),
                             numericInput("HighVuln","High vulnerability threshold",max=2.828427,min=0,value=2.2),
                             selectInput("xyChoice","Assessments to plot",choices=NULL,multiple=TRUE))),
             plotOutput("PSA_Results",width="700px",height='700px'))
  )
  
  )


write.files<-function(input,output,session,PSMatrix,bord,Quants.Matrix,split3){
  
  save.csv.name<-paste0(getwd(),"/",input$AssessmentName,"_AttVals.csv")
  save.PvS.name<-paste0(getwd(),"/",input$AssessmentName,"_PvS.png")
  save.Vuln.name<-paste0(getwd(),"/",input$AssessmentName,"_Vuln.png")
  save.AttSD.name<-paste0(getwd(),"/",input$AssessmentName,"_AttSD.png")
  save.PSV.name<-paste0(getwd(),"/",input$AssessmentName,"_PSVVals.csv")
  
  #files<-c(save.csv.name,save.PvS.name,save.Vuln.name,save.AttSD.name)
  
  write.csv(PSMatrix,file=save.csv.name)
  
  PSVMatrix<-matrix(NA,nrow=3,ncol=8)
  colnames(PSVMatrix)<-c("Attribute","mean","standard deviation","2.5 percent quantile","25 percent quantile","50 percent quantile", "75 percent quantile", "97.5 percent quantile")
  rownames(PSVMatrix)
  PSVMatrix[,1]<-c("Productivity","Susceptability","Vulnerability")
  prodMatrix<-matrix(NA,nrow=10000,ncol=11)
  suscMatrix<-matrix(NA,nrow=10000,ncol=13)
  prod.uncert<-vector(length=10)
  susc.uncert<-vector(length=12)
  sumProdWeights<-0
  quantsProd<-matrix(NA,nrow=7,ncol=10)
  for(i in 1:10){
    prodMatrix[,i]<-eval(parse(text=paste0("input$ProbPA",i,4)))*sample(c(3,2,1),10000,replace=TRUE,prob=c(eval(parse(text=paste0("input$ProbPA",i,3))),eval(parse(text=paste0("input$ProbPA",i,2))),eval(parse(text=paste0("input$ProbPA",i,1)))))
    sumProdWeights<-sumProdWeights+eval(parse(text=paste0("input$ProbPA",i,4)))
    temp.data<-sort(prodMatrix[,i])
    quantsProd[,i]<-c(temp.data[1],sum(temp.data[1:2500])/2500,sum(temp.data[1:5000])/5000,sum(temp.data)/10000,sum(temp.data[5001:10000])/5000,sum(temp.data[7501:10000])/2500,temp.data[10000])
    prod.uncert[i]<-sum(abs(eval(parse(text=paste0("input$ProbPA",i,3)))-eval(parse(text=paste0("input$ProbPA",i,2)))),
                        abs(eval(parse(text=paste0("input$ProbPA",i,3)))-eval(parse(text=paste0("input$ProbPA",i,1)))),
                        abs(eval(parse(text=paste0("input$ProbPA",i,2)))-eval(parse(text=paste0("input$ProbPA",i,1)))))/(2*sum(eval(parse(text=paste0("input$ProbPA",i,3))),eval(parse(text=paste0("input$ProbPA",i,2))),eval(parse(text=paste0("input$ProbPA",i,1)))))
  }
  
  prodMatrix[,11]<-apply(prodMatrix[,1:10],1,sum)
  prodMatrix[,11]<-prodMatrix[,11]/sumProdWeights
  
  if(min(prodMatrix[,11])==max(prodMatrix[,11])){
    ProdQuants<-rep(min(prodMatrix[,11]),5)
  }else{
    ProdDens<-density(prodMatrix[,11],adjust=1)
    ProdY<-ProdDens$y[ProdDens$x>=min(prodMatrix[,11]) & ProdDens$x<=max(prodMatrix[,11])]
    ProdX<-ProdDens$x[ProdDens$x>=min(prodMatrix[,11]) & ProdDens$x<=max(prodMatrix[,11])]
    ProdDensCum<-cumsum(ProdY/sum(ProdY))
    ProdQuants<-ProdX[c(length(ProdDensCum[ProdDensCum<=0.025]),length(ProdDensCum[ProdDensCum<=0.25]),length(ProdDensCum[ProdDensCum<=0.5]),length(ProdDensCum[ProdDensCum<=0.75]),length(ProdDensCum[ProdDensCum<=0.975]))]
  }
  meanprod<-apply(prodMatrix,2,sum)/10000
  varprod<-apply(prodMatrix,2,var)
  stdevprod<-sqrt(varprod)
  PSVMatrix[1,2:8]<-c(meanprod[11],stdevprod[11],ProdQuants[1],ProdQuants[2],ProdQuants[3],ProdQuants[4],ProdQuants[5])
  quantsSusc<-matrix(NA,nrow=7,ncol=12)
  
  sumSuscWeights<-0
  for(i in 1:12){
    suscMatrix[,i]<-eval(parse(text=paste0("input$ProbSA",i,4)))*sample(c(3,2,1),10000,replace=TRUE,prob=c(eval(parse(text=paste0("input$ProbSA",i,1))),eval(parse(text=paste0("input$ProbSA",i,2))),eval(parse(text=paste0("input$ProbSA",i,3)))))
    sumSuscWeights<-sumSuscWeights+eval(parse(text=paste0("input$ProbSA",i,4)))
    temp.data<-sort(suscMatrix[,i])
    quantsSusc[,i]<-c(temp.data[1],sum(temp.data[1:2500])/2500,sum(temp.data[1:5000])/5000,sum(temp.data)/10000,sum(temp.data[5001:10000])/5000,sum(temp.data[7501:10000])/2500,temp.data[10000])
    susc.uncert[i]<-sum(abs(eval(parse(text=paste0("input$ProbSA",i,3)))-eval(parse(text=paste0("input$ProbSA",i,2)))),
                        abs(eval(parse(text=paste0("input$ProbSA",i,3)))-eval(parse(text=paste0("input$ProbSA",i,1)))),
                        abs(eval(parse(text=paste0("input$ProbSA",i,2)))-eval(parse(text=paste0("input$ProbSA",i,1)))))/(2*sum(eval(parse(text=paste0("input$ProbSA",i,3))),eval(parse(text=paste0("input$ProbSA",i,2))),eval(parse(text=paste0("input$ProbSA",i,1)))))
  }
  suscMatrix[,13]<-apply(suscMatrix[,1:12],1,sum)
  suscMatrix[,13]<-suscMatrix[,13]/sumSuscWeights
  
  if(min(suscMatrix[,13])==max(suscMatrix[,13])){
    SuscQuants<-rep(min(suscMatrix[,13]),5)
  }else{
    SuscDens<-density(suscMatrix[,13],adjust=1)
    SuscY<-SuscDens$y[SuscDens$x>=min(suscMatrix[,13]) & SuscDens$x<=max(suscMatrix[,13])]
    SuscX<-SuscDens$x[SuscDens$x>=min(suscMatrix[,13]) & SuscDens$x<=max(suscMatrix[,13])]
    SuscDensCum<-cumsum(SuscY/sum(SuscY))
    SuscQuants<-SuscX[c(length(SuscDensCum[SuscDensCum<=0.025]),length(SuscDensCum[SuscDensCum<=0.25]),length(SuscDensCum[SuscDensCum<=0.5]),length(SuscDensCum[SuscDensCum<=0.75]),length(SuscDensCum[SuscDensCum<=0.975]))]
  }
  meansusc<-apply(suscMatrix,2,sum)/10000
  varsusc<-apply(suscMatrix,2,var)
  stdevsusc<-sqrt(varsusc)
  PSVMatrix[2,2:8]<-c(meansusc[13],stdevsusc[13],SuscQuants)
  
  vuln<-sqrt((((3-prodMatrix[,11])*(3-prodMatrix[,11]))+((suscMatrix[,13]-1)*(suscMatrix[,13]-1))))
  
  png(file=save.PvS.name, width = 700, height = 700, units = "px")
  plot(NA,xlab="Productivity",ylab="Susceptibility",xlim=c(3,1),ylim=c(1,3))
  polygon(x=c(3.1,3.1,0.9,0.9),y=c(0.9,3.1,3.1,0.9),col="red")
  polygon(x=c(3.1,seq(3.1,0.9,-0.01),0.9),y=c(1,ifelse((sqrt((input$HighVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$HighVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$HighVuln)^2))+1)<3.1,(sqrt((input$HighVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$HighVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$HighVuln)^2))+1),3.1),1),col="orange",border=NA)
  polygon(x=c(3.1,seq(3.1,0.9,-0.01),0.9),y=c(1,ifelse((sqrt((input$ModVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$ModVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$HighVuln)^2))+1)<3.1,(sqrt((input$ModVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$ModVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$ModVuln)^2))+1),3.1),1),col="yellow",border=NA)
  polygon(x=c(3.1,seq(3.1,0.9,-0.01),0.9),y=c(1,ifelse((sqrt((input$LowVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$LowVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$LowVuln)^2))+1)<3.1,(sqrt((input$LowVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$LowVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$LowVuln)^2))+1),3.1),1),col="green",border=NA)
  polygon(x=c(3.1,seq(3.1,0.9,-0.01),0.9),y=c(1,(-(ifelse((sqrt((input$HighVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$HighVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$HighVuln)^2))+1)<3.1,(sqrt((input$HighVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$HighVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$HighVuln)^2))+1),3.1))+2),1),col="orange",border=NA)
  polygon(x=c(3.1,seq(3.1,0.9,-0.01),0.9),y=c(1,(-(ifelse((sqrt((input$ModVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$ModVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$HighVuln)^2))+1)<3.1,(sqrt((input$ModVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$ModVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$ModVuln)^2))+1),3.1))+2),1),col="yellow",border=NA)
  polygon(x=c(3.1,seq(3.1,0.9,-0.01),0.9),y=c(1,(-(ifelse((sqrt((input$LowVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$LowVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$LowVuln)^2))+1)<3.1,(sqrt((input$LowVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$LowVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$LowVuln)^2))+1),3.1))+2),1),col="green",border=NA)
  
  newVal<-0
  for(i in 1:11)
  {
    newVal<-newVal+0.25
    lines(x=ifelse(seq(3.1,(3.1-newVal-0.1),-0.01)>=0.9,seq(3.1,(3.1-newVal-0.1),-0.01),0.9),y=ifelse((sqrt(newVal^2-seq(-0.1,newVal,0.01)^2)+1)<=3.1,(sqrt(newVal^2-seq(-0.1,newVal,0.01)^2)+1),3.1),lty=3,lwd=1,col="black")
    lines(x=ifelse(seq(3.1,(3.1-newVal-0.1),-0.01)>=0.9,seq(3.1,(3.1-newVal-0.1),-0.01),0.9),y=(-(ifelse((sqrt(newVal^2-seq(-0.1,newVal,0.01)^2)+1)<=3.1,(sqrt(newVal^2-seq(-0.1,newVal,0.01)^2)+1),3.1))+2),lty=3,lwd=1,col="black")
  }
  polygon(x=c(3.08,3.08,0.92,0.92),y=c(0.92,3.08,3.08,0.92),lwd=4)
  
  
  
  if(((ProdQuants[5]-ProdQuants[1])==0) | ((SuscQuants[5]-SuscQuants[1])==0)){
    bord[1]<-"black"
  }else{
    bord[1]<-NA
  }
  #draw.ellipse(x=2,y=2,a=,b=1,lty=3,lwd=3,deg=T,segment=c(0,90))
  draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[5]-ProdQuants[3],b=SuscQuants[5]-SuscQuants[3],deg=T,segment=c(0,90),arc.only=FALSE,lty=2,lwd=2,border=bord[1],col=gray(0.4,0.2))
  draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[3]-ProdQuants[1],b=SuscQuants[5]-SuscQuants[3],deg=T,segment=c(90,180),arc.only=FALSE,lty=2,lwd=2,border=bord[1],col=gray(0.4,0.2))
  draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[3]-ProdQuants[1],b=SuscQuants[3]-SuscQuants[1],deg=T,segment=c(180,270),arc.only=FALSE,lty=2,lwd=2,border=bord[1],col=gray(0.4,0.2))
  draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[5]-ProdQuants[3],b=SuscQuants[3]-SuscQuants[1],deg=T,segment=c(270,360),arc.only=FALSE,lty=2,lwd=2,border=bord[1],col=gray(0.4,0.2))
  
  draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[4]-ProdQuants[3],b=SuscQuants[4]-SuscQuants[3],deg=T,segment=c(0,90),arc.only=FALSE,lty=1,lwd=4,border=bord[1],col=gray(0.1,0.5))
  draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[3]-ProdQuants[2],b=SuscQuants[4]-SuscQuants[3],deg=T,segment=c(90,180),arc.only=FALSE,lty=1,lwd=4,border=bord[1],col=gray(0.1,0.5))
  draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[3]-ProdQuants[2],b=SuscQuants[3]-SuscQuants[2],deg=T,segment=c(180,270),arc.only=FALSE,lty=1,lwd=4,border=bord[1],col=gray(0.1,0.5))
  draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[4]-ProdQuants[3],b=SuscQuants[3]-SuscQuants[2],deg=T,segment=c(270,360),arc.only=FALSE,lty=1,lwd=4,border=bord[1],col=gray(0.1,0.5))
  
  if(length(input$xyChoice)>=1){
    
    sortingMat<-matrix(NA,nrow=length(input$xyChoice),ncol=2)
    sortingMat[1:length(input$xyChoice),1]<-1:length(input$xyChoice)
    sortingMat[1:length(input$xyChoice),2]<-match(input$xyChoice,split3)
    sortingMat<-sortingMat[order(sortingMat[,2]),,drop=FALSE]
    
    draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),5]-Quants.Matrix[match(input$xyChoice,split3),3],b=Quants.Matrix[match(input$xyChoice,split3),10]-Quants.Matrix[match(input$xyChoice,split3),8],deg=T,segment=c(0,90),arc.only=FALSE,lty=2,lwd=2,border=bord[match(input$xyChoice,split3)+1],col=gray(0.4,0.2))
    draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),3]-Quants.Matrix[match(input$xyChoice,split3),1],b=Quants.Matrix[match(input$xyChoice,split3),10]-Quants.Matrix[match(input$xyChoice,split3),8],deg=T,segment=c(90,180),arc.only=FALSE,lty=2,lwd=2,border=bord[match(input$xyChoice,split3)+1],col=gray(0.4,0.2))
    draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),3]-Quants.Matrix[match(input$xyChoice,split3),1],b=Quants.Matrix[match(input$xyChoice,split3),8]-Quants.Matrix[match(input$xyChoice,split3),6],deg=T,segment=c(180,270),arc.only=FALSE,lty=2,lwd=2,border=bord[match(input$xyChoice,split3)+1],col=gray(0.4,0.2))
    draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),5]-Quants.Matrix[match(input$xyChoice,split3),3],b=Quants.Matrix[match(input$xyChoice,split3),8]-Quants.Matrix[match(input$xyChoice,split3),6],deg=T,segment=c(270,360),arc.only=FALSE,lty=2,lwd=2,border=bord[match(input$xyChoice,split3)+1],col=gray(0.4,0.2))
    
    draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),4]-Quants.Matrix[match(input$xyChoice,split3),3],b=Quants.Matrix[match(input$xyChoice,split3),9]-Quants.Matrix[match(input$xyChoice,split3),8],deg=T,segment=c(0,90),arc.only=FALSE,lty=1,lwd=4,border=bord[match(input$xyChoice,split3)+1],col=gray(0.1,0.5))
    draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),3]-Quants.Matrix[match(input$xyChoice,split3),2],b=Quants.Matrix[match(input$xyChoice,split3),9]-Quants.Matrix[match(input$xyChoice,split3),8],deg=T,segment=c(90,180),arc.only=FALSE,lty=1,lwd=4,border=bord[match(input$xyChoice,split3)+1],col=gray(0.1,0.5))
    draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),3]-Quants.Matrix[match(input$xyChoice,split3),2],b=Quants.Matrix[match(input$xyChoice,split3),8]-Quants.Matrix[match(input$xyChoice,split3),7],deg=T,segment=c(180,270),arc.only=FALSE,lty=1,lwd=4,border=bord[match(input$xyChoice,split3)+1],col=gray(0.1,0.5))
    draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),4]-Quants.Matrix[match(input$xyChoice,split3),3],b=Quants.Matrix[match(input$xyChoice,split3),8]-Quants.Matrix[match(input$xyChoice,split3),7],deg=T,segment=c(270,360),arc.only=FALSE,lty=1,lwd=4,border=bord[match(input$xyChoice,split3)+1],col=gray(0.1,0.5))
    
    points(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],pch=16,cex=2,col="black")
    text(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],labels=substr(input$xyChoice,1,nchar(input$xyChoice)-4),pos=4,col="white")
  }
  
  points(x=ProdQuants[3],y=SuscQuants[3],pch=16,cex=2,col="Blue")
  
  dev.off()
  
  png(file=save.Vuln.name, width = 700, height = 700, units = "px")
  
  if(min(vuln)==max(vuln)){
    dens<-list()
    dens$x<-min(vuln)
    dens$y<-1
  }else{
    dens<-density(vuln,adjust=3/min(1,(max(vuln)-min(vuln))))
    dens$y<-dens$y[dens$x>=min(vuln) & dens$x<=max(vuln)]
    dens$x<-dens$x[dens$x>=min(vuln) & dens$x<=max(vuln)]
  }
  plot(x=dens$x,y=dens$y/max(dens$y),xlim=c(min(vuln)-0.1,max(vuln)+0.1),ylim=c(0,1.1),xlab="Vulnerability",ylab="Probability density",type="l",lty=1,lwd=2,col="dark blue")
  polygon(x=c(dens$x[1],dens$x,dens$x[length(dens$x)]),y=c(0,dens$y/max(dens$y),0),col="red")
  if(min(dens$x)<input$HighVuln){
    polygon(x=c(dens$x[1],dens$x[dens$x<=input$HighVuln],dens$x[length(dens$x[dens$x<=input$HighVuln])]),y=c(0,dens$y[dens$x<=input$HighVuln]/max(dens$y),0),col="orange")
  }
  if(min(dens$x)<input$LowVuln){
    polygon(x=c(dens$x[1],dens$x[dens$x<=input$LowVuln],dens$x[length(dens$x[dens$x<=input$LowVuln])]),y=c(0,dens$y[dens$x<=input$LowVuln]/max(dens$y),0),col="green")
  }
  locations<-quantile(dens$x,probs=c(0.15,0.5,0.85))
  locations<-c((min(vuln)-0.1+0.15*(max(vuln)-min(vuln)+0.2)),(min(vuln)-0.1+0.5*(max(vuln)-min(vuln)+0.2)),(min(vuln)-0.1+0.85*(max(vuln)-min(vuln)+0.2)))
  text(x=locations,y=rep(1.11,3),labels=c("Probability Low Vulnerability","Probability Moderate Vulnerability","Probability High Vulnerability"))
  text(x=locations,y=rep(1.05,3),labels=c(round(sum(dens$y[dens$x<=input$LowVuln])/sum(dens$y),2),round(sum(dens$y[dens$x>input$LowVuln & dens$x<input$HighVuln])/sum(dens$y),2),round(sum(dens$y[dens$x>=input$HighVuln])/sum(dens$y),2)))
  
  dev.off()
  
  png(file=save.AttSD.name, width = 700, height = 700, units = "px")
  
  vulnCummDen<-cumsum(dens$y)
  vulnCummDen<-vulnCummDen/vulnCummDen[length(vulnCummDen)]
  vulnQuants<-dens$x[c(length(vulnCummDen[vulnCummDen<=0.025]),length(vulnCummDen[vulnCummDen<=0.25]),length(vulnCummDen[vulnCummDen<=0.5]),length(vulnCummDen[vulnCummDen<=0.75]),length(vulnCummDen[vulnCummDen<=0.975]))]
  meanvuln<-mean(vuln,na.rm = T)
  varvuln<-var(vuln,na.rm = T)
  stdevvuln<-sqrt(varvuln)
  
  PSVMatrix[3,2:8]<-c(meanvuln,stdevvuln,vulnQuants)
  
  write.csv(PSVMatrix,file=save.PSV.name,row.names = FALSE)
  
  plot(NA,xlim=c(-1.1,5),ylim=c(0.5,24.5),xlab="",ylab="",axes=FALSE)
  
  #scale_prod<-vector(length=10)
  for(i in 1:10){
    text(x=-1.1,y=(24-i),labels=paste0("Productivity Attribute ",i),pos=4)
    text(x=3.7,y=(24-i),labels=paste0(round(prod.uncert[i],2)),pos=4)
    text(x=4.5,y=(24-i),labels=paste0(round(as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),2)),pos=4)
    #scale_prod[i]<-as.numeric(eval(parse(text=paste0("input$ProbSA",i,4))))
    #as.numeric(eval(parse(text=paste0("input$ProbSA",i,4))))
    if(as.numeric(eval(parse(text=paste0("input$ProbPA",i,4))))==0){
      
    }else if(prod.uncert[i]<=0.25){
      lines(x=c((quantsProd[2,i]),(quantsProd[6,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=6,col="red")
      lines(x=c((quantsProd[3,i]),(quantsProd[5,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=12,col="red")
      points(x=c((meanprod[i])/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4))))),y=c((24-i)),pch=16,cex=3,col="red")
    }else if(prod.uncert[i]<=0.5){
      lines(x=c((quantsProd[2,i]),(quantsProd[6,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=6,col="orange")
      lines(x=c((quantsProd[3,i]),(quantsProd[5,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=12,col="orange")
      points(x=c((meanprod[i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i)),pch=16,cex=3,col="orange")
    }else if(prod.uncert[i]<=0.75){
      lines(x=c((quantsProd[2,i]),(quantsProd[6,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=6,col="yellow")
      lines(x=c((quantsProd[3,i]),(quantsProd[5,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=12,col="yellow")
      points(x=c((meanprod[i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i)),pch=16,cex=3,col="yellow")
    }else{
      lines(x=c((quantsProd[2,i]),(quantsProd[6,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=6,col="green")
      lines(x=c((quantsProd[3,i]),(quantsProd[5,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=12,col="green")
      points(x=c((meanprod[i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i)),pch=16,cex=3,col="green")
    }
    lines(x=c(-1.2,5),y=c(((24-i)-0.5),((24-i)-0.5)),lty=1,lwd=3,col="black")
  }
  
  for(i in 1:12){
    text(x=-1.1,y=(24-(11+i)),labels=paste0("Susceptibility Attribute ",i),pos=4)
    text(x=3.7,y=(24-(11+i)),labels=paste0(round(susc.uncert[i],2)),pos=4)
    text(x=4.5,y=(24-(11+i)),labels=paste0(round(as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),2)),pos=4)
    if(as.numeric(eval(parse(text=paste0("input$ProbSA",i,4))))==0){
      
    }else if(susc.uncert[i]<=0.25){
      lines(x=c((quantsSusc[2,(i)]),(quantsSusc[6,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=6,col="red")
      lines(x=c((quantsSusc[3,(i)]),(quantsSusc[5,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=12,col="red")
      points(x=c((meansusc[i]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i))),pch=16,cex=3,col="red")
    }else if(susc.uncert[i]<=0.5){
      lines(x=c((quantsSusc[2,(i)]),(quantsSusc[6,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=6,col="orange")
      lines(x=c((quantsSusc[3,(i)]),(quantsSusc[5,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=12,col="orange")
      points(x=c((meansusc[i]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i))),pch=16,cex=3,col="orange")
    }else if(susc.uncert[i]<=0.75){
      lines(x=c((quantsSusc[2,(i)]),(quantsSusc[6,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=6,col="yellow")
      lines(x=c((quantsSusc[3,(i)]),(quantsSusc[5,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=12,col="yellow")
      points(x=c((meansusc[i]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i))),pch=16,cex=3,col="yellow")
    }else{
      lines(x=c((quantsSusc[2,(i)]),(quantsSusc[6,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=6,col="green")
      lines(x=c((quantsSusc[3,(i)]),(quantsSusc[5,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=12,col="green")
      points(x=c((meansusc[i]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i))),pch=16,cex=3,col="green")
    }
    
    lines(x=c(-1.2,5),y=c((24-(11+i)-0.5),(24-(11+i)-0.5)),lty=1,lwd=3,col="black")
  }
  
  text(x=2,y=24,cex=2,labels="Attribute Score Range, Quality, and Weight",pos=3)
  mtext(text=c("Score","Quality","Weight"),cex=1.5,side=1,at=c(2,3.85,4.6),line=1)
  #mtext(text=c("*Quality is a value between 0 (equal probability of all 3 scores) and 1 (100% probability of a single score)"),cex=1,side=1,at=c(2),line=2)
  mtext(text=c("*Quality is a value between 0 and 1 equal to the probability "),cex=1,side=1,at=c(2),line=2)
  mtext(text=c("of the most likely score minus the probability of the least likely score."),cex=1,side=1,at=c(2),line=3)
  
  lines(x=c(0.6,0.6),y=c(0.5,12.5),lty=1,lwd=3)
  lines(x=c(0.6,0.6),y=c(13.5,23.5),lty=1,lwd=3)
  lines(x=c(3.4,3.4),y=c(0.5,12.5),lty=1,lwd=3)
  lines(x=c(3.4,3.4),y=c(13.5,23.5),lty=1,lwd=3)
  lines(x=c(4.2,4.2),y=c(0.5,12.5),lty=1,lwd=3)
  lines(x=c(4.2,4.2),y=c(13.5,23.5),lty=1,lwd=3)
  polygon(x=c(-1.2,-1.2,5,5),y=c(0.5,12.5,12.5,0.5),lty=1,lwd=3)
  polygon(x=c(-1.2,-1.2,5,5),y=c(13.5,23.5,23.5,13.5),lty=1,lwd=3)
  axis(side=1,at=c(1,2,3),pos=0.5)
  
  dev.off()
}

server <- function(input, output, session) {
  
  Quants.Matrix<-matrix(NA,nrow=100,ncol=10)
  bord<-vector(length=100)
  
  curr<-getwd()
  #Read all the directories in Assessments folder
  assessments<-c("",list.files(paste(curr,"/Assessments",sep=""),recursive = FALSE))
  
  split<-strsplit(assessments,".csv")
  for(i in 1:(length(assessments)))
  {
    split[[i]]<-split[[i]][1]
  }
  
  split2<-strsplit(assessments,"djsfhfsdu")
  
  split[[1]]<-"Choose an existing assessment"
  split2[[1]]<-"DNU"
  names(split2)<-split
  
  
  updateSelectInput(session,"chooseAssessment",choices=split2)
  
  split3<-split2
  names(split3)<-split
  split3[[1]]<-NULL
  updateSelectInput(session,"xyChoice",choices=split3)
  
  PSMatrix<-matrix(NA,nrow=22,ncol=5)
  
  observeEvent(input$AssessmentName,{
  output$downloadAssessment<-downloadHandler(
    filename = paste0(input$AssessmentName,"_PSA_Results.zip"),
    content =function(file){
      true.wd<-getwd()
      setwd(paste0(true.wd,"/Downloads"))
      save.csv.name<-paste0(input$AssessmentName,"_AttVals.csv")
      save.PvS.name<-paste0(input$AssessmentName,"_PvS.png")
      save.Vuln.name<-paste0(input$AssessmentName,"_Vuln.png")
      save.AttSD.name<-paste0(input$AssessmentName,"_AttSD.png")
      save.PSV.name<-paste0(input$AssessmentName,"_PSVVals.csv")
      
      files<-c(save.csv.name,save.PvS.name,save.Vuln.name,save.AttSD.name,save.PSV.name)
      unlink(files)
      
      write.files(input,output,session,PSMatrix,bord,Quants.Matrix,split3)
      
      zip::zip(file,files)
      unlink(files)
      setwd(true.wd)
      
    },
    contentType =  "application/zip"
  )
  })
  
  observeEvent(input$xyChoice,{
    output$downloadAssessment<-downloadHandler(
      filename = paste0(input$AssessmentName,"_PSA_Results.zip"),
      content =function(file){
        true.wd<-getwd()
        setwd(paste0(true.wd,"/Downloads"))
        save.csv.name<-paste0(input$AssessmentName,"_AttVals.csv")
        save.PvS.name<-paste0(input$AssessmentName,"_PvS.png")
        save.Vuln.name<-paste0(input$AssessmentName,"_Vuln.png")
        save.AttSD.name<-paste0(input$AssessmentName,"_AttSD.png")
        save.PSV.name<-paste0(input$AssessmentName,"_PSVVals.csv")
        
        files<-c(save.csv.name,save.PvS.name,save.Vuln.name,save.AttSD.name,save.PSV.name)
        unlink(files)
        
        write.files(input,output,session,PSMatrix,bord,Quants.Matrix,split3)
        
        zip::zip(file,files)
        unlink(files)
        setwd(true.wd)
        
      },
      contentType =  "application/zip"
    )
  })
  
  observe({
    outputData<-list()
    
    for(i in 1:10){
      PSMatrix[i,]<<-c(eval(parse(text=paste0("input$ProbPA",i,3))),
                       eval(parse(text=paste0("input$ProbPA",i,2))),
                       eval(parse(text=paste0("input$ProbPA",i,1))),
                       eval(parse(text=paste0("input$ProbPA",i,4))),
                       eval(parse(text=paste0("input$ProbPA",i,5))))
    }
    
    for(i in 1:12){
      PSMatrix[(i+10),]<<-c(eval(parse(text=paste0("input$ProbSA",i,1))),
                            eval(parse(text=paste0("input$ProbSA",i,2))),
                            eval(parse(text=paste0("input$ProbSA",i,3))),
                            eval(parse(text=paste0("input$ProbSA",i,4))),
                            eval(parse(text=paste0("input$ProbSA",i,5))))
    }
    
    colnames(PSMatrix)<<-c("Probability Score = High (3)","Probability Score = Moderate (2)","Probability Score = Low (1)","Weight","Comments")
    rownames(PSMatrix)<<-c(paste0("Productivity attribute ",1:10),paste0("Susceptibility attribute ",1:12))
  })
  
  observe({
    
    for(i in 1:10){
      for(j in 1:4){
        if(is.na(eval(parse(text=paste0("input$ProbPA",i,j))))){
          updateNumericInput(session=session,inputId = paste0("ProbPA",i,j),value=0)
        }
      }
      
      if(!is.na(eval(parse(text=paste0("input$ProbPA",i,1)))) && !is.na(eval(parse(text=paste0("input$ProbPA",i,2)))) && !is.na(eval(parse(text=paste0("input$ProbPA",i,3))))){
      if(eval(parse(text=paste0("input$ProbPA",i,1)))==0 && eval(parse(text=paste0("input$ProbPA",i,2)))==0 && eval(parse(text=paste0("input$ProbPA",i,3)))==0){
        updateNumericInput(session=session,inputId = paste0("ProbPA",i,1),value=1)
        updateNumericInput(session=session,inputId = paste0("ProbPA",i,2),value=1)
        updateNumericInput(session=session,inputId = paste0("ProbPA",i,3),value=1)
      }}
    }
    
    for(i in 1:12){
      for(j in 1:4){
        if(is.na(eval(parse(text=paste0("input$ProbSA",i,j))))){
          updateNumericInput(session=session,inputId = paste0("ProbSA",i,j),value=0)
        }
      }
      
      if(!is.na(eval(parse(text=paste0("input$ProbSA",i,1)))) && !is.na(eval(parse(text=paste0("input$ProbSA",i,2)))) && !is.na(eval(parse(text=paste0("input$ProbSA",i,3))))){
      if(eval(parse(text=paste0("input$ProbSA",i,1)))==0 && eval(parse(text=paste0("input$ProbSA",i,2)))==0 && eval(parse(text=paste0("input$ProbSA",i,3)))==0){
        updateNumericInput(session=session,inputId = paste0("ProbSA",i,1),value=1)
        updateNumericInput(session=session,inputId = paste0("ProbSA",i,2),value=1)
        updateNumericInput(session=session,inputId = paste0("ProbSA",i,3),value=1)
      }}
    }
    
  })
  
  observeEvent(input$chooseAssessment,{
    if(input$chooseAssessment!="DNU"){
      updateTextInput(session,"AssessmentName",value=strsplit(input$chooseAssessment,".csv")[1])
      inputData<-read.csv(file=paste0(getwd(),"/Assessments/",input$chooseAssessment),header=TRUE,row.names = 1)
      
      for(i in 1:10){
        updateNumericInput(session,paste0("ProbPA",i,3),value=inputData[i,1])
        updateNumericInput(session,paste0("ProbPA",i,2),value=inputData[i,2])
        updateNumericInput(session,paste0("ProbPA",i,1),value=inputData[i,3])
        updateNumericInput(session,paste0("ProbPA",i,4),value=inputData[i,4])
        updateNumericInput(session,paste0("ProbPA",i,5),value=inputData[i,5])
      }
      
      for(i in 1:12){
        updateNumericInput(session,paste0("ProbSA",i,1),value=inputData[(i+10),1])
        updateNumericInput(session,paste0("ProbSA",i,2),value=inputData[(i+10),2])
        updateNumericInput(session,paste0("ProbSA",i,3),value=inputData[(i+10),3])
        updateNumericInput(session,paste0("ProbSA",i,4),value=inputData[(i+10),4])
        updateNumericInput(session,paste0("ProbSA",i,5),value=inputData[(i+10),5])
      }
    }
  })
  
  observeEvent(input$saveAssessment,{
    write.csv(PSMatrix,file=paste0(getwd(),"/Assessments/",input$AssessmentName,".csv"))
    
    curr<-getwd()
    #Read all the directories in Assessments folder
    assessments<-c("",list.files(paste(curr,"/Assessments",sep=""),recursive = FALSE))
    
    split<-strsplit(assessments,".csv")
    for(i in 1:(length(assessments)))
    {
      split[[i]]<-split[[i]][1]
    }
    
    split2<-strsplit(assessments,"djsfhfsdu")
    
    split[[1]]<-"Choose an existing assessment"
    split2[[1]]<-"DNU"
    names(split2)<-split
    
    updateSelectInput(session,"chooseAssessment",choices=split2)
    
    split3<-split2
    names(split3)<-split
    split3[[1]]<-NULL
    split3<<-split3
    updateSelectInput(session,"xyChoice",choices=split3)
  })
  
 
  output$PSA_Results <- renderPlot({
    prodMatrix<-matrix(NA,nrow=10000,ncol=11)
    suscMatrix<-matrix(NA,nrow=10000,ncol=13)
    prod.uncert<-vector(length=10)
    susc.uncert<-vector(length=12)
    sumProdWeights<-0
    quantsProd<-matrix(NA,nrow=7,ncol=10)
    for(i in 1:10){
      prodMatrix[,i]<-eval(parse(text=paste0("input$ProbPA",i,4)))*sample(c(3,2,1),10000,replace=TRUE,prob=c(eval(parse(text=paste0("input$ProbPA",i,3))),eval(parse(text=paste0("input$ProbPA",i,2))),eval(parse(text=paste0("input$ProbPA",i,1)))))
      sumProdWeights<-sumProdWeights+eval(parse(text=paste0("input$ProbPA",i,4)))
      temp.data<-sort(prodMatrix[,i])
      quantsProd[,i]<-c(temp.data[1],sum(temp.data[1:2500])/2500,sum(temp.data[1:5000])/5000,sum(temp.data)/10000,sum(temp.data[5001:10000])/5000,sum(temp.data[7501:10000])/2500,temp.data[10000])
      prod.uncert[i]<-sum(abs(eval(parse(text=paste0("input$ProbPA",i,3)))-eval(parse(text=paste0("input$ProbPA",i,2)))),
                          abs(eval(parse(text=paste0("input$ProbPA",i,3)))-eval(parse(text=paste0("input$ProbPA",i,1)))),
                          abs(eval(parse(text=paste0("input$ProbPA",i,2)))-eval(parse(text=paste0("input$ProbPA",i,1)))))/(2*sum(eval(parse(text=paste0("input$ProbPA",i,3))),eval(parse(text=paste0("input$ProbPA",i,2))),eval(parse(text=paste0("input$ProbPA",i,1)))))
    }
    
    prodMatrix[,11]<-apply(prodMatrix[,1:10],1,sum)
    prodMatrix[,11]<-prodMatrix[,11]/sumProdWeights
    
    if(min(prodMatrix[,11])==max(prodMatrix[,11])){
      ProdQuants<-rep(min(prodMatrix[,11]),5)
    }else{
      ProdDens<-density(prodMatrix[,11],adjust=1)
      ProdY<-ProdDens$y[ProdDens$x>=min(prodMatrix[,11]) & ProdDens$x<=max(prodMatrix[,11])]
      ProdX<-ProdDens$x[ProdDens$x>=min(prodMatrix[,11]) & ProdDens$x<=max(prodMatrix[,11])]
      ProdDensCum<-cumsum(ProdY/sum(ProdY))
      ProdQuants<-ProdX[c(length(ProdDensCum[ProdDensCum<=0.025]),length(ProdDensCum[ProdDensCum<=0.25]),length(ProdDensCum[ProdDensCum<=0.5]),length(ProdDensCum[ProdDensCum<=0.75]),length(ProdDensCum[ProdDensCum<=0.975]))]
    }
    meanprod<-apply(prodMatrix,2,sum)/10000
    varprod<-apply(prodMatrix,2,var)
    stdevprod<-sqrt(varprod)
    quantsSusc<-matrix(NA,nrow=7,ncol=12)
    
    sumSuscWeights<-0
    for(i in 1:12){
      suscMatrix[,i]<-eval(parse(text=paste0("input$ProbSA",i,4)))*sample(c(3,2,1),10000,replace=TRUE,prob=c(eval(parse(text=paste0("input$ProbSA",i,1))),eval(parse(text=paste0("input$ProbSA",i,2))),eval(parse(text=paste0("input$ProbSA",i,3)))))
      sumSuscWeights<-sumSuscWeights+eval(parse(text=paste0("input$ProbSA",i,4)))
      temp.data<-sort(suscMatrix[,i])
      quantsSusc[,i]<-c(temp.data[1],sum(temp.data[1:2500])/2500,sum(temp.data[1:5000])/5000,sum(temp.data)/10000,sum(temp.data[5001:10000])/5000,sum(temp.data[7501:10000])/2500,temp.data[10000])
      susc.uncert[i]<-sum(abs(eval(parse(text=paste0("input$ProbSA",i,3)))-eval(parse(text=paste0("input$ProbSA",i,2)))),
                          abs(eval(parse(text=paste0("input$ProbSA",i,3)))-eval(parse(text=paste0("input$ProbSA",i,1)))),
                          abs(eval(parse(text=paste0("input$ProbSA",i,2)))-eval(parse(text=paste0("input$ProbSA",i,1)))))/(2*sum(eval(parse(text=paste0("input$ProbSA",i,3))),eval(parse(text=paste0("input$ProbSA",i,2))),eval(parse(text=paste0("input$ProbSA",i,1)))))
    }
    suscMatrix[,13]<-apply(suscMatrix[,1:12],1,sum)
    suscMatrix[,13]<-suscMatrix[,13]/sumSuscWeights
    
    if(min(suscMatrix[,13])==max(suscMatrix[,13])){
      SuscQuants<-rep(min(suscMatrix[,13]),5)
    }else{
      SuscDens<-density(suscMatrix[,13],adjust=1)
      SuscY<-SuscDens$y[SuscDens$x>=min(suscMatrix[,13]) & SuscDens$x<=max(suscMatrix[,13])]
      SuscX<-SuscDens$x[SuscDens$x>=min(suscMatrix[,13]) & SuscDens$x<=max(suscMatrix[,13])]
      SuscDensCum<-cumsum(SuscY/sum(SuscY))
      SuscQuants<-SuscX[c(length(SuscDensCum[SuscDensCum<=0.025]),length(SuscDensCum[SuscDensCum<=0.25]),length(SuscDensCum[SuscDensCum<=0.5]),length(SuscDensCum[SuscDensCum<=0.75]),length(SuscDensCum[SuscDensCum<=0.975]))]
    }
    meansusc<-apply(suscMatrix,2,sum)/10000
    varsusc<-apply(suscMatrix,2,var)
    stdevsusc<-sqrt(varsusc)
    
    vuln<-sqrt((((3-prodMatrix[,11])*(3-prodMatrix[,11]))+((suscMatrix[,13]-1)*(suscMatrix[,13]-1))))
    
    if(input$plotType==1){
      showElement("xyChoice")
      plot(NA,xlab="Productivity",ylab="Susceptibility",xlim=c(3,1),ylim=c(1,3))
      polygon(x=c(3.1,3.1,0.9,0.9),y=c(0.9,3.1,3.1,0.9),col="red")
      polygon(x=c(3.1,seq(3.1,0.9,-0.01),0.9),y=c(1,ifelse((sqrt((input$HighVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$HighVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$HighVuln)^2))+1)<3.1,(sqrt((input$HighVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$HighVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$HighVuln)^2))+1),3.1),1),col="orange",border=NA)
      polygon(x=c(3.1,seq(3.1,0.9,-0.01),0.9),y=c(1,ifelse((sqrt((input$ModVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$ModVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$HighVuln)^2))+1)<3.1,(sqrt((input$ModVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$ModVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$ModVuln)^2))+1),3.1),1),col="yellow",border=NA)
      polygon(x=c(3.1,seq(3.1,0.9,-0.01),0.9),y=c(1,ifelse((sqrt((input$LowVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$LowVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$LowVuln)^2))+1)<3.1,(sqrt((input$LowVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$LowVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$LowVuln)^2))+1),3.1),1),col="green",border=NA)
      polygon(x=c(3.1,seq(3.1,0.9,-0.01),0.9),y=c(1,(-(ifelse((sqrt((input$HighVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$HighVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$HighVuln)^2))+1)<3.1,(sqrt((input$HighVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$HighVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$HighVuln)^2))+1),3.1))+2),1),col="orange",border=NA)
      polygon(x=c(3.1,seq(3.1,0.9,-0.01),0.9),y=c(1,(-(ifelse((sqrt((input$ModVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$ModVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$HighVuln)^2))+1)<3.1,(sqrt((input$ModVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$ModVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$ModVuln)^2))+1),3.1))+2),1),col="yellow",border=NA)
      polygon(x=c(3.1,seq(3.1,0.9,-0.01),0.9),y=c(1,(-(ifelse((sqrt((input$LowVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$LowVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$LowVuln)^2))+1)<3.1,(sqrt((input$LowVuln)^2-ifelse((seq(-0.1,2.1,0.01)^2)<=(input$LowVuln)^2,(seq(-0.1,2.1,0.01)^2),(input$LowVuln)^2))+1),3.1))+2),1),col="green",border=NA)
      
      newVal<-0
      for(i in 1:11)
      {
        newVal<-newVal+0.25
        lines(x=ifelse(seq(3.1,(3.1-newVal-0.1),-0.01)>=0.9,seq(3.1,(3.1-newVal-0.1),-0.01),0.9),y=ifelse((sqrt(newVal^2-seq(-0.1,newVal,0.01)^2)+1)<=3.1,(sqrt(newVal^2-seq(-0.1,newVal,0.01)^2)+1),3.1),lty=3,lwd=1,col="black")
        lines(x=ifelse(seq(3.1,(3.1-newVal-0.1),-0.01)>=0.9,seq(3.1,(3.1-newVal-0.1),-0.01),0.9),y=(-(ifelse((sqrt(newVal^2-seq(-0.1,newVal,0.01)^2)+1)<=3.1,(sqrt(newVal^2-seq(-0.1,newVal,0.01)^2)+1),3.1))+2),lty=3,lwd=1,col="black")
      }
      polygon(x=c(3.08,3.08,0.92,0.92),y=c(0.92,3.08,3.08,0.92),lwd=4)
      
      
      
      if(((ProdQuants[5]-ProdQuants[1])==0) | ((SuscQuants[5]-SuscQuants[1])==0)){
        bord[1]<-"black"
      }else{
        bord[1]<-NA
      }
      draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[5]-ProdQuants[3],b=SuscQuants[5]-SuscQuants[3],deg=T,segment=c(0,90),arc.only=FALSE,lty=2,lwd=2,border=bord[1],col=gray(0.4,0.2))
      draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[3]-ProdQuants[1],b=SuscQuants[5]-SuscQuants[3],deg=T,segment=c(90,180),arc.only=FALSE,lty=2,lwd=2,border=bord[1],col=gray(0.4,0.2))
      draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[3]-ProdQuants[1],b=SuscQuants[3]-SuscQuants[1],deg=T,segment=c(180,270),arc.only=FALSE,lty=2,lwd=2,border=bord[1],col=gray(0.4,0.2))
      draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[5]-ProdQuants[3],b=SuscQuants[3]-SuscQuants[1],deg=T,segment=c(270,360),arc.only=FALSE,lty=2,lwd=2,border=bord[1],col=gray(0.4,0.2))
      
      draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[4]-ProdQuants[3],b=SuscQuants[4]-SuscQuants[3],deg=T,segment=c(0,90),arc.only=FALSE,lty=1,lwd=4,border=bord[1],col=gray(0.1,0.5))
      draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[3]-ProdQuants[2],b=SuscQuants[4]-SuscQuants[3],deg=T,segment=c(90,180),arc.only=FALSE,lty=1,lwd=4,border=bord[1],col=gray(0.1,0.5))
      draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[3]-ProdQuants[2],b=SuscQuants[3]-SuscQuants[2],deg=T,segment=c(180,270),arc.only=FALSE,lty=1,lwd=4,border=bord[1],col=gray(0.1,0.5))
      draw.ellipse(x=ProdQuants[3],y=SuscQuants[3],a=ProdQuants[4]-ProdQuants[3],b=SuscQuants[3]-SuscQuants[2],deg=T,segment=c(270,360),arc.only=FALSE,lty=1,lwd=4,border=bord[1],col=gray(0.1,0.5))
      
      if(length(input$xyChoice)>=1){
        
        sortingMat<-matrix(NA,nrow=length(input$xyChoice),ncol=2)
        sortingMat[1:length(input$xyChoice),1]<-1:length(input$xyChoice)
        sortingMat[1:length(input$xyChoice),2]<-match(input$xyChoice,split3)
        sortingMat<-sortingMat[order(sortingMat[,2]),,drop=FALSE]
        for(j in sortingMat[,1])
        {
          print(input$xyChoice[j])
          inputData.temp<-read.csv(file=paste0(getwd(),"/Assessments/",input$xyChoice[j]),header=TRUE,row.names = 1)
          prodMatrix.temp<-matrix(NA,nrow=10000,ncol=11)
          suscMatrix.temp<-matrix(NA,nrow=10000,ncol=13)
          sumProdWeights.temp<-0
          for(i in 1:10){
            prodMatrix.temp[,i]<-inputData.temp[i,4]*sample(c(3,2,1),10000,replace=TRUE,prob=c(inputData.temp[i,1:3]))
            sumProdWeights.temp<-sumProdWeights.temp+inputData.temp[i,4]
          }
          
          prodMatrix.temp[,11]<-apply(prodMatrix.temp[,1:10],1,sum)
          prodMatrix.temp[,11]<-prodMatrix.temp[,11]/sumProdWeights.temp
          
          if(min(prodMatrix.temp[,11])==max(prodMatrix.temp[,11])){
            ProdQuants.temp<-rep(min(prodMatrix.temp[,11]),5)
          }else{
            ProdDens.temp<-density(prodMatrix.temp[,11],adjust=1)
            ProdY.temp<-ProdDens.temp$y[ProdDens.temp$x>=min(prodMatrix.temp[,11]) & ProdDens.temp$x<=max(prodMatrix.temp[,11])]
            ProdX.temp<-ProdDens.temp$x[ProdDens.temp$x>=min(prodMatrix.temp[,11]) & ProdDens.temp$x<=max(prodMatrix.temp[,11])]
            ProdDensCum.temp<-cumsum(ProdY.temp/sum(ProdY.temp))
            ProdQuants.temp<-ProdX.temp[c(length(ProdDensCum.temp[ProdDensCum.temp<=0.025]),length(ProdDensCum.temp[ProdDensCum.temp<=0.25]),length(ProdDensCum.temp[ProdDensCum.temp<=0.5]),length(ProdDensCum.temp[ProdDensCum.temp<=0.75]),length(ProdDensCum.temp[ProdDensCum.temp<=0.975]))]
          }
          
          sumSuscWeights.temp<-0
          for(i in 1:12){
            suscMatrix.temp[,i]<-inputData.temp[(i+10),4]*sample(c(3,2,1),10000,replace=TRUE,prob=c(inputData.temp[(i+10),1:3]))
            sumSuscWeights.temp<-sumSuscWeights.temp+inputData.temp[(i+10),4]
          }
          suscMatrix.temp[,13]<-apply(suscMatrix.temp[,1:12],1,sum)
          suscMatrix.temp[,13]<-suscMatrix.temp[,13]/sumSuscWeights.temp
          
          if(min(suscMatrix.temp[,13])==max(suscMatrix.temp[,13])){
            SuscQuants.temp<-rep(min(suscMatrix.temp[,13]),5)
          }else{
            SuscDens.temp<-density(suscMatrix.temp[,13],adjust=1)
            SuscY.temp<-SuscDens.temp$y[SuscDens.temp$x>=min(suscMatrix.temp[,13]) & SuscDens.temp$x<=max(suscMatrix.temp[,13])]
            SuscX.temp<-SuscDens.temp$x[SuscDens.temp$x>=min(suscMatrix.temp[,13]) & SuscDens.temp$x<=max(suscMatrix.temp[,13])]
            SuscDensCum.temp<-cumsum(SuscY.temp/sum(SuscY.temp))
            SuscQuants.temp<-SuscX.temp[c(length(SuscDensCum.temp[SuscDensCum.temp<=0.025]),length(SuscDensCum.temp[SuscDensCum.temp<=0.25]),length(SuscDensCum.temp[SuscDensCum.temp<=0.5]),length(SuscDensCum.temp[SuscDensCum.temp<=0.75]),length(SuscDensCum.temp[SuscDensCum.temp<=0.975]))]
          }
          
          Quants.Matrix[which(split3==input$xyChoice[j]),1:5]<<-ProdQuants.temp
          Quants.Matrix[which(split3==input$xyChoice[j]),6:10]<<-SuscQuants.temp
          
          if(((Quants.Matrix[which(split3==input$xyChoice[j]),5]-Quants.Matrix[which(split3==input$xyChoice[j]),1])==0) | ((Quants.Matrix[which(split3==input$xyChoice[j]),10]-Quants.Matrix[which(split3==input$xyChoice[j]),6])==0)){
            bord[which(split3==input$xyChoice[j])+1]<<-"black"
          }else{
            bord[which(split3==input$xyChoice[j])+1]<<-NA
          }
        }
        
        draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),5]-Quants.Matrix[match(input$xyChoice,split3),3],b=Quants.Matrix[match(input$xyChoice,split3),10]-Quants.Matrix[match(input$xyChoice,split3),8],deg=T,segment=c(0,90),arc.only=FALSE,lty=2,lwd=2,border=bord[match(input$xyChoice,split3)+1],col=gray(0.4,0.2))
        draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),3]-Quants.Matrix[match(input$xyChoice,split3),1],b=Quants.Matrix[match(input$xyChoice,split3),10]-Quants.Matrix[match(input$xyChoice,split3),8],deg=T,segment=c(90,180),arc.only=FALSE,lty=2,lwd=2,border=bord[match(input$xyChoice,split3)+1],col=gray(0.4,0.2))
        draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),3]-Quants.Matrix[match(input$xyChoice,split3),1],b=Quants.Matrix[match(input$xyChoice,split3),8]-Quants.Matrix[match(input$xyChoice,split3),6],deg=T,segment=c(180,270),arc.only=FALSE,lty=2,lwd=2,border=bord[match(input$xyChoice,split3)+1],col=gray(0.4,0.2))
        draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),5]-Quants.Matrix[match(input$xyChoice,split3),3],b=Quants.Matrix[match(input$xyChoice,split3),8]-Quants.Matrix[match(input$xyChoice,split3),6],deg=T,segment=c(270,360),arc.only=FALSE,lty=2,lwd=2,border=bord[match(input$xyChoice,split3)+1],col=gray(0.4,0.2))

        draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),4]-Quants.Matrix[match(input$xyChoice,split3),3],b=Quants.Matrix[match(input$xyChoice,split3),9]-Quants.Matrix[match(input$xyChoice,split3),8],deg=T,segment=c(0,90),arc.only=FALSE,lty=1,lwd=4,border=bord[match(input$xyChoice,split3)+1],col=gray(0.1,0.5))
        draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),3]-Quants.Matrix[match(input$xyChoice,split3),2],b=Quants.Matrix[match(input$xyChoice,split3),9]-Quants.Matrix[match(input$xyChoice,split3),8],deg=T,segment=c(90,180),arc.only=FALSE,lty=1,lwd=4,border=bord[match(input$xyChoice,split3)+1],col=gray(0.1,0.5))
        draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),3]-Quants.Matrix[match(input$xyChoice,split3),2],b=Quants.Matrix[match(input$xyChoice,split3),8]-Quants.Matrix[match(input$xyChoice,split3),7],deg=T,segment=c(180,270),arc.only=FALSE,lty=1,lwd=4,border=bord[match(input$xyChoice,split3)+1],col=gray(0.1,0.5))
        draw.ellipse(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],a=Quants.Matrix[match(input$xyChoice,split3),4]-Quants.Matrix[match(input$xyChoice,split3),3],b=Quants.Matrix[match(input$xyChoice,split3),8]-Quants.Matrix[match(input$xyChoice,split3),7],deg=T,segment=c(270,360),arc.only=FALSE,lty=1,lwd=4,border=bord[match(input$xyChoice,split3)+1],col=gray(0.1,0.5))

        points(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],pch=16,cex=2,col="black")
        text(x=Quants.Matrix[match(input$xyChoice,split3),3],y=Quants.Matrix[match(input$xyChoice,split3),8],labels=substr(input$xyChoice,1,nchar(input$xyChoice)-4),pos=4,col="white")
      }
      points(x=ProdQuants[3],y=SuscQuants[3],pch=16,cex=2,col="Blue")
      
    }else if(input$plotType==2){
      hideElement("xyChoice")
      if(min(vuln)==max(vuln)){
        dens<-list()
        dens$x<-min(vuln)
        dens$y<-1
      }else{
        dens<-density(vuln,adjust=3/min(1,(max(vuln)-min(vuln))))
        dens$y<-dens$y[dens$x>=min(vuln) & dens$x<=max(vuln)]
        dens$x<-dens$x[dens$x>=min(vuln) & dens$x<=max(vuln)]
      }
      plot(x=dens$x,y=dens$y/max(dens$y),xlim=c(min(vuln)-0.1,max(vuln)+0.1),ylim=c(0,1.1),xlab="Vulnerability",ylab="Probability density",type="l",lty=1,lwd=2,col="dark blue")
      polygon(x=c(dens$x[1],dens$x,dens$x[length(dens$x)]),y=c(0,dens$y/max(dens$y),0),col="red")
      if(min(dens$x)<input$HighVuln){
        polygon(x=c(dens$x[1],dens$x[dens$x<=input$HighVuln],dens$x[length(dens$x[dens$x<=input$HighVuln])]),y=c(0,dens$y[dens$x<=input$HighVuln]/max(dens$y),0),col="orange")
      }
      if(min(dens$x)<input$LowVuln){
        polygon(x=c(dens$x[1],dens$x[dens$x<=input$LowVuln],dens$x[length(dens$x[dens$x<=input$LowVuln])]),y=c(0,dens$y[dens$x<=input$LowVuln]/max(dens$y),0),col="green")
      }
      locations<-quantile(dens$x,probs=c(0.15,0.5,0.85))
      locations<-c((min(vuln)-0.1+0.15*(max(vuln)-min(vuln)+0.2)),(min(vuln)-0.1+0.5*(max(vuln)-min(vuln)+0.2)),(min(vuln)-0.1+0.85*(max(vuln)-min(vuln)+0.2)))
      text(x=locations,y=rep(1.11,3),labels=c("Probability Low Vulnerability","Probability Moderate Vulnerability","Probability High Vulnerability"))
      text(x=locations,y=rep(1.05,3),labels=c(round(sum(dens$y[dens$x<=input$LowVuln])/sum(dens$y),2),round(sum(dens$y[dens$x>input$LowVuln & dens$x<input$HighVuln])/sum(dens$y),2),round(sum(dens$y[dens$x>=input$HighVuln])/sum(dens$y),2)))
    }else if(input$plotType==3){
      hideElement("xyChoice")
      plot(NA,xlim=c(-1.1,5),ylim=c(0.5,24.5),xlab="",ylab="",axes=FALSE)
      
      #scale_prod<-vector(length=10)
      for(i in 1:10){
        text(x=-1.1,y=(24-i),labels=paste0("Productivity Attribute ",i),pos=4)
        text(x=3.7,y=(24-i),labels=paste0(round(prod.uncert[i],2)),pos=4)
        text(x=4.5,y=(24-i),labels=paste0(round(as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),2)),pos=4)
        #scale_prod[i]<-as.numeric(eval(parse(text=paste0("input$ProbSA",i,4))))
        #as.numeric(eval(parse(text=paste0("input$ProbSA",i,4))))
        if(as.numeric(eval(parse(text=paste0("input$ProbPA",i,4))))==0){
          
        }else if(prod.uncert[i]<=0.25){
          lines(x=c((quantsProd[2,i]),(quantsProd[6,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=6,col="red")
          lines(x=c((quantsProd[3,i]),(quantsProd[5,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=12,col="red")
          points(x=c((meanprod[i])/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4))))),y=c((24-i)),pch=16,cex=3,col="red")
        }else if(prod.uncert[i]<=0.5){
          lines(x=c((quantsProd[2,i]),(quantsProd[6,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=6,col="orange")
          lines(x=c((quantsProd[3,i]),(quantsProd[5,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=12,col="orange")
          points(x=c((meanprod[i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i)),pch=16,cex=3,col="orange")
        }else if(prod.uncert[i]<=0.75){
          lines(x=c((quantsProd[2,i]),(quantsProd[6,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=6,col="yellow")
          lines(x=c((quantsProd[3,i]),(quantsProd[5,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=12,col="yellow")
          points(x=c((meanprod[i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i)),pch=16,cex=3,col="yellow")
        }else{
          lines(x=c((quantsProd[2,i]),(quantsProd[6,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=6,col="green")
          lines(x=c((quantsProd[3,i]),(quantsProd[5,i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i),(24-i)),lty=1,lwd=12,col="green")
          points(x=c((meanprod[i]))/as.numeric(eval(parse(text=paste0("input$ProbPA",i,4)))),y=c((24-i)),pch=16,cex=3,col="green")
        }
        lines(x=c(-1.2,5),y=c(((24-i)-0.5),((24-i)-0.5)),lty=1,lwd=3,col="black")
      }
      
      for(i in 1:12){
        text(x=-1.1,y=(24-(11+i)),labels=paste0("Susceptibility Attribute ",i),pos=4)
        text(x=3.7,y=(24-(11+i)),labels=paste0(round(susc.uncert[i],2)),pos=4)
        text(x=4.5,y=(24-(11+i)),labels=paste0(round(as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),2)),pos=4)
        if(as.numeric(eval(parse(text=paste0("input$ProbSA",i,4))))==0){
          
        }else if(susc.uncert[i]<=0.25){
          lines(x=c((quantsSusc[2,(i)]),(quantsSusc[6,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=6,col="red")
          lines(x=c((quantsSusc[3,(i)]),(quantsSusc[5,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=12,col="red")
          points(x=c((meansusc[i]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i))),pch=16,cex=3,col="red")
        }else if(susc.uncert[i]<=0.5){
          lines(x=c((quantsSusc[2,(i)]),(quantsSusc[6,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=6,col="orange")
          lines(x=c((quantsSusc[3,(i)]),(quantsSusc[5,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=12,col="orange")
          points(x=c((meansusc[i]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i))),pch=16,cex=3,col="orange")
        }else if(susc.uncert[i]<=0.75){
          lines(x=c((quantsSusc[2,(i)]),(quantsSusc[6,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=6,col="yellow")
          lines(x=c((quantsSusc[3,(i)]),(quantsSusc[5,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=12,col="yellow")
          points(x=c((meansusc[i]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i))),pch=16,cex=3,col="yellow")
        }else{
          lines(x=c((quantsSusc[2,(i)]),(quantsSusc[6,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=6,col="green")
          lines(x=c((quantsSusc[3,(i)]),(quantsSusc[5,(i)]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i)),(24-(11+i))),lty=1,lwd=12,col="green")
          points(x=c((meansusc[i]))/as.numeric(eval(parse(text=paste0("input$ProbSA",i,4)))),y=c((24-(11+i))),pch=16,cex=3,col="green")
        }
        
        lines(x=c(-1.2,5),y=c((24-(11+i)-0.5),(24-(11+i)-0.5)),lty=1,lwd=3,col="black")
      }
      
      text(x=2,y=24,cex=2,labels="Attribute Score Range, Quality, and Weight",pos=3)
      mtext(text=c("Score","Quality","Weight"),cex=1.5,side=1,at=c(2,3.85,4.6),line=1)
      #mtext(text=c("*Quality is a value between 0 (equal probability of all 3 scores) and 1 (100% probability of a single score)"),cex=1,side=1,at=c(2),line=2)
      mtext(text=c("*Quality is a value between 0 and 1 equal to the probability "),cex=1,side=1,at=c(2),line=2)
      mtext(text=c("of the most likely score minus the probability of the least likely score."),cex=1,side=1,at=c(2),line=3)
      
      lines(x=c(0.6,0.6),y=c(0.5,12.5),lty=1,lwd=3)
      lines(x=c(0.6,0.6),y=c(13.5,23.5),lty=1,lwd=3)
      lines(x=c(3.4,3.4),y=c(0.5,12.5),lty=1,lwd=3)
      lines(x=c(3.4,3.4),y=c(13.5,23.5),lty=1,lwd=3)
      lines(x=c(4.2,4.2),y=c(0.5,12.5),lty=1,lwd=3)
      lines(x=c(4.2,4.2),y=c(13.5,23.5),lty=1,lwd=3)
      polygon(x=c(-1.2,-1.2,5,5),y=c(0.5,12.5,12.5,0.5),lty=1,lwd=3)
      polygon(x=c(-1.2,-1.2,5,5),y=c(13.5,23.5,23.5,13.5),lty=1,lwd=3)
      axis(side=1,at=c(1,2,3),pos=0.5)
    }
  })
}

shinyApp(ui = ui, server = server)

