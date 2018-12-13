###COTE SERVEUR
###INSTALLATION DES PACKAGES
#install.packages(c("shiny","shinydashboard","FactoMineR","ade4","factoextra","magrittr","ggplot2","gclus","RColorBrewer","vegan","clustsig","labdsv","shinycssloaders","stringr","sp","gstat","DT","colourpicker","shinyalert","lmtest","pwr","PMCMR","ngram","spdep"))

###CHARGEMENT DES LIBRARIES
x<-c("shiny","shinydashboard","FactoMineR","ade4","factoextra","magrittr","ggplot2","gclus","RColorBrewer","vegan","clustsig","labdsv","shinycssloaders","stringr","sp","gstat","DT","colourpicker","shinyalert","lmtest","pwr","PMCMR","ngram","spdep")
lapply(x, require, character.only = TRUE)

shinyServer(
  function(input,output,session){
    data <- reactive({
      file1 <- input$file
      if(is.null(file1)){return()}
      read.table(file=file1$datapath,
                 sep=input$sep,
                 dec=input$dec,
                 header = input$header)
    })
    shinyalert(
      title = "Bienvenue sur l'appli stats de Bioinfo",
      text = "Bonne utilisation!",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "success",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
    
    ###
    output$filedf <- renderTable({
      if(is.null(data())){return()}
      input$file
    })
    ####
    output$sum <- renderPrint({
      if(is.null(data())){return()}
      summary(data())
    })
    ####
    output$table <- DT::renderDataTable({
      if(is.null(data())){return()}
      data()
    })
   
    choicesnbvar<-c("1","2","plus de 2")
    output$selectnbvar<-renderUI({
      if(is.null(data())){return()}
      selectInput("nbvar",label = "Nombre de variables à visualiser",choices = choicesnbvar)
    })
    ####
    output$selectx <-renderUI({
      if(is.null(data())){return()}
      selectInput("x",label = "Axe x",choices = names(data()))
    })
    output$selecty<-renderUI({
      if(is.null(data())){return()}
      selectInput("y",label = "Axe y",choices = names(data()))
    })
    ####
    output$selectgraph<-renderUI({
      if(is.null(data())){return()}
      else{
        listgraph<-c("")
        if(input$nbvar=="1"){
          try(
            if(is.numeric(data()[,input$x])|is.double(data()[,input$x])){
              listgraph<-c("Diagramme en barre","Boxplot","Histogramme")
            }
            else{
              listgraph<-c("Diagramme circulaire","Diagramme en barre")
            }
          )
        }
        else if(input$nbvar=="2"){
          try(
            if((is.numeric(data()[,input$x])|is.double(data()[,input$x]))&(is.numeric(data()[,input$y])|is.double(data()[,input$y]))){
              listgraph<-c("Nuage de points","Ligne","Diagramme en barre","Histogramme","Boxplot")
            }
            else if((is.numeric(data()[,input$x])|is.double(data()[,input$x]))|(is.numeric(data()[,input$y])|is.double(data()[,input$y]))){
              listgraph<-c("Boxplot")
            }
            else{
              listgraph<-c("")
            }
          )
        }
        else{
          listgraph<-c("Matrice de scatter plot")
        }
        selectInput("Graph",label = "Type of graph",choices = listgraph)
      }
    })
    ####
    output$plot<-renderPlot({
      if(is.null(data())){return()}
      else{
        if(input$Graph=="Diagramme circulaire"){
          pie(table(data()[,input$x]))
        }
        else if(input$Graph=="Nuage de points"){
          plot(data()[,input$x],data()[,input$y],xlab = input$x,ylab = input$y)
        }
        else if(input$Graph=="Ligne"){
          plot(data()[,input$x],data()[,input$y],type = "o",xlab = input$x,ylab = input$y)
        }
        else if(input$Graph=="Matrice de scatter plot"){
          ind.quant<-sapply(data(),is.numeric)
          pairs(data()[,ind.quant])
        }
        else if(input$Graph=="Diagramme en barre"){
          if(input$nbvar=="1"){
            if(is.numeric(data()[,input$x])|is.double(data()[,input$x])){
              barplot(data()[,input$x],xlab = input$x,ylab="Fréquences")
            }
            else{
              barplot(table(data()[,input$x]),xlab = input$x,ylab = "Fréquences")
            }
          }
          else{
            dataset<-table(data()[,input$x],data()[,input$y])
            barplot(dataset,xlab = input$x,ylab = input$y)
          }
        }
        else if(input$Graph=="Boxplot"){
          if(input$nbvar=="1"){
            boxplot(data()[,input$x],xlab=input$x,ylab="Fréquences")
          }
          else if((is.numeric(data()[,input$x])|is.double(data()[,input$x]))&(is.numeric(data()[,input$y])|is.double(data()[,input$y]))){
            boxplot(data()[,input$x]~data()[,input$y],xlab=input$x,ylab=input$y)
          }
          else{
            if(is.numeric(data()[,input$x])|is.double(data()[,input$x])){
              boxplot(data()[,input$x]~data()[,input$y],xlab=input$x,ylab=input$y)
            }
            else{
              boxplot(data()[,input$y]~data()[,input$x],xlab=input$x,ylab=input$y)
            }
          }
        }
        else if(input$Graph=="Histogramme"){
          if(input$nbvar=="1"){
            hist(data()[,input$x],xlab = input$x,ylab = "Fréquences")
          }
          else{-
            x<-data()[,input$x]
            y<-data()[,input$y]
            hist(x,col =rgb(1,0,0,0.5))
            hist(y,add=T,col =rgb(0,0,1,0.5))
          }
        }
        else{
          return()
        }
      }
    })
    
    ####PAGE PRINCIPALE#####
    output$tb <- renderUI({
      if(is.null(data())){
        h3("Powered by",tags$img(src='R.png',height="60%",width="60%"))
      }
      else{
        tabsetPanel(
          tabPanel("A propos",tableOutput("filedf")),
          tabPanel("Les données",DT::dataTableOutput("table")),
          tabPanel("Résumé",verbatimTextOutput("sum")),
          tabPanel("Graph",plotOutput("plot"))
        )
      }
    })
    
    ###########################ANALYSES UNIVARIÉES ###############################
    
    ###PRINCIPE###
    output$PrincipleRL<-renderUI({
      img(src='RelierVar.jpg',height='60%',width='100%')
    })
    
    #####RÉGRESSION LINÉAIRE#####
    output$VariablesRL1<-renderUI({
      if(is.null(data())){return()}
      selectInput("VarExplicative",label = "Variable explicative",choices = names(data()))
    })
    
    output$VariablesRL2<-renderUI({
      if(is.null(data())){return()}
      selectInput("VarAExpliquer",label = "Variable à expliquer",choices = names(data()))
    })
    
    ###GRAPHE DES CONDITIONS D'APPLICATIONS
    VarExpli<-reactive({
      input$VarExplicative
    })
    
    VarAExpli<-reactive({
      input$VarAExpliquer
    })
    
    stockageRL<-reactive({
      lm(reformulate(input$VarAExpliquer,input$VarExplicative),data())
      #lm(as.formula(paste(input$VarAExpliquer," ~ ",paste(input$VarExplicative))),data=data())
    })
    
    output$CARL<-renderPlot({
      par(mfrow=c(2,2))
      plot(stockageRL())
    })
    
    #### CREATION DU MODELE LINEAIRE ####
    
    creerModele <- reactive({
      lm(as.formula(paste(input$VarAExpliquer," ~ ",paste(input$VarExplicative,collapse="+"))),data=data())
    })
    
    creerModele1 <- reactive({
      aov(as.formula(paste(input$VarAExpliquer," ~ ",paste(input$VarExplicative,collapse="+"))),data=data())
    })
    
    
    ### CONDITIONS D'APPLICATIONS POUR TESTS UNIVARIES ###
    
    ### Durbun-Watson ###
    output$DWtest <- renderPrint ({
      dwtest(creerModele())
    })
    
    ### Goldfeld-Quandt ###
    output$GQtest <- renderPrint({
      gqtest(creerModele())
    })
    
    ### Shapiro-Wilk ###
    output$SWtest <- renderPrint({
      shapiro.test(creerModele()$res)
    })
    
    xx<-reactive({
      input$VarAExpliquer
    })
    
    xRL<-reactive({
      data()[,xx()]
    })
    
    yy<-reactive({
      input$VarExplicative
    })
    
    yRL<-reactive({
      data()[,yy()]
    })
    
    ####CORRÉLATION DE BRAVAIS PEARSON
    output$corBV<-renderPrint({
      cor.test(yRL(),xRL(), method = "pearson")
    })
    
    ###CORRÉLATION DE SPEARMEAN
    output$corSP<-renderPrint({
    cor.test(yRL(),xRL(), method = "spearman")
    }) 
    
    ###CORRÉLATION DE KENDALL
    output$corKD<-renderPrint({
      cor.test(yRL(),xRL(), method = "kendall")
    }) 
    
    ###PUISSANCE DU TEST
    output$powerRL<-renderPrint({
      pwr.r.test(r=0.5,n=20,sig.level=0.05,alternative="two.sided")
    })
    
    ###RÉSUMÉ DE LA REGRESSION LINÉAIRE
    output$summaryRL1<-renderPrint({
      summary(creerModele())
    })
    
    ###RÉSUMÉ DE LA REGRESSION LINÉAIRE AVEC CA PAS RESPECTÉES
    output$summaryRL2<-renderPrint({
      summary(creerModele())
    })
    
    #### REGRESSION LINEAIRE #### 
    output$regLin1 <- renderPlot({
      plot(data()[,input$VarAExpliquer],data()[,input$VarExplicative],xlab = input$VarAExpliquer,ylab = input$VarExplicative)
      abline(creerModele(),col ="red", lwd=2)
    })
    
    #### REGRESSION LINEAIRE #### 
    output$regLin2 <- renderPlot({
      plot(data()[,input$VarAExpliquer],data()[,input$VarExplicative],xlab = input$VarAExpliquer,ylab = input$VarExplicative)
      abline(creerModele(),col ="red", lwd=2)
    })
    
    ###TEST POST HOC
    output$phTukey<-renderPrint({
      TukeyHSD(aov(yRL()~xRL(),data()))
    })
    
    ###TEST POST HOC SI CA PAS RESPECTÉES
    output$phNemenyi<-renderPrint({
      posthoc.kruskal.nemenyi.test(xRL(),yRL())
    })
    
    #### CORRELATION NON LINEAIRE ####
    correl <- reactive ({
      type <- switch(input$method,
                     Pearson = "pearson",
                     Spearman = "spearman",
                     Kendall = "kendall")
      
      round(cor(cbind(data()), method = type, use = "pairwise.complete.obs"),3)
    })    
    
    
    ##################REGRESSION LINÉRAIRE MULTIIPLE########################
    
    ###PRINCIPE###
    output$PrincipleRLM<-renderUI({
      img(src='RelierVar.jpg',height='60%',width='100%')
    })
    
    ###VAR EXPLICATIVE
    output$VariablesRLM1<-renderUI({
      if(is.null(data())){return()}
      selectInput("VarExplicativeRLM",label = "Variable explicative",choices = names(data()))
    })
    
    ###NBE DE VAR À EXPLIQUER
    choicesnbvarRLM<-c("1","2 ou plus")
    output$selectnbvarRLM<-renderUI({
      if(is.null(data())){return()}
      selectInput("nbvarRLM",label = "Nombre de variables à expliquer",choices = choicesnbvarRLM)
    })
    
    
    output$VariablesRLM2<-renderUI({
      if(is.null(data())){return()}
      else if(input$nbvarRLM==1){ 
      selectInput("VarAExpliquer1RLM",label = "Variable à expliquer",choices = names(data()))
      }
      else if(input$nbvarRLM=="2 ou plus"){ 
        selectInput("VarAExpliquer2RLM",label = "Variable à expliquer",choices = names(data()),multiple = TRUE)
      }
    })
    
    ###STOCKAGE DE L'ANOVA
    creerModeleRLM <- reactive({
      if(input$nbvarRLM==1){ 
      lm(as.formula(paste(input$VarExplicativeRLM," ~ ",paste(input$VarAExpliquer1RLM,collapse="+"))),data=data())
      }
      else if(input$nbvarRLM=="2 ou plus"){
        lm(as.formula(paste(input$VarExplicativeRLM," ~ ",manipRLM(),collapse="+")),data=data())
      }
      })
    
    ###STOCKAGE POUR BOXPLOT - EFFET SEUL
    stockageBoxplotRLM1<-reactive({
    as.formula(paste(input$VarExplicativeRLM," ~ ",paste(input$VarAExpliquer1RLM,collapse="+")))
    })
    
    stockageBoxplotRLM2<-reactive({
      as.formula(paste(input$VarExplicativeRLM," ~ ",manipRLM(),collapse="+"))
    })
    
    ###COMPTER LE NBRE DE MOTS DANS LA RÉPONSE
      nbWords<-reactive({
        wordcount(input$VarAExpliquer2RLM, sep = " ", count.function = sum)
    })
    
    manipRLM<-reactive({
      paste(input$VarAExpliquer2RLM,sep="", collapse="+")
    })
    

    ####CONDITIONS D'APPLICATIONS
    ### Durbun-Watson ###
    output$DWtestRLM <- renderPrint ({
      dwtest(creerModeleRLM())
    })
    
    ### Goldfeld-Quandt ###
    output$GQtestRLM <- renderPrint({
      gqtest(creerModeleRLM())
    })
    
    ### Shapiro-Wilk ###
    output$SWtestRLM <- renderPrint({
      shapiro.test(creerModeleRLM()$res)
    })
    
    ###GRAPHE DES CONDITIONS D'APPLICATIONS
    output$CARLM<-renderPlot({
      par(mfrow=c(2,2))
      plot(creerModeleRLM())
    })
    
    ###BOXPLOT POUR VISUALISER LES VARIABLES
    output$boxplotRLM2<-renderPlot({
      boxplot(stockageBoxplotRLM2(),data(), main="Effet combiné")
    })
    
    ###BOXPLOT POUR VISUALISER LES VARIABLES
    output$boxplotRLM1<-renderPlot({
      boxplot(stockageBoxplotRLM1(),data(), main="Effet seul")
    })
    
    ###RÉSUMÉ DE L'ANOVA
    output$summaryAN1<-renderPrint({
      anova(creerModeleRLM())
    })
    
    ###RÉSUMÉ DU TEST
    output$summaryRLM1<-renderPrint({
      summary(creerModeleRLM())
    })
    
    ###GRAPHE
    output$RLMPlot<-renderPlot({
        plot(data()[,input$VarAExpliquer1RLM],data()[,input$VarExplicativeRLM],xlab = input$VarAExpliquer1RLM,ylab = input$VarExplicativeRLM)
    })
    
    ###TEST TUKEY
    output$phTukeyRLM<-renderPrint({
      TukeyHSD(aov(stockageBoxplotRLM2(),data()))
    })
    
    
    ###############ANOVA MULTIFACTORIELLE#####################
    
    ###PRINCIPE###
    output$PrincipleANOVAMF<-renderUI({
      img(src='RelierVar.jpg',height='60%',width='100%')
    })
    
    ###VAR EXPLICATIVE
    output$VariablesANOVA1<-renderUI({
      if(is.null(data())){return()}
      selectInput("VarExplicativeANOVA",label = "Variable explicative",choices = names(data()))
    })
    
    ###NBE DE VAR À EXPLIQUER
    choicesnbvarANOVA<-c("1","2 ou plus")
    output$selectnbvarANOVA<-renderUI({
      if(is.null(data())){return()}
      selectInput("nbvarANOVA",label = "Nombre de variables à expliquer",choices = choicesnbvarANOVA)
    })
    
    
    output$VariablesANOVA2<-renderUI({
      if(is.null(data())){return()}
      else if(input$nbvarANOVA==1){ 
        selectInput("VarAExpliquer1ANOVA",label = "Variable à expliquer",choices = names(data()))
      }
      else if(input$nbvarANOVA=="2 ou plus"){ 
        selectInput("VarAExpliquer2ANOVA",label = "Variable à expliquer",choices = names(data()),multiple = TRUE)
      }
    })
    
    ###STOCKAGE DE L'ANOVA
    creerModeleANOVA <- reactive({
      if(input$nbvarANOVA==1){ 
        lm(as.formula(paste(input$VarExplicativeANOVA," ~ ",paste(input$VarAExpliquer1ANOVA,collapse="+"))),data=data())
      }
      else if(input$nbvarANOVA=="2 ou plus"){
        lm(as.formula(paste(input$VarExplicativeANOVA," ~ ",manipANOVA(),collapse="+")),data=data())
      }
    })
    
    ###STOCKAGE POUR BOXPLOT - EFFET SEUL
    stockageBoxplotANOVA1<-reactive({
      as.formula(paste(input$VarExplicativeANOVA," ~ ",paste(input$VarAExpliquer1ANOVA,collapse="+")))
    })
    
    stockageBoxplotANOVA2<-reactive({
      as.formula(paste(input$VarExplicativeANOVA," ~ ",manipANOVA(),collapse="+"))
    })
    
    ###COMPTER LE NBRE DE MOTS DANS LA RÉPONSE
    nbWords<-reactive({
      wordcount(input$VarAExpliquer2ANOVA, sep = " ", count.function = sum)
    })
    
    manipANOVA<-reactive({
      paste(input$VarAExpliquer2ANOVA,sep="", collapse="*")
    })
    
    ####CONDITIONS D'APPLICATIONS
    ### DurbIn-Watson ###
    output$DWtestANOVA <- renderPrint ({
      dwtest(creerModeleANOVA())
    })
    
    ### Goldfeld-Quandt ###
    output$GQtestANOVA <- renderPrint({
      gqtest(creerModeleANOVA())
    })
    
    ### Shapiro-Wilk ###
    output$SWtestANOVA <- renderPrint({
      shapiro.test(creerModeleANOVA()$res)
    })
    
    ###GRAPHE DES CONDITIONS D'APPLICATIONS
    output$CAANOVA<-renderPlot({
      par(mfrow=c(2,2))
      plot(creerModeleANOVA())
    })
    
    ###BOXPLOT POUR VISUALISER LES VARIABLES
    output$boxplotANOVA2<-renderPlot({
      boxplot(stockageBoxplotANOVA2(),data(), main="Effet combiné")
    })
    
    ###BOXPLOT POUR VISUALISER LES VARIABLES
    output$boxplotANOVA1<-renderPlot({
      boxplot(stockageBoxplotANOVA1(),data(), main="Effet seul")
    })
    
    ###RÉSUMÉ DE L'ANOVA
    output$summaryANOVA1<-renderPrint({
      anova(creerModeleANOVA())
    })
    
    ###RÉSUMÉ DU TEST
    output$summaryANOVAMF1<-renderPrint({
      summary(creerModeleANOVA())
    })
    
    ###GRAPHE
    output$ANOVAPlot<-renderPlot({
      #plot(data()[,input$VarAExpliquer1RLM],data()[,input$VarExplicativeRLM],xlab = input$VarAExpliquer1RLM,ylab = input$VarExplicativeRLM)
    })
    
    ###TEST TUKEY
    output$phTukeyANOVA<-renderPrint({
      TukeyHSD(aov(stockageBoxplotANOVA2(),data()))
    })
    
  
    ####-----ANALYSE DE GROUPEMENT----####
    
    ###CHOIX DES VARIABLES À NE PAS CONSERVER POUR L'ANALYSE
    output$selectVarAG<-renderUI({
      numericInput("VarAG",NA,NA)
    })
    
    output$selectVarAG1<-renderUI({
      numericInput("VarAG1",NA,NA)
    })
    
    output$selectVarAG2<-renderUI({
      numericInput("VarAG2","Intervalles de colonnes à supprimer",NA)
    })
    
    output$selectVarAG3<-renderUI({
      numericInput("VarAG3",NA,NA)
    })
    
    ###RÉCUPÉRATION DES COLONNES - UTILISATEUR MET 0 S'IL A TROP DE NUMERICINPUT
    valueVarAG<-reactive({
      input$VarAG
    })
    valueVarAG1<-reactive({
      input$VarAG1
    })
    valueVarAG2<-reactive({
      input$VarAG2
    })
    valueVarAG3<-reactive({
      input$VarAG3
    })
    
    choixAG <-reactive({
      data()[,-c(valueVarAG(),valueVarAG1(),valueVarAG2():valueVarAG3())] #FAUT TRANSFORMER POUR QUE UTILISATEUR SAISISSE LES COLONNES QU'IL NE VEUT PAS 
    })
    
    ### BOXPLOT DES DONNÉES BRUTES
    output$DBAG <- renderPlot({
      boxplot(choixAG())
    })
    
    ###BOXPLOT DES DONNÉES TRANSFORMÉES
    output$DTAG  <- renderPlot({
      boxplot(transformationAG())
    })
    
    ###APERÇU DES DONNÉES
    output$dataAG <- renderPlot({
      table.value(choixAG())
    })
  
    ###TRANSFORMATION DES DATA
    #Transformation des données
    transformationAG <- reactive({
      if (input$selectAG==1){
        try(scale(choixAG()))
      }
      else if (input$selectAG==2){
        log1p(choixAG())
      }
      else if (input$selectAG==3){
        sqrt(choixAG())
      }
      else if (input$selectAG==4){
        sqrt(sqrt(choixAG()))
      }
      else if (input$selectAG==5){
        choixAG()
      }
    })
    
    output$selectMatrix<-renderUI({
      if(input$selectMode==1 & input$selectType==1){
        selectInput("selectMatrixAG", NA ,                        
                    c("Distance Euclidienne" = 1, "Distance Bray Curtis" = 2,
                      "Distance Chi-deux" = 3))
      }      
      else if(input$selectMode==2 & input$selectType==1){
        selectInput("selectMatrixAGIndirect", NA ,
                    c("Corrélation Pearson" = 1, "Corrélation Kendall" = 2,"Corrélation Spearman"=3,
                      "Distance Chi-deux" = 4))
      }
      else if(input$selectMode==1 | input$selectMode==2 & input$selectType==2 ){
        selectInput("selectMatrixAGBinaires", NA ,
                    c("Dissimilarité de simple concordance" = 1, "Dissimilarité de Jacquard" = 2,
                      "Dissimilarité de Sorenssen" = 3))
      }
    })
    
    ###CHOIX DE LA MATRICE DE DISSIMILARITÉ---MODE DIRECT
    matriceAG <- reactive({
      if (input$selectMatrixAG==1 & input$selectAG==1){
        vegdist(scale(choixAG()),method="euclidean")
      }
      else if (input$selectMatrixAG==1 & input$selectAG==5){
        vegdist(choixAG(),method="euclidean")
      }
      else if (input$selectMatrixAG==2 & input$selectAG==2){
        vegdist(log1p(choixAG()),method="bray")
      }
      else if (input$selectMatrixAG==2 & input$selectAG==3){
        vegdist(sqrt(choixAG()),method="bray")
      }
      else if (input$selectMatrixAG==2 & input$selectAG==4){
        vegdist(sqrt(sqrt(choixAG())),method="bray")
      }
      else if (input$selectMatrixAG==2 & input$selectAG==5){
        vegdist(choixAG(),method="bray")
      }
      else if (input$selectMatrixAG==3 & input$selectAG==2){
        dist(decostand(log1p(choixAG()),'chi.square'),method='euclidean')
      }
      else if (input$selectMatrixAG==3 & input$selectAG==3){
        dist(decostand(sqrt(choixAG()),'chi.square'),method='euclidean')
      }
      else if (input$selectMatrixAG==3 & input$selectAG==4){
        dist(decostand(sqrt(sqrt(choixAG())),'chi.square'),method='euclidean')
      }
      else if (input$selectMatrixAG==3 & input$selectAG==5){
        dist(decostand(choixAG(),'chi.square'),method='euclidean')
      }
    })
    
    ###CHOIX DE LA MATRICE DE DISSIMILARITÉ---MODE INDIRECT
    matriceAGIndirect <- reactive({
      if (input$selectMatrixAGIndirect==1 & input$selectAG==1){
        1-abs(as.dist(cor(scale(choixAG()),method="pearson")))
      }
      else if (input$selectMatrixAGIndirect==1 & input$selectAG==5){
        1-abs(as.dist(cor(choixAG(),method="pearson")))
      }
      else if (input$selectMatrixAGIndirect==2 & input$selectAG==1){
        1-abs(as.dist(cor(scale(choixAG()),method="kendall")))
      }
      else if (input$selectMatrixAGIndirect==2 & input$selectAG==5){
        1-abs(as.dist(cor(choixAG(),method="kendall")))
      }
      else if (input$selectMatrixAGIndirect==3 & input$selectAG==1){
        1-abs(as.dist(cor(scale(choixAG()),method="spearman")))
      }
      else if (input$selectMatrixAGIndirect==3 & input$selectAG==5){
        1-abs(as.dist(cor(choixAG(),method="spearman")))
      }
      else if (input$selectMatrixAGIndirect==4 & input$selectAG==2){
        dist(decostand(log1p(choixAG()),'chi.square'),method='euclidean')
      }
      else if (input$selectMatrixAGIndirect==4 & input$selectAG==3){
        dist(decostand(sqrt(choixAG()),'chi.square'),method='euclidean')
      }
      else if (input$selectMatrixAGIndirect==4 & input$selectAG==4){
        dist(decostand(sqrt(sqrt(choixAG())),'chi.square'),method='euclidean')
      }
      else if (input$selectMatrixAGIndirect==4 & input$selectAG==5){
        dist(decostand(choixAG(),'chi.square'),method='euclidean')
      }
    })
    
    ###CHOIX DE LA MATRICE DE DISSIMILARITÉ---MODE DIRECT BINAIRES
    matriceAGBinaires <- reactive({
      if (input$selectMatrixAGBinaires==1){
        dist.binary(choixAG(),method = 2)
      }
      else if (input$selectMatrixAGBinaires==2){
        dist.binary(choixAG(),method = 1)
      }
      else if (input$selectMatrixAGBinaires==3){
        dist.binary(choixAG(),method = 5)
      }
    })
    
    ###CHOIX DE LA MATRICE DE DISSIMILARITÉ---MODE INDIRECT BINAIRES
    matriceAGIndirectBinaires <- reactive({
      if (input$selectMatrixAGIndirectBinaires==1){
        dist.binary(t(choixAG()),method = 2)
      }
      else if (input$selectMatrixAGIndirectBinaires==2){
        dist.binary(t(choixAG()),method = 1)
      }
      else if (input$selectMatrixAGIndirectBinaires==3){
        dist.binary(t(choixAG()),method = 5)
      }
    })
    
    ###LIEN SIMPLE
    lienSimple <- reactive({
      if (input$selectMode==1 & input$selectType==1){
        hclust(matriceAG(), method="single")
      }
      else if(input$selectMode==2 & input$selectType==1){
        hclust(matriceAGIndirect(), method="single")
      }
      else if(input$selectMode==1 & input$selectType==2){
        hclust(matriceAGBinaires(), method="single")
      }
      else if(input$selectMode==2 & input$selectType==2){
        hclust(matriceAGIndirectBinaires(), method="single")
      }
    })
    
    ###GRAPHE LIEN SIMPLE
    output$grapheLienSimple <- renderPlot({
      plot(lienSimple(), hang=-1, main="Liens simples", xlab="", ylab="")
    }) 
    
    ###LIENS COMPLETS
    lienComplet <- reactive({
      if (input$selectMode==1 & input$selectType==1){
      cah.complete<-hclust(matriceAG(), method="complete")
      }
      else if(input$selectMode==2 & input$selectType==1){
      cah.complete<-hclust(matriceAGIndirect(), method="complete")  
      }
      else if(input$selectMode==1 & input$selectType==2){
      cah.complete<-hclust(matriceAGBinaires(), method="complete")
      }
      else if(input$selectMode==2 & input$selectType==2){
      cah.complete<-hclust(matriceAGIndirectBinaires(), method="complete")
      }
    })
    
    ###GRAPHE LIEN COMPLET
    output$grapheLienComplet <- renderPlot({
      plot(lienComplet(), hang=-1, main="Liens complets", xlab="", ylab="")
    }) 
    
    ###LIENS COMPLETS
    lienUPGMA <- reactive({
      if (input$selectMode==1 & input$selectType==1){
      cah.UPGMA<-hclust(matriceAG(), method="average")
      }
      else if(input$selectMode==2 & input$selectType==1){
      cah.UPGMA<-hclust(matriceAGIndirect(), method="average")
      }
      else if(input$selectMode==1 & input$selectType==2){
      cah.UPGMA<-hclust(matriceAGBinaires(), method="average")
      }
      else if(input$selectMode==2 & input$selectType==2){
      cah.UPGMA<-hclust(matriceAGIndirectBinaires(), method="average")
      }
    })
    
    ###GRAPHE LIEN UPGMA
    output$grapheLienUPGMA <- renderPlot({
      plot(lienUPGMA(), hang=-1, main="Liens UPGMA", xlab="", ylab="")
    }) 
    
    ###RECUPÉRATION DE LA DISTANCE
    diss<-reactive({
      matriceAG()
    })
    
    ###RECUPÉRATION VALEUR MATRICE COPHÉNÉTIQUE
    copheneticAGSingle<-reactive({
      cophenetic(hclust(diss(), method="single")) 
    })
    
    copheneticAGComplete<-reactive({
      cophenetic(hclust(diss(), method="complete"))
    })
    
    copheneticAGAverage<-reactive({
      cophenetic(hclust(diss(), method="average")) 
    })
    
    ###STOCKAGE RESULTAT DES MATRICES
    resultAGSingle<-reactive({
      cor(diss(),copheneticAGSingle())
    })
    
    resultAGComplete<-reactive({
      cor(diss(),copheneticAGComplete())
    })
    
    resultAGAverage<-reactive({
      cor(diss(),copheneticAGAverage())
    })
    
    ###RÉSULTAT DE LA MATRICE COPHÉNÉTIQUE
    output$resultMatrixCopheneticSingle<-renderText({
      resultAGSingle()
    })
    output$resultMatrixCopheneticComplete<-renderText({
      resultAGComplete()
    })
    output$resultMatrixCopheneticAverage<-renderText({
      resultAGAverage()
    })
    
    ###AFFICHER LA HEATMAP EN FONCTION DU GRAPHE SÉLECTIONNÉ  
    spechwo<-reactive({
      if (input$selectAGGraphes==1){
        reorder.hclust(lienSimple(),matriceAG())
      }
      else if(input$selectAGGraphes==2){
        reorder.hclust(lienComplet(),matriceAG())  
      }
      else if(input$selectAGGraphes==3){
        reorder.hclust(lienUPGMA(),matriceAG()) 
      }
    })
   
    or<-reactive({
      or<-vegemite(choixAG(),spechwo(),scale="log",zero="-")
    })
    
    output$heatmapAG<-renderPlot({
      heatmap(t(choixAG()[rev(or()$species)]), col=c("white", brewer.pal(9,"Greens")),scale="col", margin=c(4,4), ylab="", xlab="")
    })
    
    ###TEST SIMPROF
    simprofPlot<-reactive({
      simprof(data=transformationAG(),num.expected=1000, num.simulated=999,method.cluster="average",method.distance="braycurtis",sample.orientation="row")
    })
    
    output$simprofPlot<-renderPlot({
      simprof.plot(simprofPlot())
    })
    
    ###CUT TREE
    cutTree<-reactive({
      cutree(simprofPlot$hclust,input$valueCutTree)
    })
    
    ###TEST INDVAL , + IL EST FORT + IL EST CARACTÉRISTIQUE DU GROUPE
    indval<-reactive({
      indval(log1p(choixAG()),cutree(simprofPlot()$hclust,6))
    })
    
    gr<-reactive({
      indval()$maxcls[indval()$pval<=0.05]
    })
    
    iv<-reactive({
      indval()$indcls[indval()$pval<=0.05]
    })
    
    pv<-reactive({
      indval()$pval[indval()$pval<=0.05]
    })
    
    tabIV<-reactive({
      data.frame(group=gr(),indval=iv(),pvalue=pv())
    })
    
    ###AFFICHAGE POUR LES TABLEAUX AVEC LES ESPÈCES SIGNIFICATIVES INDICATRICES, APPARTENANCE ET LA PVALUE
    output$indVal<-renderText({
      tabIV()
    })
    
    
    ####-----ANALYSE DE PROXIMITÉ----####
    output$selectMatrixAP<-renderUI({
      if(input$selectModeAP==1 & input$selectTypeAP==1){
        selectInput("selectMatrixAP", NA ,                        
                    c("Distance Euclidienne" = 1, "Distance Bray Curtis" = 2,
                      "Distance Chi-deux" = 3))
      }      
      else if(input$selectModeAP==2 & input$selectTypeAP==1){
        selectInput("selectMatrixAPIndirect", NA ,
                    c("Corrélation Pearson" = 1, "Corrélation Kendall" = 2,"Corrélation Spearman"=3,
                      "Distance Chi-deux" = 4))
      }
      else if(input$selectModeAP==1 | input$selectModeAP==2 & input$selectTypeAP==2 ){
        selectInput("selectMatrixAPBinaires", NA ,
                    c("Dissimilarité de simple concordance" = 1, "Dissimilarité de Jacquard" = 2,
                      "Dissimilarité de Sorenssen" = 3))
      }
    })
    
    ###CHOIX DES VARIABLES À NE PAS CONSERVER POUR L'ANALYSE
    output$selectVarAP<-renderUI({
      numericInput("VarAP",NA,NA)
    })
    
    output$selectVarAP1<-renderUI({
      numericInput("VarAP1",NA,NA)
    })
    
    output$selectVarAP2<-renderUI({
      numericInput("VarAP2","Intervalles de colonnes à supprimer",NA)
    })
    
    output$selectVarAP3<-renderUI({
      numericInput("VarAP3",NA,NA)
    })
    
    ###RÉCUPÉRATION DES COLONNES - UTILISATEUR MET 0 S'IL A TROP DE NUMERICINPUT
    valueVarAP<-reactive({
      input$VarAP
    })
    valueVarAP1<-reactive({
      input$VarAP1
    })
    valueVarAP2<-reactive({
      input$VarAP2
    })
    valueVarAP3<-reactive({
      input$VarAP3
    })
    
    choixAP <-reactive({
      data()[,-c(valueVarAP(),valueVarAP1(),valueVarAP2():valueVarAP3())]  
    })
    
    ### BOXPLOT DES DONNÉES BRUTES
    output$DBAP <- renderPlot({
      boxplot(choixAP())
    })
    
    ###BOXPLOT DES DONNÉES TRANSFORMÉES
    output$DTAP  <- renderPlot({
      boxplot(transformationAP())
    })
    
    ###APERÇU DES DONNÉES
    output$dataAP <- renderPlot({
      table.value(choixAP())
    })
    
    ###TRANSFORMATION DES DATA
    #Transformation des données
    transformationAP <- reactive({
      if (input$selectAP==1){
        Batrf <- scale(choixAP())
      }
      else if (input$selectAP==2){
        Batrf2 <-log1p(choixAP())
      }
      else if (input$selectAP==3){
        Batrf3 <-sqrt(choixAP())
      }
      else if (input$selectAP==4){
        Batrf4 <- sqrt(sqrt(choixAP()))
      }
      else if (input$selectAP==5){
        choixAP()
      }
    })
    
    ###CHOIX DE LA MATRICE DE DISSIMILARITÉ---MODE DIRECT
    matriceAP <- reactive({
      if (input$selectMatrixAP==1 & input$selectAP==1){
        vegdist(scale(choixAP()),method="euclidean")
      }
      else if (input$selectMatrixAP==1 & input$selectAP==5){
        vegdist(choixAP(),method="euclidean")
      }
      else if (input$selectMatrixAP==2 & input$selectAP==2){
        vegdist(log1p(choixAP()),method="bray")
      }
      else if (input$selectMatrixAP==2 & input$selectAP==3){
        vegdist(sqrt(choixAP()),method="bray")
      }
      else if (input$selectMatrixAP==2 & input$selectAP==4){
        vegdist(sqrt(sqrt(choixAP())),method="bray")
      }
      else if (input$selectMatrixAG==2 & input$selectAG==5){
        vegdist(choixAP(),method="bray")
      }
      else if (input$selectMatrixAP==3 & input$selectAP==2){
        dist(decostand(log1p(choixAP()),'chi.square'),method='euclidean')
      }
      else if (input$selectMatrixAP==3 & input$selectAP==3){
        dist(decostand(sqrt(choixAP()),'chi.square'),method='euclidean')
      }
      else if (input$selectMatrixAP==3 & input$selectAP==4){
        dist(decostand(sqrt(sqrt(choixAP())),'chi.square'),method='euclidean')
      }
      else if (input$selectMatrixAP==3 & input$selectAP==5){
        dist(decostand(choixAP(),'chi.square'),method='euclidean')
      }
    })
    
    ###CHOIX DE LA MATRICE DE DISSIMILARITÉ---MODE INDIRECT
    matriceAPIndirect <- reactive({
      if (input$selectMatrixAPIndirect==1 & input$selectAP==1){
        1-abs(as.dist(cor(scale(choixAP()),method="pearson")))
      }
      else if (input$selectMatrixAPIndirect==1 & input$selectAP==5){
        1-abs(as.dist(cor(choixAP(),method="pearson")))
      }
      else if (input$selectMatrixAPIndirect==2 & input$selectAP==1){
        1-abs(as.dist(cor(scale(choixAP()),method="kendall")))
      }
      else if (input$selectMatrixAPIndirect==2 & input$selectAP==5){
        1-abs(as.dist(cor(choixAP(),method="kendall")))
      }
      else if (input$selectMatrixAPIndirect==3 & input$selectAP==1){
        1-abs(as.dist(cor(scale(choixAP()),method="spearman")))
      }
      else if (input$selectMatrixAPIndirect==3 & input$selectAP==5){
        1-abs(as.dist(cor(choixAP(),method="spearman")))
      }
      else if (input$selectMatrixAPIndirect==4 & input$selectAP==2){
        dist(decostand(log1p(choixAP()),'chi.square'),method='euclidean')
      }
      else if (input$selectMatrixAPIndirect==4 & input$selectAP==3){
        dist(decostand(sqrt(choixAP()),'chi.square'),method='euclidean')
      }
      else if (input$selectMatrixAPIndirect==4 & input$selectAP==4){
        dist(decostand(sqrt(sqrt(choixAP())),'chi.square'),method='euclidean')
      }
      else if (input$selectMatrixAPIndirect==4 & input$selectAP==5){
        dist(decostand(choixAP(),'chi.square'),method='euclidean')
      }
    })
    
    ###CHOIX DE LA MATRICE DE DISSIMILARITÉ---MODE DIRECT BINAIRES
    matriceAPBinaires <- reactive({
      if (input$selectMatrixAPBinaires==1){
        dist.binary(choixAP(),method = 2)
      }
      else if (input$selectMatrixAPBinaires==2){
        dist.binary(choixAP(),method = 1)
      }
      else if (input$selectMatrixAPBinaires==3){
        dist.binary(choixAP(),method = 5)
      }
    })
    
    ###CHOIX DE LA MATRICE DE DISSIMILARITÉ---MODE INDIRECT BINAIRES
    matriceAPIndirectBinaires <- reactive({
      if (input$selectMatrixAPIndirectBinaires==1){
        dist.binary(t(choixAP()),method = 2)
      }
      else if (input$selectMatrixAPIndirectBinaires==2){
        dist.binary(t(choixAP()),method = 1)
      }
      else if (input$selectMatrixAPIndirectBinaires==3){
        dist.binary(t(choixAP()),method = 5)
      }
    })
    
    ###EFFECTUER NMDS
    resultNMDS <- reactive({
      nMDS<-metaMDS(matriceAP(), k=2, autotransform=F) 
    })
    
    ####AFFICHER LA VALEUR DU STRESS
    output$valeurStress <- renderText({
      resultNMDS()$stress
    })
    
    ###AFFICHAGE DE LA REPRÉSENTATION
    output$plotNMDS <- renderPlot({
      stressplot(resultNMDS())
    })
    
    
    
    ####----- ACP -------####
    ###TABLEAU DES VALEURS PROPRES
    output$VPACP <- renderPrint({
      recup()$eig
      })
    
    ###BARPLOT DES VALEURS PROPRES
    output$BPACP <-renderPlot({
      barplot(recup()$eig[,1], xlab="Composantes", ylab="Valeur propre")
    })
    
   #TEST DE RÉCUPÉRATION VARIABLE SAISIS AU SELECT INPUT
    #output$resp<-renderPrint (input$select)
    
    ##RECUPERATION TEXT INPUT
    #col <-reactive({
     # output$text
    #})
    
    #resp2 <- reactive({
     # (which(colnames(data()) %in% input$z))
    #})
    
    #resp3<-reactive({
     # paste(resp2(),"", sep = ",")
    #})
    
    #resp4<-reactive({
      #gsub(".$" , "", resp3()) 
      #resp3().substring(,,resp3().length-1);
    #})
    
    #output$resp5<-renderText({
     # result()
    #})
    
    ##SELECT INPUT POUR VAR À NE PAS CONSERVER
    output$selectVarACP<-renderUI({
     numericInput("VarACP","Rentrez le numéro des colonnes",NA)
    })
    output$selectVarACP1<-renderUI({
      numericInput("VarACP1",NA,NA)
    })
    output$selectVarACP2<-renderUI({
      numericInput("VarACP2","Intervalles de colonnes",NA)
    })
    output$selectVarACP3<-renderUI({
      numericInput("VarACP3",NA,NA)
    })
     
    #### PARTIE TEST
    
    #result1 <- reactive({
     # grep(input$z,colnames(data()))
        #which(colnames(data()) == input$z)
      #for(i in length(names(data()))){
       # if(colnames(data())== input$z){
        #  print(colnames(data()))
        #}
      #}
    #})
    
    #    resp33<-reactive({
    #     #paste(result(),"", sep = ",")
    #    #cat(1:10, sep = ",")
    #   gsub("(?!^)(?=(?:\\d{1})+$)", ",", input$z , perl=T)
    #})
    
    #resp34<-reactive({
    # as.integer(result())
    #})
    
    
    #    output$resp6<-renderText({
    #     resp33()
    #  })
    
    #result44<-reactive({
     # replace('"','',result())
    #})
    
    ##Afficher l'indice de colonne des variables
    valueVarACP <- reactive({
      input$VarACP
    })
    
    valueVarACP1<-reactive({
      input$VarACP1
    })
    
    valueVarACP2<-reactive({
      input$VarACP2
    })
    
    valueVarACP3<-reactive({
      input$VarACP3
    })

    #Choix des variables à ne pas conserver pour l'analyser
    choix <-reactive({
      #data()[,colnames(data()) %in% c(input$z)]
      #data()[!colnames(data()) %in% c(result()),]
      #data()[,!(which(colnames(data())==result()))]
      data()[,-c(valueVarACP(),valueVarACP1(),valueVarACP2():valueVarACP3())] #FAUT TRANSFORMER POUR QUE UTILISATEUR SAISISSE LES COLONNES QU'IL NE VEUT PAS 
    })
    
    ### BOXPLOT DES DONNÉES BRUTES
    output$DBACP <- renderPlot({
      boxplot(choix(),col=input$col)
    })
    
    ###BOXPLOT DES DONNÉES TRANSFORMÉES
    output$DTACP  <- renderPlot({
      boxplot(transformation())
    })
    
    #Transformation des données
    transformation<- reactive({
      if (input$select==1){
        scale(choix())
      }
      else if (input$select==2){
        log1p(choix())
      }
      else if (input$select==3){
        sqrt(choix())
      }
      else if (input$select==4){
       sqrt(sqrt(choix()))
      }
      else if (input$select==5){
       choix()
      }
    })
    
    ##GRAPHE ACP
    output$grapheACP <- renderPlot({
      transformation()
      PCA(transformation(),scale.unit = TRUE, ncp = ncol(transformation()), graph = TRUE)
    })
    
    ##GRAPHE POSITION INDIVIDUS
    output$indCard <- renderPlot({
      plot(transformation(),choix="ind",axes = c(1,2),cex=0.7)
    })
    
  
    ###RÉCUPÉRATION PCA
    recup <- reactive({
      PCA(transformation(),scale.unit = TRUE, ncp = ncol(transformation()), graph = FALSE)
    })
    
    ##GRAPHE CONTRIBUTION VARIABLES COS²
    output$CosCircleACP <- renderPlot({
    fviz_pca_var(recup(),
                 col.var = "cos2", 
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE     
    )})
    
    ##CONTRIBUTION DES VARIABLES
    output$ContribCircleACP <- renderPlot({
      fviz_pca_var(recup(),
                   col.var = "contrib", 
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE     
      )})
    
    ##MAP
    output$mapACP <- renderPlot({
      fviz_pca_biplot(recup(), repel = TRUE,
                      col.var = "#2E9FDF", 
                      col.ind = "#696969"  
      )
      })
    
    ###################AFC##############################
    output$PrincipleACP<-renderUI({
      img(src='AnaMulti.png',height='60%',width='100%')
    })
    output$PrincipleAP<-renderUI({
      img(src='AnaMulti.png',height='60%',width='100%')
    })
    output$PrincipleAG<-renderUI({
      img(src='AnaMulti.png',height='60%',width='100%')
    })
    output$PrincipleAFC<-renderUI({
      img(src='AnaMulti.png',height='60%',width='100%')
    })
      
    ### BOXPLOT DES DONNÉES BRUTES
    output$DBAFC <- renderPlot({
      boxplot(choixAFC())
    })
    
    ###BOXPLOT DES DONNÉES TRANSFORMÉES
    output$DTAFC  <- renderPlot({
      boxplot(transformationAFC())
    })
    
    ##SELECT INPUT POUR VAR À NE PAS CONSERVER
#    output$selectVarAFC<-renderUI({
 #     numericInput("VarAF","Rentrez le numéro des colonnes",NA)
  #  })
   # output$selectVarAFC1<-renderUI({
    #  numericInput("VarAF1",NA,NA)
    #})
    output$selectVarAFC2<-renderUI({
      numericInput("VarAF2","Intervalles de colonnes",NA)
    })
    output$selectVarAFC3<-renderUI({
      numericInput("VarAF3",NA,NA)
    })
    
    ###RÉCUPÉRATION DES COLONNES - UTILISATEUR MET 0 S'IL A TROP DE NUMERICINPUT
    valueVarAF<-reactive({
      input$VarAF
    })
    valueVarAF1<-reactive({
      input$VarAF1
    })
    valueVarAF2<-reactive({
      input$VarAF2
    })
    valueVarAF3<-reactive({
      input$VarAF3
    })
    
    ###TABLEAU DES VALEURS PROPRES
    output$VPAFC <- renderPrint({
      recupAFC()$eig
    })
    
    ###BARPLOT DES VALEURS PROPRES
    output$BPAFC <-renderPlot({
      barplot(recupAFC()$eig[,1], xlab="Composantes", ylab="Valeur propre")
    })
    
    #CHOIX DES VARIABLES À NE PAS CONSERVER POUR L'ANALYSE
    choixAFC <-reactive({
      acm.disjonctif(data()[,valueVarAF2():valueVarAF3()])
    })
    
    #Transformation des données
    transformationAFC<- reactive({
      if (input$selectAFC==1){
        scale(choixAFC())
      }
      else if (input$selectAFC==2){
        log1p(choixAFC())
      }
      else if (input$selectAFC==3){
        sqrt(choixAFC())
      }
      else if (input$selectAFC==4){
        sqrt(sqrt(choixAFC()))
      }
      else if (input$selectAFC==5){
        choixAFC()
      }
    })
    
    ###GRAPHE AFC
    output$grapheAFC <- renderPlot({
      transformationAFC()
      CA(transformationAFC(), ncp = ncol(transformationAFC())-1, graph = TRUE)
    })
    
    ###GRAPHE POSITION INDIVIDUS
    output$indCardAFC <- renderPlot({
      plot(transformationAFC(),choix="ind",axes = c(1,2),cex=0.7)
    })
    
    ###RÉCUPÉRATION AFC
    recupAFC <- reactive({
      CA(transformationAFC(), ncp = ncol(transformationAFC())-1, graph = FALSE)
    })
    
    ###GRAPHE CONTRIBUTION COS²
    output$CosGrapheAFC <- renderPlot({
      fviz_ca_row(recupAFC(), col.row="cos2",
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE     
      )})
    
    ##CONTRIBUTION DES VARIABLES
    output$ContribGrapheAFC <- renderPlot({
      fviz_ca_col(recupAFC(),
                   col.col = "contrib", 
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE     
      )})
    ##MAP
    output$mapAFC <- renderPlot({
      fviz_ca_biplot(recupAFC(), repel = TRUE,
                      col.row = "#2E9FDF", 
                      col.col = "#696969"  
      )
    })

                                     ###ANALYSE SPATIALE###


    ###
    output$filedf2 <- renderTable({
      if(is.null(inputcontents())){return()}
      input$fileTwo
    })
    ####
    output$sum2 <- renderPrint({
      if(is.null(inputcontents())){return()}
      summary(inputcontents())
    })
    ####
    output$table2 <- renderTable({
      if(is.null(inputcontents())){return()}
      inputcontents()
    })
    
#    output$selectx2 <-renderUI({
 #     if(is.null(data2())){return()}
  #    selectInput("x2",label = "Axe x",choices = names(data2()))
   # })
#    output$selecty2<-renderUI({
 #     if(is.null(data2())){return()}
  #    selectInput("y2",label = "Axe y",choices = names(data2()))
   # })
    
    choicesnbvar2<-c("1","2","plus de 2")
    output$selectnbvar2<-renderUI({
      if(is.null(data2())){return()}
      selectInput("nbvar2",label = "Nombre de variables à visualiser",choices = choicesnbvar2)
    })
    
    output$selectgraph2<-renderUI({
      if(is.null(data2())){return()}
      else{
        listgraph2<-c("")
        if(input$nbvar2=="1"){
          try(
            if(is.numeric(data2()[,input$x2])|is.double(data2()[,input$x2])){
              listgraph2<-c("Diagramme en barre","Boxplot","Histogramme","Bubble","SS Plot")
            }
            else{
              listgraph2<-c("Diagramme circulaire","Diagramme en barre")
            }
          )
        }
        else if(input$nbvar2=="2"){
          try(
            if((is.numeric(data2()[,input$x2])|is.double(data2()[,input$x2]))&(is.numeric(data2()[,input$y2])|is.double(data2()[,input$y2]))){
              listgraph2<-c("Nuage de points","Ligne","Diagramme en barre","Histogramme","Boxplot")
            }
            else if((is.numeric(data2()[,input$x2])|is.double(data2()[,input$x2]))|(is.numeric(data2()[,input$y2])|is.double(data2()[,input$y2]))){
              listgraph2<-c("Boxplot")
            }
            else{
              listgraph2<-c("")
            }
          )
        }
        else{
          listgraph2<-c("Matrice de scatter plot")
        }
        selectInput("Graph2",label = "Type of graph",choices = listgraph2)
      }
    })
    ####
    output$plot2<-renderPlot({
      if(is.null(data2())){return()}
      else{
        if(input$Graph2=="Diagramme circulaire"){
          pie(table(data2()[,input$x2]))
        }
        else if(input$Graph2=="Nuage de points"){
          plot(data2()[,input$x2],data2()[,input$y2],xlab = input$x2,ylab = input$y2)
        }
        else if(input$Graph2=="Ligne"){
          plot(data2()[,input$x2],data2()[,input$y2],type = "o",xlab = input$x2,ylab = input$y2)
        }
        else if(input$Graph2=="Matrice de scatter plot"){
          ind.quant2<-sapply(data2(),is.numeric)
          pairs(data2()[,ind.quant2])
        }
        else if(input$Graph2=="Diagramme en barre"){
          if(input$nbvar2=="1"){
            if(is.numeric(data2()[,input$x2])|is.double(data2()[,input$x2])){
              barplot(data2()[,input$x2],xlab = input$x2,ylab="Fréquences")
            }
            else{
              barplot(table(data2()[,input$x2]),xlab = input$x2,ylab = "Fréquences")
            }
          }
          else{
            dataset2<-table(data2()[,input$x2],data2()[,input$y2])
            barplot(dataset2,xlab = input$x2,ylab = input$y2)
          }
        }
        else if(input$Graph2=="Boxplot"){
          if(input$nbvar2=="1"){
            boxplot(data2()[,input$x2],xlab=input$x2,ylab="Fréquences")
          }
          else if((is.numeric(data2()[,input$x2])|is.double(data2()[,input$x2]))&(is.numeric(data2()[,input$y2])|is.double(data2()[,input$y2]))){
            boxplot(data2()[,input$x2]~data2()[,input$y2],xlab=input$x2,ylab=input$y2)
          }
          else{
            if(is.numeric(data2()[,input$x2])|is.double(data2()[,input$x2])){
              boxplot(data2()[,input$x2]~data2()[,input$y2],xlab=input$x2,ylab=input$y2)
            }
            else{
              boxplot(data2()[,input$y2]~data2()[,input$x2],xlab=input$x2,ylab=input$y2)
            }
          }
        }
        else if(input$Graph2=="Histogramme"){
          if(input$nbvar2=="1"){
            hist(data2()[,input$x2],xlab = input$x2,ylab = "Fréquences")
          }
          else{
              x3<-data2()[,input$x2]
              y3<-data2()[,input$y2]
              hist(x3,col =rgb(1,0,0,0.5))
              hist(y3,add=T,col =rgb(0,0,1,0.5))
          }
        }
        else{
          return()
        }
      }
    })
 
    ###PAGE ANALYSE SPATIALE - FICHIER DE CONTOUR
    dataAS <- reactive({
      fileAS <- input$fileAS
      if(is.null(fileAS)){return()}
      read.table(file=fileAS$datapath,
                 sep=input$sepAS,
                 dec=input$decAS,
                 header = input$headerAS,
                 stringsAsFactors = input$stringAsFactorsAS)
    })
    
    ###SELECTION DES COLONNES POUR EFFECTUER DES GRAPHES
    output$spr<-renderUI({
      if(is.null(dataAS())){return()}
      selectInput("spr",label = "Richesse spécifique",choices = names(dataAS()))
    })
    output$selectxAS <-renderUI({
      if(is.null(dataAS())){return()}
      selectInput("xAS",label = "Axe x",choices = names(dataAS()))
    })
    output$selectyAS<-renderUI({
      if(is.null(dataAS())){return()}
      selectInput("yAS",label = "Axe y",choices = names(dataAS()))
    })
    
    ###
    output$filedfAS <- renderTable({
      if(is.null(dataAS())){return()}
      input$fileAS
    })
    ####
    output$sumAS <- renderPrint({
      if(is.null(dataAS())){return()}
      summary(dataAS())
    })
    ####
    output$tableAS <- DT::renderDataTable({
      if(is.null(dataAS())){return()}
      dataAS()
    })
    
    ####PARTIE COORDINATION
    Coordination <- function(){
      data<-dataAS()
      data2<-data
      choixX<- input$xAS
      choixY<- input$yAS
      coordinates(data2) = (data[, c(grep(choixX, colnames(data)),grep(choixY, colnames(data)))])
      return(data2)
    }
    
    output$plotAS <-renderPlot({
      if (is.null(dataAS()))
        return(NULL)
      data<- dataAS()  
      data2<-Coordination()
      plot (data2)
    })
    
    output$bubleAS <-renderPlot({
      if (is.null(dataAS()))
        return(NULL)
      data<- dataAS()  
      data2<-Coordination()
      bubble (data2, "SpRichness", col = c("#00ff0088", "#00ff0088"), main = "Species richness")
    })
    
    output$ssAS <-renderPlot({
      if (is.null(dataAS()))
        return(NULL)
      data<- dataAS()  
      data2<-Coordination()
      spplot (data2, "SpRichness", main = "Species richness") 
    })
    
    ###HISTOGRAMME DE FRÉQUENCE
    output$histoFreqAS <- renderPlot({
      if (is.null(dataAS()))
        return(NULL)
      data<- dataAS()
      choix<-input$spr
      Histogramme<-data[, grep(choix, colnames(data))]
      plot(hist(Histogramme),main = choix)
    })
    
    ###INTERPOLATION PAR SURFACE DE TENDANCE=>TSA
    znsppol <- function(){
      if (is.null(dataAS()))
        return(NULL)
      data<- data()
      choixX<- input$x
      choixY<- input$y
      XY<-(data[, c(grep(choixX, colnames(data)),grep(choixY, colnames(data)))])
      Zp <- Polygon (as.matrix (XY), hole = TRUE)
      Zps <- Polygons (list (Zp), ID = "Polyg")
      Zsp <- SpatialPolygons (list (Zps))
      return(Zsp)
    }
    
    zone.grid <- function(){
      data <- dataAS()
      zsp <- znsppol()
      gx<- input$grx
      gy<- input$gry
      Zone.grid <- spsample (zsp, cellsize = c(gx,gy), type = "regular", offset = c(0, 0))
      return(Zone.grid)
    }
    
    SpRichnesstsa <- function(orders){
      data <- dataAS()
      choix<- input$spr
      zgrid <- zone.grid()
      data2 <- Coordination()
      SpRichn<-krige (as.formula(paste(choix,"~ 1")) , data2, zgrid, degree = orders)
      return(SpRichn)
    }
    
    output$tsa <- renderPlot({
      orders <-  input$order
      SpRichn<-SpRichnesstsa(orders)
      spplot (SpRichn ["var1.pred"], main = "TSA interpolation")
    })
    
    ###INTERPOLATION PAR L'INVERSE DE LA DISTANCE
    var.emp <- function(spRich,donnees){
      SpRichness.var.emp <- variogram (as.formula(paste(spRich, "~ 1")), donnees)
      return(SpRichness.var.emp)
    }
    
    output$inversdistPlot<- renderPlot({
      spRich<- input$spr
      donnees <- Coordination()
      zg <- zone.grid()
      idplvl<-input$idp
      SpRichness.idw = idw (as.formula(paste(spRich, "~ 1")), donnees, zg, idp = idplvl)
      spplot (SpRichness.idw ["var1.pred"], main = "Species richness - inverse distance weighted interpolation")
    })
    
    ###KRIGEAGE
    fit <- function(SpRichness.var.emp){
      spRich<- input$spr
      donnees <- Coordination()
      sil <- input$sill
      mod <- input$model
      rang <- input$range
      nug <- input$nugget
      SpRichness.fit.1 <- fit.variogram (SpRichness.var.emp, model = vgm (sill = sil, mod ,range = rang,nugget = nug ))
      return(SpRichness.fit.1)
    }
    
    output$modelk <- renderPlot({
      spRich<- input$spr
      donnees <- Coordination()
      SpRichness.var.emp <- var.emp(spRich,donnees)
      SpRichness.fit.1 <- fit(SpRichness.var.emp)
      plot(SpRichness.var.emp, SpRichness.fit.1)
    })
    
    output$krig <- renderPlot({
      spRich<- input$spr
      donnees <- Coordination()
      SpRichness.var.emp <- var.emp(spRich,donnees)
      SpRichness.fit.1 <- fit(SpRichness.var.emp)
      plot(SpRichness.var.emp, SpRichness.fit.1)
      zg <- zone.grid()
      SpRichness.kriged.1 <- krige (as.formula(paste(spRich, "~ 1")), donnees, zg, model = SpRichness.fit.1)
      switch(input$krigplot,
             int =  spplot (SpRichness.kriged.1 ["var1.pred"]),# sert a l'interpolation 
             err = spplot (SpRichness.kriged.1 ["var1.var"]))# represente l'erreur
      
    })
    
    ###AUTOCORRÉLATION SPATIALE###
    relneig <- function(xy){
      relneiggraph <- relativeneigh (as.matrix(xy))
      relng <-  graph2nb (relneiggraph)
      return(relng)
    }
    
    relSym <- function(xy){
      relneiggraph <- relativeneigh (as.matrix(xy))
      relneig.sym <-  graph2nb (relneiggraph, sym = T)
      return(relneig.sym)
    }
    
    pondRelneig <-function(xy){
      relneig.sym<-relSym(xy)
      pond.relneig.sym.bin <- nb2listw (relneig.sym, style = "B", zero.policy=FALSE)
      return(pond.relneig.sym.bin)
    }
    
    ###GRAPHE DE GABRIEL
    XY <- function(){
      data <- dataAS()
      choixX <- input$xAS
      choixY <- input$yAS
      XY <- data[, c(grep(choixX, colnames(data)),grep(choixY, colnames(data)))]
      return(XY)
    }
    
    output$gab <- renderPlot({
      xy <- XY()
      zone <- data()
      gabgraph <- gabrielneigh (as.matrix (xy))
      gab <- graph2nb (gabgraph) # transformation d'un objet de classe "graph" a un objet de classe "nb"
      xy.empty.gab <- xy [which (card (gab) == 0),]
      
      area.plot (zone)
      s.label (xy, clab = 0, add.p = T)
      s.label (xy.empty.gab, clab=0, pch = 17, cpoint = 1.5, add.p=T)
      
      #s.label (xy, neig = neig (list = gab), clab=0, cpoint = 0, add.p=T)
      #s.label (xy.empty.gab, clab=0, pch = 17, cpoint = 1.5, add.p=T)
    })
    
    
    ###GRAPHE DE DELAUNAY
    output$delaunay <- renderPlot({
      xy <- XY()
      zone <- data()
      tri <- tri2nb (xy) # Creation d'un objet de classe "nb" directement a partir des coordonnees (plus simple que pour le graphe de Gabriel)
      area.plot (zone)
      s.label (xy, neig = neig(list = tri), clab=0, add.p=T)
    })
    
    ###GRAPHE VOISIN
    output$voisin <- renderPlot({
      xy <- XY()
      zone <- data()
      near <- knn2nb (knearneigh (as.matrix(xy), k=input$nbvoisin))
      area.plot (zone)
      s.label (xy, neig = neig(list = near), clab=0, add.p=T)
    })
    
    ###GRAPHE PLUS PROCHE VOISINS
    output$d1d2 <- renderPlot({
      xy <- XY()
      zone <- data()
      if(input$dmax<=input$dmin){input$dmax<-input$dmin+1}
      if(input$dmin>=input$dmax){input$dmin<-input$dmax-1}
      distneig50 <- dnearneigh (as.matrix (xy), input$dmin, input$dmax)
      area.plot (zone)
      s.label (xy, neig = neig(list = distneig50), clab=0, add.p=T)
    })
    
    ###GRAPHE
    output$spantree <- renderPlot({
      xy <- XY()
      zone <- data()
      rel <-relneig(xy)
      area.plot (zone)
      s.label (xy, neig = neig (list = rel), clab=0, add.p=T, sub = "Minimum spanning tree")
    })
    
    ###AFFICHAGE DE L'I DE MORAN
    choixreactI <-reactive({
      data<-dataAS()
      updateSelectInput(session, "Im",choices = colnames( data)) })
    
    output$Imoran <- renderPrint({
      choixreactI()
      xy <- XY()
      data <- dataAS()
      pond.relneig.sym.bin<-pondRelneig(xy)
      colname <- input$Im
      vect <- data[, c(grep(colname, colnames(data)))] 
      # zp <- input$Izerop
      switch(input$autotest,
             I = moran.test (vect, pond.relneig.sym.bin, alternative = input$Ialt , randomisation = input$Irand),#, zero.policy = zp
             C = geary.test (vect, pond.relneig.sym.bin, alternative = input$Ialt , randomisation = input$Irand),
             J = joincount.test (as.factor(vect), pond.relneig.sym.bin, alternative = input$Ialt))#, zero.policy = FALSE
    })
    
    ###GRAPHE DE CORRÉLATION DE L'IC
    output$correloIC <- renderPlot({
      data <- dataAS()
      xy <- XY()
      zone <- data()
      colname <- input$Im
      vect <- data[, c(grep(colname, colnames(data)))]
      relneig.sym<- relSym(xy)
      if(input$autotest != "J"){
        cor.Spaver <- sp.correlogram (relneig.sym, vect, order = 20, method = input$autotest, style = "B", randomisation = input$Irand)
        plot(cor.Spaver)}
      if(input$autotest == "J"){
        area.plot (zone)
        couleurs = c ("darkorange", "red", "forestgreen", "gold", "royalblue", "purple")
        s.class (xy, fac = vect, col = couleurs, cstar = 0, cellipse = 0, clabel = 0, add.p = T)
      }
    })
    
    ###AFFICHAGE DU MANTEL TEST
    output$manteltest <- renderPrint({
      data <- dataAS()
      xy <- XY()
      d.geo <- dist (xy)
      donnees.compo <- data[, c (input$firstSpCOl:ncol (data))]#a remplacer par selction manuelle
      d.compo <- dist (donnees.compo)
      mantel.randtest (d.geo,d.compo)
    })
    
    ###AFFICHAGE DU P DE MANTEL
    output$Pmant <- renderPlot({
      data <- dataAS()
      xy <- XY()
      d.geo <- dist (xy)
      donnees.compo <- data[, c (input$firstSpCOl:ncol (data))]#a remplacer par selction manuelle
      d.compo <- dist (donnees.compo)
      plot (mantel.randtest (d.geo, d.compo), main = "Test de Mantel sur la composition specifique")
    })
    
    ###PAGE PRINCIPALE
    output$tb2 <- renderUI({
        tabsetPanel(
          tabPanel("A propos",tableOutput("filedfAS")),
          tabPanel("Les données",DT::dataTableOutput("tableAS")),
          tabPanel("Résumé",verbatimTextOutput("sumAS"))
          #tabPanel("Graph",plotOutput("plot2"))
        )
      
    })
  }
)