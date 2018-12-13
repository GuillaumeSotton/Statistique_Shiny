#COTE CLIENT

###INSTALLATION DES PACKAGES
#install.packages(c("shiny","shinydashboard","FactoMineR","ade4","factoextra","magrittr","ggplot2","gclus","RColorBrewer","vegan","clustsig","labdsv","shinycssloaders","stringr","sp","gstat","DT","colourpicker","shinyalert","lmtest","pwr","PMCMR","ngram","spdep"))

###CHARGEMENT DES LIBRARIES
x<-c("shiny","shinydashboard","FactoMineR","ade4","factoextra","magrittr","ggplot2","gclus","RColorBrewer","vegan","clustsig","labdsv","shinycssloaders","stringr","sp","gstat","DT","colourpicker","shinyalert","lmtest","pwr","PMCMR","ngram","spdep")
lapply(x, require, character.only = TRUE)

shinyUI(
dashboardPage(skin = "yellow",
  dashboardHeader(title = "Stats test"),
  dashboardSidebar(
    sidebarMenu(
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        label = "Search..."),
      menuItem("Acceuil",icon=icon("home",lib = "font-awesome"),tabName = "Acceuil"),
      menuItem("Relier des variables", tabName="RL",
      menuSubItem("Principe",tabName = "PrincipeRL"), 
      menuSubItem("Tests et conditions",tabName = "RL1"),
      menuSubItem("Graphiques et résultats",tabName = "RL2")),
      menuItem("Régression linéaire multiple", tabName="RLM",
               menuSubItem("Principe",tabName = "PrincipeRLM"), 
               menuSubItem("Tests et conditions",tabName = "RLM1"),
               menuSubItem("Graphiques et résultats",tabName = "RLM2")),
      menuItem("ANOVA multifactorielles", tabName="ANOVAMF",
               menuSubItem("Principe",tabName = "PrincipeANOVAMF"), 
               menuSubItem("Tests et conditions",tabName = "ANOVAMF1"),
               menuSubItem("Graphiques et résultats",tabName = "ANOVAMF2")),
      menuItem("Analyse de groupement",tabName="AG",
      menuSubItem("Principe",tabName = "PrincipeAG"),
      menuSubItem ( "Test et conditions" ,
                    tabName = "AG1 " ) ,
      menuSubItem ( "Graphiques et résultats" ,
                    tabName = "AG2" )),
      menuItem("Analyse de proximité",tabName="AP",
      menuSubItem("Principe",tabName = "PrincipeAP"),
      menuSubItem ( "Test et résultats" ,
                    tabName = "AP1 ")), 
    #  menuSubItem ( "Graphiques et résultats" ,
     #               tabName = "AP2" )),
      menuItem("ACP", tabName="ACP",
      menuSubItem("Principe",tabName = "PrincipeACP"),    
      menuSubItem ( "Test et conditions" ,
                    tabName = "ACP1 " ) ,
      menuSubItem ( "Graphiques et résultats" ,
                    tabName = "ACP2" )),
      menuItem("AFC",tabName="AFC",
      menuSubItem("Principe",tabName = "PrincipeAFC"),
      menuSubItem ( "Test et conditions" ,
                    tabName = "AFC1 " ) ,
      menuSubItem ( "Graphiques et résultats" ,
                    tabName = "AFC2" )),
      menuItem("Analyses Spatiales",tabName = 'AS',
      menuSubItem("Fichier de contour",tabName = "AS1"),
      menuSubItem("Interpolations",tabName = "AS2"),
      menuSubItem("Krigeage",tabName = "AS3"),
      menuSubItem("Connexions entre les points",tabName = "AS4"),
      menuSubItem("Autocorrélation",tabName = "AS5")),
      
      menuItem("A propos",icon=icon("info-circle",lib = "font-awesome"),tabName = "About")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Acceuil",
              sidebarLayout(
                sidebarPanel(
                  fileInput("file","Upload the file"),
                  helpText("Taille maximale = 5Mb"),
                  tags$hr(),
                  checkboxInput(inputId = 'header',label = 'En-tête',value = FALSE),
                  checkboxInput(inputId = 'stringAsFactors',label = 'StringsAsFactors',value = FALSE),
                  br(),
                  radioButtons(inputId = 'sep',label='Séparateur',choices = c(Virgule=',',Point_virgule=';',Tab='\t',Espace=' '),selected = ','),
                  radioButtons(inputId = 'dec',label='Décimale', choices = c(Virgule=',',Point='.'),selected = '.' ),
                  tags$hr(),
                  uiOutput("selectnbvar"),
                  helpText("Pour la visualisation d'une seule variable, sélectionner là dans l'axe x"),
                  uiOutput("selectx"),
                  uiOutput("selecty"),
                  uiOutput("selectgraph")
                ),
                mainPanel(
                  #tags$style(type="text/css",
                   #          ".shiny-output-error { visibility: hidden; }",
                    #         ".shiny-output-error:before { visibility: hidden; }"),
                  
                  uiOutput("tb"),
                  useShinyalert()
                ))
      ),
      tabItem(tabName= "PrincipeRL",
              uiOutput("PrincipleRL") 
              ),
      tabItem(tabName = "RL1",
              fluidRow(
              box(title = "Quelles sont les variables explicatives et à expliquer ?",status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 6,
                  uiOutput("VariablesRL1"),
                  uiOutput("VariablesRL2")),
              tabBox(title = "Conditions d'applications",width = 6,
                  tabPanel("Durbin Watson",verbatimTextOutput("DWtest")),
                  tabPanel("Goldfeld Quant",verbatimTextOutput("GQtest")),
                  tabPanel("Shapiro Wilks",verbatimTextOutput("SWtest"))
              )),
              fluidRow(
          box(title = "Graphique des conditions d'applications",status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 6,
                  withSpinner(plotOutput("CARL"))),
          box(title = "Respect des conditions d'applications",status = "primary", solidHeader = TRUE,
              collapsible = TRUE,width = 6,
              selectInput("RespectCARL",label = "Les conditions d'applications sont elles respectées ?",c("Oui" = 1, "Non" = 2)
          )))),
      tabItem(tabName = "RL2",
              conditionalPanel(
                condition = "input.RespectCARL == 1",
                titlePanel("Régression linéaire"),
                fluidRow(
                box(title = "Test de corrélation de Bravais-Pearson",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = 6,
                    verbatimTextOutput("corBV")
                ),
                box(title = "Puissance du test",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = 6,
                    verbatimTextOutput("powerRL")
                )),
                fluidRow(
                box(title = "Résumé du test",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = 6,
                    verbatimTextOutput("summaryRL1")
                ),
                box(title = "Graphe de corrélation",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = 6,
                    plotOutput("regLin1")))
               # fluidRow(
              #  box(title = "Graphe de corrélation",status = "primary", solidHeader = TRUE,
               #     collapsible = TRUE,width = 6,
                #    verbatimTextOutput("phTukey")
              #  ))
              ),
              conditionalPanel(
                condition = "input.RespectCARL == 2",
                titlePanel("Corrélation de Spearman/Kendall"),
                fluidRow(
                  box(title = "Test de corrélation de Spearman",status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,width = 6,
                      verbatimTextOutput("corSP")
                  ),
                  box(title = "Test de corrélation de Kendall",status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,width = 6,
                      verbatimTextOutput("corKD")
                  )),
                  fluidRow(
                    box(title = "Résumé du test",status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,width = 6,
                        verbatimTextOutput("summaryRL2")
                    ),
                    box(title = "Graphe de corrélation",status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,width = 6,
                        plotOutput("regLin2"))),
                    fluidRow(
                    box(title = "Test Post Hoc",status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,width = 12,
                        verbatimTextOutput("phNemenyi"))
                  ))
              ),
      tabItem(tabName= "PrincipeRLM",
              uiOutput("PrincipleRLM") 
      ),
      tabItem(tabName = "RLM1",
              fluidRow(
                box(title = "Quelles sont les variables explicatives et à expliquer ?",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = 6,
                    uiOutput("VariablesRLM1"),
                    uiOutput("selectnbvarRLM"),
                    uiOutput("VariablesRLM2"),
                    verbatimTextOutput("manipRLM")),
                tabBox(title = "Boxplot",width = 6,
                       tabPanel("Boxplot avec effet seul",withSpinner(plotOutput("boxplotRLM1"))),
                       tabPanel("Boxplot avec effet combiné",withSpinner(plotOutput("boxplotRLM2"))))),
                fluidRow(
                tabBox(title = "Conditions d'applications",width = 6,
                       tabPanel("Durbin Watson",verbatimTextOutput("DWtestRLM")),
                       tabPanel("Goldfeld Quant",verbatimTextOutput("GQtestRLM")),
                       tabPanel("Shapiro Wilks",verbatimTextOutput("SWtestRLM"))
                ),
                box(title = "Graphe des Conditions d'applications",status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 6,
                  withSpinner(plotOutput("CARLM")))
                )),
      tabItem(tabName = "RLM2",
              fluidRow(
                box(title = "Résumé de la régression linéaire",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = 6,
                    verbatimTextOutput("summaryAN1")
                ),
                box(title = "Résumé du test",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = 6,
                    verbatimTextOutput("summaryRLM1")
                )),
                fluidRow(
                box(title = "Test de Tukey",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = 12,
                    verbatimTextOutput("phTukeyRLM")
                ))),
      tabItem(tabName= "PrincipeANOVAMF",
              uiOutput("PrincipleANOVAMF") 
      ),
      tabItem(tabName = "ANOVAMF1",
              fluidRow(
                box(title = "Quelles sont les variables explicatives et à expliquer ?",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = 6,
                    uiOutput("VariablesANOVA1"),
                    uiOutput("selectnbvarANOVA"),
                    uiOutput("VariablesANOVA2")),
                    
                tabBox(title = "Boxplot",width = 6,
                       tabPanel("Boxplot avec effet seul",withSpinner(plotOutput("boxplotANOVA1"))),
                       tabPanel("Boxplot avec effet combiné",withSpinner(plotOutput("boxplotANOVA2"))))),
              fluidRow(
                tabBox(title = "Conditions d'applications",width = 6,
                       tabPanel("Durbin Watson",verbatimTextOutput("DWtestANOVA")),
                       tabPanel("Goldfeld Quant",verbatimTextOutput("GQtestANOVA")),
                       tabPanel("Shapiro Wilks",verbatimTextOutput("SWtestANOVA"))
                ),
                box(title = "Graphe des Conditions d'applications",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = 6,
                    withSpinner(plotOutput("CAANOVA")))
              )),
      tabItem(tabName = "ANOVAMF2",
              fluidRow(
                box(title = "Résumé de l'ANOVA",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = 6,
                    verbatimTextOutput("summaryANOVA1")
                ),
                box(title = "Résumé du test",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = 6,
                    verbatimTextOutput("summaryANOVAMF1")
                )),
              fluidRow(
                box(title = "Test de Tukey",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = 12,
                    verbatimTextOutput("phTukeyANOVA")
                ))),    
              
      tabItem(tabName="PrincipeACP",
          uiOutput("PrincipleACP")     
      ),
      tabItem(tabName="ACP1",
              fluidRow(
              box(
                title = "Variables à ne pas conserver",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                uiOutput("selectVarACP"),
                uiOutput("selectVarACP1"),
                uiOutput("selectVarACP2"),
                uiOutput("selectVarACP3"),
                verbatimTextOutput("resp6")
              ),
              box(
                title = "Transformation des données ",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                selectInput("select", NA ,
                            c("Transformation Centrée-réduite" = 1, "Transformation logarythme (log(x+1))" = 2,
                              "Transformation racine carré (sqrt())" = 3,"Transformation double racine carré (sqrt(sqrt()))"=4,"Pas de transformation"=5, selected = 1)))),
              fluidRow(
              tabBox(
                title = "Boxplot",
                id = "tabsetACP1",width = 6,
                colourInput("col", "Select colour", "purple"),
                tabPanel("DataBrute",withSpinner(plotOutput("DBACP"))),
                tabPanel("DataTransforme",withSpinner(plotOutput("DTACP")))
              ), 
              tabBox(
                title ="Axes à conserver",
                id="tabsetACP2",width = 6,
                tabPanel("Valeurs Propres",verbatimTextOutput("VPACP")),
                tabPanel("Barplot",withSpinner(plotOutput("BPACP"))))
              )),
              tabItem(tabName = "ACP2",
              fluidRow(
              box(
                title = "Cercle contribution cos²", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                withSpinner(plotOutput("CosCircleACP"))
              ),
              box(
                title = "Cercle contribution variables", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                withSpinner(plotOutput("ContribCircleACP"))
              ),
              box(
                title = "Cercle de corrélation", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                withSpinner(plotOutput("grapheACP"))
              ),
              box(
                title = "Map", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                withSpinner(plotOutput("mapACP"))
              ))),
      
      ###ANALYSE DE GROUPEMENT - PAGE 1
      tabItem(tabName="PrincipeAG",
              uiOutput("PrincipleAG")     
      ),
      tabItem(tabName="AG1",
              fluidRow(
                
              ###Boite pour variable à ne pas conserver
              box(
                title = "Variables à ne pas conserver",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 5,
                uiOutput("selectVarAG"),
                uiOutput("selectVarAG1"),
                uiOutput("selectVarAG2"),
                uiOutput("selectVarAG3")
              ),
              box(
                title = "Mode",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 3,
                selectInput("selectMode", NA ,
                            c("Mode Direct" = 1, "Mode Indirect" = 2, selected = 1))
              ),
              box(
                title = "Types des données",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 3,
                selectInput("selectType", NA ,
                            c("Quantitatives" = 1, "Binaires" = 2, selected = 1))
              ),
              box(
                title = "Transformation des données ",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 3,
                selectInput("selectAG", NA ,
                            c("Transformation Centrée-réduite" = 1, "Transformation logarythme (log(x+1))" = 2,
                              "Transformation racine carré (sqrt())" = 3,"Transformation double racine carré (sqrt(sqrt()))"=4,"Pas de transformation"=5, selected = 1))
               ),
              box(
                title = "Matrice",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 3,
                uiOutput("selectMatrix"))),
              fluidRow(
              tabBox(
                title = "",
                id = "tabsetAG",width=6,
                tabPanel("Données Brute",withSpinner(plotOutput("DBAG"))),
                tabPanel("Données Transformées",withSpinner(plotOutput("DTAG"))),
                tabPanel("Aperçu",withSpinner(plotOutput("dataAG")))         
                ),
              tabBox(
                title = "",
                id = "tabsetGraphAG",width=6,
                tabPanel("Lien Simple",withSpinner(plotOutput("grapheLienSimple"))),
                tabPanel("Lien Complets",withSpinner(plotOutput("grapheLienComplet"))),
                tabPanel("Lien UPGMA",withSpinner(plotOutput("grapheLienUPGMA")))         
              ))),
      tabItem(tabName="AG2",
              fluidRow(
              box(
                title = "Matrice cophénétiques",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width=6,
                "Coefficients de corrélations des graphes simples,complets et UPGMA",
                verbatimTextOutput("resultMatrixCopheneticSingle"),
                verbatimTextOutput("resultMatrixCopheneticComplete"),
                verbatimTextOutput("resultMatrixCopheneticAverage")
              ),
              box(
                title = "Quelles graphes conservez vous ?",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width=6,
                selectInput("selectAGGraphes", NA ,
                            c("Graphes liens simples" = 1, "Graphes liens complets" = 2,
                             "Graphes UPGMA" = 3, selected = 1))
              )),
              fluidRow(
              box(
                title = "Heatmap",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width=6,
                withSpinner(plotOutput("heatmapAG"))
              ),
              box(
                title = "SIMPROF",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width=6,
                withSpinner(plotOutput("simprofPlot"))
              )
#              box(
 #             title = "Cut tree",status = "primary", solidHeader = TRUE,
  #            collapsible = TRUE,
   #           numericInput("valueCutTree",NA,NA)
    #          ),
     #         box(
      #          title = "Espèces indicatrices",status = "primary", solidHeader = TRUE,
       #         collapsible = TRUE,
        #        verbatimTextOutput("indVal")
         #     )
              )),
      
      ###ANALYSE DE PROXIMITÉ - PAGE 1
      tabItem(tabName="PrincipeAP",
              uiOutput("PrincipleAP")     
      ),
      tabItem(tabName="AP1",
              fluidRow(
              ###BOX POUR LES VARIABLES À NE PAS CONSERVER
              box(
              title = "Variables à ne pas conserver",status = "primary", solidHeader = TRUE,
              collapsible = TRUE,width = 5,
              uiOutput("selectVarAP"),
              uiOutput("selectVarAP1"),
              uiOutput("selectVarAP2"),
              uiOutput("selectVarAP3")
              ),
              box(
                title = "Mode",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 3,
                selectInput("selectModeAP", NA ,
                            c("Mode Direct" = 1, "Mode Indirect" = 2, selected = 1))
              ),
              box(
                title = "Types des données",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 3,
                selectInput("selectTypeAP", NA ,
                            c("Quantitatives" = 1, "Binaires" = 2, selected = 1))
              ),
              box(
                title = "Transformation des données ",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 3,
                selectInput("selectAP", NA ,
                            c("Transformation Centrée-réduite" = 1, "Transformation logarythme (log(x+1))" = 2,
                              "Transformation racine carré (sqrt())" = 3,"Transformation double racine carré (sqrt(sqrt()))"=4,"Pas de transformation"=5, selected = 1))
              ),
              box(
                title = "Matrice",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 3,
                uiOutput("selectMatrixAP")
              )),
              fluidRow(
              tabBox(
                title = "",
                id = "tabsetAP",width=6,
                tabPanel("Données Brute",withSpinner(plotOutput("DBAP"))),
                tabPanel("Données Transformées",withSpinner(plotOutput("DTAP"))),
                tabPanel("Aperçu",withSpinner(plotOutput("dataAP")))         
              ),
              box(
                title = "Graphe de proximité",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                withSpinner(plotOutput("plotNMDS"))
              )),
              fluidRow(
                box(
                  title = "Valeur du stress de la représentation",status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 12,
                  verbatimTextOutput("valeurStress")
                )
              )
      ),
      ###ANALYSE FACTORIELLE CORRESPONDANCE- PAGE 1
      tabItem(tabName="PrincipeAFC",
              uiOutput("PrincipleAFC")     
      ),
      tabItem(tabName="AFC1",
              fluidRow(
              ###Boite pour variable à ne pas conserver
              box(
              title = "Variables à conserver",status = "primary", solidHeader = TRUE,
              collapsible = TRUE,width = 6,
              uiOutput("selectVarAFC"),
              uiOutput("selectVarAFC1"),
              uiOutput("selectVarAFC2"),
              uiOutput("selectVarAFC3")
              ),
              box(
                title = "Transformation des données ",status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                selectInput("selectAFC", NA ,
                            c("Transformation Centrée-réduite" = 1, "Transformation logarythme (log(x+1))" = 2,
                              "Transformation racine carré (sqrt())" = 3,"Transformation double racine carré (sqrt(sqrt()))"=4,"Pas de transformation"=5, selected = 1))
              )),
              fluidRow(
              tabBox(
                title = "Boxplot",
                id = "tabset1AFC",width = 6,
                tabPanel("DataBrute",withSpinner(plotOutput("DBAFC"))),
                tabPanel("DataTransforme",withSpinner(plotOutput("DTAFC"))
                )),
              
              tabBox(
                title ="Axes à conserver",
                id="tabset2AFC",width = 6,
                tabPanel("Valeurs Propres",verbatimTextOutput("VPAFC")),
                tabPanel("Barplot",withSpinner(plotOutput("BPAFC"))
                )))),
      tabItem(tabName="AFC2",
              fluidRow(
              box(
                title = "Cercle contribution cos²", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                withSpinner(plotOutput("CosGrapheAFC"))
              ),
              box(
                title = "Cercle contribution variables", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                withSpinner(plotOutput("ContribGrapheAFC"))
              ),
              box(
                title = "Cercle de corrélation", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                withSpinner(plotOutput("grapheAFC"))
              ),
              box(
                title = "Map", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                withSpinner(plotOutput("mapAFC"))
              ))),
      tabItem(tabName="AS1",
              sidebarLayout(
              sidebarPanel(
                fileInput("fileAS","Upload the file"),
                helpText("Taille maximale = 5Mb"),
                tags$hr(),
                checkboxInput(inputId = 'headerAS',label = 'En-tête',value = FALSE),
                checkboxInput(inputId = 'stringAsFactorsAS',label = 'StringsAsFactors',value = FALSE),
                br(),
                radioButtons(inputId = 'sepAS',label='Séparateur',choices = c(Virgule=',',Point_virgule=';',Tab='\t',Espace=' '),selected = ','),
                radioButtons(inputId = 'decAS',label='Décimale', choices = c(Virgule=',',Point='.'),selected = '.' ),
                tags$hr(),
                uiOutput("spr"),
                uiOutput("selectxAS"),
                uiOutput("selectyAS")
              ),
              mainPanel(
                uiOutput("tb2")
              ))
              ),
      tabItem(tabName ="AS2",
              fluidRow(
              tabBox(
                title ="Plot",
                id="tabsetAS",width = 6,
                tabPanel("Plot",withSpinner(plotOutput("plotAS"))),
                tabPanel("Buble Plot",withSpinner(plotOutput("bubleAS"))),
                tabPanel("SS Plot",withSpinner(plotOutput("ssAS")))
              ),
              box(
                title = "Histogramme de fréquence", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                withSpinner(plotOutput("histoFreqAS"))
              )),
              fluidRow(
              box(
                title = "TSA", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                sliderInput("grx","Cellsize X:",
                            min = 1,
                            max = 100,
                            value = 10),
                sliderInput("gry","Cellsize y:",
                            min = 1,
                            max = 100,
                            value = 10),
                sliderInput("order","TSA order:",
                            min = 1,
                            max = 3,
                            value = 1)
              ),
              box(
                  title = "TSA Graphe", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 6,
                  withSpinner(plotOutput("tsa"))
              )),
              fluidRow(
                box(title = "Interpolation", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = 6,
                    sliderInput("idp",
                                "Idp levels:",
                                min = 0,
                                max = 20,
                                value = 2)),
                box(title = "Interpolation", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = 6,
                    withSpinner(plotOutput("inversdistPlot"))
              ))
      ),
      tabItem(tabName = "AS3",
              fluidRow(col=12,
              sidebarPanel(
                numericInput("sill", 
                             h3("Seuil"), 
                             value = 1),
                radioButtons("model", h3("Choix du modèle"),
                             choices = list("Nugget" = "Nug",
                                            "Exponentiel" = "Exp",
                                            "Spherique" = "Sph",
                                            "Gaussien" = "Gau",
                                            "Exponentiel" = "Exc",
                                            "Matern" = "Mat",
                                            "Paramétrisation Stein" = "Ste",
                                            "Circulaire" = "Cir",
                                            "Lineaire" = "Lin",
                                            "Bessel" = "Bes",
                                            "Pentaspherique" = "Pen",
                                            "Periodique" = "Per",
                                            "Wave" = "Wav",
                                            "Hole" = "Hol",
                                            "Logarithmic" = "Log",
                                            "Power" = "Pow",
                                            "Spline" = "Spl",
                                            "Legendre" = "Leg",
                                            "Measuement error" = "Err",
                                            "Intercept" = "Int")
                             ,selected = 1),
                sliderInput("range",
                            "Range:",
                            min = 0,
                            max = 1000,
                            value = 2),
                numericInput("nugget", 
                             h3("Valeur Pépite"), 
                             value = 1),
                radioButtons("krigplot", "Choix du  plot de Krigeage",
                             c("Interpolation" = "int",
                               "erreur" = "err")) 
              )),
              fluidRow(
                box(
                  title = "Sélection du modèle", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 6,
                  withSpinner(plotOutput("modelk"))
                ),
                box(
                  title = "Krigeage", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 6,
                  withSpinner(plotOutput("krig"))
                )
              )),
      tabItem(tabName = "AS4",
              fluidRow(
              box(
                title = "Graphe de Gabriel", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                withSpinner(plotOutput("gab"))
              ),
              box(
                title = "Triangulation de Delaunay", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,
                withSpinner(plotOutput("delaunay"))
              )),
              fluidRow(
              box(
                title = "Graphe connexion au plus proche voisin", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,width = 6,  
                numericInput("nbvoisin",h3("Nombre de plus proche voisin"),value = 1),
                withSpinner(plotOutput("voisin"))
              ),  
              box(
                  title = "Graphe de connexion", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 6,  
                  numericInput("dmin", 
                               h3("Distance minimum"), 
                               value = 1),
                  numericInput("dmax", 
                               h3("Distance maximum"), 
                               value = 50),
                  withSpinner(plotOutput("d1d2"))
                )),  
              fluidRow(
                box(
                  title = "Minimum spanning tree", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 12,  
                  withSpinner(plotOutput("spantree"))
                ))
      ),
      tabItem(tabName = "AS5",
            fluidRow(
                box(
                  title = "Graphe connexion", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 6,
                  selectInput('Im', 'Sélection des données', ""),
                  radioButtons("autotest", "Technique",
                               c("I de Moran" = "I",
                                 "C de Geary" = "C",
                                 "Join count" = "J")),
                  radioButtons("Irand", "Randomisation",
                               c("TRUE" = TRUE,
                                 "FALSE" = FALSE)),
                  radioButtons("Ialt", "Alternative",
                               c("Greater" = "greater",
                                 "Less" = "less",
                                 "Two.sided" = "two.sided"))
                ),
                box(
                  title = "I de Moran", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 6,  
                  verbatimTextOutput("Imoran")
                )),
              fluidRow(
                box(
                  title = "Corrélogramme", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 12,
                  withSpinner(plotOutput("correloIC"))
              )),
              fluidRow(
                box(
                  title = "Mantel Test", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 12,
                  numericInput("firstSpCOl", 
                               h3("Selectionner la premiere colonne d'espèce"), 
                               value = 9)
              )),
              fluidRow(
                box(
                  title = "Mantel Test", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 6,  
                  verbatimTextOutput("manteltest")
                ),  
                box(
                  title = "Graphe Mantel", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,width = 6,
                  withSpinner(plotOutput("Pmant"))
                ))
              ),
      tabItem(tabName = "About",
              p("Cet outil statistique a été développé par des étudiants de master 2 Bio-informatique de Bordeaux en 2018."),
              p("Il a été réalisé pour l'UE Statistiques perfectionnement de l'université de Bordeaux. C'est notamment grâce au package",em("'Shiny'")," dans R.")
      )
    )
  )
)
)

    