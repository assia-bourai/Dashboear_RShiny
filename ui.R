library(shiny)
library(shinydashboard)
ui = dashboardPage(
    dashboardHeader(title = "Machine Learning"),
        
    sidebar = dashboardSidebar(
        fileInput("file", "Load file"),
        checkboxGroupInput('norm', 'Normalization', c('center', 'scale')),
        sliderInput('rows', 'Nomber of rows (%)', min = 1, max=100, value = 25),
        radioButtons('type', 'Type of variables ', c('all', 'numeric', 'categorical')),
        selectInput('remove', 'Remove some variables', c(), multiple = TRUE),
        selectInput('numeric_nan', 'processing of NaN on numerical variables', 
                        c('delete','most frequent','mean', 'std', 'constant')),
        uiOutput('constant_numeric_nan'),
        
        selectInput('categoric_nan', 'processing of NaN on categorical variables',
                        c('delete', 'most frequent', 'constant')),
        uiOutput('constant_categoric_nan'),
        actionButton('apply', 'Apply')
    ),
    body = dashboardBody(
        tabsetPanel(
            id = 'main',
            tabPanel('Visualization', dataTableOutput("dataset_content")),
            tabPanel('Exploration', 
                     fluidRow(box(title="SELECT", column(8,selectInput("variable_type", "Type of variables", c("All variables", 
                                                                                                      "Categorical variables",
                                                                                                      "Numerical variables"))))),
                     verbatimTextOutput("exploration")),
            tabPanel("Univariate analysis",
                     fluidRow(box(title="SELECT", 
                                  column(8, selectInput("univariate_var", "Variable", c("None"))),
                                  column(8, selectInput("univariate_label", "Label", c("None")))
                                  )),
                     fluidRow(
                         column(12, verbatimTextOutput("univariate_summary"))
                     ), 
                     fluidRow(
                         column(12, plotOutput("univariate_plot1"))
                     ), 
                     fluidRow(12, plotOutput("univariate_plot2"))
                     ), 
            tabPanel("Bivariate analysis",
                fluidRow(
                    box(title="SELECT",
                        column(8, selectInput("bivariate_var1", "Variable 1", c("None"))),
                        column(8, selectInput("bivariate_var2", "Variable 2", c("None"))),
                        column(8, selectInput("bivariate_label", "Label", c("None")))
                        )
                ),
                fluidRow(
                    column(12, verbatimTextOutput("bivariate_summary"))
                ),
                fluidRow(
                    column(12, plotOutput("bivariate_plot"))
                )
            ), 
            tabPanel('Factorial analysis',fluidRow(box(title = "SELECT",
                column(8, selectInput("dimension_reduction", "Algorithms", c("","PCA", "CA", "MCA", "FAMD", "MFA"))),
                column(8, sliderInput('ncp', 'ncp', min = 1, max=20, value = 2)))
            ), 
            fluidRow(
                column(12, verbatimTextOutput("summary_dimension_reduction")),
                column(8, selectInput("infos_input", "Infos", c("","EigenValues", "Variables", "Index"))),
                column(12, verbatimTextOutput("infos_output"))
            ),
            fluidRow(
                column(8, selectInput("graph1", "Type of graphe", c("","eigenvalues/variances","variables - contrib",
                                                                    "variables - cos2", "individuals - contrib",
                                                                    "individuals - cos2", "variables and individuals"))),
                column(12, plotOutput("plot1"))
            ),   
        ),
            tabPanel('Clustering', fluidRow(
                box(title = "SELECT",
                    column(8, selectInput("kmeans", "Algorithms", c("","kmeans","spherical kmeans"))),
                    column(8, sliderInput("kmeans_cluster", "Number of clusters", min=2, max=20, value = 3)),
                    column(8, sliderInput("kmeans_iter", "Number of itérations", min=10, max=1000, value = 10))
                    )
            ), 
            fluidRow(
                column(12, verbatimTextOutput("kmeans_summary"))
            ),
            fluidRow(
                column(12, plotOutput("kmeans_plot"))
            )
            ),
            tabPanel('Supervised classification', fluidRow(box(title = "SELECT",
                                                               column(8, selectInput("algo_sc", "Algorithms", c("","Logistic Regression", "rpart", "Naive Bayes"))),
                                                               column(8, selectInput("label_name", "Label", c())),
                                                               column(8, sliderInput("train_size", "Train size %", min = 0.5, max=0.8, value = 0.5)),
                                                               column(8, selectInput("positive", "Positive", c()))
                                                               )),
                     fluidRow(
                         column(12, verbatimTextOutput("model_summary"))
                     ),
                     fluidRow(
                         column(12, verbatimTextOutput("confusion_matrix"))
                     ),
                     fluidRow(
                         column(12, plotOutput("roc_curve"))
                     )
                     ),
            tabPanel('About', verbatimTextOutput("test"))
        )
    )
    
)