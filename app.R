#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(FactoMineR)
library(factoextra)

# Charger les données
data <- read.csv("user_behavior_dataset_modified.csv")
data1 <- read.csv("user_behavior_dataset_modified.csv")

# Définir l'interface utilisateur
ui <- fluidPage(
  titlePanel("Analyse du comportement des utilisateurs d'appareils"),
  sidebarLayout(
    sidebarPanel(
      selectInput("device", "Sélectionnez un modèle d'appareil :", 
                  choices = unique(data$Device.Model), 
                  selected = "Google Pixel 5", 
                  multiple = TRUE),
      selectInput("os", "Sélectionnez un système d'exploitation :", 
                  choices = unique(data$Operating.System), 
                  selected = "Android", 
                  multiple = TRUE),
      selectInput("class", "Sélectionnez la classe de comportement :", 
                  choices = unique(data$User.Behavior.Class), 
                  selected = unique(data$User.Behavior.Class), 
                  multiple = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Utilisation et comportement",
                 plotOutput("hist_app_usage"),
                 plotOutput("bar_battery_usage"),
                 plotOutput("boxplot_usage")),
        tabPanel("Démographie et préférences",
                 plotOutput("pie_gender"),
                 plotOutput("hist_age")),
        tabPanel("Relations et corrélations",
                 plotOutput("scatter_usage"),
                 plotOutput("heatmap_corr")),
        tabPanel("Tableau de données",
                 DTOutput("data_table")),
        tabPanel("Analyses ACP et ACM",
                 selectInput("analysis_type", "Type d'analyse :", choices = c("ACP", "ACM")),
                 plotOutput("analysis_plot_ind"),
                 plotOutput("analysis_plot_var"))
      )
    )
  )
)

# Définir le serveur
server <- function(input, output) {
  
  # Filtrage des données en fonction des sélections de l'utilisateur
  filtered_data <- reactive({
    data %>%
      filter(Device.Model %in% input$device,
             Operating.System %in% input$os,
             User.Behavior.Class %in% input$class)
  })
  
  # Histogramme de l'utilisation des applications
  output$hist_app_usage <- renderPlot({
    ggplot(filtered_data(), aes(x = App.Usage.Time..min.day.)) +
      geom_histogram(binwidth = 30, fill = "skyblue", color = "black") +
      labs(title = "Temps d'utilisation des applications", x = "Temps (min/jour)", y = "Nombre d'utilisateurs")
  })
  
  # Diagramme en barres de la consommation de batterie par modèle
  output$bar_battery_usage <- renderPlot({
    ggplot(filtered_data(), aes(x = Device.Model, y = Battery.Drain..mAh.day., fill = Operating.System)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Consommation de batterie par modèle", x = "Modèle de l'appareil", y = "Consommation de batterie (mAh/jour)")
  })
  
  # Boxplot de l'utilisation des applications par classe de comportement
  output$boxplot_usage <- renderPlot({
    ggplot(filtered_data(), aes(x = as.factor(User.Behavior.Class), y = App.Usage.Time..min.day.)) +
      geom_boxplot(fill = "lightgreen", color = "darkgreen") +
      labs(title = "Utilisation des applications par classe de comportement", x = "Classe de comportement", y = "Temps d'utilisation (min/jour)")
  })
  
  # Diagramme circulaire de la répartition par sexe
  output$pie_gender <- renderPlot({
    gender_data <- filtered_data() %>%
      group_by(Gender) %>%
      summarise(Count = n())
    ggplot(gender_data, aes(x = "", y = Count, fill = Gender)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = "Répartition par sexe") +
      theme_void()
  })
  
  # Histogramme de l'âge
  output$hist_age <- renderPlot({
    ggplot(filtered_data(), aes(x = Age)) +
      geom_histogram(binwidth = 5, fill = "coral", color = "black") +
      labs(title = "Distribution d'âge", x = "Âge", y = "Nombre d'utilisateurs")
  })
  
  # Nuage de points entre l'utilisation des applications et le temps d'écran
  output$scatter_usage <- renderPlot({
    ggplot(filtered_data(), aes(x = App.Usage.Time..min.day., y = Screen.On.Time..hours.day.)) +
      geom_point(color = "purple") +
      labs(title = "Corrélation entre utilisation des applications et temps d'écran", x = "Temps d'utilisation (min/jour)", y = "Temps d'écran (heures/jour)")
  })
  
  # Carte thermique des corrélations
  output$heatmap_corr <- renderPlot({
    correlation_data <- filtered_data() %>% 
      select(App.Usage.Time..min.day., Screen.On.Time..hours.day., Battery.Drain..mAh.day., Number.of.Apps.Installed, Data.Usage..MB.day., Age) %>%
      cor()
    ggplot(data = as.data.frame(as.table(correlation_data)), aes(x = Var1, y = Var2, fill = Freq)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      labs(title = "Carte thermique des corrélations", x = "", y = "")
  })
  
  # Tableau de données interactif
  output$data_table <- renderDT({
    datatable(filtered_data())
  })
  
  # ACP et ACM en fonction du type d'analyse sélectionné
  output$analysis_plot_ind <- renderPlot({
    if (input$analysis_type == "ACP") {
      quant_data <- data %>% select_if(is.numeric)
      res_acp <- PCA(quant_data, graph = FALSE)
      fviz_pca_ind(res_acp, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
    } else if (input$analysis_type == "ACM") {
      qual_data <- data %>%
        mutate_if(~!is.numeric(.), as.factor) %>%
        select_if(is.factor)
      res_acm <- MCA(qual_data, graph = FALSE)
      fviz_mca_ind(res_acm, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
    }
  })
  
  output$analysis_plot_var <- renderPlot({
    if (input$analysis_type == "ACP") {
      quant_data <- data %>% select_if(is.numeric)
      res_acp <- PCA(quant_data, graph = FALSE)
      fviz_pca_var(res_acp, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
    } else if (input$analysis_type == "ACM") {
      qual_data <- data %>%
        mutate_if(~!is.numeric(.), as.factor) %>%
        select_if(is.factor)
      res_acm <- MCA(qual_data, graph = FALSE)
      fviz_mca_var(res_acm, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
    }
  })
  # Téléchargement des graphiques
  output$download_ind_plot <- downloadHandler(
    filename = function() {
      paste("individus_", input$analysis_type, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_ind(), device = "png", width = 8, height = 6)
    }
  )
  
  output$download_var_plot <- downloadHandler(
    filename = function() {
      paste("variables_", input$analysis_type, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_var(), device = "png", width = 8, height = 6)
    }
  )
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)
