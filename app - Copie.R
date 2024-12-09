#Projet de session
#"Fatoumata Seck; NJIKAM YOGWA NELSON I; KOBENAN KOUMAN DUA; CEDRIC KESSAHOU

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(FactoMineR)
library(factoextra)
library(ggcorrplot)

# Charger les données
data <- read.csv("user_behavior_dataset_modified.csv")

# Interface utilisateur avec shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Analyse du Comportement des Utilisateurs", titleWidth = 450),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tableau de Données", tabName = "data_table", icon = icon("table")),
      menuItem("Utilisation et Comportement", tabName = "usage", icon = icon("mobile")),
      menuItem("Démographie et Préférences", tabName = "demographics", icon = icon("users", class = "Classe_d_age")),
      menuItem("Relations et Corrélations", tabName = "correlations", icon = icon("chart-line")),
      menuItem("Analyses ACP et ACM", tabName = "analysis", icon = icon("project-diagram")),
      menuItem("Graphiques Avancés", tabName = "advanced_graphs", icon = icon("chart-bar"))
    ),
    selectInput("device", "Sélectionnez un modèle d'appareil :", choices = unique(data$Device.Model), selected = "Google Pixel 5", multiple = TRUE),
    selectInput("os", "Sélectionnez un système d'exploitation :", choices = unique(data$Operating.System), selected = "Android", multiple = TRUE),
    selectInput("class", "Sélectionnez la classe de comportement :", choices = unique(data$User.Behavior.Class), selected = unique(data$User.Behavior.Class), multiple = TRUE)
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "usage",
              fluidRow(
                box(plotOutput("hist_app_usage"), title = "Utilisation des Applications", status = "primary"),
                box(plotOutput("bar_battery_usage"), title = "Consommation de Batterie", status = "warning"),
                box(plotOutput("boxplot_usage"), title = "Classe de Comportement", status = "success")
              )),
      
      tabItem(tabName = "demographics",
              fluidRow(
                box(plotOutput("pie_gender"), title = "Répartition par Sexe", status = "info"),
                box(plotOutput("dependance_by_age_plot"), title = "Distribution des niveaux de dépendance par classe d'âge", status = "primary")
              )),
      
      tabItem(tabName = "correlations",
              fluidRow(
                box(plotOutput("scatter_usage"), title = "Relation avec Régression : Utilisation vs Batterie", status = "primary"),
                box(plotOutput("heatmap_corr"), title = "Carte de Chaleur de la Consommation de Batterie", status = "info"),
                box(plotOutput("corr_plot"), title = "Diagramme de Corrélation", status = "warning")
              )),
      
      tabItem(tabName = "data_table",
              box(DTOutput("data_table"), width = 12)),
      
      tabItem(tabName = "analysis",
              selectInput("analysis_type", "Type d'analyse :", choices = c("ACP", "ACM")),
              box(plotOutput("analysis_plot_ind"), title = "Individus", status = "primary"),
              box(plotOutput("analysis_plot_var"), title = "Variables", status = "warning")),
      
      tabItem(tabName = "advanced_graphs",
              fluidRow(
                box(plotOutput("violin_apps_behavior"), title = "Violons par Classe de Comportement", status = "primary"),
                box(plotOutput("density_battery_device"), title = "Densité de la Consommation de Batterie", status = "warning"),
                box(plotOutput("stacked_bar_gender_behavior"), title = "Barres Empilées Sexe/Classe", status = "success")
              ))
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$device, input$os, input$class)
    data %>%
      filter(Device.Model %in% input$device,
             Operating.System %in% input$os,
             User.Behavior.Class %in% input$class)
  })
  
  # Calcul de la matrice de corrélation
  cor_matrix <- reactive({
    req(filtered_data())
    cor(filtered_data() %>% select_if(is.numeric), use = "complete.obs")
  })
  
  # Histogramme de l'utilisation des applications
  output$hist_app_usage <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = App.Usage.Time..min.day.)) +
      geom_histogram(binwidth = 30, fill = "skyblue", color = "black") +
      labs(title = "Temps d'utilisation des applications", 
           x = "Temps (min/jour)", y = "Nombre d'utilisateurs")
  })
  
  # Graphique de consommation de batterie par modèle
  output$bar_battery_usage <- renderPlot({
    ggplot(data, aes(x = Device.Model, y = Battery.Drain..mAh.day., fill = Operating.System)) +
      geom_col() +
      theme_minimal()
  })
  
  # Boxplot de l'utilisation des applications par classe de comportement
  output$boxplot_usage <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = as.factor(User.Behavior.Class), y = App.Usage.Time..min.day.)) +
      geom_boxplot(fill = "lightgreen", color = "darkgreen") +
      labs(title = "Utilisation des applications par classe de comportement", 
           x = "Classe de comportement", y = "Temps d'utilisation (min/jour)")
  })
  
  # Diagramme circulaire de la répartition par sexe
  output$pie_gender <- renderPlot({
    req(filtered_data())
    gender_data <- filtered_data() %>%
      group_by(Gender) %>%
      summarise(Count = n())
    
    ggplot(gender_data, aes(x = "", y = Count, fill = Gender)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = "Répartition par sexe") +
      theme_void()
  })
  
  # Graphique : Dépendance par classe d'âge
  output$dependance_by_age_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Classe_d_age, fill = Dépendance)) +
      geom_bar(position = "fill") + 
      labs(
        title = "Distribution des niveaux de dépendance par classe d'âge",
        x = "Classe d'âge",
        y = "Proportion",
        fill = "Niveau de dépendance"
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal()
  })
  
  # Graphique de régression : Relation Utilisation vs Batterie
  output$scatter_usage <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = App.Usage.Time..min.day., y = Battery.Drain..mAh.day., color = Operating.System)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(title = "Relation avec Régression : Utilisation vs Batterie",
           x = "Utilisation des Applications (minutes/jour)",
           y = "Consommation de Batterie (mAh/jour)",
           color = "Système d'exploitation") +
      theme_minimal()
  })
  # Carte de Chaleur : Consommation de Batterie par Système d'Exploitation et Modèle d'Appareil
  output$heatmap_corr <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = Operating.System, y = Device.Model, fill = Battery.Drain..mAh.day.)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +
      labs(title = "Carte de Chaleur de la Consommation de Batterie",
           x = "Système d'exploitation", y = "Modèle de l'appareil",
           fill = "Consommation (mAh/jour)") +
      theme_minimal()
  })
  
  # Diagramme de corrélation
  output$corr_plot <- renderPlot({
    req(cor_matrix())  # Utilisation du calcul réactif de la matrice de corrélation
    ggcorrplot(cor_matrix(), 
               lab = TRUE,  # Afficher les valeurs de corrélation
               lab_size = 4,  # Taille du texte des valeurs de corrélation
               title = "Diagramme de Corrélation", 
               colors = c("blue", "white", "red"))  # Couleurs pour la matrice de corrélation
  })
  # Violons par Classe de Comportement
  output$violin_apps_behavior <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = as.factor(User.Behavior.Class), y = App.Usage.Time..min.day.)) +
      geom_violin(fill = "lightblue", color = "darkblue") +
      labs(title = "Distribution de l'utilisation des applications par classe de comportement", 
           x = "Classe de comportement", y = "Temps d'utilisation (min/jour)")
  })
  
  # Densité de la consommation de batterie
  output$density_battery_device <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = Battery.Drain..mAh.day., fill = Device.Model)) +
      geom_density(alpha = 0.6) +
      labs(title = "Densité de la Consommation de Batterie par Modèle", 
           x = "Consommation de Batterie (mAh/jour)", y = "Densité")
  })
  
  # Barres Empilées Sexe/Classe de comportement
  output$stacked_bar_gender_behavior <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = Gender, fill = as.factor(User.Behavior.Class))) +
      geom_bar(position = "stack") +
      labs(title = "Barres Empilées : Sexe et Classe de Comportement", 
           x = "Sexe", y = "Nombre d'utilisateurs", fill = "Classe de comportement")
  })
  
  # Table des données filtrées
  output$data_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  

  # Analyse en composantes multiples (ACM)
  output$analysis_plot_ind <- renderPlot({
    req(input$analysis_type)
    if (input$analysis_type == "ACM") {
      qual_data <- filtered_data() %>%
        mutate_if(~!is.numeric(.), as.factor) %>%
        select_if(is.factor)
      
      res_acm <- MCA(qual_data, graph = FALSE)
      fviz_mca_ind(res_acm, col.ind = "cos2", 
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                   repel = TRUE)
    }
  })
  
  
  ### Tableau de Données Interactif ###
  output$data_table <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE))
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
    req(input$analysis_type)
    if (input$analysis_type == "ACP") {
      # Sélection des variables quantitatives et suppression des NA
      quant_data <- filtered_data() %>%
        select_if(is.numeric) %>%
        na.omit()
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
}

shinyApp(ui, server)
