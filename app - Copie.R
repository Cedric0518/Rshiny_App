
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(FactoMineR)
library(factoextra)

# Charger les données
data <- read.csv("user_behavior_dataset_modified.csv")

# Interface utilisateur avec shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Analyse du Comportement des Utilisateurs",titleWidth = 450),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Utilisation et Comportement", tabName = "usage", icon = icon("mobile")),
      menuItem("Démographie et Préférences", tabName = "demographics", icon = icon("users","Classe_d_age")),
      menuItem("Relations et Corrélations", tabName = "correlations", icon = icon("chart-line")),
      menuItem("Tableau de Données", tabName = "data_table", icon = icon("table")),
      menuItem("Analyses ACP et ACM", tabName = "analysis", icon = icon("project-diagram"))
    ),
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
                box(plotOutput("scatter_usage"), title = "Corrélation Temps d'Utilisation vs Temps d'Écran", status = "primary"),
                box(plotOutput("heatmap_corr"), title = "Carte Thermique des Corrélations", status = "info")
              )),
      
      tabItem(tabName = "data_table",
              box(DTOutput("data_table"), width = 12)),
      
      tabItem(tabName = "analysis",
              selectInput("analysis_type", "Type d'analyse :", choices = c("ACP", "ACM")),
              box(plotOutput("analysis_plot_ind"), title = "Individus", status = "primary"),
              box(plotOutput("analysis_plot_var"), title = "Variables", status = "warning"))
    )
  )
)



server <- function(input, output, session) {
  
  # Filtrer les données en fonction des sélections de l'utilisateur
  filtered_data <- reactive({
    req(input$device, input$os, input$class)
    data %>%
      filter(Device.Model %in% input$device,
             Operating.System %in% input$os,
             User.Behavior.Class %in% input$class)
  })
  
  ### Graphiques Utilisation et Classe de Comportement ###
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
    req(filtered_data())
    ggplot(filtered_data(), aes(x = Device.Model, y = Battery.Drain..mAh.day., fill = Operating.System)) +
      geom_col(position = "dodge") +
      labs(title = "Consommation de batterie par modèle", x = "Modèle", y = "Batterie (mAh/jour)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
      geom_bar(position = "fill") + # Position "fill" pour les proportions
      labs(
        title = "Distribution des niveaux de dépendance par classe d'âge",
        x = "Classe d'âge",
        y = "Proportion",
        fill = "Niveau de dépendance"
      ) +
      scale_y_continuous(labels = scales::percent) + # Afficher en pourcentage
      theme_minimal()
  })
  
  # Nuage de points entre utilisation des applications et temps d'écran
  output$scatter_usage <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = App.Usage.Time..min.day., y = Screen.On.Time..hours.day.)) +
      geom_point(color = "purple") +
      labs(title = "Corrélation entre utilisation des applications et temps d'écran", 
           x = "Temps d'utilisation (min/jour)", y = "Temps d'écran (heures/jour)")
  })
  
  
  # Nuage de points entre utilisation des applications et temps d'écran
  output$scatter_usage <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = App.Usage.Time..min.day., y = Battery.Drain..mAh.day.)) +
      geom_point(color = "purple") +
      labs(title = "Corrélation entre utilisation des applications et la durée de vie de la batterie", 
           x = "Temps d'utilisation (min/jour)", y = "Durée de vie batterie (jour)")
  })
  
  # Carte thermique des corrélations
  output$heatmap_corr <- renderPlot({
    req(filtered_data())
    correlation_data <- filtered_data() %>%
      select(App.Usage.Time..min.day., Screen.On.Time..hours.day., Battery.Drain..mAh.day., 
             Number.of.Apps.Installed, Data.Usage..MB.day., Age) %>%
      cor(use = "complete.obs")
    
    ggplot(data = as.data.frame(as.table(correlation_data)), aes(x = Var1, y = Var2, fill = Freq)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      labs(title = "Carte thermique des corrélations")
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


# Lancer l'application
shinyApp(ui = ui, server = server)
