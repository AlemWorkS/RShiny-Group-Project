# Single-file Shiny app matching the provided dashboard brief

# Minimal package bootstrap ---------------------------------------------------
required_packages <- c(
  "shiny", "ggplot2", "plotly", "dplyr", "DT", "leaflet",
  "readxl", "gganimate", "gifski", "transformr", "scales"
)
optional_packages <- c("rAmCharts")
missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}
invisible(lapply(required_packages, library, character.only = TRUE, warn.conflicts = FALSE))
has_rAmCharts <- FALSE
if (requireNamespace("rAmCharts", quietly = TRUE)) {
  needed <- c("amBoxplot", "amHistogram", "amChartsOutput", "renderAmCharts")
  has_rAmCharts <- all(needed %in% getNamespaceExports("rAmCharts")) &&
    exists("controlShinyPlot", envir = asNamespace("rAmCharts"), inherits = FALSE)
  if (has_rAmCharts) {
    suppressPackageStartupMessages(library(rAmCharts))
  }
}

# Helpers ---------------------------------------------------------------------
app_colors <- list(
  accent = "#7c3aed",
  accent_soft = "#6366f1",
  background = "#f5f7fb",
  card = "#ffffff",
  text = "#111827"
)

load_assure_data <- function() {
  potential <- c("data/MASTER_MIAGE_BASE_ETUDE_1.xlsx", "MASTER_MIAGE_BASE_ETUDE_1.xlsx")
  for (p in potential) {
    if (file.exists(p)) {
      return(readxl::read_excel(p))
    }
  }
  tibble::tibble(
    ANNEE = integer(), INTERMEDIAIRE = character(), ASSURE = character(),
    PRIME_EMISE = double(), CHARGE_SIN_SURVENANCE = double(), NB_SINISTRE = integer()
  )
}

assure_data_raw <- load_assure_data()
assure_data <- assure_data_raw %>%
  mutate(ratio_sp = ifelse(PRIME_EMISE > 0, CHARGE_SIN_SURVENANCE / PRIME_EMISE, NA_real_))

ratio_table <- function(group_var = "ASSURE") {
  df <- assure_data
  df$group_key <- as.character(df[[group_var]])
  
  df <- df %>%
    group_by(group_key) %>%
    summarise(
      Prime = sum(PRIME_EMISE, na.rm = TRUE),
      Sinistre = sum(CHARGE_SIN_SURVENANCE, na.rm = TRUE),
      NB_SINISTRE = sum(NB_SINISTRE, na.rm = TRUE),
      Ratio_SP = ifelse(Prime > 0, Sinistre / Prime, NA_real_),
      .groups = "drop"
    ) %>%
    arrange(desc(Prime))
  
  total_row <- tibble::tibble(
    group_key = "TOTAL",
    Prime = sum(df$Prime, na.rm = TRUE),
    Sinistre = sum(df$Sinistre, na.rm = TRUE),
    NB_SINISTRE = sum(df$NB_SINISTRE, na.rm = TRUE),
    Ratio_SP = ifelse(sum(df$Prime, na.rm = TRUE) > 0,
                      sum(df$Sinistre, na.rm = TRUE) / sum(df$Prime, na.rm = TRUE),
                      NA_real_
    )
  )
  
  bind_rows(df, total_row) %>%
    mutate(group_key = as.character(group_key)) %>%
    rename(!!group_var := group_key)
}

dashboard_stats <- list(
  assures = dplyr::n_distinct(assure_data$ASSURE),
  prime = sum(assure_data$PRIME_EMISE, na.rm = TRUE),
  sinistre = sum(assure_data$CHARGE_SIN_SURVENANCE, na.rm = TRUE),
  ratio = ifelse(
    sum(assure_data$PRIME_EMISE, na.rm = TRUE) > 0,
    sum(assure_data$CHARGE_SIN_SURVENANCE, na.rm = TRUE) /
      sum(assure_data$PRIME_EMISE, na.rm = TRUE),
    NA_real_
  )
)
dashboard_stats$ratio_text <- ifelse(
  is.na(dashboard_stats$ratio),
  "n/a",
  paste0(round(dashboard_stats$ratio * 100, 2), "%")
)

# UI snippets -----------------------------------------------------------------
login_ui <- div(
  class = "login-shell",
  div(
    class = "login-card glass",
    div(class = "brand-mark", "MIAGE Viz"),
    h2("Connexion securisee"),
    p("Identifiez-vous pour acceder au tableau de bord."),
    textInput("user_id", "Utilisateur", placeholder = "admin"),
    passwordInput("pwd", "Mot de passe", placeholder = "shiny2026"),
    actionButton("login", "Se connecter", class = "primary-btn"),
    span(class = "hint", "Identifiants demo : admin / shiny2026")
  )
)

carousel_images <- paste0("carousel", 1:5, ".jpg")

value_card <- function(title, value, delta = NULL, icon_name = NULL) {
  div(
    class = "value-card",
    div(class = "value-card-top",
        span(class = "value-label", title),
        if (!is.null(icon_name)) tags$i(class = paste("fa-solid", icon_name))
    ),
    div(class = "value-number", value),
    if (!is.null(delta)) span(class = "value-delta", delta)
  )
}

# UI --------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600;700&display=swap"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css",
      integrity = "sha512-DTOQO9RWCH3ppGqcWaEA1BIZOC6xxalwEsw9c2QQeAIftl+Vegovlnee1c9QX4TctnWMn13TZye+giMm8e2LwA==",
      crossorigin = "anonymous", referrerpolicy = "no-referrer"
    ),
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$script(src = "custom.js"),
    tags$script(
      HTML(
        "Shiny.addCustomMessageHandler('set-mode', function(mode){
           document.body.classList.toggle('dark-mode', mode === 'dark');
         });"
      )
    )
  ),
  div(class = "app-wrapper", uiOutput("secured_ui"))
)

# Server ----------------------------------------------------------------------
server <- function(input, output, session) {
  selected_tab <- reactiveVal("accueil")
  logged_in <- reactiveVal(FALSE)
  
  observeEvent(input$login, {
    if (identical(input$user_id, "admin") && identical(input$pwd, "shiny2026")) {
      logged_in(TRUE)
      showNotification("Bienvenue sur le tableau de bord.", type = "message")
      selected_tab("accueil")
      updateTabsetPanel(session, "main_tabs", selected = "accueil")
    } else {
      showNotification("Identifiants invalides.", type = "error")
    }
  })
  
  observeEvent(input$logout, {
    logged_in(FALSE)
    selected_tab("accueil")
  })
  
  observeEvent(input$go_accueil, {
    selected_tab("accueil")
    updateTabsetPanel(session, "main_tabs", selected = "accueil")
  })
  observeEvent(input$go_analyse, {
    selected_tab("analyse")
    updateTabsetPanel(session, "main_tabs", selected = "analyse")
  })
  observeEvent(input$go_reponses, {
    selected_tab("reponses")
    updateTabsetPanel(session, "main_tabs", selected = "reponses")
  })
  observeEvent(input$go_guide, {
    selected_tab("guide")
    updateTabsetPanel(session, "main_tabs", selected = "guide")
  })
  
  output$sidebar_links <- renderUI({
    links <- list(
      list(id = "accueil", label = "Accueil", icon = "house"),
      list(id = "analyse", label = "Analyse", icon = "chart-simple"),
      list(id = "reponses", label = "Reponses", icon = "list"),
      list(id = "guide", label = "Guide", icon = "book-open")
    )
    
    tagList(lapply(links, function(l) {
      actionLink(
        inputId = paste0("go_", l$id),
        label = span(
          tags$i(class = paste("fa-solid", l$icon)),
          l$label
        ),
        class = paste("side-link", if (selected_tab() == l$id) "active")
      )
    }))
  })
  
  # Map state and history -----------------------------------------------------
  default_point <- data.frame(
    Latitude = 5.3363,
    Longitude = -4.0244,
    Label = "Ecole de Police d'Abidjan",
    Added = Sys.time()
  )
  map_points <- reactiveVal(default_point)
  
  observeEvent(input$add_point, {
    req(input$user_lat, input$user_lon)
    new_row <- data.frame(
      Latitude = input$user_lat,
      Longitude = input$user_lon,
      Label = ifelse(nzchar(input$user_label), input$user_label, "Point utilisateur"),
      Added = Sys.time()
    )
    map_points(rbind(map_points(), new_row))
  })
  
  output$map_leaflet <- renderLeaflet({
    pts <- map_points()
    leaflet(pts) |>
      addTiles() |>
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude, label = ~Label,
        color = app_colors$accent, radius = 6, fillOpacity = 0.9
      )
  })
  
  observeEvent(map_points(), {
    pts <- map_points()
    leafletProxy("map_leaflet", data = pts) |>
      clearMarkers() |>
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude, label = ~Label,
        color = app_colors$accent, radius = 6, fillOpacity = 0.9
      )
  })
  
  output$history_table <- DT::renderDT({
    map_points()
  }, options = list(pageLength = 5, dom = "tip"), rownames = FALSE)
  
  # Base plots and ggplot versions -------------------------------------------
  output$boxplot_base <- renderPlot({
    boxplot(Sepal.Length ~ Species,
            data = iris,
            col = "#ede9fe", border = app_colors$accent,
            main = "Base boxplot", xlab = "Species", ylab = "Sepal length"
    )
  })
  
  output$boxplot_gg <- renderPlot({
    ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
      geom_boxplot(alpha = 0.85) +
      scale_fill_manual(values = c("#7c3aed", "#818cf8", "#38bdf8")) +
      labs(
        title = "Boxplot ggplot2 avec titre",
        x = "Species", y = "Sepal length"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  output$scatter_base <- renderPlot({
    plot(iris$Sepal.Length, iris$Sepal.Width,
         col = as.numeric(iris$Species),
         pch = 19, main = "Base scatter", xlab = "Sepal length", ylab = "Sepal width"
    )
    legend("topright", legend = levels(iris$Species), col = 1:3, pch = 19)
  })
  
  output$scatter_gg <- renderPlot({
    ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
      geom_point(size = 3, alpha = 0.9) +
      scale_color_manual(values = c("#7c3aed", "#22d3ee", "#f97316")) +
      labs(title = "Scatter ggplot2", x = "Sepal length", y = "Sepal width") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")
  })
  
  output$hist_base <- renderPlot({
    hist(iris$Sepal.Length,
         col = "#ef4444", border = "#991b1b",
         main = "Histogramme base", xlab = "Sepal length"
    )
  })
  
  output$hist_gg <- renderPlot({
    ggplot(iris, aes(Sepal.Length)) +
      geom_histogram(fill = "#ef4444", color = "#991b1b", bins = 15) +
      labs(title = "Histogramme ggplot2", x = "Sepal length", y = "Frequence") +
      theme_minimal(base_size = 14)
  })
  
  output$scatter_grad_base <- renderPlot({
    cols <- scales::col_numeric(
      palette = c("#7c3aed", "#e0f2fe"),
      domain = range(iris$Petal.Width)
    )(iris$Petal.Width)
    plot(
      iris$Sepal.Length, iris$Sepal.Width,
      col = cols, pch = 19,
      main = "Base scatter gradient",
      xlab = "Sepal length", ylab = "Sepal width"
    )
  })
  
  output$scatter_grad_gg <- renderPlot({
    ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Petal.Width)) +
      geom_point(size = 3, alpha = 0.9) +
      scale_color_gradient(low = "#7c3aed", high = "#0ea5e9") +
      labs(
        title = "Scatter ggplot2 degrade par largeur de petale",
        x = "Sepal length", y = "Sepal width", color = "Petal width"
      ) +
      theme_minimal(base_size = 14)
  })
  
  output$plotly_scatter <- plotly::renderPlotly({
    iris %>%
      group_by(Species) %>%
      plotly::plot_ly(
        x = ~Sepal.Length, y = ~Sepal.Width,
        type = "scatter", mode = "markers",
        color = ~Species, colors = c("#7c3aed", "#22d3ee", "#f97316"),
        marker = list(size = 12, opacity = 0.85)
      ) %>%
      plotly::layout(
        title = "Plotly + dplyr scatter",
        xaxis = list(title = "Sepal length"),
        yaxis = list(title = "Sepal width")
      )
  })
  
  output$boxplot_ggplotly <- plotly::renderPlotly({
    p <- ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
      geom_boxplot(alpha = 0.85) +
      scale_fill_manual(values = c("#7c3aed", "#22d3ee", "#f97316")) +
      labs(title = "Boxplot interactif", x = "Species", y = "Sepal length") +
      theme_minimal(base_size = 14)
    plotly::ggplotly(p)
  })
  
  output$amcharts_ui <- renderUI({
    if (!has_rAmCharts) {
      div(class = "info-card",
          strong("rAmCharts non installe."),
          p("Installez/chargez rAmCharts (version avec amBoxplot/amHistogram et controlShinyPlot) pour activer ces graphiques.")
      )
    } else {
      tagList(
        rAmCharts::amChartsOutput("ram_box", height = "320px"),
        rAmCharts::amChartsOutput("ram_hist", height = "320px")
      )
    }
  })
  
  if (has_rAmCharts) {
    output$ram_box <- rAmCharts::renderAmCharts({
      tryCatch(
        rAmCharts::amBoxplot(x = "Sepal.Length", data = iris, group = "Species"),
        error = function(e) {
          shiny::validate(shiny::need(FALSE, paste("Erreur rAmCharts:", conditionMessage(e))))
        }
      )
    })
    
    output$ram_hist <- rAmCharts::renderAmCharts({
      tryCatch(
        rAmCharts::amHistogram(x = iris$Sepal.Length, breaks = 15, col = "#ef4444"),
        error = function(e) {
          shiny::validate(shiny::need(FALSE, paste("Erreur rAmCharts:", conditionMessage(e))))
        }
      )
    })
  }
  
  output$mpg_anim <- renderImage({
    validate(need(requireNamespace("gganimate", quietly = TRUE), "gganimate est requis."))
    tmp <- tempfile(fileext = ".gif")
    p <- ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
      geom_boxplot(alpha = 0.85) +
      scale_fill_manual(values = c("#c7d2fe", "#a5b4ff", "#7c3aed")) +
      labs(
        title = "Transition par gear: {closest_state}",
        x = "Cylindres", y = "Consommation (mpg)"
      ) +
      gganimate::transition_states(gear, transition_length = 2, state_length = 1) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
    
    renderer_fun <- NULL
    if ("renderer_gifski" %in% getNamespaceExports("gganimate")) {
      renderer_fun <- gganimate::renderer_gifski
    } else if ("gifski_renderer" %in% getNamespaceExports("gganimate")) {
      renderer_fun <- gganimate::gifski_renderer
    }
    
    anim <- tryCatch({
      validate(need(!is.null(renderer_fun), "Mettre a jour gganimate/gifski pour disposer du renderer gifski."))
      gganimate::animate(
        p,
        renderer = renderer_fun(loop = TRUE),
        width = 700, height = 420
      )
    }, error = function(e) {
      validate(need(FALSE, paste("Renderer gifski manquant ou trop ancien :", conditionMessage(e))))
    })
    gganimate::anim_save(tmp, animation = anim)
    list(src = tmp, contentType = "image/gif", alt = "gganimate boxplot", width = "100%")
  }, deleteFile = TRUE)
  
  # Ratios table --------------------------------------------------------------
  output$ratio_table <- DT::renderDT({
    req(input$ratio_view)
    group_var <- if (identical(input$ratio_view, "assure")) "ASSURE" else "ANNEE"
    ratio_table(group_var) %>%
      mutate(Ratio_SP = round(Ratio_SP * 100, 2)) %>%
      rename(`Ratio SP (%)` = Ratio_SP)
  }, options = list(pageLength = 7, dom = "tip"), rownames = FALSE)
  
  # Responses text ------------------------------------------------------------
  output$response_details <- renderUI({
    tagList(
      h4("a) Boxplots (base et ggplot2)"),
      tags$pre("
# Base
boxplot(Sepal.Length ~ Species, data = iris)

# ggplot2
ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
  geom_boxplot() +
  theme_minimal()
"),
      h4("b) Avantages de ggplot2"),
      tags$ul(
        tags$li("Approche declarative: aes() se combine aux geometries."),
        tags$li("Themes et palettes integrees (theme_minimal, scale_*)."),
        tags$li("Extensions rapides (ggplotly, gganimate)."),
        tags$li("Facettage et overlays simples.")
      ),
      h4("c) Ajouter un titre au boxplot ggplot2"),
      tags$pre("
ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
  geom_boxplot() +
  labs(title = 'Distribution de Sepal.Length par espece')
"),
      h4("d) Scatter plots Sepal.Width vs Sepal.Length"),
      tags$pre("
# Base
plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species, pch = 19)

# ggplot2
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point()
"),
      h4("e) Histogrammes Sepal.Length"),
      tags$pre("
hist(iris$Sepal.Length, col = 'red', xlab = 'Sepal.Length', main = 'Base')

ggplot(iris, aes(Sepal.Length)) +
  geom_histogram(fill = 'red', color = 'darkred')
"),
      h4("f) Scatter degrade sur Petal.Width"),
      tags$pre("
cols <- scales::col_numeric(c('purple','lightblue'), domain = range(iris$Petal.Width))(iris$Petal.Width)
plot(iris$Sepal.Length, iris$Sepal.Width, col = cols, pch = 19)

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Petal.Width)) +
  geom_point() +
  scale_color_gradient(low = 'purple', high = 'skyblue')
"),
      h4("g) rAmCharts"),
      tags$pre("
amBoxplot(x = 'Sepal.Length', data = iris, group = 'Species')
amHistogram(x = iris$Sepal.Length, breaks = 15)
"),
      h4("h) Leaflet (fonction coordonnees)"),
      tags$pre("
show_point <- function(lat, lon, label = 'Point') {
  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(lng = lon, lat = lat, label = label)
}
"),
      h4("i) Plotly + dplyr"),
      tags$pre("
iris %>%
  group_by(Species) %>%
  plot_ly(x = ~Sepal.Length, y = ~Sepal.Width,
          type = 'scatter', mode = 'markers', color = ~Species)
"),
      h4("j) Boxplot interactif ggplotly"),
      tags$pre("
p <- ggplot(iris, aes(Species, Sepal.Length, fill = Species)) + geom_boxplot()
ggplotly(p)
"),
      h4("k) gganimate sur mtcars"),
      tags$pre("
p <- ggplot(mtcars, aes(factor(cyl), mpg, fill = factor(cyl))) +
  geom_boxplot() +
  transition_states(gear, transition_length = 2, state_length = 1)
animate(p, renderer = gifski_renderer())
"),
      h4("l) Ratios S/P par annee ou assure"),
      tags$pre("
df %>%
  group_by(ANNEE) %>%
  summarise(Prime = sum(PRIME_EMISE),
            Sinistre = sum(CHARGE_SIN_SURVENANCE),
            Ratio_SP = Sinistre / Prime) %>%
  bind_rows(tibble(ANNEE = 'TOTAL', ...))
")
    )
  })
  
  # Main secured UI -----------------------------------------------------------
  output$secured_ui <- renderUI({
    if (!logged_in()) {
      return(login_ui)
    }
    
    fluidPage(
      class = "dashboard-page",
      div(
        class = "dashboard-shell",
        div(
          class = "sidebar",
          div(class = "brand", span(class = "brand-icon", "B"), span("Business Dashboard")),
          div(class = "nav-block", uiOutput("sidebar_links")),
          div(class = "nav-block muted",
              actionLink("logout", label = span(tags$i(class = "fa-solid fa-arrow-right-from-bracket"), "Se deconnecter"),
                         class = "side-link")
          ),
          div(class = "helper-card",
              img(src = "helper.jpg", class = "helper-img"),
              strong("Besoin d'aide ?"),
              p("Nous restons disponibles pour tout support.")
          )
        ),
        div(
          class = "main-panel",
          div(
            class = "topbar",
            div(
              class = "top-left",
              h3("Analytics"),
              dateRangeInput(
                "daterange",
                NULL,
                start = Sys.Date() - 30, end = Sys.Date(),
                separator = " - "
              )
            ),
            div(
              class = "top-right",
              actionButton("mode_light", label = tags$i(class = "fa-regular fa-sun"), class = "icon-btn"),
              actionButton("mode_dark", label = tags$i(class = "fa-regular fa-moon"), class = "icon-btn"),
              div(class = "user-chip",
                  img(src = "avatar.jpg", class = "avatar"),
                  span("Kristi Kamiyoko")
              )
            )
          ),
          tabsetPanel(
            id = "main_tabs", type = "hidden", selected = selected_tab(),
            tabPanel(
              "accueil",
              div(
                class = "home-grid",
                div(
                  class = "hero-card",
                  div(
                    class = "hero-copy",
                    h2("Light Theme dashboard"),
                    p("Visualisez vos indicateurs en un seul endroit, inspire du layout fourni.")
                  ),
                  div(
                    id = "hero-carousel",
                    lapply(carousel_images, function(img) {
                      tags$img(src = img, class = "carousel-image")
                    })
                  )
                ),
                div(
                  class = "stats-grid",
                  value_card("Assures uniques", dashboard_stats$assures, "+ actifs", "fa-users"),
                  value_card("Prime emise", scales::comma(dashboard_stats$prime), NULL, "fa-money-bill"),
                  value_card("Sinistres", scales::comma(dashboard_stats$sinistre), NULL, "fa-shield-halved"),
                  value_card("Ratio S/P", dashboard_stats$ratio_text, NULL, "fa-chart-column")
                )
              )
            ),
            tabPanel(
              "analyse",
              div(
                class = "analysis-section",
                h3("Analyse"),
                tabsetPanel(
                  type = "pills",
                  tabPanel(
                    "DATA_CHART",
                    div(class = "card",
                        selectInput(
                          "ratio_view",
                          "Choisissez la vue",
                          choices = c("Par assure" = "assure", "Par annee" = "annee"),
                          selected = "assure"
                        ),
                        DT::DTOutput("ratio_table")
                    ),
                    div(class = "card",
                        h4("Historique des points leaflet"),
                        DT::DTOutput("history_table")
                    )
                  ),
                  tabPanel(
                    "GRAPHICS",
                    selectInput(
                      "graphic_question",
                      "Choisissez une question",
                      choices = c(
                        "a. Boxplots base + ggplot2" = "qa",
                        "d. Scatter Sepal.Width vs Sepal.Length" = "qd",
                        "e. Histogrammes Sepal.Length" = "qe",
                        "f. Scatter degrade Petal.Width" = "qf",
                        "g. rAmCharts" = "qg",
                        "h. Leaflet + historique" = "qh",
                        "i. Plotly + dplyr" = "qi",
                        "j. Boxplot interactif ggplotly" = "qj",
                        "k. Boxplot anime (gganimate)" = "qk"
                      ),
                      selected = "qa"
                    ),
                    uiOutput("graphic_zone")
                  )
                )
              )
            ),
            tabPanel(
              "reponses",
              div(class = "card", h3("Details des reponses"), uiOutput("response_details"))
            ),
            tabPanel(
              "guide",
              div(
                class = "card",
                h3("Guide utilisateur"),
                tags$ul(
                  tags$li("Authentification : utiliser admin / shiny2026."),
                  tags$li("Navigation : barre laterale pour ouvrir Accueil, Analyse, Reponses, Guide."),
                  tags$li("Analyse : deux sous-onglets DATA_CHART (tableaux) et GRAPHICS (plots)."),
                  tags$li("Leaflet : saisir latitude/longitude puis cliquer sur Ajouter pour tracer un point."),
                  tags$li("Ratios : choisir le regroupement (assure ou annee) via la liste deroulante."),
                  tags$li("Mode clair/sombre : boutons soleil et lune pour basculer le theme.")
                )
              )
            )
          )
        )
      )
    )
  })
  
  # Graphic area rendering ----------------------------------------------------
  output$graphic_zone <- renderUI({
    req(input$graphic_question)
    switch(input$graphic_question,
           qa = div(
             class = "grid-2",
             div(class = "card", h4("Boxplot base R"), plotOutput("boxplot_base")),
             div(class = "card", h4("Boxplot ggplot2"), plotOutput("boxplot_gg"))
           ),
           qd = div(
             class = "grid-2",
             div(class = "card", h4("Scatter base R"), plotOutput("scatter_base")),
             div(class = "card", h4("Scatter ggplot2"), plotOutput("scatter_gg"))
           ),
           qe = div(
             class = "grid-2",
             div(class = "card", h4("Histogramme base"), plotOutput("hist_base")),
             div(class = "card", h4("Histogramme ggplot2"), plotOutput("hist_gg"))
           ),
           qf = div(
             class = "grid-2",
             div(class = "card", h4("Scatter gradient base"), plotOutput("scatter_grad_base")),
             div(class = "card", h4("Scatter gradient ggplot2"), plotOutput("scatter_grad_gg"))
           ),
           qg = div(class = "card", h4("rAmCharts"), uiOutput("amcharts_ui")),
           qh = div(
             class = "grid-2",
             div(
               class = "card",
               h4("Leaflet (coordonnees libres)"),
               numericInput("user_lat", "Latitude", value = default_point$Latitude, step = 0.0001),
               numericInput("user_lon", "Longitude", value = default_point$Longitude, step = 0.0001),
               textInput("user_label", "Label", value = default_point$Label),
               actionButton("add_point", "Ajouter le point", class = "primary-btn"),
               leafletOutput("map_leaflet", height = 320)
             ),
             div(class = "card", h4("Historique des points"), DT::DTOutput("history_table"))
           ),
           qi = div(class = "card", h4("Plotly + dplyr scatter"), plotlyOutput("plotly_scatter")),
           qj = div(class = "card", h4("Boxplot interactif"), plotlyOutput("boxplot_ggplotly")),
           qk = div(class = "card", h4("gganimate mpg ~ cyl"), imageOutput("mpg_anim"))
    )
  })
  
  observeEvent(logged_in(), {
    if (isTRUE(logged_in())) {
      session$sendCustomMessage("restart-carousel", list())
    }
  })
  
  # Theme toggles -------------------------------------------------------------
  observeEvent(input$mode_light, {
    session$sendCustomMessage("set-mode", "light")
  })
  observeEvent(input$mode_dark, {
    session$sendCustomMessage("set-mode", "dark")
  })
}

shinyApp(ui, server)
