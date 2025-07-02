library(shiny)
library(shinythemes)
library(nnet)  
library(ggplot2)
library(dplyr)
library(stringr)

# --- Carga y limpieza de datos ---
data <- read.csv("conjunto_corazon_final_11.csv")

data <- data %>%
  mutate(
    sexo = str_to_lower(sexo),
    sexo = case_when(
      sexo %in% c("mujer", "muje", "femenino") ~ "Mujer",
      sexo %in% c("varón", "varon", "hombre", "masculino") ~ "Hombre",
      TRUE ~ NA_character_
    ),
    sexo = factor(sexo, levels = c("Mujer", "Hombre")),
    
    glucosa_ayunas = str_trim(str_to_lower(glucosa_ayunas)),
    glucosa_ayunas = na_if(glucosa_ayunas, ""),
    glucosa_ayunas = case_when(
      glucosa_ayunas %in% c("no", "n", "n0") ~ "No",
      glucosa_ayunas %in% c("si", "sí", "s") ~ "Sí",
      TRUE ~ NA_character_
    ),
    glucosa_ayunas = factor(glucosa_ayunas, levels = c("No", "Sí")),
    
    pendiente_st = str_trim(str_to_lower(pendiente_st)),
    pendiente_st = na_if(pendiente_st, ""),
    pendiente_st = case_when(
      pendiente_st %in% c("asc", "ascendente") ~ "ascendente",
      pendiente_st %in% c("desc", "descend", "descendente") ~ "descendente",
      pendiente_st %in% c("flat", "plana") ~ "plana",
      TRUE ~ NA_character_
    ),
    pendiente_st = factor(pendiente_st, levels = c("ascendente", "plana", "descendente")),
    
    angina_ejercicio = factor(angina_ejercicio, levels = c(0,1), labels = c("No", "Sí")),
    ecg_reposo = factor(ecg_reposo, levels = c(0,1,2), labels = c("Normal", "Anormalidad en la onda ST-T", "Hipertrofia ventricular izquierda")),
    tipo_dolor_pecho = factor(tipo_dolor_pecho, levels = c(0,1,2,3,4),
                              labels = c("Angina típica", "Angina típica", "Angina atípica", "Dolor no anginoso", "Asintomático"))
  )

data$edad <- cut(data$edad, breaks = c(-Inf, 39, 49, 59, 69, 79),
                 labels = c("0-39", "40-49", "50-59", "60-69", "70-79"))
data$edad <- factor(data$edad, levels = c("0-39", "40-49", "50-59", "60-69", "70-79"))

data$Enfermedad <- as.numeric(data$Enfermedad == "Sí")

eliminar_valores_atipicos <- function(data, var_name) {
  var <- data[[var_name]]
  Q1 <- quantile(var, 0.25, na.rm = TRUE)
  Q3 <- quantile(var, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQR_val
  lim_sup <- Q3 + 1.5 * IQR_val
  data[var >= lim_inf & var <= lim_sup | is.na(var), ]
}

vars_outliers <- c("presion_reposo", "colesterol", "frec_max", "descenso_st")
for (var in vars_outliers) {
  data <- eliminar_valores_atipicos(data, var)
}

data_original <- data
data <- na.omit(data)

variables_numericas <- sapply(data, is.numeric)
num_cols <- names(variables_numericas[variables_numericas == TRUE])

medias <- sapply(data[, num_cols], mean)
desvios <- sapply(data[, num_cols], sd)

data[, num_cols] <- scale(data[, num_cols], center = medias, scale = desvios)

X <- model.matrix(Enfermedad ~ . - 1, data = data)
y <- data$Enfermedad

modelo_ann <- nnet(X, y, size = 5, maxit = 100, decay = 0.01, trace = FALSE)

# --- UI ---
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(HTML("
    <link href='https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&display=swap' rel='stylesheet'>
    <link href='https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&family=Raleway:wght@700&display=swap' rel='stylesheet'>
    <style>
      label.control-label {
        display: block;
        text-align: center;
        font-family: 'Montserrat', sans-serif;
        color: #00AEDA;
        font-weight: 500;
      }
      
      /* Centrar inputs de tipo select */
      select.form-control {
        margin: 0 auto;
        display: block;
        width: 80%;
      }
      
      /* Centrar sliders */
      .js-range-slider {
        margin: 0 auto !important;
        display: block;
        width: 80% !important;
      }
      
      /* Centrar columnas en general */
      .center-col {
        display: flex;
        flex-direction: column;
        align-items: center;
      }
      h2.title-custom {
        font-family: 'Raleway', sans-serif;
        font-weight: 700;
        color: #ffffff;
        margin-top: 20px;
      }
      h4.resultado-texto {
        font-family: 'Montserrat', sans-serif;
        font-weight: 600;
        font-size: 24px;
        text-align: center;
        color: #00AEDA;
        margin-bottom: 10px;
      }
      body {
        font-family: 'Montserrat', sans-serif;
        background-color: #002859;
        color: white;
      }
      h2, label {
        color: #00AEDA;
      }
      .well {
        background-color: #003d73;
        border-color: #00AEDA;
      }
      .btn-primary {
        background-color: #00AEDA;
        border-color: #00AEDA;
      }
      .selectize-input {
        background-color: #ffffff;
        color: #000000;
      }
      .btn-center {
        display: flex;
        justify-content: center;
        align-items: center;
        margin-top: 20px;
      }
    </style>
  ")),
  
  div(class = "text-center",
      h2("Predicción de Enfermedad Cardiovascular", class = "title-custom")
  )
  ,
  
  fluidRow(
    column(6, class = "center-col",
           selectInput("edad", "Edad:", choices = levels(data_original$edad)),
           selectInput("sexo", "Sexo:", choices = levels(data_original$sexo)),
           selectInput("tipo_dolor_pecho", "Tipo de dolor de pecho:", choices = levels(data_original$tipo_dolor_pecho)),
           sliderInput("presion_reposo", "Presión arterial en reposo:",
                       min = min(data_original$presion_reposo, na.rm = TRUE),
                       max = max(data_original$presion_reposo, na.rm = TRUE),
                       value = median(data_original$presion_reposo, na.rm = TRUE)),
           sliderInput("colesterol", "Colesterol:",
                       min = min(data_original$colesterol, na.rm = TRUE),
                       max = max(data_original$colesterol, na.rm = TRUE),
                       value = median(data_original$colesterol, na.rm = TRUE)),
           selectInput("glucosa_ayunas", "Glucosa en ayunas:", choices = levels(data_original$glucosa_ayunas))
    ),
    column(6, class = "center-col",
           selectInput("ecg_reposo", "ECG en reposo:", choices = levels(data_original$ecg_reposo)),
           sliderInput("frec_max", "Frecuencia cardíaca máxima:",
                       min = min(data_original$frec_max, na.rm = TRUE),
                       max = max(data_original$frec_max, na.rm = TRUE),
                       value = median(data_original$frec_max, na.rm = TRUE)),
           selectInput("angina_ejercicio", "Angina inducida por ejercicio:", choices = levels(data_original$angina_ejercicio)),
           sliderInput("descenso_st", "Descenso del segmento ST:",
                       min = min(data_original$descenso_st, na.rm = TRUE),
                       max = max(data_original$descenso_st, na.rm = TRUE),
                       value = median(data_original$descenso_st, na.rm = TRUE), step = 0.1),
           selectInput("pendiente_st", "Pendiente del ST:", choices = levels(data_original$pendiente_st)),
           div(class = "btn-center",
               actionButton("predecir", "Predecir", class = "btn-primary btn-lg"))
    )
  ),
  br(),
  uiOutput("barraRiesgo")
)

# --- Server ---
server <- function(input, output) {
  observeEvent(input$predecir, {
    nuevo_dato <- data.frame(
      edad = factor(input$edad, levels = levels(data_original$edad)),
      sexo = factor(input$sexo, levels = levels(data_original$sexo)),
      tipo_dolor_pecho = factor(input$tipo_dolor_pecho, levels = levels(data_original$tipo_dolor_pecho)),
      presion_reposo = as.numeric(input$presion_reposo),
      colesterol = as.numeric(input$colesterol),
      glucosa_ayunas = factor(input$glucosa_ayunas, levels = levels(data_original$glucosa_ayunas)),
      ecg_reposo = factor(input$ecg_reposo, levels = levels(data_original$ecg_reposo)),
      frec_max = as.numeric(input$frec_max),
      angina_ejercicio = factor(input$angina_ejercicio, levels = levels(data_original$angina_ejercicio)),
      descenso_st = as.numeric(input$descenso_st),
      pendiente_st = factor(input$pendiente_st, levels = levels(data_original$pendiente_st))
    )
    
    vars_num <- c("presion_reposo", "colesterol", "frec_max", "descenso_st")
    for (v in vars_num) {
      nuevo_dato[[v]] <- (nuevo_dato[[v]] - medias[v]) / desvios[v]
    }
    
    nuevo_X <- model.matrix(~ . - 1, data = nuevo_dato)
    
    faltan <- setdiff(colnames(X), colnames(nuevo_X))
    if(length(faltan) > 0) {
      for(col in faltan) {
        nuevo_X <- cbind(nuevo_X, 0)
        colnames(nuevo_X)[ncol(nuevo_X)] <- col
      }
    }
    
    nuevo_X <- nuevo_X[, colnames(X), drop = FALSE]
    prob <- predict(modelo_ann, nuevo_X, type = "raw")
    prob <- as.numeric(prob)
    
    output$barraRiesgo <- renderUI({
      prob_porc <- round(prob * 100, 1)
      
      color <- if (prob < 0.33) {
        "#28a745"
      } else if (prob < 0.66) {
        "#ffc107"
      } else {
        "#dc3545"
      }
      
      mensaje <- if (prob < 0.33) {
        "El riesgo de enfermedad cardiovascular es bajo. Continúe con hábitos saludables."
      } else if (prob < 0.66) {
        "El riesgo es moderado. Sería recomendable realizar un chequeo médico."
      } else {
        "El riesgo es alto. Se recomienda consultar con un especialista lo antes posible."
      }
      
      div(
        style = "margin-top: 40px;",
        h4(paste0("Probabilidad estimada: ", prob_porc, "%"), class = "resultado-texto"),
        p(mensaje, style = "text-align:center; color:#cccccc; font-size:16px;"),
        div(
          style = "width: 80%; margin: 20px auto; background-color: #e9ecef; border-radius: 10px; overflow: hidden;",
          div(
            style = paste0("width:", prob_porc, "%; background-color:", color, "; padding: 15px 0; font-size: 20px; font-weight: bold; color: white; transition: width 0.5s ease;"),
            paste0(prob_porc, "%")
          )
        )
      )
    })
  })
}

shinyApp(ui = ui, server = server)
