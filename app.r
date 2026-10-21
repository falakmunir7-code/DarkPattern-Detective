
library(shiny)
library(ggplot2)

# Check that the existing project objects are available
if (!exists("data") || !exists("model2") ||
    !exists("dtm_small") || !exists("predict_dark_pattern")) {
  
  # DARKPATTERN DETECTIVE - STARTUP SETUP
  
  library(shiny)
  library(ggplot2)
  library(tm)
  library(e1071)
  
  # 1. Load the dataset from the app folder
  data <- read.delim(
    "dataset.tsv",
    header = TRUE,
    sep = "\t",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # 2. Clean the text
  data$clean_text <- tolower(data$text)
  data$clean_text <- trimws(data$clean_text)
  data$clean_text <- gsub("\\s+", " ", data$clean_text)
  
  # 3. Create the text corpus
  corpus <- VCorpus(VectorSource(data$clean_text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  
  # 4. Create document-term matrix
  dtm_small <- DocumentTermMatrix(
    corpus,
    control = list(bounds = list(global = c(5, Inf)))
  )
  
  # 5. Prepare the features and labels
  text_features2 <- as.data.frame(as.matrix(dtm_small))
  text_features2$label <- factor(data$label, levels = c(0, 1))
  
  # 6. Train the model when the app starts
  set.seed(123)
  
  total_rows <- nrow(text_features2)
  random_rows <- sample(total_rows)
  train_size <- floor(0.8 * total_rows)
  
  train_features2 <- text_features2[
    random_rows[1:train_size],
    ,
    drop = FALSE
  ]
  
  model2 <- naiveBayes(
    label ~ .,
    data = train_features2,
    laplace = 1
  )
  
  # 7. Define the prediction function
  predict_dark_pattern <- function(user_text) {
    
    user_text <- tolower(user_text)
    user_text <- gsub("[[:punct:]]", " ", user_text)
    user_text <- gsub("\\s+", " ", user_text)
    user_text <- trimws(user_text)
    
    vocabulary <- colnames(dtm_small)
    words <- unlist(strsplit(user_text, " "))
    
    new_text <- matrix(
      0,
      nrow = 1,
      ncol = length(vocabulary),
      dimnames = list(NULL, vocabulary)
    )
    
    matched_words <- words[words %in% vocabulary]
    
    for (word in matched_words) {
      new_text[1, word] <- new_text[1, word] + 1
    }
    
    result <- predict(
      model2,
      newdata = as.data.frame(new_text),
      type = "class"
    )
    
    if (as.character(result) == "1") {
      "Potential Dark Pattern"
    } else {
      "Not Dark Pattern"
    }
  }
  
}

# Prepare chart data from the real dataset
category_counts <- as.data.frame(
  table(data$`Pattern Category`)
)
names(category_counts) <- c("Category", "Count")

label_counts <- as.data.frame(table(data$label))
names(label_counts) <- c("Label", "Count")

label_counts$Label <- factor(
  label_counts$Label,
  levels = c("0", "1"),
  labels = c("Not Dark Pattern", "Dark Pattern")
)

# ---------------- USER INTERFACE ----------------

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background: #F4F6FC;
        color: #20243A;
        font-family: 'Segoe UI', sans-serif;
      }

      .container-fluid {
        max-width: 1250px;
        padding: 25px;
      }

      .hero {
        background: linear-gradient(120deg, #171B3D, #5145CD);
        color: white;
        padding: 35px;
        border-radius: 20px;
        margin-bottom: 24px;
      }

      .hero h1 {
        font-size: 34px;
        font-weight: 750;
      }

      .hero p {
        color: #E1E4FF;
        font-size: 16px;
      }

      .eyebrow {
        color: #C9C4FF;
        font-size: 12px;
        font-weight: bold;
        letter-spacing: 2px;
      }

      .panel-card, .metric {
        background: white;
        border: 1px solid #E5E7F1;
        border-radius: 16px;
        padding: 24px;
        margin-bottom: 22px;
        box-shadow: 0 5px 18px rgba(25,30,70,.04);
      }

      .metric {
        min-height: 115px;
      }

      .metric-label {
        color: #737991;
        font-size: 13px;
      }

      .metric-value {
        color: #5145CD;
        font-size: 27px;
        font-weight: 750;
        margin-top: 10px;
      }

      .section-title {
        font-size: 21px;
        font-weight: 750;
        margin-bottom: 12px;
      }

      .muted {
        color: #737991;
        line-height: 1.7;
      }

      .category {
        background: #F5F4FF;
        border-radius: 10px;
        padding: 13px;
        margin-bottom: 10px;
      }

      .category strong {
        color: #5145CD;
      }

      .btn-primary {
        background: #5145CD;
        border-color: #5145CD;
        border-radius: 9px;
        padding: 10px 18px;
        font-weight: 650;
      }

      .btn-primary:hover {
        background: #3930A6;
        border-color: #3930A6;
      }

      .form-control {
        border-radius: 9px;
        border-color: #D9DDED;
      }

      .result-card {
        padding: 20px;
        border-radius: 12px;
        margin-top: 20px;
        line-height: 1.7;
      }

      .flagged {
        background: #FFF1F0;
        border: 1px solid #FFC9C4;
        color: #9D2929;
      }

      .safe {
        background: #EAF8F0;
        border: 1px solid #B8E5CA;
        color: #176B42;
      }

      .footer-note {
        color: #737991;
        text-align: center;
        padding: 20px;
        font-size: 12px;
      }
    "))
  ),
  
  # HEADER
  div(
    class = "hero",
    div(class = "eyebrow", "AI  •  MACHINE LEARNING  •  ONLINE SAFETY"),
    h1("DarkPattern Detective"),
    p("Detect potentially manipulative language in online shopping text."),
    p("Paste a product message, promotion, stock notice or website text to begin.")
  ),
  
  # KPI CARDS
  fluidRow(
    column(
      4,
      div(
        class = "metric",
        div(class = "metric-label", "Dataset Records"),
        div(class = "metric-value", format(nrow(data), big.mark = ","))
      )
    ),
    column(
      4,
      div(
        class = "metric",
        div(class = "metric-label", "Preliminary Test Accuracy"),
        div(class = "metric-value", "82.20%")
      )
    ),
    column(
      4,
      div(
        class = "metric",
        div(class = "metric-label", "Pattern Categories"),
        div(
          class = "metric-value",
          nrow(category_counts)
        )
      )
    )
  ),
  
  # ANALYSIS AND CATEGORY GUIDE
  fluidRow(
    column(
      7,
      div(
        class = "panel-card",
        div(class = "section-title", "Analyze Website Text"),
        p(
          class = "muted",
          "Choose a sample or paste your own text below."
        ),
        
        selectInput(
          "example",
          "Try a sample:",
          choices = c(
            "Urgency — limited-time offer" =
              "FLASH SALE! Limited time only. Shop now!",
            "Scarcity — low stock" =
              "Hurry! Only 2 items left in stock.",
            "Social proof — customer popularity" =
              "Thousands of customers love this product.",
            "Ordinary product description" =
              "Pillowcases and shams",
            "Newsletter subscription" =
              "Subscribe to our newsletter."
          )
        ),
        
        actionButton(
          "use_example",
          "Use Example"
        ),
        
        br(), br(),
        
        textAreaInput(
          "user_text",
          "Your website text",
          placeholder = "Paste website text here...",
          width = "100%",
          height = "140px"
        ),
        
        actionButton(
          "analyze",
          "Analyze Text",
          class = "btn-primary",
          icon = icon("search")
        ),
        
        uiOutput("prediction")
      )
    ),
    
    column(
      5,
      div(
        class = "panel-card",
        div(class = "section-title", "What Can It Detect?"),
        p(
          class = "muted",
          "These categories are represented in the dataset. ",
          "The current ML model predicts only the two main labels, ",
          "not the exact category of new text."
        ),
        
        div(
          class = "category",
          strong("Urgency"),
          p("Pressure to act quickly or fear of missing an offer.")
        ),
        
        div(
          class = "category",
          strong("Scarcity"),
          p("Claims about limited stock or availability.")
        ),
        
        div(
          class = "category",
          strong("Social Proof"),
          p("Language about popularity or other customers' actions.")
        ),
        
        div(
          class = "category",
          strong("Misdirection"),
          p("Language that may distract from a choice.")
        ),
        
        div(
          class = "category",
          strong("Other patterns"),
          p("Obstruction, Sneaking and Forced Action.")
        )
      )
    )
  ),
  
  # CATEGORY CHART
  div(
    class = "panel-card",
    div(class = "section-title", "Pattern Category Distribution"),
    p(
      class = "muted",
      "Number of records in each category in the dataset."
    ),
    plotOutput("category_chart", height = "360px")
  ),
  
  # LABEL CHART
  div(
    class = "panel-card",
    div(class = "section-title", "Dark Pattern vs Not Dark Pattern"),
    p(
      class = "muted",
      "Distribution of the two target labels in the dataset."
    ),
    plotOutput("label_chart", height = "320px")
  ),
  
  # FOOTER
  div(
    class = "panel-card",
    div(class = "section-title", "Important Information"),
    p(
      class = "muted",
      "A flagged message is an ML-based warning, not proof ",
      "of deception. The system analyzes text, not the full ",
      "website layout or its actual behavior."
    )
  ),
  
  div(
    class = "footer-note",
    "DarkPattern Detective | Falak Munir Ahmed Tanga"
  )
)

# ---------------- SERVER LOGIC ----------------

server <- function(input, output, session) {
  
  prediction_result <- reactiveVal(NULL)
  
  # Fill the text box with the chosen sample
  observeEvent(input$use_example, {
    updateTextAreaInput(
      session,
      "user_text",
      value = input$example
    )
  })
  
  # Run the ML prediction
  observeEvent(input$analyze, {
    
    txt <- trimws(input$user_text)
    
    if (is.null(txt) || nchar(txt) == 0) {
      prediction_result(
        list(error = "Please enter some text first.")
      )
    } else {
      prediction_result(
        tryCatch(
          list(result = predict_dark_pattern(txt)),
          error = function(e) {
            list(error = paste("Prediction error:", e$message))
          }
        )
      )
    }
  })
  
  # Display the prediction
  output$prediction <- renderUI({
    
    result <- prediction_result()
    
    if (is.null(result)) {
      return(
        div(
          class = "result-card",
          style = "background:#F5F4FF;color:#5145CD;",
          h4("Your result will appear here"),
          p("Enter text and click Analyze Text.")
        )
      )
    }
    
    if (!is.null(result$error)) {
      return(
        div(
          class = "result-card flagged",
          h4("Unable to analyze"),
          p(result$error)
        )
      )
    }
    
    if (result$result == "Potential Dark Pattern") {
      div(
        class = "result-card flagged",
        h4("Potential Dark Pattern"),
        p("The model classified this text as resembling examples labeled as dark patterns."),
        p("Review the full context before drawing conclusions.")
      )
    } else {
      div(
        class = "result-card safe",
        h4("Not Dark Pattern"),
        p("The model classified this text as not a dark pattern."),
        p("This does not guarantee that the full website is free of manipulative design.")
      )
    }
  })
  
  # Interactive bar chart
  output$category_chart <- renderPlot({
    
    ggplot(
      category_counts,
      aes(
        x = reorder(Category, Count),
        y = Count,
        fill = Category
      )
    ) +
      geom_col(show.legend = FALSE, width = 0.72) +
      geom_text(
        aes(label = Count),
        hjust = -0.2,
        size = 4
      ) +
      coord_flip() +
      scale_fill_brewer(palette = "Set2") +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.15))
      ) +
      labs(
        x = NULL,
        y = "Number of records"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.background = element_rect(
          fill = "white",
          colour = NA
        ),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "#454B64")
      )
  })
  
  # Interactive pie chart
  output$label_chart <- renderPlot({
    
    ggplot(
      label_counts,
      aes(x = "", y = Count, fill = Label)
    ) +
      geom_col(width = 1, colour = "white") +
      coord_polar(theta = "y") +
      geom_text(
        aes(label = paste0(Count, " records")),
        position = position_stack(vjust = 0.5),
        size = 4
      ) +
      scale_fill_manual(
        values = c(
          "Not Dark Pattern" = "#22A06B",
          "Dark Pattern" = "#E05252"
        )
      ) +
      labs(fill = NULL) +
      theme_void(base_size = 12) +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(
          fill = "white",
          colour = NA
        )
      )
  })
}

# ---------------- LAUNCH APP ----------------

shinyApp(ui = ui, server = server)
