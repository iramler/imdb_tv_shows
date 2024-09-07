# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)


# Read in data
tv_shows <- read_csv(file = "popular_tv_shows.csv")
tv_show_info <- readRDS("show_names.rds")
tv_show_names <- tv_show_info$Show_Name

# Define the UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap');
      h1, h2 {
        background-color: #f5c518;
        color: black;
        padding: 10px;
        border-radius: 5px;
        text-align: center;
        font-family: 'Roboto', sans-serif;
        font-weight: 700;
        letter-spacing: 1px;
      }
      .author {
        background-color: #f5c518;
        color: black;
        padding: 5px;
        border-radius: 5px;
        text-align: center;
        font-family: 'Roboto', sans-serif;
        font-weight: 400;
        margin-bottom: 15px;
      }
      a:hover {
        color: #ffe066;
        text-decoration: underline;
      }
      details {
        background-color: #c1c1c1;
        color: black;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 10px;
      }
      body {
        background-color: #f0f0f0;
        color: black;
        max-width: 8.5in;  /* Set max width to 8.5 inches */
        margin-left: auto;  /* Center the content */
        margin-right: auto;

      }
      
      h2, h3 {
        background-color: #f5c518;
        border-bottom: 2px solid #ccc;
        padding: 10px;
        margin-bottom: 10px;
        border-radius: 5px;
        color: black;
      }
      .btn-custom {
        background-color: #f5c518;
        color: black;
        border: none;
        border-radius: 5px;
        padding: 10px;
        font-size: 16px;
      }
      .btn-custom:hover {
        background-color: #ffe066;
        color: black;
      }
    "))
  ),
  
  
  # Title, Author, Date in a single yellow block
  div(class = "header-block",
      h1("IMDb TV Show Analysis"),
      div(class = "author", "Maintained by Ivan Ramler"),
      div(class = "date", "Last updated September 2024")
  ),
  

  # Overview section
  h2("Overview"),
  div(class = "overview-text", 
      HTML('<p><a href="https://www.imdb.com/" target="_blank">IMDb (Internet Movie Database)</a> is a comprehensive online platform that provides detailed information about movies, TV shows, and various entertainment content. One of the core features of IMDb is its user-driven rating system.</p>
      <p><strong>Series Rating</strong> vs. <strong>Episode Ratings</strong>:</p>
      <ul>
        <li><strong>Series Rating</strong>: This is the overall rating for a TV show, calculated as an average of all user ratings for the entire series. It represents the general consensus of IMDb users regarding the quality of the show as a whole.</li>
        <li><strong>Episode Ratings</strong>: These are individual ratings provided by users for each episode of a series. Episode ratings often fluctuate due to factors like the plot, guest stars, or standout moments within a specific episode. As a result, some episodes may receive higher or lower ratings than the overall series rating.</li>
      </ul>
      <p>This tool allows you to analyze IMDb ratings data for popular TV series, offering insights into how individual episodes are rated compared to the overall series.</p>')
  ),
  
  # Analyzing a TV Show section
  h2("Analyzing a TV Show"),
  h3("1. Choose a show from the list below."),
  div(class = "analysis-text", 
      p("You can type the name of the show in the box to avoid manually searching through the entire list. The list contains popular shows that have started since 1980.")
  ),
  
  
  HTML("<hr style='border-top: 1px solid #ccc;'>"),
  
  h3("2. IMDb Episode Ratings across Seasons"),
  
  fluidRow(
    selectizeInput(inputId = "show_name", label = "TV Shows:",
                   choices = NULL, selected = NULL, multiple = FALSE),
    actionButton("submit", "Plot Data!"),
    p(style = "color: grey;", "Be patient, this may take a few seconds for shows with lots of episodes.")
  ),
  
  HTML("<hr style='border-top: 1px solid #ccc;'>"),
  
  uiOutput("imdb_link_and_input"),
  uiOutput("discussion_note"),
  uiOutput("tooltip_info"),
  plotOutput("episode_vs_rating_plot", click = "plot_click"),
  

  uiOutput("discussion_questions"),
  
  # Data source and disclaimer at the bottom
  HTML("
    <div style='background-color: #000000; color: #f5c518; padding: 10px; text-align: center;'>
      Data source: <a href='https://developer.imdb.com/non-commercial-datasets/' target='_blank' style='color: #f5c518;'>IMDb Non-Commercial Datasets</a> <br>
      <small>This analysis tool is for educational and non-commercial purposes only. It is not endorsed by or affiliated with IMDb or its parent companies.</small>
    </div>
  ")
)

# Define the server logic
server <- function(input, output, session) {
  # Update selectize input for TV show selection
  updateSelectizeInput(session, 'show_name', choices = tv_show_names, 
                       selected = "Gilmore Girls", server = TRUE)
  
  # Fetch the data for the selected show
  tv_shows_chosen <- eventReactive(input$submit, {
    withProgress(message = "Fetching data for selected show...", value = 0, {
      incProgress(0.5)
      show_data <- tv_shows %>%
        filter(Show_Name %in% as.character(input$show_name)) %>%
        arrange(episodeNumber_overall)
      
      selected_show_id <- tv_show_info %>%
        filter(Show_Name %in% as.character(input$show_name)) %>%
        pull(parentTconst)
      
      incProgress(0.5, detail = "Data fetched successfully")
      
      # Reset the series rating when a new show is selected
      series_rating(NULL)
      
      return(list(data = show_data, show_name = input$show_name, series_id = selected_show_id))
    })
  })
  
  # Display the IMDb link and input for series rating
  output$imdb_link_and_input <- renderUI({
    tv_shows_chosen_result <- tv_shows_chosen()
    selected_show_name <- tv_shows_chosen_result$show_name
    selected_show_id <- tv_shows_chosen_result$series_id
    
    imdb_url <- paste0("https://www.imdb.com/title/", selected_show_id)
    
    tagList(
      HTML(paste0(
        "Now, go to the <a href='", imdb_url, "' target='_blank'>IMDb page for the TV Series</a> and find its rating. ",
        "Enter this value in the box below."
      )),
      div(style = "display: flex; align-items: baseline;",
          div(style = "margin-right: 10px;", "IMDb Series Rating:"),
          numericInput(inputId = "rating_line", label = NULL, 
                       value = NA, min = 0, max = 10, width = "80px"),
          actionButton(inputId = "update_rating", label = "Add Series Rating", style = "margin-left: 10px; height: 34px;")
      ),
      HTML("<p><strong>You can click on one of the dots to display extra information about that episode.</strong></p>")
    )
  })
  
  # Create a reactive value for series rating
  series_rating <- reactiveVal(NULL)
  
  # Placeholder plot before a show is selected
  output$episode_vs_rating_plot <- renderPlot({
    ggplot() +
      labs(title = "Select a TV Show to Display IMDb Episode Ratings", x = "Season", y = "Episode Rating") +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 14),  # Adjust x-axis label size
        axis.title.y = element_text(size = 14),  # Adjust y-axis label size
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
      )
  })
  
  # Update the series rating when the "Add Series Rating" button is clicked
  observeEvent(input$update_rating, {
    series_rating(input$rating_line)
  })
  
  # Render the plot after "Plot Data" is clicked
  observeEvent(input$submit, {
    output$episode_vs_rating_plot <- renderPlot({
      tv_shows_chosen_result <- tv_shows_chosen()
      tv_shows_chosen_data <- tv_shows_chosen_result$data
      selected_show_name <- tv_shows_chosen_result$show_name
      
      req(nrow(tv_shows_chosen_data) > 0)
      withProgress(message = "Rendering plot...", value = 0, {
        incProgress(0.3, detail = "Identifying seasons...")
        
        season_starts <- tv_shows_chosen_data %>%
          group_by(seasonNumber) %>%
          summarize(min_episode = min(episodeNumber_overall),
                    mid_episode = (min(episodeNumber_overall) + max(episodeNumber_overall)) / 2)
        
        p <- ggplot(tv_shows_chosen_data, aes(x = episodeNumber_overall, y = averageRating, 
                                              color = factor(seasonNumber), size = numVotes)) +
          geom_vline(xintercept = season_starts$min_episode, color = "grey50", linetype = "solid", linewidth = 0.3) +
          geom_point(alpha = 1) +
          scale_x_continuous(breaks = season_starts$mid_episode, 
                             labels = tv_shows_chosen_data$seasonNumber[!duplicated(tv_shows_chosen_data$seasonNumber)]) +
          labs(title = paste("IMDb Episode Ratings by Episode Number for", selected_show_name), 
               x = "Season", y = "Episode Rating") +
          theme_minimal() +
          theme(
            axis.title.x = element_text(size = 14),  # Adjust x-axis label size
            axis.title.y = element_text(size = 14),  # Adjust y-axis label size
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)
            ) +
          guides(color = "none", size = "none")
        
        if (!is.null(series_rating())) {
          p <- p + geom_hline(yintercept = series_rating(), color = "black", linetype = "solid", linewidth = 0.7)
        }
        
        incProgress(1, detail = "Plot rendered successfully")
        p
      })
    })
  })
  
  # Capture clicks and display episode details
  output$tooltip_info <- renderUI({
    req(input$plot_click)
    clicked_point <- nearPoints(tv_shows_chosen()$data, input$plot_click, threshold = 5, maxpoints = 1)
    
    if (nrow(clicked_point) > 0) {
      imdb_episode_url <- paste0("https://www.imdb.com/title/", clicked_point$tconst)
      
      HTML(paste0(
        "<div style='background-color: #f0f0f0; padding: 10px; border-radius: 5px;'>",
        "<strong>Episode Name:</strong> <a href='", imdb_episode_url, "' target='_blank'>", clicked_point$Episode_Name, "</a><br>",
        "<strong>Episode Number:</strong> ", clicked_point$episodeNumber_overall, "<br>",
        "<strong>Rating:</strong> ", clicked_point$averageRating, "<br>",
        "<strong>Season:</strong> ", clicked_point$seasonNumber, "<br>",
        "<strong>Votes:</strong> ", clicked_point$numVotes,
        "</div>"
      ))
    } else {
      "No point clicked."
    }
  })
  
  # Initially hide the discussion questions until the plot is created
  output$discussion_questions <- renderUI({
    NULL
  })
  
  # Show discussion questions after plot is created
  observeEvent(input$submit, {
    tv_shows_chosen_result <- tv_shows_chosen()
    selected_show_name <- tv_shows_chosen_result$show_name
    
    output$discussion_questions <- renderUI({
      HTML(paste0("
        <hr style='border-top: 1px solid #ccc;'>
      
        <a id='discussion_section'></a> <!-- Anchor for the link -->
        <h3>Potential Discussion Questions for ", selected_show_name, "</h3>
        
        <p><em>Click a triangle to expand the section.</em></p> <!-- Instruction for users -->

        <details>
          <summary><strong>Trends in Episode Ratings</strong></summary>
          <ul>
            <li>Look at the overall trend of episode ratings across ", selected_show_name, ". Any changes in the ratings between seasons?</li>
            <li>Do any episodes stand out with particularly high or low ratings?</li>
          </ul>
        </details>
        <details>
          <summary><strong>Investigating Outliers</strong></summary>
          <ul>
            <li>Click on the episodes with the highest and lowest ratings in ", selected_show_name, ". What can you learn from the episode details displayed in the box above the plot?</li>
          </ul>
        </details>
        <details>
          <summary><strong>Patterns Within Seasons</strong></summary>
          <ul>
            <li>Are the episode ratings within a season of ", selected_show_name, " consistent, or do they vary?</li>
          </ul>
        </details>
        <details>
          <summary><strong>Popularity and Votes</strong></summary>
          <ul>
            <li>How does the number of votes vary across episodes and seasons in ", selected_show_name, "?</li>
          </ul>
        </details>
      "))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
