#app_combined.R has combined code from app_A, app_B, app_C, and app_D.
#Throughout the file, I noted which versions each section came from and explained why I chose certain parts.
#
#Comment labels:
#[Shared A/B/C/D] means the code is the same or similar in all four apps.
#[From ...] identifies the version(s) I used for that section.
#[Combined adjustment] identifies changes added when combining the apps.
#
#Main choices:
# * A/B/D: I used titlePanel() to replace the placeholder in C.
# * A/D: I kept the explicit argument names and sidebar headings
#   because they make the code easier to follow.
# * B: I used the education labels because they better describe
#   the categories included in each group.
# * B: I also kept the conditionalPanel condition, fluidRow(),
#   guess step size, all_of(), complete.obs, and aes() syntax.
# * A/C/D: I kept the sample size range of 20-500, starting at 20.
# * B/C/D: I used "data/my_sample_temp.rds" for the data file path.
# * All four apps used similar filtering, weighted sampling,
#   reactive storage, and correlation guessing code.
#
#I also added checks for invalid inputs, empty subsets, and samples
#where the correlation cannot be calculated. I saved the variable
#names with each sample so changing the dropdowns does not change
#the axes of an existing plot.
#
#Keep this file in Homework8 with helpers.R and the data folder.
#helpers.R should define numeric_vars, HHLvals, FSvals, and SCHLvals.
#The app still needs to be tested with the helper and data files.

#[Shared A/B/C/D] Load the packages and helper code used by the app.
library(shiny)
library(shinyalert)
library(tidyverse)
source("helpers.R")

#USER INTERFACE
ui <- fluidPage(
  #[From A/B/D] Replaces C's title placeholder.
  titlePanel("Correlation Exploration"),
  # [Shared A/B/C/D] Inputs at left; scatterplot and game at right.
  sidebarLayout(
    sidebarPanel(
      h2("Select Variables to Find Correlation:"),
      #[Shared A/B/C/D; argument style A/C/D] Default income and commute.
      selectizeInput(
        inputId = "corr_x", label = "x Variable",
        choices = numeric_vars, selected = "PINCP"
      ),
      selectizeInput(
        inputId = "corr_y", label = "y Variable",
        choices = numeric_vars, selected = "JWMNP"
      ),
      #[From A/B/D] Descriptive heading; C uses spacing alone.
      h2("Choose a subset of the data:"),
      #[Shared A/B/C/D] Same internal values used by server filtering.
      #[From A/C/D] Explicit default and 'English only' wording.
      radioButtons(
        inputId = "hhl_corr", label = "Household Language",
        choices = c("All" = "all", "English only" = "english",
                    "Spanish" = "spanish", "Other" = "other"),
        selected = "all"
      ),
      #[From A/C/D] SNAP label; logic is shared with B.
      radioButtons(
        inputId = "fs_corr", label = "SNAP Recipient",
        choices = c("All" = "all", "Yes" = "yes", "No" = "no"),
        selected = "all"
      ),
      #[From B] Labels describe the shared ranges: 16:19 includes some
      #college, and 20:24 includes associate's degrees and above.
      #A/C/D's 'High School or GED' and 'College Degree' are less precise.
      radioButtons(
        inputId = "schl_corr", label = "Educational Attainment",
        choices = c("All" = "all", "Less than high school" = "no_hs",
                    "High school or some college" = "hs",
                    "Associate's degree or higher" = "college"),
        selected = "all"
      ),
      h2("Select a Sample Size"),
      #[From A/C/D] Keep their limits/default; B's shorter label.
      sliderInput(
        inputId = "corr_n", label = "Sample size",
        min = 20, max = 500, value = 20
      ),
      #[Shared A/B/C/D] Sampling occurs when this button is clicked.
      actionButton("corr_sample", "Get a Sample!")
    ),
    mainPanel(
      #[Shared A/B/C/D] Connects to output$corr_plot on the server.
      plotOutput("corr_plot"),
      #[From B] Explicitly show after the first sampling-button click.
      #This condition tracks clicks, not whether sampling succeeded.
      conditionalPanel(
        condition = "input.corr_sample > 0",
        h2("Guess the correlation!"),
        #[From B] fluidRow groups the two six-column controls together.
        fluidRow(
          column(6,
                 #[From B] 0.01 step; limits/default shared by all versions.
                 numericInput("corr_guess", label = NULL, value = 0,
                              min = -1, max = 1, step = 0.01)
          ),
          column(6, actionButton("corr_submit", "Check Your Guess!"))
        )
      )
    )
  )
)

#[From B/C/D] Data are stored under Homework8/data/.
my_sample <- readRDS("data/my_sample_temp.rds")

#SERVER
server <- function(input, output, session) {
  #[Shared A/B/C/D] Each observer removes the other selected variable
  #from its dropdown while preserving the current selection.
  observeEvent(input$corr_x, {
    corr_x <- input$corr_x
    corr_y <- input$corr_y
    choices <- numeric_vars
    if (corr_x != corr_y) {
      choices <- choices[-which(choices == corr_x)]
      updateSelectizeInput(session, "corr_y",
                           choices = choices, selected = corr_y)
    }
  })
  observeEvent(input$corr_y, {
    corr_x <- input$corr_x
    corr_y <- input$corr_y
    choices <- numeric_vars
    if (corr_x != corr_y) {
      choices <- choices[-which(choices == corr_y)]
      updateSelectizeInput(session, "corr_x",
                           choices = choices, selected = corr_x)
    }
  })
  
  #[Shared A/B/C/D] Start without sampled data or a correlation.
  #[Combined adjustment] corr_vars saves the axes used for that sample.
  #This keeps the plot and answer consistent if controls later change.
  sample_corr <- reactiveValues(
    corr_data = NULL, corr_truth = NULL, corr_vars = NULL
  )
  
  #[Shared A/B/C/D] Filter and sample only on a button click.
  observeEvent(input$corr_sample, {
    #[Combined adjustment] Defensive check in addition to the dropdowns.
    req(input$corr_x, input$corr_y)
    validate(need(input$corr_x != input$corr_y,
                  "Please select two different variables."))
    
    #[Shared A/B/C/D] Map language choices to helper-defined labels.
    #'Other' retains the supplied handling of code 0 and codes 3-5.
    if (input$hhl_corr == "all") {
      hhl_sub <- HHLvals
    } else if (input$hhl_corr == "english") {
      hhl_sub <- HHLvals["1"]
    } else if (input$hhl_corr == "spanish") {
      hhl_sub <- HHLvals["2"]
    } else {
      hhl_sub <- HHLvals[c("0", "3", "4", "5")]
    }
    #[Shared A/B/C/D] Map SNAP choices to helper-defined labels.
    if (input$fs_corr == "all") {
      fs_sub <- FSvals
    } else if (input$fs_corr == "yes") {
      fs_sub <- FSvals["1"]
    } else {
      fs_sub <- FSvals["2"]
    }
    #[Shared A/B/C/D] Retain the supplied education code groupings.
    #Code 0 remains in no_hs as in every source; verify its meaning
    #against helpers.R if this category needs further refinement.
    if (input$schl_corr == "all") {
      schl_sub <- SCHLvals
    } else if (input$schl_corr == "no_hs") {
      schl_sub <- SCHLvals[c("0", "01", "02", "03", "04", "05",
                             "06", "07", "08", "09", "10", "11",
                             "12", "13", "14", "15")]
    } else if (input$schl_corr == "hs") {
      schl_sub <- SCHLvals[as.character(16:19)]
    } else {
      schl_sub <- SCHLvals[as.character(20:24)]
    }
    corr_vars <- c(input$corr_x, input$corr_y)
    
    #[Shared A/B/C/D] Apply categorical filters, then the supplied
    #variable-specific eligibility rules only for selected variables.
    #%>% is retained because these braced expressions use its '.' syntax.
    subsetted_data <- my_sample |>
      filter(HHLfac %in% hhl_sub, FSfac %in% fs_sub,
             SCHLfac %in% schl_sub) %>%
      {if ("WKHP" %in% corr_vars) filter(., WKHP > 0) else .} %>%
      {if ("VALP" %in% corr_vars) filter(., !is.na(VALP)) else .} %>%
      {if ("TAXAMT" %in% corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
      {if ("GRPIP" %in% corr_vars) filter(., GRPIP > 0) else .} %>%
      {if ("GASP" %in% corr_vars) filter(., GASP > 0) else .} %>%
      {if ("ELEP" %in% corr_vars) filter(., ELEP > 0) else .} %>%
      {if ("WATP" %in% corr_vars) filter(., WATP > 0) else .} %>%
      {if ("PINCP" %in% corr_vars) filter(., AGEP > 18) else .} %>%
      {if ("JWMNP" %in% corr_vars) filter(., !is.na(JWMNP)) else .}
    
    #[Combined adjustment] Exclude unusable sampling weights and check
    #for an empty subset before calling sample().
    subsetted_data <- subsetted_data |>
      filter(is.finite(PWGTP), PWGTP > 0)
    if (nrow(subsetted_data) == 0) {
      shinyalert(title = "No eligible observations",
                 text = "Please choose a different subset or variables.",
                 type = "warning")
      return()
    }
    
    #[Shared A/B/C/D] Sample with replacement using PWGTP probabilities.
    #[Combined adjustment] seq_len handles row indexing explicitly.
    index <- sample(seq_len(nrow(subsetted_data)), size = input$corr_n,
                    replace = TRUE,
                    prob = subsetted_data$PWGTP / sum(subsetted_data$PWGTP))
    corr_data <- subsetted_data[index, ]
    
    #[From B] all_of() selects columns named in corr_vars;
    #complete.obs uses rows with both measurements present.
    #[Combined adjustment] Check complete pairs and variability first,
    #preventing an undefined correlation from reaching the guessing game.
    paired <- corr_data |> select(all_of(corr_vars))
    paired <- paired[complete.cases(paired), , drop = FALSE]
    if (nrow(paired) < 2 ||
        any(!vapply(paired, function(x) all(is.finite(x)), logical(1))) ||
        any(vapply(paired, function(x) length(unique(x)) < 2, logical(1)))) {
      shinyalert(title = "Correlation unavailable",
                 text = "Try another sample or subset: both variables need variation and at least two complete pairs.",
                 type = "warning")
      return()
    }
    corr_truth <- cor(corr_data |> select(all_of(corr_vars)),
                      use = "complete.obs")[1, 2]
    
    #[Shared A/B/C/D] Store the sample and its Pearson correlation.
    #Sampling is weighted; this cor() is an ordinary sample correlation,
    #as in all four source apps, not a survey-weighted correlation.
    sample_corr$corr_data <- corr_data
    sample_corr$corr_truth <- corr_truth
    sample_corr$corr_vars <- corr_vars  # [Combined adjustment] Save axes.
  })
  
  #[Shared A/B/C/D] Validate that a sample exists before plotting.
  output$corr_plot <- renderPlot({
    validate(need(!is.null(sample_corr$corr_data),
                  "Please select your variables, subset, and click the 'Get a Sample!' button."))
    #[From B] aes(.data[[...]]) replaces A/C/D's aes_string().
    #[Combined adjustment] Use saved axes instead of isolate(input$...).
    #Complete pairs match the observations used by cor(complete.obs).
    plot_data <- sample_corr$corr_data |>
      filter(if_all(all_of(sample_corr$corr_vars), ~ !is.na(.x)))
    ggplot(plot_data,
           aes(x = .data[[sample_corr$corr_vars[1]]],
               y = .data[[sample_corr$corr_vars[2]]])) +
      geom_point()
  })
  
  #[Shared A/B/C/D] A guess within 0.05 succeeds; otherwise give direction.
  observeEvent(input$corr_submit, {
    #[Combined adjustment] Avoid comparing against a missing answer/guess.
    req(!is.null(sample_corr$corr_truth), input$corr_guess)
    validate(need(is.finite(input$corr_guess) &&
                    input$corr_guess >= -1 && input$corr_guess <= 1,
                  "Enter a guess between -1 and 1."))
    close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
    if (close) {
      shinyalert(title = "Nicely done!",
                 text = paste0("The sample correlation is ",
                               round(sample_corr$corr_truth, 4), "."),
                 type = "success")
    } else if (input$corr_guess > sample_corr$corr_truth) {
      shinyalert(title = "Try again!", text = "Try guessing a lower value.")
    } else {
      shinyalert(title = "Try again!", text = "Try guessing a higher value.")
    }
  })
}

#Connect the interface and server to launch the app.
shinyApp(ui = ui, server = server)