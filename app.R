
# Loead cvs file
tasks_dataset<-read.csv("all_tasks.csv", stringsAsFactor=F) 
disorders_dataset<-read.csv("all_disorders.csv", stringsAsFactor=F) 
concepts_dataset<-read.csv("all_concepts.csv", stringsAsFactor=F) 

# Create mandatory field (for each section) and label them with *
fieldsMandatory1 <- c("name", "surname", "email", "affiliation")
fieldsMandatory2 <- c("patients", "diagnosis", "controls", "tasks", "disorders",
                      "concepts")
                    #  , "neural_measures", "neural_space", "longitudinal")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    titlePanel("Submit a dataset to NeuroCausal"),
      
    div(
      id = "section1",
      textInput("name", labelMandatory("Name")),
      textInput("surname", labelMandatory("Surname")),
      textInput("email", labelMandatory("Contact e-mail (please use institutional e-mail if possible)")),      
      textInput("affiliation", labelMandatory("Affiliation")),
      actionButton("next_button1", "Next", class = "btn-primary")
    ),
      
    shinyjs::hidden(
      div(
        id = "section2",
        textInput("description", "Description of the dataset: please help us make your data easily findable by sharing the key details of its meta-data."),
        textInput("patients", labelMandatory("How many patients?")),
        textInput("diagnosis", labelMandatory("What was the diagnosis at time of testing?")),
        textInput("controls", labelMandatory("How many healthy controls?")),
        textInput("notes", "Any note on the sample? Any further specification of the patients and/or healthy control population used?"),
        selectInput("tasks", labelMandatory("Which task (i.e., behavioural, cognitive or clinical measure) was collected? select all that apply."),
                    unique(tasks_dataset$name), multiple=TRUE),
        selectInput("disorders", labelMandatory("Which symptom (or collection of thereof) was isolated? Select all that apply."),
                    unique(disorders_dataset$name), multiple=TRUE),
        selectInput("concepts", labelMandatory("Which cognitive concepts can be investigated with this dataset? Select all that apply."),
                    unique(concepts_dataset$name), multiple=TRUE),

        actionButton("next_button2", "Next", class = "btn-primary")
     )
    ),
    
    shinyjs::hidden(
      div(
        id = "section3",
        checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
        sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
        actionButton("submit", "Submit", class = "btn-primary")
     )
    ),
  ),
  
  server = function(input, output, session) {
    # Check that 'Mandatoryfields1' are filled before enabling 'Next' to go to the next section
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory1,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "next_button1", condition = mandatoryFilled)
    })
    
    # Check that 'Mandatory fields' are filled before enabling 'Submit'
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory2,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "next_button2", condition = mandatoryFilled)
    })    
    
    # Allow to go to next section: action to take when next button is pressed
    observeEvent(input$next_button1, {
      #saveData(formData()) --> not implemented yet
      shinyjs::reset("section1")
      shinyjs::hide("section1")
      shinyjs::show("section2")
    })
    
    observeEvent(input$next_button2, {
      #saveData(formData()) --> not implemented yet
      shinyjs::reset("section2")
      shinyjs::hide("section2")
      shinyjs::show("section3")
    })
  }
)