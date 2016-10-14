ui <- shinyUI(
  pageWithSidebar(
  headerPanel("Minimal example"),
  sidebarPanel(
    textInput(
      inputId = "comment",
      label = "Say something ?",
      value = "")),
    mainPanel(
      h1("This is you saying"),
      textOutput(
        outputId = "textDisplay")
    )
))