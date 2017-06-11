library(ggvis)

shinyUI(navbarPage("Sample Size Calculator", 
                   tabPanel("t-test",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput('significance_level_ui_t'),
                                checkboxInput('significance_level_range_t', 'Range?', value = FALSE),
                                uiOutput('power_ui_t'),
                                checkboxInput('power_range_t', 'Range?', value = FALSE),
                                uiOutput('effect_size_ui_t'),
                                checkboxInput('effect_size_range_t', 'Range?', value = FALSE),
                                br(),
                                checkboxInput('two_sided_t', "Two Sided Test?", value = TRUE)
                              ),
                              
                              #Render the results
                              mainPanel(
                                uiOutput('selected_t'),
                                br(),
                                uiOutput('answer_t'),
                                plotOutput("plot_t")
                                )
                            
                   )
                   ),
                   tabPanel("z-test",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput('significance_level_ui_z'),
                                checkboxInput('significance_level_range_z', 'Range?', value = FALSE),
                                uiOutput('power_ui_z'),
                                checkboxInput('power_range_z', 'Range?', value = FALSE),
                                sliderInput('prob_one', "Probability of Group One",
                                            min=0.01, max=0.99, step=0.01, value=.5),
                                sliderInput('prob_two', "Probability of Group Two",
                                            min=0.01, max=0.99, step=0.01, value=.4),
                                br(),
                                checkboxInput('two_sided_z', "Two Sided Test?", value = TRUE)
                              ),
                              
                              #Render the results
                              mainPanel(
                                uiOutput('selected_z'),
                                br(),
                                uiOutput('answer_z'),
                                plotOutput("plot_z")
                              )
                              
                            )
                   )
                   
                   
)
)
