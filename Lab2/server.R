library(ggplot2)

server <- function(input, output) {
  #======START SERVER FOR T-TEST========
  get_alt_t <- function(){
    if(input$two_sided_t){
      alt <- "two.sided"
    } else {
      alt <- "one.sided"
    }
    return(alt)
  }
  
  output$significance_level_ui_t <- renderUI({
    if(input$significance_level_range_t) {
      sliderInput('significance_level_t', 'Significance Level', min = 0.01, max=0.2,
                  step = 0.01, value = c(0.05, 0.1))
    } else {
      sliderInput('significance_level_t', 'Significance Level', min = 0.01, max=0.2,
                  step = 0.01, value=0.1)
    }
  })
  
  output$power_ui_t <- renderUI({
    if(input$power_range_t) {
      sliderInput('power_t', 'Power', min = 0.4, max=0.99, step = 0.01, value = c(0.75, 0.9))
    } else {
      sliderInput('power_t', 'Power', min = 0.40, max=0.99, step = 0.01, value=0.9)
    }
  })
  
  output$effect_size_ui_t <- renderUI({
    if(input$effect_size_range_t) {
      sliderInput('effect_size_t', 'Effect Size', min = 0.1, max=10.0, step = 0.1, value = c(0.2, 0.4))
    } else {
      sliderInput('effect_size_t', 'Effect Size', min = 0.1, max=10.0, step = 0.1, value=0.5)
    }
  })
  
  output$selected_t <- renderUI({
    if(input$two_sided_t){
      alt <- "Two Sided"
    } else {
      alt <- "One Sided"
    }
    ranges <- c(input$significance_level_range_t, input$power_range_t, input$effect_size_range_t)
    total_ranges = sum(ranges)
    validate(
      need(total_ranges < 2, 
           'You may only select one range.')
    )
    range_param <- which(ranges)
    
    if (total_ranges == 0){
      significance_level_txt <- input$significance_level_t
      power_txt <- input$power_t
      effect_size_txt <- input$effect_size_t
    } else if(range_param == 1){
      significance_level_txt <- paste("<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Min:", 
                                      input$significance_level_t[1],
                                      "<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Max", 
                                      input$significance_level_t[2])
      power_txt <- input$power_t
      effect_size_txt <- input$effect_size_t
    } else if(range_param == 2){
      power_txt <- paste("<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Min:",
                         input$power_t[1],
                         "<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Max",
                         input$power_t[2])
      significance_level_txt <- input$significance_level_t
      effect_size_txt <- input$effect_size_t
    } else {
      effect_size_txt <- paste("<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Min:",
                         input$effect_size_t[1],
                         "<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Max",
                         input$effect_size_t[2])
      significance_level_txt <- input$significance_level_t
      power_txt <- input$power_t
    }
    
    HTML(paste("<h4>Selected Parameters:<br/>",
          "&emsp;Significance Level: ", 
          significance_level_txt,
          "<br/>&emsp;Power: ",
          power_txt,
          "<br/>&emsp;Effect Size: ",
          effect_size_txt,
          "<br/>&emsp;Alternative: ",
          alt,
          "</h4>",
          sep=''))
  })
  
  output$answer_t <- renderText({
    alt <- get_alt_t()
    if(sum(input$significance_level_range_t, input$power_range_t, input$effect_size_range_t) == 0){
      return(HTML(paste("<h2>Total Sample Size Needed =", ceiling(power.t.test(n=NULL,
                                                       delta=input$effect_size_t,
                                                       power=input$power_t,
                                                       sig.level = input$significance_level_t,
                                                       alternative = alt)$n),
                        "</h2>")))
    } else {
      return("")
    }
  })
  
  output$plot_t <- renderPlot({
    params <- c('Significance Level', 'Power', 'Effect Size')
    ranges <- c(input$significance_level_range_t, input$power_range_t, input$effect_size_range_t)
    total_ranges = sum(ranges)
    validate(
      need(total_ranges > 0, '')
    )
    validate(
      need(total_ranges < 2, '')
    )
    alt <- get_alt_t()
    range_param <- which(ranges)
    
    if(range_param == 1){
      input_range <- seq(input$significance_level_t[1], input$significance_level_t[2], 0.01)
      results <- lapply(input_range, 
                     power.t.test, 
                     n=NULL, 
                     delta=input$effect_size_t,
                     power=input$power_t,
                     sd=1,
                     alternative = alt
                     )
      } else if(range_param == 2){
        input_range <- seq(input$power_t[1], input$power_t[2], 0.01)
        results <- lapply(input_range, 
                          power.t.test, 
                          n=NULL, 
                          delta=input$effect_size_t,
                          sig.level=input$significance_level_t,
                          sd=1,
                          alternative = alt
                          )
        
        } else {
          input_range <- seq(input$effect_size_t[1], input$effect_size_t[2], 0.1)
          results <- lapply(input_range, 
                            power.t.test, 
                            n=NULL, 
                            sig.level=input$significance_level_t,
                            power=input$power_t,
                            sd=1,
                            alternative = alt
                            )
        }
      
      sample_sizes <- sapply(results, function(x) x$n)
      xvar <- params[range_param]
      ggplot(data.frame(x=input_range, y=sample_sizes), aes(x=x, y=y)) + 
        geom_line(size=1) + geom_point(size=2) + 
        labs(x=xvar, y='Required Sample Size', title=paste("Sample Size vs", xvar)) +
        theme( 
              plot.title = element_text(hjust=0.5, size=20),
              axis.title.x = element_text(size=15),
              axis.title.y = element_text(size=15),
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10)
        )
  })
  
  #======START SERVER FOR Z-TEST========
  get_alt_z <- function(){
    if(input$two_sided_z){
      alt <- "two.sided"
    } else {
      alt <- "one.sided"
    }
    return(alt)
  }
  
  output$significance_level_ui_z <- renderUI({
    if(input$significance_level_range_z) {
      sliderInput('significance_level_z', 'Significance Level', min = 0.01, max=0.2,
                  step = 0.01, value = c(0.05, 0.1))
    } else {
      sliderInput('significance_level_z', 'Significance Level', min = 0.01, max=0.2,
                  step = 0.01, value=0.1)
    }
  })
  
  output$power_ui_z <- renderUI({
    if(input$power_range_z) {
      sliderInput('power_z', 'Power', min = 0.4, max=0.99, step = 0.01, value = c(0.75, 0.9))
    } else {
      sliderInput('power_z', 'Power', min = 0.40, max=0.99, step = 0.01, value=0.9)
    }
  })
  
  output$selected_z <- renderUI({
    if(input$two_sided_z){
      alt <- "Two Sided"
    } else {
      alt <- "One Sided"
    }
    ranges <- c(input$significance_level_range_z, input$power_range_z)
    total_ranges = sum(ranges)
    validate(
      need(total_ranges < 2, 
           'You may only select one range.')
    )
    range_param <- which(ranges)
    
    if (total_ranges == 0){
      significance_level_txt <- input$significance_level_z
      power_txt <- input$power_z
    } else if(range_param == 1){
      significance_level_txt <- paste("<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Min:", 
                                      input$significance_level_z[1],
                                      "<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Max", 
                                      input$significance_level_z[2])
      power_txt <- input$power_z
    } else {
      power_txt <- paste("<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Min:",
                         input$power_z[1],
                         "<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Max",
                         input$power_z[2])
      significance_level_txt <- input$significance_level_z
    }
    
    HTML(paste("<h4>Selected Parameters:<br/>",
               "&emsp;Significance Level: ", 
               significance_level_txt,
               "<br/>&emsp;Power: ",
               power_txt,
               "<br/>&emsp;Probability of Group One: ",
               input$prob_one,
               "<br/>&emsp;Probability of Group Two: ",
               input$prob_two,
               "<br/>&emsp;Alternative: ",
               alt,
               "</h4>",
               sep=''))
  })
  
  output$answer_z <- renderText({
    alt <- get_alt_z()
    validate(
      need(input$prob_one != input$prob_two, "The Two Probabilities Must Be Different")
    )
    if(sum(input$significance_level_range_z, input$power_range_z) == 0){
      return(HTML(paste("<h2>Total Sample Size Needed =", 
                        ceiling(power.prop.test(n=NULL,
                                             p1=input$prob_one,
                                             p2=input$prob_two,
                                             power=input$power_z,
                                             sig.level = input$significance_level_z,
                                             alternative = alt)$n),
                        "</h2>")))
    } else {
      return("")
    }
  })
  
  output$plot_z <- renderPlot({
    validate(
      need(input$prob_one != input$prob_two, "")
    )
    params <- c('Significance Level', 'Power')
    ranges <- c(input$significance_level_range_z, input$power_range_z)
    total_ranges = sum(ranges)
    validate(
      need(total_ranges > 0, '')
    )
    validate(
      need(total_ranges < 2, '')
    )
    alt <- get_alt_z()
    range_param <- which(ranges)
    
    if(range_param == 1){
      input_range <- seq(input$significance_level_z[1], input$significance_level_z[2], 0.01)
      results <- lapply(input_range, 
                        power.prop.test, 
                        n=NULL, 
                        p1=input$prob_one,
                        p2=input$prob_two,
                        power=input$power_z,
                        alternative = alt
      )
    } else {
      input_range <- seq(input$power_z[1], input$power_z[2], 0.01)
      results <- lapply(input_range, 
                        power.prop.test, 
                        n=NULL, 
                        p1=input$prob_one,
                        p2=input$prob_two,
                        sig.level=input$significance_level_z,
                        alternative = alt
      )
      
    }
    
    sample_sizes <- sapply(results, function(x) x$n)
    xvar <- params[range_param]
    ggplot(data.frame(x=input_range, y=sample_sizes), aes(x=x, y=y)) + 
      geom_line(size=1) + geom_point(size=2) + 
      labs(x=xvar, y='Required Sample Size', title=paste("Sample Size vs", xvar)) +
      theme( 
        plot.title = element_text(hjust=0.5, size=20),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10)
      )
  })
  
}