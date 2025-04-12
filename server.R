library(shiny)
library(ggplot2)
library(plotly)
source("functions.R")

#Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Observe random variable parent type
  observeEvent(input$randvariable, {
    updateSelectInput(session, "randvalue", 
                      choices = if (input$randvariable == "discrete") {
                        c("Binomial", "Geometric", "Negative Binomial", "HyperGeometric", "Poisson")
                      } else {
                        c("Uniform", "Normal", "Exponential")
                      })
  })
  
  #Observe distribution child type
  observeEvent(input$randvalue, {
    if (input$randvalue != "Normal") {
      updateSelectInput( session, "pdforcdf", choices = c("PDF", "CDF") )
    }
    else {
      updateSelectInput( session, "pdforcdf", choices = c("PDF", "CDF", "P-Curve") )
    }
     
    })
  
  #Uniform slider UI
  output$dynamicUI <- renderUI({
    req(input$randvalue)
    if (input$randvalue == "Uniform" ) {
      sliderInput("unifstepsize", "Uniform Distribution step size:", min = 0, max = 1, value = 0.1)
    }
  })
  
  #UI Math Equation
  output$mathEquation <- renderUI({
    req(input$randvalue)
    withMathJax(
      if (input$randvalue == "Binomial") {
        if(input$pdforcdf == "PDF") {
          withMathJax(
            div(style="font-size: 20px; font-weight: bold; ",
                tags$b(helpText("For a binomial distribution:")) ), 
            helpText("$$P(X = k) = \\binom{n}{k} p^k (1-p)^{n-k}, \\space n \\le 170$$"),
            helpText("$$E(X) = np, \\space n \\le 170$$"),
            strong(tags$i(helpText("Reference: Rice, John A. (2007). Mathematical statistics and data analysis (3rd ed.). Duxbury Press.")))
          )
        }
        else {
          withMathJax(
            div(style="font-size: 20px; font-weight: bold",
                tags$b(helpText("For a binomial distribution:")) ), 
            helpText("$$F(X = k_i) = \\sum_{i}^N \\binom{n}{k_i} p^k (1-p)^{n-k_i}, \\space n \\le 170$$"),
            strong(tags$i(helpText("Reference: Rice, John A. (2007). Mathematical statistics and data analysis (3rd ed.). Duxbury Press.")))
          )
        }
        
      } 
      else if (input$randvalue == "Geometric"){
        if(input$pdforcdf == "PDF"){
          withMathJax(
            div(style="font-size: 20px; font-weight: bold",
                tags$b(helpText("For a Geometric distribution:")) ),
            helpText("$$p(k)=P(X = k) = (1-p)^{k-1}p$$"),
            helpText("$$E(X=k) = \\frac{1}{p}$$"),
            strong(tags$i(helpText("Reference: Rice, John A. (2007). Mathematical statistics and data analysis (3rd ed.). Duxbury Press.")))
          )
        }
        else {
          withMathJax(
            div(style="font-size: 20px; font-weight: bold",
                tags$b(helpText("For a Geometric distribution:")) ),
            helpText("$$F(X = k_i) = \\sum_i^N (1-p)^{k_i-1}p$$"),
            strong(tags$i(helpText("Reference: Rice, John A. (2007). Mathematical statistics and data analysis (3rd ed.). Duxbury Press.")))
          )
        }
        
      }
      else if (input$randvalue == "Negative Binomial"){
        if(input$pdforcdf == "PDF") {
          withMathJax(
            div(style="font-size: 20px; font-weight: bold",
                tags$b(helpText("For a Negative Binomial distribution:")) ),
            helpText("$$P(X=k)= \\binom{k-1}{r-1}p^r(1-p)^{k-r}$$"),
            helpText("$$E(X=k) = \\frac{r}{p}$$"),
            strong(tags$i(helpText("Reference: Rice, John A. (2007). Mathematical statistics and data analysis (3rd ed.). Duxbury Press.")))
          )
        }
        else {
          withMathJax(
            div(style="font-size: 20px; font-weight: bold",
                tags$b(helpText("For a Negative Binomial distribution:")) ),
            helpText("$$F(X=k)= \\sum_{i}^N \\binom{k-1}{r-1}p^r(1-p)^{k-r}$$"),
            strong(tags$i(helpText("Reference: Rice, John A. (2007). Mathematical statistics and data analysis (3rd ed.). Duxbury Press.")))
          )
          
        }
        
      }
      else if (input$randvalue == "HyperGeometric"){
        if(input$pdforcdf == "PDF") {
          withMathJax(
            div(style="font-size: 20px; font-weight: bold",
                tags$b(helpText("For a HyperGeometric distribution:")) ),
            helpText("$$P(X=k)= \\frac{\\binom{r}{k} \\binom{n-r}{m-k}}{\\binom{n}{m}}$$"),
            helpText("$$E(X=k) = n \\frac{K}{N}$$"),
            strong(tags$i(helpText("Reference: Rice, John A. (2007). Mathematical statistics and data analysis (3rd ed.). Duxbury Press.")))
          )
        }
        else {
          withMathJax(
            div(style="font-size: 20px; font-weight: bold",
                tags$b(helpText("For a HyperGeometric distribution:")) ),
            helpText("$$F(X=k_i)= \\sum_{i}^N \\frac{\\binom{r}{k_i} \\binom{n-r}{m-k_i}}{\\binom{n}{m}}$$"),
            strong(tags$i(helpText("Reference: Rice, John A. (2007). Mathematical statistics and data analysis (3rd ed.). Duxbury Press.")))
          )
          
        }
        
      }
      else if (input$randvalue == "Poisson"){
        if(input$pdforcdf == "PDF") {
          withMathJax(
            div(style="font-size: 20px; font-weight: bold",
                tags$b(helpText("For a Poisson distribution:")) ),
            helpText("$$P(X=k)= \\frac{\\lambda^k}{k!}e^{-\\lambda}$$"),
            helpText("$$E(X=k) = \\lambda$$"),
            strong(tags$i(helpText("Reference: Rice, John A. (2007). Mathematical statistics and data analysis (3rd ed.). Duxbury Press.")))
          )
        }
        else {
          withMathJax(
            div(style="font-size: 20px; font-weight: bold",
                tags$b(helpText("For a Poisson distribution:")) ),
            helpText("$$F(X=k_i)= \\sum_{i}^N \\frac{\\lambda^k}{k!}e^{-\\lambda}$$"),
            strong(tags$i(helpText("Reference: Rice, John A. (2007). Mathematical statistics and data analysis (3rd ed.). Duxbury Press.")))
          )
          
        }
        
      }
      else if (input$randvalue == "Uniform"){
        if(input$pdforcdf == "PDF") {
          withMathJax(
            div(style="font-size: 20px; font-weight: bold",
                tags$b(helpText("For a Poisson distribution:")) ),
            helpText("$$
              f(x) =
              \\begin{cases} 
              \\frac{1}{b-a}, & a \\leq x \\leq b \\\\
              0, & x < a \\space or \\space x > b
              \\end{cases}
              $$"),
            helpText("$$E[X] = \\frac{a + b}{2}$$"),
            strong(tags$i(helpText("Reference: Rice, John A. (2007). Mathematical statistics and data analysis (3rd ed.). Duxbury Press.")))
          )
        }
        else {
          withMathJax(
            div(style="font-size: 20px; font-weight: bold",
                tags$b(helpText("For a Poisson distribution:")) ),
            helpText("$$
              F(x) =
              \\begin{cases} 
              a, & x \\leq a \\\\
              x, & a \\leq x \\leq b \\\\
              b, & x \\ge b
              \\end{cases}
              $$"),
            strong(tags$i(helpText("Reference: Rice, John A. (2007). Mathematical statistics and data analysis (3rd ed.). Duxbury Press.")))
          )
          
        }
        
      }
      else if (input$randvalue == "Normal"){
        if(input$pdforcdf == "PDF") {
          withMathJax(
            div(style="font-size: 20px; font-weight: bold",
                tags$b(helpText("For a Normal or Gaussian distribution:")) ),
            helpText("$$
              f(x) = \\phi = \\frac{1}{\\sigma \\sqrt{2\\pi}}e^{\\frac{-(x-\\mu)^2}{2\\sigma^2}}$$"),
            helpText("$$E[X] = \\mu$$"),
            #helpText("$$SEM = \\frac{\\sigma}{\\sqrt{n}}$$"),
            #helpText("$$Z = \\frac{\\overline{x}-\\mu}{SEM}$$"),
            #helpText("$$Z = \\frac{X-\\mu}{\\sigma}$$"),
            #helpText("$$CI = \\overline{x} \\pm z\\frac{\\sigma}{\\sqrt{n}}$$"),
            strong(tags$i(helpText("Reference: Rice, John A. (2007). Mathematical statistics and data analysis (3rd ed.). Duxbury Press.")))
          )
        }
        else {
          withMathJax(
            div(style="font-size: 15px; font-weight: bold",
                tags$b(helpText("For a Normal or Gaussian distribution:")) ),
            helpText("$$erf(z) = \\frac{2}{\\sqrt{\\pi}}\\int_0^{z}e^{-t^2}dt$$"),
            helpText("$$erf(z) = \\frac{2}{\\sqrt{\\pi}}\\sum_0^{\\infty}\\frac{-1^nz^{2n+1}}{n!(2n+1)}$$"),
            #helpText("$$erf(z) = 1 - \\frac{1}{(1+0.3275911z)^4}e^{-z^2}$$"),
            #helpText(HTML('<span style="font-size: 8px;">$$erf(z) = 1 - \\frac{1}{(1+0.278393z+0.230389z^2+0.000972z^3+0.078108z^4)^4}$$</span>')),
            helpText("$$F(x) = \\Phi = \\frac{1}{2}[1 + erf(\\frac{(x-\\mu)}{\\sigma\\sqrt{2}})]$$"),
            strong(tags$i(helpText("Reference: Rice, John A. (2007). Mathematical statistics and data analysis (3rd ed.). Duxbury Press.")))
          )
          
        }
        
      }
       else if (input$randvalue == "exponential") {
         if(input$pdforcdf == "PDF") {
           #nothing happens here
         }
       }
      else {
        helpText("Select a distribution to see the equation. You need JESUS!")
      }
    )
  })
  
  #UI distribution parameters
  output$distributionProperties <- renderUI({
    req(input$randvalue)
    if (input$randvalue == "Binomial") {
      div(
        div(style = "font-size: 20px; color: red; font-weight: 2rem", "Binomial Distribution Settings for a Bernoulli Random Variable"),
        div(
          style = "display: flex; flex-direction: row; align-items: center; gap: 100px;",
          selectInput("binomprob", label = withMathJax(strong("$$Binomial \\space Probability \\space (p)$$")), choices = c(0, 0.1, 0.3, 0.5, 0.8, 1), width = "40%"),
          selectInput("binomsampleSpace", label = withMathJax("$$Sample \\space Space \\space (\\Omega = n)$$"), choices = c(4, 10, 50, 80, 100, 150, 160, 170, 175, 180, 190, 200, 250, 300, 500, 2000), width = "40%")
        )
      )
    } 
    else if(input$randvalue == "Geometric"){
      div(
        div(style = "font-size: 20px; color: red; font-weight: 2rem", "Geometric Distribution Settings for a Bernoulli Random Variable"),
        div(
          style = "display: flex; flex-direction: row; align-items: center; gap: 100px;",
          selectInput("geomprob", label = withMathJax(strong("$$Geometric \\space Probability \\space (p)$$")), choices = c(0, 0.1, round(1/9,5),  0.3, 0.5, 0.8, 1), width = "40%"),
          selectInput("geomsampleSpace", label = withMathJax("$$Sample \\space Space \\space (\\Omega = n)$$"), choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 50, 80, 100, 150, 160, 170, 175, 180, 190, 200, 250, 300, 500, 2000, 3000, 4000, 5000, 7000), width = "40%")
        )
      )
    }
    else if(input$randvalue == "Negative Binomial"){
      div(
        div(style = "font-size: 20px; color: red; font-weight: 2rem", "Negative Binomial Distribution Settings for a Bernoulli Random Variable"),
        div(
          style = "display: flex; flex-direction: row; align-items: center; gap: 100px;",
          selectInput("negBprob", label = withMathJax(strong("$$Negative \\space Binomial \\space Probability \\space (p)$$")), choices = c(0, 0.1, round(1/9,5), 0.3, 0.5, 0.8, 1), width = "33%"),
          selectInput("negBsampleSpace", label = withMathJax("$$Sample \\space Space \\space (\\Omega = n)$$"), choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 50, 80, 100, 150, 160, 170, 175, 180, 190, 200, 250, 300, 500, 2000, 3000, 4000, 5000, 7000), width = "33%"),
          selectInput("rs", label = withMathJax("$$r \\space successes$$"), choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), width = "33%")
        )
      )
    }
    else if(input$randvalue == "HyperGeometric"){
      div(
        div(style = "font-size: 20px; color: red; font-weight: 2rem", "HyperGeometric Distribution Settings"),
        div(
          style = "display: flex; flex-direction: row; align-items: center; gap: 100px;",
          selectInput("hyperGeom_N", label = withMathJax(strong("$$Population \\space sample \\space \\space (N)$$")), choices = c(50, 53, 80, 100, 150, 160, 170, 175, 180, 190, 200, 250, 300, 500, 2000, 3000, 4000, 5000, 7000), width = "33%"),
          #selectInput("hyperGeomdraw", label = withMathJax("$$ Draw \\space Space \\space (n)$$"), choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20), width = "33%"),
          selectInput("rs", label = withMathJax("$$r \\space successes \\space (r=m)$$"), choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), width = "33%"),
          selectInput("hyperGeomsampleSpace", label = withMathJax("$$Sample \\space space \\space (k \\le r)$$"), choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50), width = "33%")
        )
      )
    }
    else if(input$randvalue == "Poisson"){
      div(
        div(style = "font-size: 20px; color: red; font-weight: 2rem", "Poisson Distribution Settings for a Poisson Process"),
        div(
          style = "display: flex; flex-direction: row; align-items: center; gap: 100px;",
          selectInput("poissonLamba", label = withMathJax(strong("$$Lambda \\space (\\lambda)$$")), choices = c(0, 0.1, round(1/9,5), round(((1/36) * 100),3), 0.3, 0.33, 0.5, 0.8, 1, 2, 3, 3.5, 3.96, 4, 5, 6, 7, 8, 9, 9.5, 10), width = "33%"),
          selectInput("PoissonsampleSpace", label = withMathJax("$$Sample \\space space \\space ( \\Omega)$$"), choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 20, 30, 40, 50, 100, 200, 300, 365, 400, 500, 600, 1000), width = "33%")
        )
      )
    }
    else if(input$randvalue == "Uniform"){
      div(
        div(style = "font-size: 20px; color: blue; font-weight: 2rem", "Uniform Distribution Settings"),
        div(
          style = "display: flex; flex-direction: row; align-items: center; gap: 100px;",
          selectInput("adeckUnifA", label = withMathJax(strong("$$a \\space (\\text{Lower Limit})$$")), choices = c(seq(-100,100)), selected = 0, width = "33%"),
          selectInput("adeckUnifB", label = withMathJax("$$b \\space ( \\text{Upper Limit})$$"), choices = c(seq(-100,100)), selected = 1, width = "33%"),
          selectInput("UnifsampleSpace", label = withMathJax("$$Sample \\space Space \\space (x_i)$$"), choices = c(seq(0,500)), selected = 100, width = "33%")
        )
      )
    }
    else if (input$randvalue == "Normal"){
      div (
        div(style = "font-size: 20px; color: blue; font-weight: 2rem", "Normal Distribution Settings"),
        div(
          style = "display: flex; flex-direction: row; align-items: center; gap: 100px;",
          selectInput("kaitySampleSpace", label = withMathJax(strong("$$\\Omega$$")), choices = c(seq(2,500, by=1)), selected = 0, width = "100%"),
          selectInput("kaityMu", label = withMathJax(strong("$$\\mu$$")), choices = c(seq(0,10, by=1)), selected = 0, width = "100%"),
          selectInput("kaitySigma", label = withMathJax(strong("$$\\sigma$$")), choices = c(seq(0,10, by=1)), selected = 1, width = "100%"),
        )
      )
    }
    else {
      div()
    }
  })
  
  #Server component
  output$distroPlots <- renderPlotly({
    if (input$randvalue == "Binomial") {
      
      if (input$pdforcdf == "PDF") {
        req(input$binomsampleSpace)
        x <- 0:input$binomsampleSpace
        binomdistroPDF <- binomdistro(x, (length(x) - 1), as.numeric(input$binomprob) )
        ggplotly(ggplot(binomdistroPDF, aes(x = k, y = prob)) +
                   geom_bar(stat = "identity", fill = "gray") +
                   geom_line() +
                   labs(title = paste("Binomial Probability Distribution p=",input$binomprob, "n=", input$binomsampleSpace),
                        x = "Number of Successes (k)",
                        y = "Probability P(X = k)") +
                   theme_minimal())
      }
      else {
        req(input$binomsampleSpace)
        x <- 0:input$binomsampleSpace
        binomdistroCDF <- binomdistro(x, (length(x) - 1), as.numeric(input$binomprob) )
        ggplotly(ggplot(binomdistroCDF, aes(x = k, y = cum_prob)) +
                   geom_line() +
                   geom_point() +
                   labs(title = paste("Binomial Probability Distribution p=",input$binomprob, "n=", input$binomsampleSpace),
                        x = "Number of Successes (k)",
                        y = "F(X = k)") +
                   theme_minimal())
      }
      
    }
    else if (input$randvalue == "Geometric") {
      
      if (input$pdforcdf == "PDF") {
        req(input$geomsampleSpace)
        req(input$geomprob)
        if (as.numeric(input$geomsampleSpace) < 10) {
          x <- seq(from = 1, to = input$geomsampleSpace, by = 0.1)
        }
        else {
          x <- seq(from = 1, to = input$geomsampleSpace, by = 1)
        }
        
        geomdistroPDF <- geomdistro(x, as.numeric(input$geomprob) )
        ggplotly(ggplot(geomdistroPDF, aes(x = k, y = prob)) +
                   geom_line() +
                   geom_point() +
                   labs(title = paste("Geometric Probability Distribution p=",input$geomprob, "n=", input$geomsampleSpace),
                        x = "x",
                        y = "Probability P(X = k)") +
                   theme_minimal())
      }
      else {
        p <- ggplot() +
          annotate("text", x = 1, y = 1, label = "You need JESUS FOR NOW! Cumulative Distribution Function Coming Soon! ", size = 5, fontface = "bold") +
          theme_void()  # Removes all axis, grids, etc.
        
        ggplotly(p)
      }
      
    }
    else if (input$randvalue == "Negative Binomial") {
      
      if (input$pdforcdf == "PDF") {
        req(input$negBsampleSpace)
        req(input$negBprob)
        req(input$rs)
        if (as.numeric(input$negBsampleSpace) < 10) {
          x <- seq(from = 1, to = input$negBsampleSpace, by = 0.1)
        }
        else {
          x <- seq(from = 1, to = input$negBsampleSpace, by = 1)
        }
        
        negBinomdistroPDF <- negBinomdistro(x, as.numeric(input$rs), as.numeric(input$negBprob) )
        ggplotly(ggplot(negBinomdistroPDF, aes(x = k, y = prob)) +
                   geom_line() +
                   geom_point() +
                   labs(title = paste("Negative Binomial Probability Distribution p=",input$negBprob, "n=", input$negBsampleSpace, "r=", input$rs),
                        x = "x",
                        y = "Probability P(X = k)") +
                   theme_minimal())
      }
      else {
        req(input$negBsampleSpace)
        req(input$negBprob)
        req(input$rs)
        if (as.numeric(input$negBsampleSpace) < 10) {
          x <- seq(from = 1, to = input$negBsampleSpace, by = 0.1)
        }
        else {
          x <- seq(from = 1, to = input$negBsampleSpace, by = 1)
        }
        
        negBinomdistroPDF <- negBinomdistro(x, as.numeric(input$rs), as.numeric(input$negBprob) )
        ggplotly(ggplot(negBinomdistroPDF, aes(x = k, y = cum_prob)) +
                   geom_line() +
                   geom_point() +
                   labs(title = paste("Negative Binomial Probability Distribution p=",input$negBprob, "n=", input$negBsampleSpace, "r=", input$rs),
                        x = "x",
                        y = "Probability P(X = k)") +
                   theme_minimal())
      }
      
    }
    else if (input$randvalue == "HyperGeometric") {
      if (input$pdforcdf == "PDF") {
        #req(input$hyperGeomdraw)
        req(input$hyperGeom_N)
        req(input$hyperGeomsampleSpace)
        req(input$rs)
        
        x <- seq(from = 0, to = input$hyperGeomsampleSpace, by = 1)
        
        hyperGeomdistroPDF <- hyperGeomdistro(x, as.numeric(input$rs), as.numeric(input$hyperGeom_N) )
        
        ggplotly(ggplot(hyperGeomdistroPDF, aes(x = k, y = prob)) +
                   geom_bar(stat = "identity", fill = "gray") +
                   geom_line() +
                   labs(title = paste("HyperGeometric PDF r=",input$rs, "N=", input$hyperGeom_N),
                        x = "x",
                        y = "Probability P(X = k)") +
                   theme_minimal())
      }
      else {
        req(input$hyperGeom_N)
        req(input$hyperGeomsampleSpace)
        req(input$rs)
        
        x <- seq(from = 0, to = input$hyperGeomsampleSpace, by = 1)
        
        hyperGeomdistroCDF <- hyperGeomdistro(x, as.numeric(input$rs), as.numeric(input$hyperGeom_N) )
        
        ggplotly(ggplot(hyperGeomdistroCDF, aes(x = k, y = cum_prob)) +
                   geom_bar(stat = "identity", fill = "gray") +
                   geom_line() +
                   geom_point() +
                   labs(title = paste("HyperGeometric CDF r=",input$rs, "N=", input$hyperGeom_N),
                        x = "x",
                        y = "Probability F(X = k)") +
                   theme_minimal())
      }
      
    }
    else if (input$randvalue == "Poisson") {
      if (input$pdforcdf == "PDF") {
        req(input$poissonLamba)
        req(input$PoissonsampleSpace)
        
        x <- seq(from = 0, to = input$PoissonsampleSpace, by = 1)
        
        poissondistroPDF <- poissondistro(x, as.numeric(input$poissonLamba) )
        
        ggplotly(ggplot(poissondistroPDF, aes(x = k, y = prob)) +
                   geom_bar(stat = "identity", fill = "gray") +
                   geom_line() +
                   labs(title = paste("Poisson PDF lamba=",input$poissonLamba, "N=", input$PoissonsampleSpace),
                        x = "x",
                        y = "Probability P(X = k)") +
                   theme_minimal())
      }
      else {
        req(input$poissonLamba)
        req(input$PoissonsampleSpace)
        
        x <- seq(from = 0, to = input$PoissonsampleSpace, by = 1)
        
        poissondistroCDF <- poissondistro(x, as.numeric(input$poissonLamba) )
        
        ggplotly(ggplot(poissondistroCDF, aes(x = k, y = cum_prob)) +
                   geom_line() +
                   geom_point() +
                   labs(title = paste("Poisson CDF lamba=",input$poissonLamba, "N=", input$PoissonsampleSpace),
                        x = "x",
                        y = "Probability F(X = k)") +
                   theme_minimal())
      }
      
    }
    else if (input$randvalue == "Uniform") {
      if (input$pdforcdf == "PDF") {
        req(input$adeckUnifA)
        req(input$adeckUnifB)
        
        x <- seq(from = as.numeric(input$adeckUnifA), to = as.numeric(input$adeckUnifB), by = as.numeric(input$unifstepsize))
        
        UnifdistroPDF <- uniformdistro(x, as.numeric(input$adeckUnifA), as.numeric(input$adeckUnifB) )
        #print(UnifdistroPDF)
        
        ggplotly(ggplot(UnifdistroPDF, aes(x = k, y = prob)) +
                   geom_bar(stat = "identity", fill = "gray") +
                   geom_line() +
                   labs(title = paste("Uniform PDF a=",input$adeckUnifA, "b=", input$adeckUnifB),
                        x = "x",
                        y = "Probability f(x)") +
                   theme_minimal())
      }
      else {
        req(input$adeckUnifA)
        req(input$adeckUnifB)
        
        x <- seq(from = as.numeric(input$adeckUnifA), to = as.numeric(input$adeckUnifB), by = as.numeric(input$unifstepsize))
        
        UnifdistroCDF <- uniformdistro(x, as.numeric(input$adeckUnifA), as.numeric(input$adeckUnifB) )
        
        ggplotly(ggplot(UnifdistroCDF, aes(x = k, y = cum_prob)) +
                   geom_line() +
                   geom_point() +
                   labs(title = paste("Uniform CDF a=",input$adeckUnifA, "b=", input$adeckUnifB),
                        x = "x",
                        y = "Probability F(x)") +
                   theme_minimal())
      }
      
    }
    
    else if (input$randvalue == "Normal") {
      if (input$pdforcdf == "PDF") {
        req(input$kaitySampleSpace)
        req(input$kaityMu)
        req(input$kaitySigma)
        
        x <- getRnorm(input$kaitySampleSpace, as.numeric(input$kaityMu), as.numeric(input$kaitySigma) )
        normalDistroPDF <- adecknormDistro(x)
        #normalDistroPDF <- adecknormDistro(x, norm_reverse=TRUE)
        print(normalDistroPDF)
        ggplotly(ggplot(normalDistroPDF, aes(x = k, y = prob)) +
                   geom_bar(stat = "identity", fill = "gray") +
                   geom_line() +
                   labs(title = paste("Normal PDF mean=",input$kaityMu, "stdev=", input$kaitySigma),
                        x = "x",
                        y = "Probability f(x)") +
                   theme_minimal())
          
      }
      else if (input$pdforcdf == "CDF"){
        req(input$kaitySampleSpace)
        req(input$kaityMu)
        req(input$kaitySigma)
        
        x <- getRnorm(input$kaitySampleSpace, as.numeric(input$kaityMu), as.numeric(input$kaitySigma) )
        normalDistroCDF <- adecknormDistro(x)
        print(normalDistroCDF)
        ggplotly(ggplot(normalDistroCDF, aes(x = k, y = cum_prob)) +
                   geom_bar(stat = "identity", fill = "gray") +
                   geom_line() +
                   labs(title = paste("Normal CDF mean=",input$kaityMu, "stdev=", input$kaitySigma),
                        x = "x",
                        y = "Cumulative Probability F(x)") +
                   theme_minimal())
      }
      else if (input$pdforcdf == "P-Curve") {
        req(input$kaitySampleSpace)
        req(input$kaityMu)
        req(input$kaitySigma)
        
        x <- getRnorm(input$kaitySampleSpace, as.numeric(input$kaityMu), as.numeric(input$kaitySigma) )
        normalDistroCDF <- adecknormDistro(x)
        print(normalDistroCDF)
        ggplotly(ggplot(normalDistroCDF, aes(x = z, y = pval)) +
                   geom_bar(stat = "identity", fill = "gray") +
                   geom_line() +
                   labs(title = paste("Normal Probability Curve mean=",input$kaityMu, "stdev=", input$kaitySigma),
                        x = "z",
                        y = "P-value (p)") +
                   theme_minimal())
      }
      
    }
  
    
  })
}

server
