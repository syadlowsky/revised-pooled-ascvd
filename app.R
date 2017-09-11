# This code implements an automated GUI risk score calculator for patients with type 2 diabetes, using the Shiny package in R.
# install the shiny app before using

library(DT)
library(shiny)

specify.decimal <- function(t, nsmall=3, zero=T) {
    if (round(t, nsmall) == 0 & !zero) {
        return(signif(t, 1))
    }
    return(format(round(t, nsmall), nsmall=nsmall))
}


ui = navbarPage("ASCVD",
                
                tabPanel("Risk Calculator",
                         
                         fluidPage(
                           
                           titlePanel("Risk Equations for Atherosclerotic CVD 10-year incidence"),
                           
                           fluidRow(
                             column(width=6,
                                    numericInput("age", label = "Age (years)", value = 60),
                                    selectInput("sex", label = "Sex", choices = list(Male=0, Female=1)),
                                    radioButtons("black", label = "Black?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("dm", label = "Diabetes mellitus?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("cursmoke", label = "Currently smoking tobacco?", choices = list(No=0, Yes=1), inline = TRUE)
                             ),
                             column(width=6,
                                    numericInput("totchol", label = "Total cholesterol (mg/dL)", value = 190),
                                    numericInput("hdlc", label = "HDL cholesterol (mg/dL)", value = 50),
                                    numericInput("sysbp", label = "Systolic blood pressure (mm Hg)", value = 140),
                                    radioButtons("rxbp", label = "On blood pressure treatment?", choices = list(No=0, Yes=1), inline = TRUE)
                             )
                           ),
                           
                           hr(),
                           
                           fluidRow(
                             column(width=6,
                                    h4("Estimated 10-yr risk of ASCVD:", align = "center")
                             ),
                             column(width=6,
                                    h4(textOutput("ascvd_risk"), align = "center")
                             )
                           ),
                           

                           hr(),
                           "Note: This calculator is intended for informational purposes only, and has not been prospectively 
                           evaluated for impact on clinical practice or patient outcomes. Calculations must be re-checked and 
                           should not be used alone to guide patient care, nor should they substitute for clinical judgment.
                           Contact: Sanjay Basu, basus@stanford.edu
                           
                           "
                           
                         )
                         
                ),
                
                tabPanel("Summary Statistics",
                         
                         h4("Risk model was derived from ARIC, JHS, MESA, CHS, CARDIA, and the Framingham Offspring Study. Summary statistics for the pooled development and validation cohorts are presented here:"),
                         
                         br(),
                         
                         fluidRow(
                           column(width=12,
                                  align="center",
                                  tableOutput('summary')
                           )
                         )
                         
                ),
                
                
                tabPanel("Disclaimers",
                         
                         h5("This website contains clinical tools and data intended for use by healthcare professionals. These tools do not give professional advice; physicians and other healthcare professionals who use these tools or data should exercise their own clinical judgment as to the information they provide. Consumers who use the tools or data do so at their own risk. Individuals with any type of medical condition are specifically cautioned to seek professional medical advice before beginning any sort of health treatment. For medical concerns, including decisions about medications and other treatments, users should always consult their physician or other qualified healthcare professional.
                            
                            Our content developers have carefully tried to create its content to conform to the standards of professional practice that prevailed at the time of development. However, standards and practices in medicine change as new data become available and the individual medical professional should consult a variety of sources.
                            
                            The contents of the Site, such as text, graphics and images are for informational purposes only. We do not recommend or endorse any specific tests, physicians, products, procedures, opinions, or other information that may be mentioned on the Site.
                            
                            While information on this site has been obtained from sources believed to be reliable, neither we nor our content providers warrant the accuracy of the information contained on this site.
                            
                            We do not give medical advice, nor do we provide medical or diagnostic services. Medical information changes rapidly. Neither we nor our content providers guarantee that the content covers all possible uses, directions, precautions, drug interactions, or adverse effects that may be associated with any therapeutic treatments.
                            
                            Your reliance upon information and content obtained by you at or through this site is solely at your own risk. Neither we nor our content providers assume any liability or responsibility for damage or injury (including death) to you, other persons or property arising from any use of any product, information, idea or instruction contained in the content or services provided to you.
                            
                            We cannot and will not be held legally, financially, or medically responsible for decisions made using these calculators, equations, and algorithms, and this Site is for the use of medical professionals only."),
                         
                         br(),
                         
                         h5("The calculations and website shown here were prepared using ACCORD, DPPOS and Look AHEAD research materials obtained from the NHLBI Biologic Specimen and Data Repository Information Coordinating Center and the NIDDK Central Database Repository. The calculations and content on this Site do not necessary reflect the opinions or views of the ACCORD, DPPOS, Look AHEAD, NHLBI, or NIDDK."),
                         
                         br(),
                         h5("The Diabetes Prevention Program (DPP) and Diabetes Prevention Program Outcomes Study (DPPOS) were conducted by the DPP Research Group and supported by the National Institute of Diabetes and Digestive and Kidney Diseases (NIDDK), the General Clinical Research Center Program, the National Institute of Child Health and Human Development (NICHD), the National Institute on Aging (NIA), the Office of Research on Women's Health, the Office of Research on Minority Health, the Centers for Disease Control and Prevention (CDC), and the American Diabetes Association. The data from the DPP and DPPOS were supplied by the NIDDK Central Repositories. This calculator or website and its related materials were not prepared under the auspices of the DPP/DPPOS and does not represent analyses or conclusions of the DPP Research Group, the NIDDK Central Repositories, or the NIH."),
                         
                         br(),
                         h5("Look AHEAD was conducted by the Look AHEAD Research Group and supported by the National Institute of Diabetes and Digestive and Kidney Diseases (NIDDK); the National Heart, Lung, and Blood Institute (NHLBI); the National Institute of Nursing Research (NINR); the National Institute of Minority Health and Health Disparities (NIMHD); the Office of Research on Women's Health (ORWH); and the Centers for Disease Control and Prevention (CDC). The data from Look AHEAD were supplied by the NIDDK Central Repositories. This calculator or website and its related materials was not prepared under the auspices of the Look AHEAD and does not represent analyses or conclusions of the Look AHEAD Research Group, the NIDDK Central Repositories, or the NIH."),
                         
                         br(),
                         h5("Financial support for this calculator and website and its related materials was provided in part by grants from the National Institute On Minority Health And Health Disparities of the National Institutes of Health under Award Numbers DP2MD010478 and U54MD010724; the National Heart, Lung, And Blood Institute of the National Institutes of Health under Award Number K08HL121056; the National Institute of Diabetes, Digestive and Kidney Diseases of The National Institutes of Health under Award Numbers P60DK20572 and K23DK109200; and the Department of Veterans Affairs HSR&D Service under Award Numbers IIR11-088 and CDA13-021. The funding agreement ensured the authors’ independence in designing the calculations, interpreting the data, writing, and publishing the results. The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health or the Department of Veterans Affairs, or of any of the authors' affiliated institutions.")
                         
                         
                         
                         )
                
                
                
                )

server = function(input, output) {
  # Access input values with input$*
  # Save output objects to output$*
  # Build objects with render*({ code })
    ascvd_estimator = reactive({
        female.risk <- 1.0 / (1.0 + exp( - (
            -13.40436 +
            0.1209233 * as.numeric(input$age) +
            0.59126 * as.numeric(input$black) +
            7.215301E-05 * (as.numeric(input$sysbp) ^ 2) +
            0.02002917 * as.numeric(input$sysbp) +
            0.8105812 * as.numeric(input$rxbp) +
            0.9452969 * as.numeric(input$dm) +
            1.017919 * as.numeric(input$cursmoke) +
            0.1485375 * (as.numeric(input$totchol) / as.numeric(input$hdlc)) +
            -0.007943772 * as.numeric(input$age) * as.numeric(input$black) +
            -0.004264248 * as.numeric(input$sysbp) * as.numeric(input$rxbp) +
            0.005912054 * as.numeric(input$sysbp) * as.numeric(input$black) +
            0.09430543 * as.numeric(input$black) * as.numeric(input$rxbp) +
            -0.0002607624 * as.numeric(input$age) * as.numeric(input$sysbp) +
            0.1040258 * as.numeric(input$black) * as.numeric(input$dm) +
            -0.08093072 * as.numeric(input$black) * as.numeric(input$cursmoke) +
            0.07144001 * as.numeric(input$black) * (as.numeric(input$totchol) / as.numeric(input$hdlc)) +
            -6.913603E-05 * as.numeric(input$age) * as.numeric(input$sysbp) * as.numeric(input$black)
            )))
        male.risk <- 1.0 / (1.0 + exp( - (
            -11.33614 +
            0.0666618 * as.numeric(input$age) +
            0.43525 * as.numeric(input$black) +
            -4.012011E-05 * (as.numeric(input$sysbp) ^ 2) +
            0.03420654 * as.numeric(input$sysbp) +
            1.957798 * as.numeric(input$rxbp) +
            0.831096 * as.numeric(input$dm) +
            0.8976064 * as.numeric(input$cursmoke) +
            0.1894506 * (as.numeric(input$totchol) / as.numeric(input$hdlc)) +
            -0.01347768 * as.numeric(input$sysbp) * as.numeric(input$rxbp) +
            0.007847015 * as.numeric(input$sysbp) * as.numeric(input$black) +
            -0.05135988 * as.numeric(input$rxbp) * as.numeric(input$black) +
            3.059223E-07 * as.numeric(input$age) * as.numeric(input$sysbp) +
            -0.09236525 * as.numeric(input$black) * as.numeric(input$dm) +
            -0.1982048 * as.numeric(input$black) * as.numeric(input$cursmoke) +
            -0.1115326 * (as.numeric(input$totchol) / as.numeric(input$hdlc)) * as.numeric(input$black) +
            0.003682936 * as.numeric(input$black) * as.numeric(input$rxbp) * as.numeric(input$sysbp) +
            -0.0001280129 * as.numeric(input$black) * as.numeric(input$age) * as.numeric(input$sysbp)
            )))
        paste(
            specify.decimal(100*(ifelse(as.numeric(input$sex) == 1, female.risk, male.risk)), nsmall=1, zero=F),
            "%", sep="")
  })
  
  output$ascvd_risk = renderText({ ascvd_estimator() })
  
  output$summary = renderTable({
df <- as.data.frame(rbind(
c("", "Black", "", "", "", "White", "", "", ""),
c("", "Development", "", "Validation", "", "Development", "", "Validation", ""),
c("", "Mean", "Std. dev.", "Mean", "Std. dev.", "Mean", "Std. dev.", "Mean", "Std. dev."),
c("Women", "n = 3765", "", "n = 944", "", "n = 7354", "", "n = 1816", ""),
c("Age Range", "(40, 79)", "", "(40, 79)", "", "(40, 79)", "", "(40, 79)", ""),
c("Age (yrs)", "55.5", "9.4", "55.7", "9.4", "57.6", "9.8", "57.3", "9.7"),
c("Total Cholesterol (mg/dl)", "206.1", "42.2", "206.4", "41.1", "216.0", "40.6", "213.9", "39.5"),
c("HDL Cholesterol (mg/dL)", "56.9", "16.1", "57.1", "15.5", "58.5", "16.6", "58.5", "16.4"),
c("Untreated SBP (mmHg)", "124.1", "19.0", "122.6", "18.1", "118.3", "18.6", "118.2", "18.1"),
c("Treated SBP (mmHg)", "134.6", "21.5", "134.4", "21.3", "133.9", "19.9", "133.5", "19.7"),
c("BP Meds (%)", "45.26", "", "47.03", "", "20.44", "", "20.81", ""),
c("Current Smoker (%)", "18.22", "", "16.10", "", "22.57", "", "21.81", ""),
c("Diabetes (%)", "16.89", "", "19.28", "", "6.23", "", "6.11", ""),
c("10yr ASCVD incidence per 1,000 person-yrs", "6.59", "", "6.78", "", "6.51", "", "6.44", ""),
c("", "Development", "", "Validation", "", "Development", "", "Validation", ""),
c("", "Mean", "Std. dev.", "Mean", "Std. dev.", "Mean", "Std. dev.", "Mean", "Std. dev."),
c("Men", "n = 2503", "", "n = 623", "", "n = 6261", "", "n = 1582", ""),
c("Age Range", "(40, 79)", "", "(40, 79)", "", "(40, 79)", "", "(40, 79)", ""),
c("Age (yrs)", "56.1", "9.4", "55.9", "9.9", "57.0", "9.4", "57.2", "9.5"),
c("Total Cholesterol (mg/dl)", "199.3", "41.8", "198.8", "42.4", "205.5", "37.9", "207.6", "38.7"),
c("HDL Cholesterol (mg/dL)", "48.4", "14.9", "48.0", "14.4", "44.3", "12.6", "44.2", "12.2"),
c("Untreated SBP (mmHg)", "127.1", "19.2", "127.2", "18.8", "121.7", "17.0", "121.9", "16.8"),
c("Treated SBP (mmHg)", "134.1", "19.0", "134.8", "18.9", "132.4", "19.9", "133.3", "21.2"),
c("BP Meds (%)", "36.56", "", "33.87", "", "19.20", "", "19.41", ""),
c("Current Smoker (%)", "27.53", "", "27.45", "", "22.89", "", "21.37", ""),
c("Diabetes (%)", "16.30", "", "16.21", "", "8.48", "", "8.85", ""),
c("10yr ASCVD incidence per 1,000 person-yrs", "9.64", "", "9.33", "", "11.31", "", "11.85", "")
))
  }, colnames=F, rownames=F)
  
  
}


runApp(shinyApp(ui = ui, server = server), port=3000)
