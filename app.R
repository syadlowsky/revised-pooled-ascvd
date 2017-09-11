# This code implements an automated GUI risk score calculator for patients with type 2 diabetes, using the Shiny package in R.
# install the shiny app before using

library(shiny)

ui = navbarPage("RECODe",
                
                tabPanel("Risk Calculator",
                         
                         fluidPage(
                           
                           titlePanel("Risk Equations for Complications Of Type 2 Diabetes"),
                           h5("Switch to:", a("version using SI units", href="https://sanjaybasu.shinyapps.io/recodesi/")),
                           
                           fluidRow(
                             column(3,
                                    numericInput("age", label = "Age (years)", value = 60),
                                    selectInput("sex", label = "Sex", choices = list(Male=0, Female=1)),
                                    radioButtons("black", label = "Black?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("hisp", label = "Hispanic?", choices = list(No=0, Yes=1), inline = TRUE)
                             ),
                             column(3,
                                    numericInput("totchol", label = "Total cholesterol (mg/dL)", value = 190),
                                    numericInput("hdlchol", label = "HDL cholesterol (mg/dL)", value = 50),
                                    radioButtons("statin", label = "On statin?", choices = list(No=0, Yes=1), inline = TRUE),
                                    numericInput("hgba1c", label = "Hemoglobin A1c (%)", value = 8),
                                    radioButtons("oralrx", label = "On oral diabetes medication?", choices = list(No=0, Yes=1), inline = TRUE)
                             ),
                             column(3,numericInput("sercreat", label = "Serum creatinine (mg/dL)", value = 1.1),
                                    numericInput("uralbcreat", label = "Urine albumin/creatinine ratio (mg/g)", value = 10),
                                    numericInput("sysbp", label = "Systolic blood pressure (mm Hg)", value = 140),
                                    radioButtons("bprx", label = "On blood pressure treatment?", choices = list(No=0, Yes=1), inline = TRUE)
                             ),
                             column(3,
                                    radioButtons("cvdhist", label = "Prior myocardial infaction or stroke?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("anticoag", label = "On anticoagulant (other than aspirin)?", choices = list(No=0, Yes=1), inline = TRUE),
                                    radioButtons("cursmoke", label = "Currently smoking tobacco?", choices = list(No=0, Yes=1), inline = TRUE)
                             )
                           ),
                           
                           hr(),
                           
                           fluidRow(
                             column(3,
                                    h4("Estimated 10-yr risk of:", align = "center")
                             )
                           ),
                           
                           hr(),
                           
                           fluidRow(
                             column(3,
                                    h4("Nephropathy (%):", align = "center")
                             ),
                             column(3,
                                    h4(textOutput("neph"), align = "center")
                             ),
                             column(6,
                                    "Renal failure/end-stage renal disease."
                             )
                           ),
                           
                           hr(),
                           
                           fluidRow(
                             column(3,
                                    h4("Retinopathy (%):", align = "center")
                             ),
                             column(3,
                                    h4(textOutput("retin"), align = "center")
                             ),
                             column(6,
                                    "Severe vision loss (<20/200)."
                             )
                           ),
                           
                           hr(),
                           
                           fluidRow(
                             column(3,
                                    h4("Neuropathy (%):", align = "center")
                             ),
                             column(3,
                                    h4(textOutput("neuro"), align = "center")
                             ),
                             column(6,
                                    "Pressure sensation loss."
                             )
                           ),
                           
                           hr(),
                           
                           fluidRow(
                             column(3,
                                    h4("Myocardial infarction or stroke (%):", align = "center")
                             ),
                             column(3,
                                    h4(textOutput("mi"), align = "center")
                             ),
                             column(6,
                                    "Fatal or nonfatal."
                             )
                           ),
                           
                           
                           hr(),
                           
                           fluidRow(
                             column(3,
                                    h4("Congestive heart failure (%):", align = "center")
                             ),
                             column(3,
                                    h4(textOutput("chf"), align = "center")
                             ),
                             column(6,
                                    "Symptomatic heart failure, NYHA Class III or IV CHF, or ejection fraction (by any method) < 25%."
                             )
                           ),
                           
                           hr(),
                           
                           fluidRow(
                             column(3,
                                    h4("Mortality (%):", align = "center")
                             ),
                             column(3,
                                    h4(textOutput("death"), align = "center")
                             ),
                             column(6,
                                    "From any cause."
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
                         
                         h4("Risk model was derived from ACCORD and validated against both DPPOS (for microvascular outcomes) and Look AHEAD (for macrovascular outcomes). Summary statistics for all three trials are presented below:"),
                         
                         br(),
                         
                         fluidRow(
                           column(12,
                                  dataTableOutput('summary')
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
  neph_beta = reactive({round(100*(1-.973^exp(0 + 
                                                -0.0193838993 * as.numeric(input$age) + 
                                                -0.0112943865 * as.numeric(input$sex) + 
                                                -0.0881241594 * as.numeric(input$black) + 
                                                0.2337712368 * as.numeric(input$hisp) + 
                                                0.0030271330 * as.numeric(input$sysbp) + 
                                                -0.0795168593 * as.numeric(input$bprx) + 
                                                0.1483078052 * as.numeric(input$cursmoke) +
                                                -0.0216363649 * as.numeric(input$cvdhist) + 
                                                -0.1255530728 * as.numeric(input$oralrx) + 
                                                0.8608801402 * as.numeric(input$sercreat) + 
                                                -0.0011121510 * as.numeric(input$totchol) + 
                                                0.0062888253 * as.numeric(input$hdlchol) +
                                                0.0319895697 * as.numeric(input$anticoag) + 
                                                0.1369126389 * as.numeric(input$hgba1c) +
                                                0.0003615507*as.numeric(input$uralbcreat)
                                              -0.2269629)))
  })
  
  
  
  
  
  output$neph = renderText({ neph_beta() })
  
  
  retin_beta = reactive({round(100*(1-.921^exp(0 + 
                                                 0.0228504061 * as.numeric(input$age) + 
                                                 0.2264337097 * as.numeric(input$sex) + 
                                                 -0.1676573729 * as.numeric(input$black) + 
                                                 0.0082431088 * as.numeric(input$sysbp) + 
                                                 0.0639339678 * as.numeric(input$bprx) + 
                                                 0.1127372373 * as.numeric(input$cvdhist) + 
                                                 -0.2348989478 * as.numeric(input$oralrx) + 
                                                 0.6946500975 * as.numeric(input$sercreat) + 
                                                 -0.0001676169 * as.numeric(input$totchol) + 
                                                 0.0054470159 * as.numeric(input$hdlchol) +
                                                 0.1449446673 * as.numeric(input$hgba1c) +
                                                 0.0001991881*as.numeric(input$uralbcreat)
                                               -4.563441)))
  })
  
  
  output$retin = renderText({ retin_beta() })
  
  
  neuro_beta = reactive({round(100*(1-0.87^exp(0 + 
                                                3.022e-02 * as.numeric(input$age) + 
                                                -1.868e-01 * as.numeric(input$sex) + 
                                                -9.448e-02 * as.numeric(input$black) + 
                                                4.561e-03 * as.numeric(input$sysbp) + 
                                                1.819e-01 * as.numeric(input$bprx) + 
                                                2.667e-01 * as.numeric(input$cvdhist) + 
                                                -2.575e-01 * as.numeric(input$oralrx) + 
                                                6.044e-01 * as.numeric(input$sercreat) + 
                                                2.185e-03 * as.numeric(input$totchol) + 
                                                -5.389e-03 * as.numeric(input$hdlchol) +
                                                1.887e-01 * as.numeric(input$hgba1c) +
                                                -4.746261)))
  })
  
  
  output$neuro = renderText({ neuro_beta() })
  
  
  
  
  mi_beta = reactive({round(100*(1-0.85^exp(0 + 
                                              0.034210 * as.numeric(input$age) + 
                                              -0.167200* as.numeric(input$sex) + 
                                              -0.118700 * as.numeric(input$black) + 
                                              0.151000*as.numeric(input$cursmoke)+
                                              0.000074 * as.numeric(input$sysbp) + 
                                              0.055790 * as.numeric(input$bprx) + 
                                              0.778400 * as.numeric(input$cvdhist) + 
                                              -0.033610 * as.numeric(input$statin) + 
                                              0.252400*as.numeric(input$anticoag)+
                                              0.435500 * as.numeric(input$sercreat) + 
                                              0.001929 * as.numeric(input$totchol) + 
                                              -0.008370 * as.numeric(input$hdlchol) +
                                              0.171600 * as.numeric(input$hgba1c) +
                                              0.000333 *as.numeric(input$uralbcreat)+
                                              -3.65)))
  })
  
  
  output$mi = renderText({ mi_beta() })
  
  
  
  
  
  
  
  chf_beta = reactive({round(100*(1-0.96^exp(0 + 
                                               5.268e-02 * as.numeric(input$age) + 
                                               2.529e-01 * as.numeric(input$sex) + 
                                               -4.969e-02 * as.numeric(input$black) + 
                                               2.905e-01*as.numeric(input$cursmoke)+
                                               1.217e-03 * as.numeric(input$sysbp) + 
                                               6.389e-01 * as.numeric(input$bprx) + 
                                               1.007e00 * as.numeric(input$cvdhist) +
                                               -1.175e-01*as.numeric(input$statin)+
                                               7.365e-01*as.numeric(input$anticoag)+
                                               4.142e-04 * as.numeric(input$uralbcreat) + 
                                               8.214e-01 * as.numeric(input$sercreat) + 
                                               -1.358e-03 * as.numeric(input$totchol) + 
                                               -1.758e-02 * as.numeric(input$hdlchol) +
                                               2.092e-01 * as.numeric(input$hgba1c) +
                                               -5.15)))
  })
  
  
  output$chf = renderText({ chf_beta() })
  
  
  
  
  death_beta = reactive({round(100*(1-0.93^exp(0 + 
                                                 6.703e-02 * as.numeric(input$age) + 
                                                 -1.529e-01 * as.numeric(input$sex) + 
                                                 -2.393e-02 * as.numeric(input$black) + 
                                                 5.399e-01*as.numeric(input$cursmoke)+
                                                 -2.988e-03 * as.numeric(input$sysbp) + 
                                                 8.766e-02* as.numeric(input$bprx) + 
                                                 5.888e-01 * as.numeric(input$cvdhist) +
                                                 -2.681e-01*as.numeric(input$statin)+
                                                 4.036e-01*as.numeric(input$anticoag)+
                                                 3.889e-04 * as.numeric(input$uralbcreat) + 
                                                 3.597e-01 * as.numeric(input$sercreat) + 
                                                 -9.478e-04 * as.numeric(input$totchol) + 
                                                 -4.378e-03 * as.numeric(input$hdlchol) +
                                                 1.659e-01 * as.numeric(input$hgba1c) +
                                                 -4.66)))
  })
  
  
  output$death = renderText({ death_beta() })
  
  
  
  
  output$summary = renderDataTable({
    rows = c("Age, mean (SD), y", "Senior in age, >75 y (%)", "Women", "Black race", "Hispanic or Latino ethnic group", "Tobacco smoking, current", "Body mass index, mean (SD), kg/m2",
             "Systolic, mean (SD), mmHg", "Diastolic, mean (SD), mmHg", "Heart rate, beats/min", "Cardiovascular disease history", "Blood pressure treatment",
             "Oral diabetes medication (including metformin)", "Insulin treatment", "Statin use", "Fibrate use", "Anticoagulant use", "Non-steroidal anti-inflammatory use",
             "Platelet aggregate inhibitor use", "Daily aspirin use", "Hemoglobin A1c, mean (SD), %", "Total cholesterol, mean (SD), mg/dL", "Direct high-density lipoprotein cholesterol, mean (SD), mg/dL",
             "Low-density lipoprotein cholesterol, mean (SD), mg/dL", "Triglycerides, mean (SD), mg/dL", "Fasting plasma glucose, mean (SD), mg/dL", "Alanine aminotransferase, mean (SD), IU/dL",
             "Creatine phosphokinase, mean (SD), U/L", "Serum potassium, mean (SD), mmol/L", "Serum creatinine, mean (SD), mg/dL", "Estimated glomerular filtration rate, mean (SD), mL/min/1.73m2",
             "Urine albumin, mean (SD), mg/L", "Urine creatinine, mean (SD), mg/L", "Urine albumin/creatinine ratio, mean (SD), mg/g")
    accord = c("62.8 (6.7)", "521 (5.4)", "3,662 (38.0)", "1,834 (19.0)", "678 (7.0)", "1,179 (12.2)", "32.2 (5.4)", "136.5 (17.1)",
               "74.9 (10.7)", "72.7 (11.8)", "3,437 (35.7)", "8,109 (84.2)", "8,024 (83.3)", "3,403 (35.3)", "6,148 (63.8)", "601 (6.2)", 
               "303 (3.1)", "851 (8.8)", "466 (4.8)", "5,274 (54.7)", "8.3 (1.1)", "183.2 (41.7)", "41.8 (11.6)", "104.7 (33.8)", "190.7 (145.8)",
               "175.3 (55.8)", "27.5 (16.0)", "140.3 (130.2)", "4.5 (0.5)", "0.9 (0.2)", "90.9 (27.3)", "10.7 (37.3)", "127.3 (65.4)", "99.2 (359.4)")
    dppos = c("50.9 (8.0)", "0 (0)", "680 (66.8)", "244 (24.0)", "175 (17.2)", "52 (5.1)", "33.9 (5.9)", "123.7 (14.0)",
              "76.4 (8.8)", "N/A", "12 (1.2)", "770 (75.6)", "336 (33.0)", "N/A", "721 (70.8)", "N/A", "N/A", "N/A", "N/A", "N/A", 
              "6.1 (0.7)", "196.0 (43.7)", "46.0 (12.3)", "99.5 (27.3)", "162.7 (256.8)", "115.8 (22.7)", "N/A", "N/A", "N/A", "0.8 (0.2)",
              "98.8 (15.7)", "10.8 (6.4)", "123.3 (73.6)", "N/A")
    lookahead = c("58.9 (6.7)", "31 (0.7)", "2,784 (58.5)", "776 (16.3)", "670 (14.1)", "202 (4.2)", "36.0 (5.9)", "129.0 (17.1)", "70.2 (9.5)",
                  "71.4 (11.4)", "665 (14.0)", "3,410 (71.6)", "3,246 (68.2)", "724 (15.2)", "2,142 (45.0)", "324 (6.8)", "N/A", "N/A", "N/A", "2,140 (45.0)",
                  "7.3 (1.2)", "191.4 (37.3)", "43.5 (11.9)", "112.7 (32.1)", "N/A", "153.2 (45.6)", "N/A", "N/A", "N/A",
                  "0.8 (0.2)", "89.9 (16.1)", "4.8 (23.0)", "121.0 (67.0)", "43.1 (201.5)")
    table = data.frame(rows, accord, dppos, lookahead)
    colnames(table) = c("", "No. (%) in ACCORD (N = 9,635 used for equation derivation)", 
                        "No. (%) in DPPOS (N = 1,018 used for equation validation)",
                        "No. (%) in Look AHEAD (N = 4,760 used for equation validation)")
    table
  }, options = list(searching = FALSE, paging = FALSE))
  
  
}

shinyApp(ui = ui, server = server)