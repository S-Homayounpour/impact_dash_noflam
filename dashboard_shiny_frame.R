library(fs)
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyBS)
library(frame)
library(ggplot2)
library(tidyverse)
library(extraDistr)
library(plotly)
library(impact) 
source("used_functions.R")


#############
#######ui
############



ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Fire Research and Modelling Environment", titleWidth = 450),
  dashboardSidebar(
    sidebarMenu( id = "tabs", 
      menuItem("Introduction",tabName = "intro",icon = icon("chalkboard")),
      menuItem("Community description",tabName = "binputs", icon = icon("envira")),
      menuItem("Fire Behaviour Analysis",tabName = "fire_beh_anls",icon = icon("fire"),
               menuSubItem("Deterministic behaviour",tabName = "det_beh_anls"),
               menuSubItem("Probabilistic behaviour",tabName = "prob_beh_anls"),
               menuSubItem("Environmental drivers ",tabName = "envr_drvr_anls"),
               menuSubItem("Contextual flammability",tabName = "stdf_site" )
               ),
      menuItem("Fire Impacts",tabName = "fr_impct", icon = icon("burn"),
               menuSubItem("Earth",tabName = "soil_impct",icon = icon("globe-asia")),
               menuSubItem("Flora",tabName = "veg_impct",icon = icon("leaf")),
               menuSubItem("Fauna",tabName = "anim_impct",icon = icon("crow")))
      )
     ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
              box(width = 12,
                h2("Introduction"),
                tags$h2("FRaME",style= "color:#874d0a",align="middle"),
                tags$h3("Fire Research and Modelling Environment",style= "color:#874d0a",align="middle"),
                tags$p("FRaME is an extensible R package that encapsulates a biophysical fire 
                       behaviour model to find the effects that an ecosystem has on fire behaviour,
                       then extends this to calculate the effects of that fire on its environment.",
                       style ="color : #03840c",align="middle"),
                tags$br(),
                tags$p("The model has advantages over many popular approaches to modelling fire
                       in Australia in that it provides a defensible, peer-reviewed platform for 
                       decision making that can account for the differences in fire behaviour 
                       across species and ecosystems (Fig. 1). FRaME gives detailed analysis of 
                       fire behaviour, not only predicting flame heights and rates of 
                       spread but also providing dynamic (1-second time-step) break-down of
                       all flame components, and identifying threshold changes such as the 
                       onset of crown fire. These are then used to calculate the impact of 
                       fire on soils, flora and fauna using standard heat transfer processes.",
                       style ="color : #03840c",align="middle"),
                tags$figure(
                  tags$img(src = "Fig1_int.png",height = 300,width = 1000),
                  tags$figcaption(tags$u(tags$em("Fig. 1|  FRaME models the differences in fire 
                                  behaviour that can be expected from variations in 
                                  plant species and leaf traits"))),align = "middle"),
                tags$br(),
                tags$p("The underlying fire behaviour model (Forest Flammability Model) 
                       was developed by Dr. Philip Zylstra", tags$sup("1") ,"through the University of NSW, 
                       with funding through the NSW Office of Environment and Heritage.
                       This was published and validated",tags$sup("2") ,"through Wollongong University, 
                       again funded by NSW OEH. Code for this stage of the process was 
                       written in C++ by Chris Thomas, then ported into Scala by Michael Bedward, 
                       who also wrote the core functions for the R package of FRaME.
                       Dr Zylstra developed the models in R for fire impact on soils, 
                       flora and fauna through Wollongong University, with funding from the
                       NSW Environmental Trust (project 2017/RD/0139 modelling fire risk to fauna) 
                       and the ACT National Parks Association. These models were built into the 
                       dashboard by Soroush Homayounpour, using the R package Shiny. 
                       The C++ code is available in GitHub",
                       tags$a(href ="https://github.com/pzylstra/ffm_cpp/tree/Zylstra2016","here"), 
                       "Scala code is", 
                       tags$a(href ="https://github.com/pzylstra/frame_scala","here"),
                       "and R code is", 
                       tags$a(href ="https://github.com/pzylstra/frame_r" ,"here. ")),
                tags$p("The FFM was validated against flame heights that occurred across 
                       a range of forests and fire conditions in the Brindabella Ranges in 2003. 
                       In the validation, we showed that surface fuels (fuel load) explained 11% of 
                       the variation in flame height, but could only predict small flames. 
                       Adding in details about the structure of plants meant that large flames could be predicted,
                       but not accurately. Adding leaf traits to this however meant that 80% of the variability in 
                       flame heights could be explained (Fig. 2)."),
        tags$figure(
          tags$img(src = "Fig2_int.png",height = 300, width = 1000),
          tags$figcaption(tags$u(tags$em("Fig. 2| Observed flame heights compared to predictions made 
                          using fuel load only (left panel), fuel load + structure 
                          (centre panel), and fuel load, structure and leaf traits 
                          (right panel)."))),align = "middle"),
        tags$h5(tags$b("References")),
        tags$ol(tags$li("Zylstra, P. J. Forest flammability: modelling and managing a complex system. 
                        (University of NSW, Australian Defence Force Academy, 2011). doi:10.13140/2.1.3722.0166"),
                tags$li("Zylstra, P. J. et al. Biophysical mechanistic modelling quantifies the effects of plant 
                        traits on fire severity: species, not surface fuel loads determine flame dimensions in 
                        eucalypt forests. PLoS One 11, e0160715 (2016).")
                )
                ),
              box(h2("Manual"),
              p("This part describes how every tab in this dashboard works.")
                )
               )
              ),
     tabItem( tabName = "binputs",
      tabsetPanel( id = "binputs_tabset",
        tabPanel( title = "Legacy Parameters ", value = "leg_par_tab",
        fluidRow(
         box(
          p("This file inputs' tab can be applied when the legacy parameters are 
            uploaded."),
          fileInput("legparms",label = tags$em("Legacy Parameters Files"),accept = "text/txt")
        ),
        box(
          DT::dataTableOutput("leg_parm_tbl")
          )
       )
      ),
    tabPanel( title = "Site number", value =  "site_nmbr_tab",
      fluidRow(
      box(
      fileInput("site_added",label = tags$em("Site"),accept = "text/.csv"),
      fileInput("struc_added",label = tags$em("Structure"),accept = "text/.csv"),
      fileInput("flora_added",label = tags$em("Flora"),accept = "text/.csv")
      ),
      box(
      fileInput("plnttrts_added",label = tags$em("Plant Traits"), accept = "text/.csv"),
      numericInput("rec_to_model","Number of record to be modelled",1),
      actionButton("prmbldr_site_addedd",label = tags$em("Build parameters"))
         ),
     box(
        bsAlert("prm_build_error"),
        textOutput("prm_build_error_txt"), 
        DT::dataTableOutput("prm_build_tbl")
        )
    )
    ),
    tabPanel(title = "Age the community",value = "age_comm_tab",
             fluidRow(
               box(
               numericInput("tr_ag_site","Tree age",50, min =0),
               numericInput("ag_site","Age",10, min =0),
               fileInput("grth_added",label = tags$em("Growth"),accept = "text/.csv"),
               fileInput("cover_added",label = tags$em("Cover"),accept = "text/.csv"),
               actionButton("grow_forst",label = tags$em("Grow forest"))
             ),
             box(
               textOutput("grow_forst_txt"),
               bsAlert("grow_forst_error"),
               DT::dataTableOutput("grow_forst_table")
               )
    )
             ),
    tabPanel( title= "Weather data set", value = "wthr_tab",
      box(
        fileInput("wthr_added",label = tags$em("Weather"),accept = "text/.csv"),
        textOutput("wthr_error_txt"),
        bsAlert("wthr_error")
      )
    )
    )
    ),
    tabItem(tabName = "det_beh_anls",
            tabsetPanel( id = "det_beh_tabst",
              tabPanel( title = "Inputs",value = "det_beh_inpt_tab",
              fluidRow(
                box(
              tags$h3(tags$b("Deterministic behaviour")),
              tags$p(tags$em("Deterministic behaviour is the most basic analysis, 
                             performing a single prediction that does not consider any variability 
                             in the site. Think of it as a single point on the ground where all 
                             plants and conditions are exactly as described in the parameter file. 
                             This analysis is not recommended for fire behaviour predictions , 
                             but should be used for diagnostic purposes. For diagnosis, this analysis 
                             will provide details of the fire dynamics, giving coordinates that describe 
                             what is burning within each plant on a second by second basis, 
                             and the flame that it is producing. To run the model, either leave the 
                             cells empty to model from the parameters in the site dataset, or else
                             enter new site parameters. 
                             Results can optionally be saved by selecting a
                             folder that they will be written to. More detail is available in 
                             the user manual."))
              
            ),
          box(
          numericInput("slp_det_beh",label = "Slope",10),
          numericInput("temp_det_beh",label = "Temperature",30),
          numericInput("dmfc_det_beh",label = "DMFC",0.03,min = 0, max = 1),
          numericInput("wdspd_det_beh",label = "Wind speed",20),
          #numericInput("jit_det_beh",label = "Iterations",20),
          actionButton("run_det_beh", "Run"),
          shinyDirButton("det_beh_fold","Select folder",title = "Please select a directory")
          ))),
          tabPanel( title = "Outputs",value = "det_beh_otpt_tab",
          fluidRow(
            bsAlert("det_beh_error"),
            textOutput("det_beh_error_txt"),
            box(
            DT::dataTableOutput("det_beh_tbl_1")
          ),
          box(
            DT::dataTableOutput("det_beh_tbl_2") 
          ), 
          box(
            DT::dataTableOutput("det_beh_tbl_3")
          ))))),
    tabItem( tabName = "prob_beh_anls",
             tabsetPanel( id = "prob_beh_tabs",
                     tabPanel( title = "Inputs" , value = "prob_beh_inp_tab",       
                           fluidRow( 
                               box(
                                 h3(tags$b("Probabilistic behaviour")),
                                 p(em("Detailed fire behaviour predictions. This will examine the 
                                  variability that can be expected in fire behaviour, reporting 
                                  the results in percentile bands. The extent of variability will 
                                  depend on the variability in the inputs. Results can optionally be 
                                   saved by selecting a folder that they will be written to. Running this 
                                   prediction will provide a suitable dataset for analysing risk to
                                   flora and fauna in fire Impacts.More detail is
                                   available in the user manual."))),
                               box(
                                  numericInput("jit_prob_beh",label = "Iterations",10),
                                  numericInput("lf_dmns_prob_beh",label = "Leaf dimension variability ",0.1),
                                  actionButton("run_prob_beh",
                                               label = "Run",
                                               icon = icon("calculator")),
                                  shinyDirButton("prob_beh_folder",
                                                 "Select the output folder",title = "Please select a directory")
                                      ),
                                  box( title = tags$em("Slope"),solidHeader = TRUE,
                                      numericInput("slpe_mean_prob_beh",label = "Mean",10),
                                      numericInput("slpe_stdev_prob_beh",label = "Standard deviation",2),
                                      numericInput("slpe_rng_prob_beh",label = "Range",5)
                                      ),
                                  box( title = tags$em("Temperature"),solidHeader = TRUE,
                                     numericInput("temp_mean_prob_beh",label = "Mean",30),
                                     numericInput("temp_stdev_prob_beh",label = "Standard deviation",5),
                                     numericInput("temp_rng_prob_beh",label = "Range",5)
                                      ),
                                  box( title = tags$em("DMFC"),solidHeader = TRUE,
                                     numericInput("dmfc_mean_prob_beh",label = "Mean",0.1),
                                     numericInput("dmfc_stdev_prob_beh",label = "Standard deviation",0.01),
                                     numericInput("dmfc_rng_prob_beh",label = "Range",2)
                                    ),
                                  box( title = tags$em("Wind speed"),solidHeader = TRUE,
                                     numericInput("wspd_mean_prob_beh",label = "Mean",10),
                                     numericInput("wspd_stdev_prob_beh",label = "Standard deviation",1),
                                    numericInput("wspd_rng_prob_beh",label = "Range",10)
                                     ),
                                  box( title = tags$em("Leaf moisture"),solidHeader = TRUE,
                                      numericInput("lfmstr_mtlpir_prob_beh",label = "Multiplier",1),
                                      numericInput("lfmstr_stdev_prob_beh",label = "Standard deviation",0.01)
                                    ),
                                  box( title = tags$em("Plant height"),solidHeader = TRUE,
                                      numericInput("plnth_rng_prob_beh",label = "Range",1.41),
                                     numericInput("plnth_stdev_prob_beh",label = "Standard deviation",2)
                                  ))),
                          tabPanel(title = "Outputs",value = "outs_prob_beh_tab",
                                   fluidRow(
                                   bsAlert("prob_beh_error"),
                                   textOutput("prob_beh_error_txt"),
                                     box( width = 12,
                                     DT::dataTableOutput("prob_beh_tbl")
                                   ))
                          ))),
    
    tabItem( tabName = "envr_drvr_anls",
             tabsetPanel( id = "envrn_drv_tabs",
              tabPanel( title = "Inputs" , value = "envrn_inp_tab",       
               fluidRow(
                 box(
                   h3(tags$b("External drivers")), 
                    p(em("Model fire behaviour across a range of 
                    external conditions to identify trends. 
                    The extent of variability will depend on the variability 
                    in the inputs. Results can optionally be saved by selecting a 
                    folder that they will be written to.More detail is available in the user manual."))
                 ),
               box(
               numericInput("jit_envr_anls",label = "Iterations",3),
               numericInput("lf_dmns_envr_anls",label = "Leaf dimension variability ",0.1),
               actionButton("run_envr_anls",label = "Run",icon = icon("calculator")),
               shinyDirButton("envr_select_folder","Select the output folder",
                              title = "Please select a directory")
                ),
             box( title = tags$em("Temperature"),solidHeader = TRUE,
                  numericInput("temp_min_envr_anls",label = "Minimum",1),
                  numericInput("temp_stpsz_envr_anls",label = "Step size",1),
                  numericInput("temp_nm_stps_envr_anls",label = "Number of steps",10)
             ),
             box( title = tags$em("DMFC"),solidHeader = TRUE,
                  numericInput("dmfc_min_envr_anls",label = "Minimum",1),
                  numericInput("dmfc_stpsz_envr_anls",label = "Step size",1),
                  numericInput("dmfc_nm_stps_envr_anls",label = "Number of steps",5)
             ),
             box( title = tags$em("Wind speed"),solidHeader = TRUE,
                  numericInput("wspd_min_envr_anls",label = "Minimum",0),
                  numericInput("wspd_stpsz_envr_anls",label = "Step size",10),
                  numericInput("wspd_nm_stps_envr_anls",label = "Number of steps",2)
             ),
             box( title = tags$em("Leaf moisture"),solidHeader = TRUE,
                  numericInput("lfmstr_mtlpir_envr_anls",label = "Multiplier",1),
                  numericInput("lfmstr_stdev_envr_anls",label = "Standard deviation",0.01)
             ),
             box( title = tags$em("Plant height"),solidHeader = TRUE,
                  numericInput("plnth_rng_envr_anls",label = "Range",1.41),
                  numericInput("plnth_stdev_envr_anls",label = "Standard deviation",0.2)
             ))),
             tabPanel(title = "Outputs",value = "outs_envrn_tab",
                      fluidRow(
                        bsAlert("envr_error"),
                        textOutput("envr_error_txt"),
                        box(
                          selectInput("envr_plt_slope",label = "Slope",
                                      choices = c("-10" = "-10","0" = "0",
                                                  "10"="10","20"="20","30"="30"))
                        ),
                        box( 
                        plotlyOutput("envrn_anls_plot_1"),
                        plotlyOutput("envrn_anls_plot_2")
                        ),
                        box(
                          plotlyOutput("envrn_anls_plot_3"),
                          plotlyOutput("envrn_anls_plot_4")
                        ),
                        box(
                            plotlyOutput("envrn_anls_plot_5"),
                            plotlyOutput("envrn_anls_plot_6")
                        )
                        )
                      ))
      
    ),
    tabItem( tabName = "stdf_site",
             tabsetPanel( id ="site_tabset",
      tabPanel( title ="Inputs", value = "inputs_stdf",
        fluidRow(
      box(
          h3(tags$b("Contextual flammability")),    
          p(em("Model fire behaviour across specific weather conditions and summarise the 
                results into risk descriptors. Running this prediction will provide a suitable 
                dataset for analysing risk to flora and fauna in fire Impacts. More detail is available 
                in the user manual."))),
      box(
        numericInput("jits","Iterations",10),
        numericInput("slp_site","Slope",0,min = 0,max = 90),
        numericInput("leafvar_site","Leaf dimension variability",0.1,min = 0),
        numericInput("ms_site","Leaf moisture standard deviation",0.001,min = 0),
        numericInput("pm_site","Leaf moisture multiplier",1,min = 0.001),
        numericInput("mr_site","Leaf moisture range",1.001)
        ),
        box(
          numericInput("hs_site","Plant height standard deviation",0.2, min = 0.001),
        numericInput("hr_site","Plant height range",1.41),
        actionButton("run_stds_anls",
                     label = "Run", icon = icon("calculator")),
        shinyDirButton("site_anls_folder","Select folder",title = "Please select a directory")
        )
      ) 
      ),
      tabPanel(title = "Outputs",value = "plots_stdf",
          bsAlert("site_anls_error"),
          textOutput("site_anls_error_txt"),
          fluidRow(
          plotlyOutput("site_stds_plot",height = 600),
          tags$br(),
          box(width = 12,
              DT::dataTableOutput("site_anls_tbl")
              )
          )
     )
     )
     ),
    tabItem(tabName = "soil_impct",
            tabsetPanel( id = "earth_tabset",
              tabPanel( title = "Inputs", value = "inputs_soil_tab",
                  fluidRow(
              box(
                h3(tags$b("Earth")),
                p(em("Models heat penetration into the soil, and consequent impacts on the soil environment. 
                By default, this models the effects from the most recent fire behaviour analysis, but 
                   if another fire is manually entered it will override the default."))
              ),
              box(
                fileInput("runs_soil_file",label = tags$em("Summay of behaviour csv")),
                fileInput("ip_soil_file",label = tags$em("IP csv"))
                ),
              box(
                numericInput("dist_soil","Fire spread distance",1),
                numericInput("trail_sec_soil","Trail seconds after fire",1800),
                numericInput("org_com_soil",label = "Organic composition of soil",0.7),
                numericInput("lit_dep_soil",label = "Depth of litter",30),
                numericInput("risk_per_soil",label = "Risk percentile",0.95)),
                box(
                numericInput("moist_soil",label = "Moisture of soil",0.2,min = 0,max = 1),
                numericInput("sea_level_perss_soil", 
                             label = "Sea level atmospheric pressure",1013.25),
                numericInput("rel_hum_soil",label = "Proportional relative humidity",0.4,min= 0,max=1),
                numericInput("alti_soil",label = "Altitude", 1000),
                selectInput("text_soil",label = "Soil texture",
                            choices = c("sand" = "sand","loamy sand"="loamy sand",
                                        "sandy loam"="sandy loam","sandy clay loam"= "sandy clay loam",
                                         "sand clay" = "sand clay","loam"="loam",
                                        "clay loam" = "clay loam","silt loam"="silt loam",
                                        "clay"="clay","silty clay"="silty clay",
                                        "silty clay loam"="silty clay loam",
                                        "silt"="silt")),
                actionButton("soil_anls","Run",icon = icon("calculator"))
              )
              )
              ),
              tabPanel( title = "Outputs", value = "outputs_soil_tab",
                fluidRow(bsAlert("soil_error"),
                textOutput("soil_error_txt"),
                box(
                plotlyOutput("soil_plt_1"),
                plotlyOutput("soil_plt_2"),
                plotlyOutput("soil_plt_3"),
                plotlyOutput("soil_plt_4")
                ),
              box(
                dataTableOutput("soil_table")
              ))
            )
            )
            ),
    tabItem(tabName = "veg_impct",
            fluidRow(
              box(
                 h3(tags$b("Flora")),
                p(em("Finds the amount of scorch and consumption of each plant stratum,
                    along with the heat penetration into the tree cambium and the consequent 
                    likelihood of ringbarking or girdling."))
              ),
              box(
                numericInput("leth_veg", label = "Scorching temperature",80),
                numericInput("dist_veg", label = "Fire spread distance",5),
                numericInput("trail_sec_veg", label = "Trail seconds after fire",300),
                numericInput("lit_dep_veg",label = "Depth of litter", 15),
                numericInput("risk_per_veg",label = "Risk percentile",0.95),
                numericInput("moist_brk_veg",label = "Moisture of the bark",0.2,min = 0 , max = 1),
                numericInput("moist_timb_veg",label = "Moisture of the timber",1, min = 0, max = 1),
                numericInput("sea_level_press_veg",
                             label = "Sea level atmospheric pressure",1013.25)
                ),
              box(      
                numericInput("rel_hum_veg",label = "Proportional relative humidity",0.4,min=0,max=1),
                numericInput("alti_veg",label = "Altitude", 1000),
                numericInput("wd_dens_veg",label = "Wood density",610),
                numericInput("brk_dens_veg",label =  "Bark density",500),
                numericInput("brk_thick_veg",label = "Bark thickness",0.05),
                numericInput("cambium_veg",label = "Thickness of the cambium",0.01),
                numericInput("xylem_thick_veg",label = "Thickness of the active xylem",0.01),
                actionButton("veg_anls","Run",icon = icon("calculator"))
                ),
              box(
                uiOutput("veg_txt"),
                bsAlert("veg_error")
              ),
              box(
                plotlyOutput("veg_plt_1"),
                plotlyOutput("veg_plt_2"),
                plotlyOutput("veg_plt_3"),
                plotlyOutput("veg_plt_4")
              ),
              box(
                DT::dataTableOutput("veg_table")
                  )
            )
            ),
    tabItem(tabName = "anim_impct",
            tabsetPanel( id = "anim_tab_panel",
                         tabPanel(title = "Hollow-sheltering",
                                  fluidRow(
                                    box(
                                      h3(tags$b("Fauna – Hollow-sheltering")),
                                      p(em("Finds fire impacts on wildlife sheltering in tree hollows,
                                       predicting the likelihood of mortality. By default, this models 
                                       the effects from the most recent fire behaviour analysis, 
                                       but if another fire is manually entered it will override the default."))
                                    ), 
                                    box(
                                      fileInput("runs_hollw_file",
                                                label = tags$em("Summay of behaviour csv")),
                                      fileInput("ip_hollw_file",label = tags$em("IP csv"))
                                    ),
                                    box(
                                      numericInput("height_hollw",label = "Height",5),
                                      numericInput("far_hollw",label = "Far",10),
                                      numericInput("cls_hollw",label = "Close",0),
                                      numericInput("risk_per_hollw",label = "Risk percentile",0.95),
                                      numericInput("wd_dens_hollw",label = "Wood density",610),
                                      numericInput("wd_thick_hollw",label = "Wood thickness",0.02),
                                      numericInput("brk_dens_hollw",label = "Bark density",500)
                                    ),
                                    box(
                                      numericInput("brk_thick_hollw",
                                                   label = "Bark thickness",0.01),
                                      numericInput("rel_hum_hollw",
                                                   label ="Proportional relative humidity",0.3,min=0,max=1),
                                      numericInput("dmfc_hollw",label = "DMFC",0.2),
                                      numericInput("sea_level_press_hollw",
                                                   label = "Sea level atmospheric pressure",1013.25),
                                      numericInput("alt_hollw",label = "Altitude",1000),
                                      numericInput("lngth_hollw","Length",0.5),
                                      numericInput("area_hollw","Area",0.2),
                                      actionButton("run_anls_hollw",
                                                   label = "Run",icon = icon("calculator"))
                                    ),
                                    box(
                                      uiOutput("hollw_text"),
                                      bsAlert("hollow_error")
                                    ),
                                      box(
                                      plotlyOutput("hollow_plot_1"),
                                      tags$br(),
                                      plotlyOutput("hollow_plot_2"),
                                      downloadButton("hollow_report","Generate report")
                                      )
                                    
                                  )
                                  ),
                         tabPanel(title = "Exposed",
                                  fluidRow(
                                    box(
                                 h3(tags$b("Fauna – Exposed")),
                                 p(em("Finds fire impacts on exposed fauna, predicting the likelihood of 
                                 burn injuries or death. By default, this models the effects from the most 
                                      recent fire behaviour analysis, but if another fire is manually 
                                      entered it will override the default."))
                                    ),
                                    box(
                                      fileInput("runs_arbreal_file",
                                                label = tags$em("Summay of behaviour csv")),
                                      fileInput("ip_arbreal_file",label = tags$em("IP csv"))
                                    ),
                                    box(
                                      numericInput("height_arbreal",label = "Height",8),
                                      numericInput("far_arbreal",label = "Far",50),
                                      numericInput("cls_arbreal",label = "Close",0),
                                      numericInput("risk_per_arbreal",
                                                   label = "Risk percentile",0.75),
                                      numericInput("rel_hum_arbreal",
                                                   label = "Proportional relative humidity",0.2,min = 0,max = 1),
                                      numericInput("sea_level_press_arbreal",
                                                   label = "Sea level atmospheric pressure",1013.25)
                                    ),
                                    box(
                                      numericInput("alt_arbreal","Altitude",1000),
                                      selectInput("anim_clas_arbreal",
                                                  "Class",choices = c("Mammalia"="mam")),
                                      numericInput("lngth_arbreal",label = "Body Length (m)",0.5),
                                      numericInput("area_arbreal","Surafce area (m2)",1),
                                      numericInput("fur_arbreal","Fur Thickness (m)",0.02),
                                      actionButton("run_anls_arbreal",
                                                   "Run",icon = icon("calculator"))
                                    ),
                                    box(
                                      uiOutput("arbreal_text"),
                                      bsAlert("arbreal_error")
                                    ),
                                    box(
                                    plotlyOutput("arbreal_plt_1"),
                                    tags$br(),
                                    plotlyOutput("arbreal_plt_2")
                                    )
                                   )
                                  ),
                         tabPanel(title = "Subterranean",
                                  fluidRow(
                                    box(
                                      h3(tags$b("Fauna – Subterranean")),
                                       p(em("Finds fire impacts on underground fauna, predicting the 
                                      likelihood of mortality. By default, this models the effects
                                         from the most recent fire behaviour analysis, but if
                                         another fire is manually entered it will override the default."))
                                    ),
                                    box(
                                      fileInput("runs_undrgrnd_file",
                                                label = tags$em("Summay of behaviour csv")),
                                      fileInput("ip_undrgrnd_file",label = tags$em("IP csv")) 
                                    ),
                                    box(
                                      numericInput("depth_undrgrnd",label = "Depth",0.1),
                                      numericInput("dist_undrgrnd",label = "Distance",10),
                                      selectInput("texture_undrgrnd",
                                                  label = "Soil Texture",
                                                  choices =  c("sand" = "sand","loamy sand"="loamy sand",
                                                               "sandy loam"="sandy loam","sandy clay loam"= "sandy clay loam",
                                                               "sand clay" = "sand clay","loam"="loam",
                                                               "clay loam" = "clay loam","silt loam"="silt loam",
                                                               "clay"="clay","silty clay"="silty clay",
                                                               "silty clay loam"="silty clay loam",
                                                               "silt"="silt")),
                                      numericInput("strt_temp_undrgrnd",
                                                   label = "Starting soil temp (oC)",
                                                   25),
                                      numericInput("orgnic_temp_undrgrnd",
                                                   label = "Organic content (0-1)",0.1),
                                      numericInput("diamtr_undrgrnd",label = "Diameter",12)
                                      ),
                                    box(
                                      numericInput("risk_per_undrgrnd",
                                                   label = "Risk percentile",0.95),
                                      numericInput("moist_undrgrnd",label = "Starting soil moisture (%ODW)",
                                                   0.02,min = 0, max = 1),
                                      numericInput("sea_level_press_undrgrnd",
                                                   label = "Sea level atmospheric pressure",
                                                   1013.25),
                                      numericInput("rel_hum_undrgrnd",
                                                   label = "Proportional relative humidity",0.4,min = 0, max =1),
                                      numericInput("alt_undrgrnd",label = "Altitude",1500),
                                      actionButton("run_anls_undrgrnd",
                                                   label = "Run",icon = icon("calculator"))
                                    ),
                                    box(
                                      plotlyOutput("undrgrnd_plt_1"),
                                      tags$br(),
                                      plotlyOutput("undrgrnd_plt_2")
                                    ),
                                    box(
                                      uiOutput("undrgrnd_text"),
                                      bsAlert("undrgrnd_error")
                                    )
                                  )
                                  
                                  )
            )
            )
    
    )
    
    )
   )
##############  
#######server side
##############


server <- function(input, output,session) {
  



#########  
##site numbers  
#########
  
# site df stored
in_site <- reactive({
        input$site_added
   })
  
site_df <- reactive({
if(!is.null(in_site())){
  temp_upl <- read.csv(in_site()$datapath)
  if(site_check(temp_upl) == "np"){
    return(temp_upl)
  }else{
    return(NULL)
  }
  }else{
    return(NULL)
}
  })

## structure df stored
in_struc <- reactive({
      input$struc_added
    })

struc_df <- reactive({
    if(!is.null(in_struc())){
      temp_upl <- read.csv(in_struc()$datapath)
      if(stctr_check(temp_upl) == "np"){
        return(temp_upl)
      }else{
        return(NULL)
      }
      }else{
    return(NULL)
}
})

## flora df stored
in_flora <- reactive({
      input$flora_added
})

flora_df <- reactive({
    if(!is.null(in_flora())){
      temp_upl <- read.csv(in_flora()$datapath)
      if(flora_check(temp_upl)=="np"){
        return(temp_upl)
      }else{
        return(NULL)
      }
      }else{
   return(NULL)
}
})  

## growth df stored
in_grth <- reactive({
        input$grth_added
})
  
grth_df <- reactive({
    if(!is.null(in_grth())){
      temp_upl <- read.csv(in_grth()$datapath)
      if(growth_check(temp_upl)=="np"){
        return(temp_upl)
      }else{
        return(NULL)
      }
}else{
  return(NULL)
}
})  

## Cover df stored
in_cover <- reactive({
   input$cover_added
})

cover_df <- reactive({
    if(!is.null(in_cover())){
       temp_upl <- read.csv(in_cover()$datapath)
       if(cover_check(temp_upl)=="np"){
         return(temp_upl)
       }else{
         return(NULL)
       }
}else{
   return(NULL)
}
})
## Weather dataframe
in_wthr <- reactive({ 
          input$wthr_added
          })
wthr_df <- reactive({
    if(!is.null(in_wthr())){
    temp_upl <- read.csv(in_wthr()$datapath)
    if(wthr_check(temp_upl) == "np"){
      return(temp_upl)
    }else{
      return(NULL)
    }
}else{
 return(NULL)
}
})

##Default parameters species 
in_species <- reactive({
    input$plnttrts_added
  })
## add the data check for spcies in the fire dyn and site anls
def_param_species_df <-  reactive({
  if(!is.null(in_species())){
    temp_upl <- read.csv(in_species()$datapath)
    if(traits_check(temp_upl)=="np"){
      return(temp_upl)
    }else{
      return(NULL)
    }
}else{
    return(NULL)
}
})
  

## base_params status holder. To see whether any basic parameters file 
##(leg file or built pars ds) are uploded for  is uploaded or not?
## it is equal to one if a leg file is uploaded
## it is equal to 2 if a parambuilder is applied
## it is equal to 3 if growth is applied
base_params_stat <- reactiveValues(status = 0)


#########  
## Legacy file
#########  

## legac files 
in_legfile <- reactive({
  input$legparms
})

## base_params is the value holder for built parameters
base_params <- reactiveValues(values =  NULL)

##writing the legacy file to the base_params Value
observe({
   req(in_legfile())
    base_params$values <- ffm_read_legacy_params(in_legfile()$datapath)
    base_params_stat$status <- 1
})

## printing output of the leg file parameters
output$leg_parm_tbl <- DT::renderDataTable({
  DT::datatable(base_params$values)
})

## Build inital parameter table  

observeEvent(input$prmbldr_site_addedd,{
  req(site_df(),struc_df(),flora_df(),
            def_param_species_df(),
            input$rec_to_model)
  if(input$rec_to_model<=max(site_df()$record)){
    base_params_init <- paramBuilder(
      site_df(),
      struc_df(),
      flora_df(),
      def_param_species_df(),
      input$rec_to_model
    )
    base_params_init$value[base_params_init$param == "leafForm"] <- "flat"
    base_params$values <- base_params_init
    base_params_stat$status <- 2
  }
  })  
## param build output
## Throw error if inputs are not uploaded  
prm_bld_error_output<- eventReactive(input$prmbldr_site_addedd,{
  # Update param file
  ## Vector of input data sources
  ## Too check if all data inputs are NULL then ask upload either leg file of data sets
  ## Too check there is no missing data sets
  if(is.null(site_df())){
    if(is.null(in_site())){
    createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                content = "You have not uploaded site data set.", append = TRUE)
    }else{
      createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                  content = "uploaded site does not follow template.", append = TRUE)
    }
  }else if(is.null(struc_df())){
    if(is.null(in_struc())){
    createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                content = "You have not uploaded structure data set.", append = TRUE)
    }else{
      createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                  content = "uploaded structure does not follow template.", append = TRUE)
    }
  }else if(is.null(flora_df())){
    if(is.null(in_flora())){
    createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                content = "You have not uploaded flora data set.", append = TRUE)
    }else{
      createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                  content = "uploaded flora does not follow template.", append = TRUE)
    }
  }else if(is.null(def_param_species_df())) {
    if(is.null(in_species())){
    createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                content = "You have not uploaded Traits data set", append = TRUE)
    }else{
      createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                  content = "uploaded species does not follow template.", append = TRUE)
    }
  }else if(input$rec_to_model>max(site_df()$record)){
    createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                content = "Number of record is not in the range.", append = TRUE)
  }else{
      closeAlert(session,"prmbld_alert")
  }
})

## error holder for the param builder output 
output$prm_build_error_txt <- renderText({
  ## error output of param builders 
  prm_bld_error_output()
})


# param build table output
output$prm_build_tbl <-  DT::renderDataTable({
      DT::datatable(base_params$values,options =  list(searching = FALSE,pageLength = 5))
 })

##grow forest status
## check whether growth cover is run
grow_forst_stat <-  reactiveValues(status = 0)

###Grow forest error
grow_forst_error_output <-  eventReactive(input$grow_forst,{
  if(base_params_stat$status == 0){
    createAlert(session, "grow_forst_error","grow_forst_alert",title = "Oops",
                content = "You should either uploade a 
                legacy file parameter or build a parameter table",append = TRUE)
  }else if(is.null(cover_df())){
    if(is.null(in_cover())){
    createAlert(session, "grow_forst_error","grow_forst_alert",title = "Oops",
                content = "You have not uploaded the cover data set.",append = TRUE)
    }else{
      createAlert(session, "grow_forst_error","grow_forst_alert",title = "Oops",
                  content = "Uploaded cover does not follow templates.",append = TRUE)
    }
  }else if(is.null(grth_df())){
    if(is.null(in_grth())){
    createAlert(session, "grow_forst_error","grow_forst_alert",title = "Oops",
                content = "You have not uploaded the growth data set.",append = TRUE)
    }else{
      createAlert(session, "grow_forst_error","grow_forst_alert",title = "Oops",
                  content = "Uploaded growth does not follow templates.",append = TRUE)
    }
  }else if( base_params_stat$status == 0){
    createAlert(session, "grow_forst_error","grow_forst_alert",title = "Oops",
                content = "You need to build parameters or upload a legacy file",append = TRUE)
  }else{
    closeAlert(session,"grow_forst_alert")
  }
})
   
## aging the community

## modifying base parameters
observeEvent(input$grow_forst,{
  req(base_params$values, grth_df(), cover_df(),flora_df())
   # AGE THE STAND
    nTable <- subset(base_params$values, param=="name")
     nSp <- as.numeric(count(nTable))
     strat <- filter(base_params$values, param == "levelName")
    nSt <- as.numeric(count(strat))
    nCanopy <- subset(nTable, stratum==nSt)
    nCsp <- as.numeric(count(nCanopy))
    nLow <- nSp-nCsp
    suspNS = ""

    # Weight of the O-horizon
    base_params$values <- olsen(base_params$values, grth_df(), input$ag_site)


    for (stNum in 1:nSt) {
       st <- strat$value[stNum]
       sep <- coverChange(st, input$rec_to_model, cover_df(), flora_df(), input$ag_site)
       base_params$values <- ffm_set_stratum_param(base_params$values, stNum, "plantSeparation", sep)
       spList <- filter(nTable, stratum == stNum)
       n_a <- as.integer(spList$species[1])
       n_b <- as.integer(max(spList$species))
       nSusp <- as.integer((subset(base_params$values, value ==suspNS))$species[1])
  
        if (stNum < nSt) {
           for (spName in n_a:n_b)
              if(spList$value != suspNS){
                 current <- growPlants(base_params$values, 
                              input$rec_to_model, sp = nTable$value[spName], 
                              stn = stNum, grth_df(), input$ag_site)
                      base_params$values <- applyGrowth(base_params$values, nTable$value[spName], current)
                     }
               } else {
                   for (spName in n_a:n_b)
                          current <- growPlants(base_params$values, 
                            input$rec_to_model, nTable$value[spName], 
                            stn = stNum, grth_df(), input$tr_ag_site)
                       base_params$values <- applyGrowth(base_params$values, nTable$value[spName], current)}
            }
            base_params_stat$status <-  3
})


output$grow_forst_txt <- renderText({
  grow_forst_error_output()
})
output$grow_forst_table <-  DT::renderDataTable({
  DT::datatable(base_params$values,options =  list(searching = FALSE,pageLength = 5))
})
### Printing out the grow function
### Checking weather data validity

wthr_error_output <-  reactive({
  if(is.null(wthr_df()) & !is.null(in_wthr())){
    createAlert(session,"wthr_error","wthr_alert", 
                title = "Oops",content = "Uploaded weather does not follow templates.",append = TRUE)
  }else{
    closeAlert(session,"wthr_alert")
  }
})

output$wthr_error_txt <- renderText({
  wthr_error_output()
})

#######
##Jumping across panels
########

##Jumping across panel in waether set tab

##jumping in site analysis tab  
observeEvent(input$run_stds_anls, {
  updateTabsetPanel(session, "site_tabset",
                    selected = "plots_stdf")
})

## Jumping in det beh panel
observeEvent(input$run_det_beh, {
  updateTabsetPanel(session, "det_beh_tabst",
                    selected = "det_beh_otpt_tab")
})

## Jumping in prob beh analysis
observeEvent(input$run_prob_beh,{
  updateTabsetPanel(session, "prob_beh_tabs",
                    selected = "outs_prob_beh_tab")
})

## Jumping in envr drivers analysis
observeEvent(input$run_envr_anls,{
  updateTabsetPanel(session, "envrn_drv_tabs",
                    selected = "outs_envrn_tab")
})

## Jumping in soil analysis tab
observeEvent(input$soil_anls,{
  updateTabsetPanel(session, "earth_tabset",
                    selected = "outputs_soil_tab")
})


############ 
## Deterministic Behaviour
############  
  
##detrm behvr dirctry
det_beh_path_status <-  reactiveValues(dir = 0)
##Roots  of user
roots <- c(HOME = fs::path_home())

shinyDirChoose(input, 'det_beh_fold', roots= roots)

## Uploading Det beh path   
det_beh_path <- reactive({
  pth <- parseDirPath(roots, input$det_beh_fold)
  pth
  })

## setting that det beh output is chosen  
observeEvent(input$det_beh_fold,{
  det_beh_path_status$dir <- 1
})
  
observe({
  print(det_beh_path())
  print(is.character(det_beh_path()))
})  
    
##determiminstic behaviour analysis
detbeh_analyis <- eventReactive(input$run_det_beh,{
  req(isolate(base_params$values))
  x <- isolate(base_params$values)
    base_params_det <- x %>%
      ffm_set_site_param("slope", input$slp_det_beh, "deg") %>%
      ffm_set_site_param("temperature", input$temp_det_beh) %>%
      ffm_set_site_param("deadFuelMoistureProp",input$dmfc_det_beh ) %>%
      ffm_set_site_param("windSpeed",input$wdspd_det_beh)
    
    #Run the model
    ffm_run(base_params_det, "Behav.db")
    res<-ffm_db_load("Behav.db")
    res
    surf <- surf(res$SurfaceResults)
    x <- stratum(res$FlameSummaries, 
                 res$Sites, res$ROS, surf)
    IP <- res$IgnitionPaths
    fnl <- list(surf = surf,x = x , IP = IP)
    fnl
    
  if(det_beh_path_status$dir == 1){
      write.csv(x,paste0(det_beh_path(),"/x_detbeh.csv"))
      write.csv(runs,paste0(det_beh_path(),"/runs_detbeh.csv"))
      write.csv(IP,paste0(det_beh_path(),"/IP_detbeh.csv"))
  }
    return(fnl)
    
  })



## Throw errors if folders are not selected or base_params is null   
det_beh_error_output <- eventReactive(input$run_det_beh,{
if(base_params_stat$status == 0 ){
  createAlert(session, "det_beh_error", "detbeh_alert", title = "Oops",
              content = "You have not built the parameters", append = TRUE)
}else{
  closeAlert(session,"detbeh_alert")
}
})
## errors in the process of running the model
#  
output$det_beh_error_txt <- renderText({ 
              
  det_beh_error_output()

  })
## Output of deterministic behaviour analysis
## tbl 1
output$det_beh_tbl_1 <- DT::renderDataTable({
  req(detbeh_analyis())
       out_tbl <- detbeh_analyis()$x%>%
        select(level, flameHeight, flameLength, flameA_degrees, ros_kph)
       DT::datatable(out_tbl, options = list(scrollX = TRUE))
  })  
  
##tbl 2
output$det_beh_tbl_2 <- DT::renderDataTable({  
     req(detbeh_analyis())
     DT::datatable(detbeh_analyis()$surf, options = list(scrollX = TRUE))
  })
  
##tbl3 
output$det_beh_tbl_3 <- DT::renderDataTable({ 
  req(detbeh_analyis())
  paths <- detbeh_analyis()$IP%>%
    select(runIndex, level, pathType, species, segIndex, x0, y0, x1, y1, length, flameLength)
  DT::datatable(paths, options = list(scrollX = TRUE,scroller = TRUE,scrollY = 300) )
  
  })


##########
#### Probalistic behaviour
##########

shinyDirChoose(input,"prob_beh_folder",roots = roots)
## prob beh path is selected or not
prob_beh_path_stat <- reactiveValues(dir = 0)
## prob beh path 
prob_beh_path <- reactive({
  pth <- parseDirPath(roots, input$prob_beh_folder)
  pth
})
##setting prob beh path status to 1 if any folder is selected
observeEvent(input$prob_beh_folder,{
  prob_beh_path_stat$dir <- 1
})
## prob_beh output error
probbeh_error_output <- eventReactive(input$run_prob_beh,{
  if(base_params_stat$status == 0){
    createAlert(session,"prob_beh_error","prob_beh_alert",title = "Oops",
                content = "You have not uploaded any parameters file.",append = TRUE)
  }else{
    closeAlert(session ,"prob_beh_alert")
  }
})

## Printing the output of error
output$prob_beh_error_txt <- renderText({
  probbeh_error_output()
})
## Running the analysis of prob beh

probbeh_anls <-  eventReactive(input$run_prob_beh,{
  req(isolate(base_params$values))
  init <- isolate(base_params$values)
  ## setting the progress bar 
  progress <- shiny::Progress$new()
  progress$set(message = "Running the probalistic behaviour \n", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  # setting progress step
  prg_steps <- input$jit_prob_beh
  updateProgress <- function(value = prg_steps, detail = NULL) {
    progress$inc(amount = 1/value, detail = detail)
  }
  prob_in <- probIn(init, out.db = "out_mc.db", jitters = input$jit_prob_beh,
                    slope = input$slpe_mean_prob_beh, 
                    slopeSD = input$slpe_stdev_prob_beh, 
                    slopeRange = input$slpe_rng_prob_beh, 
                    temp = input$temp_mean_prob_beh, 
                    tempSD = input$temp_stdev_prob_beh, tempRange = input$temp_rng_prob_beh,
                    DFMC = input$dmfc_mean_prob_beh, 
                    DFMCSD = input$dmfc_stdev_prob_beh, 
                    DFMCRange = input$dmfc_rng_prob_beh, 
                    wind = input$wspd_mean_prob_beh, 
                    windSD = input$wspd_stdev_prob_beh, 
                    windRange = input$wspd_rng_prob_beh,
                    moistureMultiplier = input$lfmstr_mtlpir_prob_beh, 
                    moistureSD = input$lfmstr_stdev_prob_beh, moistureRange = 1.001,
                    heightSD = input$plnth_stdev_prob_beh, 
                    heightRange = input$plnth_rng_prob_beh, 
                    leafVar = input$lf_dmns_prob_beh,
                    updateProgress = updateProgress)
  
  res<-ffm_db_load("out_mc.db")
  surf <- surf(res$SurfaceResults)
  x <- stratum(res$FlameSummaries, res$Sites, res$ROS, surf)
  runs <- summary(x, surf)%>%
    mutate(time = ceiling(repId/input$jit_prob_beh))
  IP <- repFlame(res$IgnitionPaths)
  if(prob_beh_path_stat$dir == 1){
    write.csv(surf,paste0(prob_beh_path(),"/surf_prob_beh.csv"))
    write.csv(x,paste0(prob_beh_path(),"/x_prob_beh.csv" ))
    write.csv(runs,paste0(prob_beh_path(),"/runs_prob_beh.csv"))
    write.csv(IP,paste0(prob_beh_path(),"/IP_prob_beh.csv"))
  }
  fnl =  list(surf = surf,x = x, runs = runs, IP = IP)
  return(fnl)
  
})

### print the table output
output$prob_beh_tbl <- DT::renderDataTable({
  req(probbeh_anls())
  
  # Find likelihood of fire spread 
  S1 <- probbeh_anls()$x%>%
    mutate(time = ceiling(repId/input$jit_prob_beh),
           spread = extinct * ifelse(level == "Surface",
                                     ifelse(oHorizon >=4,spread,0),
                                     spread))%>%
    group_by(repId)%>%
    select(repId, time, spread)%>%
    summarise_all(max)#%>%
  
  # Find spread b: likelihood of lethal temp
  runsB <- probbeh_anls()$runs%>%
    right_join(S1)%>%
    mutate(lengthSurface = lengthSurface * spread,
           heightSurface = heightSurface * spread,
           angleSurface = angleSurface * spread,
           fh = pmax(heightSurface, heightPlant),
           fl = pmax(lengthSurface, lengthPlant))
  
  # Find severity impacts using flora function
  init <- isolate(base_params$values) 
  
  severity<-flora(runsB, probbeh_anls()$IP, init, Test = 70)%>%
    mutate(sev = if_else(b4 > 50, 5,
                         if_else(sc4 > 50, 4,
                                 if_else(sc4 > 10, 3,
                                         if_else((sc2 + sc3) > 50, 2,
                                                 1)))),
           time = ceiling(repId/input$jit_prob_beh))
  
  # Summary stats
  hq <- round(quantile(probbeh_anls()$runs$fh, 
                       probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 1)
  lq <- round(quantile(probbeh_anls()$runs$fl,
                       probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 1)
  rq <- round(quantile(probbeh_anls()$runs$ros_kph, 
                       probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 2)
  Shsc <- round(quantile(severity$sc2, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
  Shk <- round(quantile(severity$b2, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
  Msc <- round(quantile(severity$sc3, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
  Mk <- round(quantile(severity$b3, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
  Csc <- round(quantile(severity$sc4, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
  Ck <- round(quantile(severity$b4, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
  probs <- c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)
  
  #Percentiles
  sev_percent <- as.data.frame(list('Probability' = probs, 'Height' = hq, 'Length' = lq, 'ROS'=rq, 'ShrubScorch'=Shsc, 'ShrubCons'=Shk, 
                                    'MidScorch'=Msc, 'MidCons'=Mk, 'CanScorch'=Csc, 'CanCons'=Ck))
  DT::datatable(sev_percent,options = list(scrollX = TRUE))
})


###########
###Environmetal Drivers
###########

shinyDirChoose(input,"envr_select_folder",roots = roots)
## envr path is selected or not
envr_path_stat <- reactiveValues(dir = 0)
## prob beh path 
envr_path <- reactive({
  pth <- parseDirPath(roots, input$envr_select_folder)
  pth
})
##setting envr path status to 1 if any folder is selected
observeEvent(input$envr_select_folder,{
  envr_path_stat$dir <- 1
})
## envr output error
envr_error_output <- eventReactive(input$run_envr_anls,{
  if( base_params_stat$status == 0 ){
  createAlert(session,"envr_error","envr_alert",title = "Oops",
              content = "You have not uploaded any parameters file.",append = TRUE)
}else{
  closeAlert(session ,"envr_alert")
}
})

## Printing the output of error
output$envr_error_txt <- renderText({
  envr_error_output()
})
## running the environment analysis
env_drivers <- eventReactive(input$run_envr_anls,{
  req(isolate(base_params$values))
  init <- isolate(base_params$values)
  
  progress <- shiny::Progress$new()
  progress$set(message = "Running the environmental drivers analysis \n", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  
  # Create a callback function to update progress.
  # Each time this is called:
  # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
  #   distance. If non-NULL, it will set the progress to that value.
  # - It also accepts optional detail text.
  # The progress step is based on the 'drivers' function
  prg_steps <- 3*5*(input$wspd_nm_stps_envr_anls+1)
  updateProgress <- function(value = prg_steps, detail = NULL) {
    progress$inc(amount = 1/value, detail = detail)
  }
  abs_in <- drivers(init, out.db = "out_mc.db", jitters = input$jit_envr_anls,
                    windMin = input$wspd_min_envr_anls, 
                    windReps = input$wspd_nm_stps_envr_anls, windStep = input$wspd_stpsz_envr_anls,
                    moistureMultiplier = input$lfmstr_mtlpir_envr_anls, 
                    moistureSD = input$lfmstr_stdev_envr_anls, 
                    moistureRange = 1.001,
                    heightSD = input$plnth_stdev_envr_anls, 
                    heightRange = input$plnth_rng_envr_anls, 
                    leafVar = input$lf_dmns_envr_anls,updateProgress = updateProgress)
  
  res<-ffm_db_load("out_mc.db")
  
  surf <- surf(res$SurfaceResults)
  x <- stratum(res$FlameSummaries, res$Sites, res$ROS, surf)
  runs <- summary(x, surf)%>%
    mutate(time = ceiling(repId/input$jit_envr_anls))
  IP <- repFlame(res$IgnitionPaths)
  
  if( envr_path_stat$dir == 1){
    write.csv(surf,paste0(envr_path(),"surf_envr_drv"))
    write.csv(x,paste0(envr_path(),"x_envr_drv"))
    write.csv(runs,paste0(envr_path(),"runs_envr_drv"))
    write.csv(IP,paste0(envr_path(),"IP_envr_drv"))
  }
  
  fnl = list(surf = surf,x = x, runs = runs, IP = IP)
  return(fnl)
  
})


# data to plot in envr drivers panel
envr_plt_data <- reactive({
  req(env_drivers())
  env_drivers()$runs %>% 
    filter(slope_degrees == input$envr_plt_slope)
})

## envr panel output
output$envrn_anls_plot_1 <-  renderPlotly({
  req(envr_plt_data())
   envr_plt_data() %>% 
  plot_ly() %>% 
    add_boxplot(x = ~wind_kph,y = ~fh,name = "Wind speed-Flame height",
                legendgroup = "Flame height") %>%           
    layout(
      xaxis = list(title = list(text = "Wind speed",
                                font = list(family ="Arial",size = 10)),
                   showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = list(text = "Flame Height",
                                font = list(family ="Arial",size = 10)),
                   tickfont=list(family = "Arial",size=10),zeroline =FALSE,
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
      showlegend =FALSE)
})

output$envrn_anls_plot_2 <- renderPlotly({
  req(envr_plt_data())
  envr_plt_data() %>% 
    plot_ly() %>% 
    add_boxplot(x = ~deadFuelMoistureProp,y = ~fh,
                name = "DFMC-Flame height",
                legendgroup = "Flame height") %>%           
    layout(
      xaxis = list(title = list(text = "DFMC",
                                font = list(family ="Arial",size = 10)),showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = list(text = "Flame Height",
                                font = list(family ="Arial",size = 10)),
                   tickfont=list(family = "Arial",size=10),zeroline =FALSE,
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
      showlegend =FALSE)
  
})


output$envrn_anls_plot_3 <- renderPlotly({
  req(envr_plt_data())
  envr_plt_data() %>% 
    plot_ly() %>% 
    add_boxplot(x = ~wind_kph,y = ~fl,
                name = "Wind speed-Flame length",
                legendgroup = "Flame length") %>%           
    layout(
      xaxis = list(title = list(text = "Wind speed",
                                font = list(family ="Arial",size = 10)),showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = list(text = "Flame length",
                                font = list(family ="Arial",size = 10)),zeroline =FALSE,
                   tickfont=list(family = "Arial",size=10),
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
      showlegend =FALSE)
  
})



output$envrn_anls_plot_4 <- renderPlotly({
  req(envr_plt_data())
  envr_plt_data() %>% 
    plot_ly() %>% 
    add_boxplot(x = ~deadFuelMoistureProp,y = ~fl,
                name = "DFMC-Flame length",
                legendgroup = "Flame length") %>%           
    layout(
      xaxis = list(title = list(text = "DFMC",
                                font = list(family ="Arial",size = 10)),showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = list(text = "Flame length",
                                font = list(family ="Arial",size = 10)),zeroline =FALSE,
                   tickfont=list(family = "Arial",size=10),
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
      showlegend =FALSE)
})



output$envrn_anls_plot_5 <- renderPlotly({
  req(envr_plt_data())
  envr_plt_data() %>%     
    plot_ly() %>% 
    add_boxplot(x = ~wind_kph,y = ~ros_kph,
                name = "Wind speed-Rate of spread" , 
                legendgroup =  "Rate of spread" ) %>%           
    layout(
      xaxis = list(title = list(text = "Wind speed",
                                font = list(family ="Arial",size = 10)),showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = list(text = "Rate of spread",
                                font = list(family ="Arial",size = 10)),zeroline =FALSE,
                   tickfont=list(family = "Arial",size=10),
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
      showlegend =FALSE)
  
})

output$envrn_anls_plot_6 <- renderPlotly({
  req(envr_plt_data())
  envr_plt_data() %>%     
    plot_ly() %>% 
    add_boxplot(x = ~deadFuelMoistureProp,y = ~ros_kph,
                name = "DFMC-Rate of spread", 
                legendgroup = "Rate of spread")  %>%           
    layout(
      xaxis = list(title = list(text = "DFMC",
                                font = list(family ="Arial",size = 10)),showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = list(text = "Rate of spread",
                                font = list(family ="Arial",size = 10)),zeroline =FALSE,
                   tickfont=list(family = "Arial",size=10),
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
      showlegend =FALSE)
  
  
})

#########    
####Weather series   
############    

###site analysis
## path is selected for the output? 0 means no.
site_anls_path_status <-  reactiveValues(dir = 0)

shinyDirChoose(input, 'site_anls_folder', roots= roots)

## Uploading the site path directory
site_anls_path <- reactive({
  pth <- parseDirPath(roots, input$site_anls_folder)
  pth
})

## setting that site analysis path is chosen
observeEvent(input$site_anls_folder,{
  site_anls_path_status$dir <- 1
})
## site analysis is run
site_anls_run_stat <- reactiveValues(status = 0)

site_anls_error_output <- eventReactive(input$run_stds_anls,{
  
  if(base_params_stat$status == 0){
    createAlert(session,"site_anls_error","site_anls_alert",title = "Oops",
                content = "You have not uploaded any parameter file.",append = TRUE)
  }else if(is.null(wthr_df())) {
    createAlert(session,"site_anls_error","site_anls_alert",title = "Oops",
                content = "You have not uploaded the weather data sets.",append = TRUE)
  }else{
    closeAlert(session,"site_anls_alert")
  }
})




site_analysis <- eventReactive(input$run_stds_anls,{
      
          req( base_params$values,wthr_df())  
           # setting base values to the init
           init <-  isolate(base_params$values)
           # Set slope
           base_params_site <-
             init %>%
             ffm_set_site_param("slope", input$slp_site, "deg")
    
           progress <- shiny::Progress$new()
           progress$set(message = "Running the site analysis \n", value = 0)
           # Close the progress when this reactive exits (even if there's an error)
           on.exit(progress$close())
           
           # Create a callback function to update progress.
           # Each time this is called:
           # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
           #   distance. If non-NULL, it will set the progress to that value.
           # - It also accepts optional detail text.
           prg_steps <- max(wthr_df()$tm)
           updateProgress <- function(value = prg_steps, detail = NULL) {
             progress$inc(amount = 1/value, detail = detail)
           }

           weatherSet(base_params_site, wthr_df(), out.db = "out_mon.db", jitters = input$jits, 
                      l = input$leafvar_site,
                      Ms = input$ms_site, Pm = input$pm_site, 
                      Mr = input$mr_site, Hs = input$hs_site, Hr = input$hr_site,
                      updateProgress = updateProgress)
           res<-ffm_db_load("out_mon.db")
           
           #Build tables
           surf <- surf(res$SurfaceResults)
           x <- stratum(res$FlameSummaries, res$Sites, res$ROS, surf)
           runs <- summary(x, surf)%>%
             mutate(time = ceiling(repId/input$jits))
           IP <- repFlame(res$IgnitionPaths)
           
           # Export to csv
             if(site_anls_path_status$dir == 1){
           write.csv(runs, paste0(site_anls_path(),"/summary_contextual.csv"))
           write.csv(IP, paste0(site_anls_path(),"/IP_contextual.csv"))
              } 
           # Find likelihood of fire spread 
           S1 <- x %>%
             mutate(time = ceiling(repId/input$jits),
                    spread = extinct * ifelse(level == "Surface",
                                              ifelse(oHorizon >=4,spread,0),
                                              spread)) %>%
              group_by(repId)%>%
             select(repId, time, spread) %>%
             summarise_all(max)
           # # Find spread b: likelihood of lethal temp
           runsB <- runs%>%
             right_join(S1)%>%
             mutate(lengthSurface = lengthSurface * spread,
                    heightSurface = heightSurface * spread,
                    angleSurface = angleSurface * spread,
                    fh = pmax(heightSurface, heightPlant),
                    fl = pmax(lengthSurface, lengthPlant))
           # Find severity impacts using flora function
           severity<-flora(runsB, IP, base_params_site, Test = 70)%>%
             mutate(sev = if_else(b4 > 50, 5,
                                  if_else(sc4 > 50, 4,
                                          if_else(sc4 > 10, 3,
                                                  if_else((sc2 + sc3) > 50, 2,
                                                          1)))),
                    time = ceiling(repId/input$jits))
           
           S2<-severity%>%
             mutate(t1 = ifelse(sc1>=90, 1, 0),
                    t2 = ifelse(sc2>=90, 1, 0),
                    t3 = ifelse(sc3>=90, 1, 0),
                    t4 = ifelse(sc4>=90, 1, 0))%>%
             group_by(time)%>%
             select(time, Height, t1, t2, t3, t4, sev)%>%
             summarise_all(mean)

           S1 <- S1%>%
             group_by(time)%>%
             select(time, spread)%>%
             summarise_all(mean)

           Likelihood <- left_join(S1,S2)%>%
             mutate(Height = Height * spread,
                    t1 = t1 * spread,
                    t2 = t2 * spread,
                    t3 = t3 * spread,
                    t4 = t4 * spread)

           Times <- runs %>%
             select(repId, time, fh, fl, ros_kph,
                    wind_kph, deadFuelMoistureProp, temperature, slope_degrees) %>%
             group_by(time) %>%
             summarize_all(mean) %>%
             right_join(Likelihood) %>%
             mutate(fh = fh * spread,
                    fl = fl * spread,
                    ros_kph = ros_kph * spread)
           # output
           fnl <- list(res = res,x = x ,runs = runs ,IP = IP,
                       surf = surf ,base_params_site = base_params_site,
                       severity = severity,Times = Times)
           # analysis is runned
           site_anls_run_stat$status <- 1 
           fnl
      
   
    })
    
    
observe({
  print(
    site_analysis()
    )
  })
# Throwing error output in site analysis
#  

output$site_anls_error_txt <- renderText({
  site_anls_error_output()
  })   
## output of site analysis (Table)
    
    output$site_anls_tbl <- DT::renderDataTable({
      req(site_analysis())
      # Summary stats
      hq <- round(quantile(site_analysis()$runs$fh,
                           probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 1)
      lq <- round(quantile(site_analysis()$runs$fl,
                           probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 1)
      rq <- round(quantile(site_analysis()$runs$ros_kph,
                           probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 2)
      Shsc <- round(quantile(site_analysis()$severity$sc2,
                             probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
      Shk <- round(quantile(site_analysis()$severity$b2,
                            probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
      Msc <- round(quantile(site_analysis()$severity$sc3,
                            probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
      Mk <- round(quantile(site_analysis()$severity$b3,
                           probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
      Csc <- round(quantile(site_analysis()$severity$sc4,
                            probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
      Ck <- round(quantile(site_analysis()$severity$b4,
                           probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
      probs <- c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)
      #Percentiles
      sev_percent <- as.data.frame(list('Probability' = probs ,'Height' = hq, 'Length' = lq, 'ROS'=rq,
                                        'ShrubScorch'=Shsc, 'ShrubCons'=Shk,
                                        'MidScorch'=Msc, 'MidCons'=Mk, 'CanScorch'=Csc, 'CanCons'=Ck))
      DT::datatable(sev_percent,options = list(scrollX = TRUE))
    })

### Output of site analysis plots
output$site_stds_plot <- renderPlotly({

  fh_smooth <- smooth.spline(site_analysis()$runs$time,site_analysis()$runs$fh,spar = 0.5)
  p1_ply <- plot_ly() %>%
    add_boxplot(x= ~site_analysis()$runs$time,y =~site_analysis()$runs$fh,
                fillcolor = "white",name = "FH") %>%
    add_lines(x = ~fh_smooth$x , y =~fh_smooth$y,name = "FH",
              line = list(shape = "spline",color ="red")) %>%
    layout(
      xaxis = list(title = "",showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = list(text = "Flame Height",
                                font = list(family ="Arial",size = 10)),
                   tickfont=list(family = "Arial",size=10),
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
      showlegend =FALSE)

      ros_smooth <- smooth.spline(site_analysis()$runs$time,site_analysis()$runs$ros_kph,spar = 0.5)
       p2_ply <- plot_ly() %>%
       add_boxplot(x= ~site_analysis()$runs$time,y =~site_analysis()$runs$ros_kph,
                fillcolor = "white",name ="ROS") %>%
      add_lines(x = ~ros_smooth$x , y =~ros_smooth$y,name = "ROS",
              line = list(shape = "spline",color ="red")) %>%
     layout(
       xaxis = list(title = "",showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
       yaxis = list(title = list(text = "ROS (km/h)",
                                font = list(family ="Arial",size = 10)),
                   tickfont=list(family = "Arial",size=10),
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
       showlegend = FALSE)

    fl_smooth <- smooth.spline(site_analysis()$runs$time,site_analysis()$runs$fl,spar = 0.5)
    p3_ply  <- plot_ly() %>%
    add_boxplot(x= ~site_analysis()$runs$time,y =~site_analysis()$runs$fl,
                fillcolor = "white",name ="FL") %>%
    add_lines(x = ~fl_smooth$x , y =~fl_smooth$y,name = "FL",
              line = list(shape = "spline",color ="red")) %>%
    layout(
      xaxis = list(title = "",showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = list(text = "Flame length (m)",
                                font = list(family ="Arial",size = 10)),
                   tickfont=list(family = "Arial",size=10),
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
      showlegend = FALSE)

  p4_ply <- plot_ly() %>%
    add_boxplot(x=~site_analysis()$severity$time,y=~site_analysis()$severity$sev,
                fillcolor = "white",name ="FS") %>%
    layout(xaxis = list(title = "",showgrid =TRUE,dtick = 1,
                        showline =TRUE,mirror = "ticks",
                        showticklabels  = FALSE),
           yaxis = list(title = list(text = "Fire Severity",
                                     font = list(family ="Arial",size = 10)),
                        tickfont=list(family = "Arial",size=10),
                        showgrid = TRUE,showline = TRUE,
                        linewidth = 3,mirror = "ticks",
                        tickmode = "array",ticktext = c("Surf","USc","lCSc","CSc","CF"),
                        range = c(0,5),tickvals = c(1,2,3,4,5)),
           showlegend = FALSE
    )

  p5_ply <- plot_ly() %>%
    add_trace(x = ~site_analysis()$Times$time,
              y = ~site_analysis()$Times$wind_kph,name = "WS",mode="markers") %>%
    add_lines(x = ~site_analysis()$Times$time,
              y = ~site_analysis()$Times$wind_kph)%>%
    layout(
      xaxis = list(title = "",showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,zeroline = FALSE,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = list(text = "Wind speed (km/h)",
                                font = list(family ="Arial",size = 10)),
                   tickfont=list(family = "Arial",size=10),
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
      showlegend = FALSE)

  p6_ply <- plot_ly() %>%
    add_trace(x = ~site_analysis()$Times$time,
              y = ~site_analysis()$Times$deadFuelMoistureProp,name = "DFMC",mode="markers") %>%
    add_lines(x = ~site_analysis()$Times$time,
              y = ~site_analysis()$Times$deadFuelMoistureProp)%>%
    layout(
      xaxis = list(title = "",showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,zeroline = FALSE,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = list(text = "Moisture content",
                                font = list(family ="Arial",size = 10)),
                   tickfont=list(family = "Arial",size=10),
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
      showlegend = FALSE)

  big_plot <- subplot(p1_ply,p2_ply,p3_ply,p4_ply,p5_ply,p6_ply,nrows = 6,titleY = TRUE)
  big_plot

})


########
### soil analysis
#########

summary_soil <- reactive({
  if(is.null(input$runs_soil_file)){
    return(NULL)
  }else{
   read.csv(input$runs_soil_file$datapath) 
  }
})

ip_soil <- reactive({
  if(is.null(input$ip_soil_file)){
    return(NULL)
  }else{
    read.csv(input$ip_soil_file$datapath) 
  }
})
## event reactive for storing error of soil analysis

soil_errors_output <- eventReactive(input$soil_anls,{
  if( as.integer(!is.null(summary_soil())) + as.integer(!is.null(ip_soil())) + 
                 as.integer(isolate(site_anls_run_stat$status)) == 0){
    createAlert(session,"soil_error","soil_alert", title = "Oops",
                content = "You should either run contextual analysis or 
                upload summary and IP data sets", append = TRUE) 
  }else if(is.null(summary_soil())){
    createAlert(session,"soil_error","soil_alert", title = "Oops",
                content = "You have not uploaded Summary data.", append = TRUE)
  }else if(is.null(ip_soil())){
    createAlert(session,"soil_error","soil_alert", title = "Oops",
                content = "You have not uploaded ignition path data.", append = TRUE)
  }else{
    closeAlert(session,"soil_alert")
  }
  
})


## Soil analysis
heat_soil <- eventReactive(input$soil_anls,{
  # at least contextual anls is run or summary and ip are uploaded
   req( site_anls_run_stat$status | (!is.null(summary_soil()) & !is.null(ip_soil())) )
   if( !is.null(summary_soil()) & !is.null(ip_soil()) ){
     arg1 <- summary_soil()
     arg2 <- ip_soil()
   }else{
     arg1 <- site_analysis()$runs
     arg2 <- site_analysis()$IP
   }
     
   STARTTEMP <- arg1$temperature
  
  
  progress <- shiny::Progress$new()
  progress$set(message = "Running the soil analysis \n", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  # calculating the time that the function goes thorough
  residence <- 0.871*input$lit_dep_soil^1.875
  ROS <- mean(arg1$ros_kph)/3.6
  Ta <- round(input$lit_dep_soil/ROS+residence)
  prg_steps <- Ta +input$trail_sec_soil
  updateProgress <- function(value = prg_steps, detail = NULL) {
    progress$inc(amount = 1/value, detail = detail)
  }
  
  heat <- soil(arg1, arg2, diameter = input$lit_dep_soil, 
               percentile = input$risk_per_soil, 
               RH = input$rel_hum_soil, moisture = input$moist_soil,
               distance = input$dist_soil, trail = input$trail_sec_soil, 
               Pressure = input$sea_level_perss_soil, 
               Altitude = input$alti_soil, texture = input$text_soil, 
               peat = input$org_com_soil, soilTemp = STARTTEMP,updateProgress = updateProgress)
     a <- heat %>%
        select(t, tempS, soilTempA, soilTempB, soilTempC, soilTempD, soilTempE,
              moistureA, moistureB, moistureC, moistureD, moistureE)%>%
      group_by(t) %>%
       summarise_all(list(Q = quantile), probs = input$risk_per_soil)%>%
       mutate(minutes = t/60) 
  
     b <- heat %>%
        select(t, startM, seedDa, seedDb, seedDc, seedDd, seedDe, seedga, seedgb, seedgc, seedgd, seedge, 
             orgA, orgB, orgC, orgD, orgE, repelA, repelB, repelC, repelD, repelE) %>%
       group_by(t) %>%
         summarise_all(mean)
  
  soilSev <-  left_join(a, b)%>%
    mutate(nonRepA = pmin(1,cumsum(1/(60*30991*exp(-0.0257*soilTempA_Q)))),
           nonRepB = pmin(1,cumsum(1/(60*30991*exp(-0.0257*soilTempB_Q)))),
           nonRepC = pmin(1,cumsum(1/(60*30991*exp(-0.0257*soilTempC_Q)))),
           nonRepD = pmin(1,cumsum(1/(60*30991*exp(-0.0257*soilTempD_Q)))),
           nonRepE = pmin(1,cumsum(1/(60*30991*exp(-0.0257*soilTempE_Q)))))
  
  #Final values
      tA <- round(max(soilSev$soilTempA_Q), 0)
      tB <- round(max(soilSev$soilTempB_Q), 0)
      tC <- round(max(soilSev$soilTempC_Q), 0)
      tD <- round(max(soilSev$soilTempD_Q), 0)
       tE <- round(max(soilSev$soilTempE_Q), 0)
      mA <- 100*round(min(soilSev$moistureA_Q), 3)
      mB <- 100*round(min(soilSev$moistureB_Q), 3)
      mC <- 100*round(min(soilSev$moistureC_Q), 3)
      mD <- 100*round(min(soilSev$moistureD_Q), 3)
      mE <- 100*round(min(soilSev$moistureE_Q), 3)
      ga <- ifelse(max(soilSev$seedga)==1, "T", "")
      gb <- ifelse(max(soilSev$seedgb)==1, "T", "")
      gc <- ifelse(max(soilSev$seedgc)==1, "T", "")
      gd <- ifelse(max(soilSev$seedgd)==1, "T", "")
      ge <- ifelse(max(soilSev$seedge)==1, "T", "")
      da <- ifelse(max(soilSev$seedDa)==1, "T", "")
      db <- ifelse(max(soilSev$seedDb)==1, "T", "")
      dc <- ifelse(max(soilSev$seedDc)==1, "T", "")
      dd <- ifelse(max(soilSev$seedDd)==1, "T", "")
      de <- ifelse(max(soilSev$seedDe)==1, "T", "")
      oa <- ifelse(round(max(soilSev$orgA),1)==1, "T", "")
      ob <- ifelse(round(max(soilSev$orgB),1)==1, "T", "")
       oc <- ifelse(round(max(soilSev$orgC),1)==1, "T", "")
       od <- ifelse(round(max(soilSev$orgD),1)==1, "T", "")
      oe <- ifelse(round(max(soilSev$orgE),1)==1, "T", "")
      ra <- ifelse(max(soilSev$repelA)==1, "T", "")
     rb <- ifelse(max(soilSev$repelB)==1, "T", "")
     rc <- ifelse(max(soilSev$repelC)==1, "T", "")
     rd <- ifelse(max(soilSev$repelD)==1, "T", "")
     re <- ifelse(max(soilSev$repelE)==1, "T", "")
     na <- ifelse(round(max(soilSev$nonRepA),0)==1, "T", "")
     nb <- ifelse(round(max(soilSev$nonRepB),0)==1, "T", "")
     nc <- ifelse(round(max(soilSev$nonRepC),0)==1, "T", "")
      nd <- ifelse(round(max(soilSev$nonRepD),0)==1, "T", "")
      ne <- ifelse(round(max(soilSev$nonRepE),0)==1, "T", "")
  
       depth <- c(-1,-2,-3,-4,-5)
       tm <- c(tA,tB,tC,tD,tE)
       mm <- c(mA,mB,mC,mD,mE)
       germ <- c(ga, gb, gc, gd, ge)
       mort <- c(da, db, dc, dd, de)
       org <- c(oa, ob, oc, od, oe)
       repel <- c(ra, rb, rc, rd, re)
       nr <- c(na, nb, nc, nd, ne)
      finals <- as.data.frame(list('Depth' = depth, 'A' = tm, 'B'=mm, 
                                   'C'=germ, 'D'=mort, 'E'=org, 'F'=repel, 'G'=nr))
      fnl <- list(heat = heat, finals = finals,soilSev = soilSev)
      return(fnl)
})

##
observe({
  
  print(heat_soil() )
})

##
output$soil_error_txt <- renderText({
  soil_errors_output()
})
##table output soil
output$soil_table <- DT::renderDataTable({
   req(heat_soil())
  out <- DT::datatable(heat_soil()$finals,caption = 'Table 1. Soil heating impacts',
                colnames =  c("Depth (cm)", "Maximum temp  (C)", "Final moisture (%ODW)", 
                              "Germination of legumes", "Seedbank death", "Loss of organic material", 
                              "Enhanced water repellency", "Destruction of water repellency"))
  out
  })


#plot output soil
output$soil_plt_1 <- renderPlotly({
  req(heat_soil())
p1  <- plot_ly(x = ~heat_soil()$soilSev$minutes) %>% 
    add_lines(y = ~heat_soil()$soilSev$soilTempA_Q ,name = "1cm" ) %>% 
    add_lines( y = ~heat_soil()$soilSev$soilTempB_Q,name = "2cm" ) %>% 
    add_lines( y = ~heat_soil()$soilSev$soilTempC_Q,name = "3cm" ) %>% 
    add_lines( y = ~heat_soil()$soilSev$soilTempD_Q,name = "4cm" ) %>% 
    add_lines( y = ~heat_soil()$soilSev$soilTempE_Q,name = "5cm" ) %>% 
    layout(   
      xaxis = list(title = list( text = "Minutes",
                                 font = list(family ="Arial",size = 10)),showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = list(text = "Temperature (°C)",
                                font = list(family ="Arial",size = 10)),
                   tickfont=list(family = "Arial",size=10),
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
      colorway = c("black","grey20","grey40","grey60","grey80"))
p1
})


output$soil_plt_2 <- renderPlotly({
  req(heat_soil())
  p2 <-  plot_ly(x = ~heat_soil()$soilSev$minutes) %>% 
    add_lines(y = ~heat_soil()$soilSev$startM ,name = "Original" ) %>% 
    add_lines(y = ~heat_soil()$soilSev$moistureA_Q ,name = "1cm" ) %>% 
    add_lines( y = ~heat_soil()$soilSev$moistureB_Q,name = "2cm" ) %>% 
    add_lines( y = ~heat_soil()$soilSev$moistureC_Q,name = "3cm" ) %>% 
    add_lines( y = ~heat_soil()$soilSev$moistureD_Q,name = "4cm" ) %>% 
    add_lines( y = ~heat_soil()$soilSev$moistureE_Q,name = "5cm" ) %>% 
    layout(  
      xaxis = list(title = list( text = "Minutes",
                                 font = list(family ="Arial",size = 10)),
                   showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = list(text = "Soil moisture",
                                font = list(family ="Arial",size = 10)),
                   tickfont=list(family = "Arial",size=10),
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
      colorway = c("red","black","grey20","grey40","grey60","grey80"))
  
  p2
  
})

output$soil_plt_3 <- renderPlotly({
  req(heat_soil())
  p3 <- plot_ly(x = ~heat_soil()$finals$A) %>% 
    add_lines(y = ~heat_soil()$finals$Depth,name = "Final temperature") %>% 
    layout(  title = list( text = "Final Temperature (°C)",
                           font = list(family ="Arial",size = 10)),
             xaxis = list(title = list( text = "Temperature (°C)",
                                        font = list(family ="Arial",size = 10)),showgrid = TRUE,
                          showline = TRUE,linewidth = 3,
                          mirror = "ticks",showticklabels  = FALSE),
             yaxis = list(title = list(text = "Depth (cm)",
                                       font = list(family ="Arial",size = 10)),
                          tickfont=list(family = "Arial",size=10),
                          showgrid = TRUE,showline = TRUE,
                          linewidth = 3,mirror = "ticks"))
  p3
  
})

output$soil_plt_4 <- renderPlotly({
  req(heat_soil())
  p4 <-  plot_ly(x = ~heat_soil()$finals$B) %>% 
    add_lines(y = ~heat_soil()$finals$Depth,name = "Final moisture") %>% 
    layout(  title = list( text = "Final moisture",
                           font = list(family ="Arial",size = 10)),
             xaxis = list(title = list( text = "Moisture (%ODW)",
                                        font = list(family ="Arial",size = 10)),showgrid = TRUE,
                          showline = TRUE,linewidth = 3,
                          mirror = "ticks",showticklabels  = FALSE),
             yaxis = list(title = list(text = "Depth (cm)",
                                       font = list(family ="Arial",size = 10)),
                          tickfont=list(family = "Arial",size=10),
                          showgrid = TRUE,showline = TRUE,
                          linewidth = 3,mirror = "ticks"))
  
  p4
  
})





##############
##### Vegetation 
##############
### error output flora anls

flora_error_output <- eventReactive(input$veg_anls,{
  if(site_anls_run_stat$status == 0){
    createAlert(session,"veg_error","veg_alert",title = "Oops",
                content = "You need to run contextual flammibility analysis first.",append = TRUE)
  }else{
    closeAlert(session, "veg_alert")
  }
})

## flora analysis
flora_heat  <- eventReactive(input$veg_anls,{
  req(site_analysis())
  # SCORCH AND CONSUMPTION
  S1 <- site_analysis()$x%>%
    mutate(time = ceiling(repId/input$jits),
           spread = extinct * ifelse(level == "Surface",
                                     ifelse(oHorizon >=4,spread,0),
                                     spread))%>%
    group_by(repId)%>%
    select(repId, time, spread)%>%
    summarise_all(max)#%>%
  
  # Find spread b: likelihood of lethal temp
  runsB <- site_analysis()$runs%>%
    right_join(S1)%>%
    mutate(lengthSurface = lengthSurface * spread,
           heightSurface = heightSurface * spread,
           angleSurface = angleSurface * spread,
           fh = pmax(heightSurface, heightPlant),
           fl = pmax(lengthSurface, lengthPlant))
  
  init <- isolate(base_params$values)
  
  severity<-flora(runsB, site_analysis()$IP, init, Test = input$leth_veg)%>%
    mutate(sev = if_else(b4 > 50, 5,
                         if_else(sc4 > 50, 4,
                                 if_else(sc4 > 10, 3,
                                         if_else((sc2 + sc3) > 50, 2,
                                                 1)))),
           time = ceiling(repId/input$jits))
  
  progress <- shiny::Progress$new()
  progress$set(message = "Running the cambium analysis \n", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  
  # calculating the time that the function goes thorough
  residence <- 0.871*input$lit_dep_veg^1.875
  ROS <- mean(site_analysis()$runs$ros_kph)/3.6
  Ta <- round(input$dist_veg/ROS+residence)
  prg_steps <- Ta + trail
  updateProgress <- function(value = prg_steps, detail = NULL) {
    progress$inc(amount = 1/value, detail = detail)
  }
  
  #CAMBIUM DAMAGE
  S <- strata(init)
  StN <- max(S$stratum)
  
  STARTTEMP <- site_analysis()$runs$temperature
  Heat <- cambium(site_analysis()$runs, site_analysis()$IP, diameter = input$lit_dep_veg, 
                  percentile = input$risk_per_veg, 
                  RH = input$rel_hum_veg, moisture = input$moist_timb_veg,
                  bMoisture = input$moist_brk_veg, distance = input$dist_veg,
                  trail = input$trail_sec_veg, 
                  Pressure = input$sea_level_press_veg, Altitude = input$alti_veg, 
                  startTemp = STARTTEMP, Height = 0.5, 
                  woodDensity = input$wd_dens_veg, 
                  barkDensity = input$brk_dens_veg, 
                  bark = input$brk_thick_veg, 
                  comBark = 700, resBark = 45, 
                  cambThick = input$cambium_veg, xylemThick = input$xylem_thick_veg, 
                  necT = 60, surfDecl = 15)
  
  camb <- Heat %>%
    select(t, tempS, barkTemp, woodTempB, woodTempC, woodTempD, woodTempE,
           moistureA, moistureB, moistureC, moistureD, moistureE)%>%
    group_by(t) %>%
    summarise_all(list(Q = quantile), probs = input$risk_per_veg)%>%
    mutate(minutes = t/60) 
  
  fnl <- list(camb = camb,Heat = Heat,severity = severity)
  return(fnl)
  
})



## printing out the table result
output$veg_table <- DT::renderDataTable({
  req(flora_heat())
  Shsc <- round(quantile(flora_heat()$severity$sc2, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
  Shk <- round(quantile(flora_heat()$severity$b2, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
  Msc <- round(quantile(flora_heat()$severity$sc3, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
  Mk <- round(quantile(flora_heat()$severity$b3, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
  Csc <- round(quantile(flora_heat()$severity$sc4, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
  Ck <- round(quantile(flora_heat()$severity$b4, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)), 0)
  probs <- c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)
  
  #Percentiles
  sev_percent <- as.data.frame(list('Probability' = probs, 'ShrubScorch'=Shsc, 'ShrubCons'=Shk, 
                                    'MidScorch'=Msc, 'MidCons'=Mk, 'CanScorch'=Csc, 'CanCons'=Ck))
  DT::datatable(sev_percent,options = list(scrollX = TRUE))
  
})

## printing ui output

output$veg_txt <- renderUI({
     flora_error_output()
  #Final values
     tA <- round(max(flora_heat()$camb$barkTemp_Q), 0)
     tB <- round(max(flora_heat()$camb$woodTempB_Q), 0)
     tC <- round(max(flora_heat()$camb$woodTempC_Q), 0)
     tD <- round(max(flora_heat()$camb$woodTempD_Q), 0)
     tE <- round(max(flora_heat()$camb$woodTempE_Q), 0)
     mA <- 100*round(min(flora_heat()$camb$moistureA_Q), 3)
     mB <- 100*round(min(flora_heat()$camb$moistureB_Q), 3)
     mC <- 100*round(min(flora_heat()$camb$moistureC_Q), 3)
     mD <- 100*round(min(flora_heat()$camb$moistureD_Q), 3)
     mE <- 100*round(min(flora_heat()$camb$moistureE_Q), 3)
     necrosis <- max(flora_heat()$Heat$necrosis)
     ringbark <- ifelse(max(flora_heat()$Heat$ringbark)==1, "T", "")
     girdle <- ifelse(max(flora_heat()$Heat$girdle)==1, "T", "")
  
     depth <- c(0,-1,-2,-3,-4)
     tm <- c(tA,tB,tC,tD,tE)
     mm <- c(mA,mB,mC,mD,mE)
  
  
  out <- HTML(paste(
  paste("Table 1. Heat penetration into the trunk, and drying of bark and wood"),
  paste(" Necrosis reached", necrosis, "cm into the wood"),
  paste("Ringbarking:", ringbark),
  paste("Girdling:", girdle),sep = "<br/>"))
  return(out)
  
})


## printing out the flora plot

output$veg_plt_1 <- renderPlotly({
  req(flora_heat())
  
  p1 <- plot_ly(x = ~ flora_heat()$camb$minutes) %>% 
    add_lines(y = ~ flora_heat()$camb$tempS_Q,name = "Surface") %>% 
    add_lines(y = ~ flora_heat()$camb$barkTemp_Q,name = "1cm") %>% 
    add_lines(y = ~ flora_heat()$camb$woodTempB_Q,name = "2cm") %>% 
    add_lines(y = ~ flora_heat()$camb$woodTempC_Q,name = "3cm") %>% 
    add_lines(y = ~ flora_heat()$camb$woodTempD_Q ,name = "4cm") %>% 
    add_lines(y = ~ flora_heat()$camb$woodTempE_Q,name = "5cm") %>% 
    layout(  title = "Temperatures",
             xaxis = list(title  = "Minutes",
                          showgrid = TRUE,dtick = 1,
                          showline = TRUE,linewidth = 3,
                          mirror = "ticks",showticklabels  = FALSE),
             yaxis = list(title =  "Temperature (°C)",
                          tickfont=list(family = "Arial",size=10),zeroline = FALSE,
                          showgrid = TRUE,showline = TRUE,
                          linewidth = 3,mirror = "ticks"),
             colorway = c("red","black","grey20","grey40","grey60","grey80"))
  p1
})

output$veg_plt_2 <- renderPlotly({
  req(flora_heat())
  p2 <- plot_ly(x = ~ flora_heat()$camb$minutes) %>% 
    add_lines(y = ~ flora_heat()$camb$moistureA_Q,name = "1cm") %>% 
    add_lines(y = ~ flora_heat()$camb$moistureB_Q,name = "2cm") %>% 
    add_lines(y = ~ flora_heat()$camb$moistureC_Q,name = "3cm") %>% 
    add_lines(y = ~ flora_heat()$camb$moistureD_Q,name = "4cm") %>% 
    add_lines(y = ~ flora_heat()$camb$moistureE_Q,name = "5cm") %>% 
    layout(  title = "Moisture",
             xaxis = list(title  = "Minutes",
                          showgrid = TRUE,dtick = 1,
                          showline = TRUE,linewidth = 3,
                          mirror = "ticks",showticklabels  = FALSE),
             yaxis = list(title  = "Moisture (%ODW)",
                          tickfont=list(family = "Arial",size=10),zeroline = FALSE,
                          showgrid = TRUE,showline = TRUE,
                          linewidth = 3,mirror = "ticks"),
             colorway = c("black","grey20","grey40","grey60","grey80"))
  p2
})

output$veg_plt_3 <- renderPlotly({
  req(flora_heat())
  #Final values
  tA <- round(max(flora_heat()$camb$barkTemp_Q), 0)
  tB <- round(max(flora_heat()$camb$woodTempB_Q), 0)
  tC <- round(max(flora_heat()$camb$woodTempC_Q), 0)
  tD <- round(max(flora_heat()$camb$woodTempD_Q), 0)
  tE <- round(max(flora_heat()$camb$woodTempE_Q), 0)
  mA <- 100*round(min(flora_heat()$camb$moistureA_Q), 3)
  mB <- 100*round(min(flora_heat()$camb$moistureB_Q), 3)
  mC <- 100*round(min(flora_heat()$camb$moistureC_Q), 3)
  mD <- 100*round(min(flora_heat()$camb$moistureD_Q), 3)
  mE <- 100*round(min(flora_heat()$camb$moistureE_Q), 3)
  necrosis <- max(flora_heat()$Heat$necrosis)
  ringbark <- ifelse(max(flora_heat()$Heat$ringbark)==1, "T", "")
  girdle <- ifelse(max(flora_heat()$Heat$girdle)==1, "T", "")
  
  depth <- c(0,-1,-2,-3,-4)
  tm <- c(tA,tB,tC,tD,tE)
  mm <- c(mA,mB,mC,mD,mE)
  finals <- as.data.frame(list('Depth' = depth, 'Temperature' = tm, 'PropMoist'=mm))
  
  p3 <- plot_ly(x = ~finals$Temperature) %>% 
    add_lines(y = ~ finals$Depth)  %>% 
    layout(  title = "Final temperature",
             xaxis = list(title  = "Temperature (°C)",
                          showgrid = TRUE,dtick = 1,
                          showline = TRUE,linewidth = 3,
                          mirror = "ticks",showticklabels  = FALSE),
             yaxis = list(title =  "Depth (cm)",zeroline = FALSE,
                          tickfont=list(family = "Arial",size=10),
                          showgrid = TRUE,showline = TRUE,
                          linewidth = 3,mirror = "ticks"),colorway = "black")
  p3
  
})

output$veg_plt_4 <- renderPlotly({
  req(flora_heat())
  #Final values
  tA <- round(max(flora_heat()$camb$barkTemp_Q), 0)
  tB <- round(max(flora_heat()$camb$woodTempB_Q), 0)
  tC <- round(max(flora_heat()$camb$woodTempC_Q), 0)
  tD <- round(max(flora_heat()$camb$woodTempD_Q), 0)
  tE <- round(max(flora_heat()$camb$woodTempE_Q), 0)
  mA <- 100*round(min(flora_heat()$camb$moistureA_Q), 3)
  mB <- 100*round(min(flora_heat()$camb$moistureB_Q), 3)
  mC <- 100*round(min(flora_heat()$camb$moistureC_Q), 3)
  mD <- 100*round(min(flora_heat()$camb$moistureD_Q), 3)
  mE <- 100*round(min(flora_heat()$camb$moistureE_Q), 3)
  necrosis <- max(flora_heat()$Heat$necrosis)
  ringbark <- ifelse(max(flora_heat()$Heat$ringbark)==1, "T", "")
  girdle <- ifelse(max(flora_heat()$Heat$girdle)==1, "T", "")
  
  depth <- c(0,-1,-2,-3,-4)
  tm <- c(tA,tB,tC,tD,tE)
  mm <- c(mA,mB,mC,mD,mE)
  finals <- as.data.frame(list('Depth' = depth, 'Temperature' = tm, 'PropMoist'=mm))
  
  p4 <-  plot_ly(x = ~finals$PropMoist) %>% 
    add_lines(y = ~ finals$Depth)  %>% 
    layout(  title = "Final moisture",
             xaxis = list(title  = "Moisture (%ODW)",
                          showgrid = TRUE,dtick = 1,
                          showline = TRUE,linewidth = 3,
                          mirror = "ticks",showticklabels  = FALSE),
             yaxis = list(title  = "Depth (cm)",
                          zeroline = FALSE,
                          tickfont=list(family = "Arial",size=10),
                          showgrid = TRUE,showline = TRUE,
                          linewidth = 3,mirror = "ticks"),colorway = "black")
  
  p4
})

############
### Impacts on animal
############


#######
###hollow
#######

## Taking inputs 
summary_hollow <- reactive({

  if(is.null(input$runs_hollw_file)){
    return(NULL)
  }else{
    read.csv(input$runs_hollw_file$datapath)
  }
})

ip_hollow <- reactive({
  
  if(is.null(input$ip_hollw_file)){
    return(NULL)
  }else{
    read.csv(input$ip_hollw_file$datapath)
  }
})


# error output for the hollow analysis

hollow_errors_output <-  eventReactive(input$run_anls_hollw,{
  
  if(as.integer(!is.null(summary_hollow())) + as.integer(!is.null(ip_hollow())) + 
                as.integer(isolate(site_anls_run_stat$status)) == 0){
    createAlert(session,"hollow_error","hollow_alert",title = "Oops",
                content ="You should either run contextual analysis or 
                upload summary and IP data sets.",append = TRUE)
  }else if(is.null(summary_hollow())){
    createAlert(session,"hollow_error","hollow_alert",title = "Oops",
                content ="You have not uploaded the summary file.",append = TRUE)
  }else if(is.null(ip_hollow())){
    createAlert(session,"hollow_error","hollow_alert",title = "Oops",
                content ="You have not uploaded the ignition path file.",append = TRUE)
  }else{
    closeAlert(session,"hollow_alert")
  }
  
})



hollow_df <- eventReactive(input$run_anls_hollw,{
  # at least contextual is run or summary and IP are uploaed
   req( site_anls_run_stat$status | (!is.null(summary_hollow()) & !is.null(ip_hollow())) )
  
  
  if( !is.null(summary_hollow()) & !is.null(ip_hollow()) ){
    arg1 <- summary_hollow()
    arg2 <- ip_hollow()
  }else{
    arg1 <- site_analysis()$runs
    arg2 <- site_analysis()$IP
  }
  
  progress <- shiny::Progress$new()
  progress$set(message = "Running the hollow analysis \n", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  # calculating the time that the function goes thorough
  ROS <- mean(arg1$ros_kph)/3.6
     prg_steps <-round((input$far_hollw - input$cls_hollw)/ROS)
  updateProgress <- function(value = prg_steps, detail = NULL) {
    progress$inc(amount = 1/value, detail = detail)
  }
  
 heat <-  hollow(arg1, arg2, percentile = input$risk_per_hollw, Height = input$height_hollw,
              woodDensity = input$wd_dens_hollw,
         barkDensity = input$brk_dens_hollw, wood = input$wd_thick_hollw, 
         bark = input$brk_thick_hollw, RH = input$rel_hum_hollw, water = input$dmfc_hollw,
         low = input$cls_hollw, high = input$far_hollw, var = 10, Pressure = input$sea_level_press_hollw, 
         Altitude = input$alt_hollw,
         Dimension = input$lngth_hollw, Area = input$area_hollw, 
         hollowTemp = 25, Shape = "Cylinder",updateProgress = updateProgress)
  
  Mheat <- heat %>%
    select(t, tempAir, tempBark, tempWood, mortality, qc, qR, Q)%>%
    mutate(qc = qc/1000,
           qR = qR/1000,
           Q = Q/1000,
           count = ceiling(t/2),
           minutes = t/60) %>%
    group_by(t) %>%
    summarise_all(list(Q = quantile), probs = input$risk_per_hollw)
  
  mort <- heat %>%
    group_by(t) %>%
    summarise_all(mean)

  
  fnl <- list(heat = heat, Mheat = Mheat,mort = mort)
  return(fnl)
})




## Table of hollow output
output$hollw_text <- renderUI({
  hollow_errors_output()
  m <- 100*max(hollow_df()$mort$mortality)
  Tm <- round(max(hollow_df()$Mheat$tempWood_Q),0)
  
  out <- HTML(
  paste(
  paste(" The hollow was heated to", Tm, "degrees Celsius"),
  "",
  paste("Mortality was", m, "% likely"),sep = '<br/>')
  )
  return(out) 
})

## Plot output 
output$hollow_plot_1 <- renderPlotly({
  req(hollow_df())
  p1 <-  plot_ly(x = ~ hollow_df()$Mheat$minutes_Q) %>%
    add_lines(y = ~ hollow_df()$Mheat$Q_Q,name = "Total flux") %>% 
    add_lines(y = ~ hollow_df()$Mheat$qR_Q,name = "Radiation" ) %>% 
    add_lines(y = ~hollow_df()$Mheat$qc_Q,name = "Convection") %>% 
    layout( title =  paste("Heat flux", input$height_hollw, "m above ground"),
      xaxis = list(title = "Minutes",
                   showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = "kW/m",
                   tickfont=list(family = "Arial",size=10),
                   showgrid = TRUE,showline = TRUE,zeroline = FALSE,
                   linewidth = 3,mirror = "ticks"),
      colorway = c("black","grey40","grey80")) 
  
  p1
})

testp1 <- reactive({
  req(hollow_df())
  p1 <-  plot_ly(x = ~ hollow_df()$Mheat$minutes_Q) %>%
    add_lines(y = ~ hollow_df()$Mheat$Q_Q,name = "Total flux") %>% 
    add_lines(y = ~ hollow_df()$Mheat$qR_Q,name = "Radiation" ) %>% 
    add_lines(y = ~hollow_df()$Mheat$qc_Q,name = "Convection") %>% 
    layout( title =  paste("Heat flux", input$height_hollw, "m above ground"),
            xaxis = list(title = "Minutes",
                         showgrid = TRUE,dtick = 1,
                         showline = TRUE,linewidth = 3,
                         mirror = "ticks",showticklabels  = FALSE),
            yaxis = list(title = "kW/m",
                         tickfont=list(family = "Arial",size=10),
                         showgrid = TRUE,showline = TRUE,zeroline = FALSE,
                         linewidth = 3,mirror = "ticks"),
            colorway = c("black","grey40","grey80")) 
  
  p1
})

testp2 <- reactive({
  req(hollow_df())
  p2 <- plot_ly(x = ~ hollow_df()$Mheat$minutes_Q) %>%
    add_lines(y = ~hollow_df()$Mheat$tempAir_Q,name = "Air temperature (°C)") %>% 
    add_lines(y = ~hollow_df()$Mheat$tempBark_Q,name = "Bark temperature (°C)" ) %>% 
    add_lines(y = ~hollow_df()$Mheat$tempWood_Q,name = "Hollow temperature (°C)") %>% 
    layout( title = paste("Temperatures", input$height_hollw, "m above ground"),
            xaxis = list(title = "Minutes",
                         showgrid = TRUE,dtick = 1,
                         showline = TRUE,linewidth = 3,
                         mirror = "ticks",showticklabels  = FALSE),
            yaxis = list(title = "Temperature (°C)",
                         tickfont=list(family = "Arial",size=10),
                         showgrid = TRUE,showline = TRUE,zeroline = FALSE,
                         linewidth = 3,mirror = "ticks"),
            colorway = c("black","grey40","grey80")) 
  p2
})

output$hollow_plot_2 <- renderPlotly({
  req(hollow_df())
  p2 <- plot_ly(x = ~ hollow_df()$Mheat$minutes_Q) %>%
    add_lines(y = ~hollow_df()$Mheat$tempAir_Q,name = "Air temperature (°C)") %>% 
    add_lines(y = ~hollow_df()$Mheat$tempBark_Q,name = "Bark temperature (°C)" ) %>% 
    add_lines(y = ~hollow_df()$Mheat$tempWood_Q,name = "Hollow temperature (°C)") %>% 
    layout( title = paste("Temperatures", input$height_hollw, "m above ground"),
      xaxis = list(title = "Minutes",
                   showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = "Temperature (°C)",
                   tickfont=list(family = "Arial",size=10),
                   showgrid = TRUE,showline = TRUE,zeroline = FALSE,
                   linewidth = 3,mirror = "ticks"),
      colorway = c("black","grey40","grey80")) 
  p2
  
})
## Generating report

output$hollow_report <- downloadHandler(
  filename = "report.html",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "hollow_report.Rmd")
    file.copy("hollow_report.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params <- list(p1= testp1(),p2 = testp2())
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

######
### Arboreal
######

summary_arbrl <- reactive({
  if(is.null(input$runs_arbreal_file)){
    return(NULL)
  }else{
    read.csv(input$runs_arbreal_file$datapath)
  }
  
})

ip_arbrl <- reactive({
  if(is.null(input$ip_arbreal_file)){
    return(NULL)
  }else{
    read.csv(input$ip_arbreal_file$datapath)
  }
  
})
## error output for arbreal analysis
arbreal_errors_output <-  eventReactive(input$run_anls_arbreal,{
  if(as.integer(!is.null(summary_arbrl())) + as.integer(!is.null(ip_arbrl())) + 
                as.integer(isolate(site_anls_run_stat$status)) == 0){
    createAlert(session,"arbreal_error","arbreal_alert",title = "Oops",
                content ="You should either run contextual analysis or upload summary and IP.",append = TRUE)
  }else if(is.null(summary_arbrl())){
    createAlert(session,"arbreal_error","arbreal_alert",title = "Oops",
                content ="You have not uploaded the summary file.",append = TRUE)
  }else if(is.null(ip_arbrl())){
    createAlert(session,"arbreal_error","arbreal_alert",title = "Oops",
                content ="You have not uploaded the ignition path file.",append = TRUE)
  }else{
    closeAlert(session,"arbreal_alert")
  }
  
})

arbrl_df <- eventReactive(input$run_anls_arbreal,{
  req( site_anls_run_stat$status | (!is.null(summary_arbrl()) & !is.null(ip_arbrl())) )
  
  if( !is.null(summary_arbrl()) & !is.null(ip_arbrl()) ){
    arg1 <- summary_arbrl()
    arg2 <- ip_arbrl()
  }else{
    arg1 <- site_analysis()$runs
    arg2 <- site_analysis()$IP
  }
  
  progress <- shiny::Progress$new()
  progress$set(message = "Running the arboreal analysis \n", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  # calculating the time that the function goes thorough
  ROS <- mean(arg1$ros_kph)/3.6
  prg_steps <- round((input$far_arbreal - input$cls_arbreal)/ROS)
  
  updateProgress <- function(value = prg_steps, detail = NULL) {
    progress$inc(amount = 1/value, detail = detail)
  }
  
 Heat <-  arboreal(arg1, arg2 , Height = input$height_arbreal,
                     percentile = input$risk_per_arbreal, 
                     low = input$cls_arbreal, 
                     high = input$far_arbreal, var = 10, 
           Pressure = input$sea_level_press_arbreal, 
           Altitude = input$alt_arbreal, RH = input$rel_hum_arbreal, 
           Class = input$anim_clas_arbreal, 
           Dimension = input$lngth_arbreal, 
           Area = input$area_arbreal,
           protection = input$fur_arbreal, count = 66, fibre = 0.01, Specific_heat = 2.5, 
           objectTemp = 30, Shape = "Cylinder",updateProgress = updateProgress)
 
 
  Mheat <-  Heat %>%
    select(t, tempAir, plume_kph, tempConservation, tempFourier, tempR, thermalDose, qc, qR, Q)%>%
    mutate(qc = qc/1000,
           qR = qR/1000,
           Q = Q/1000,
           count = ceiling(t/2),
           minutes = t/60) %>%
    group_by(t) %>%
    summarise_all(list(Q = quantile), probs = input$risk_per_arbreal)
  fnl <- list(Heat = Heat,Mheat = Mheat)
  return(fnl)
})



## printing out the arbreal text
 output$arbreal_text <- renderUI({
   
   arbreal_errors_output()
   last <- max(arbrl_df()$Heat$t)
   f <- filter(arbrl_df()$Heat, t == last)
   
   Tot <- nrow(f)
   Pain <- 100*round(nrow(filter(f, thermalDose > 92))/Tot, 2)
   D1 <- 100*round(nrow(filter(f, thermalDose > 105))/Tot, 2)
   D2 <- 100*round(nrow(filter(f, thermalDose > 290))/Tot, 2)
   D3 <- 100*round(nrow(filter(f, thermalDose > 1000))/Tot, 2)
   D <- round(mean(f$fatalityLikelihood),0)
   
   out <- HTML(paste(paste(" Pain", Pain, "% likely"),
       paste("First degree burns", D1, "% likely"),
       paste("Second degree burns", D2, "% likely"),
       paste("Third degree burns", D3, "% likely"),
       paste("Death", D, "% likely"),sep = "<br/>"))
   return(out)
   
 })
## plot of arboreal
 output$arbreal_plt_1 <- renderPlotly({
   req(arbrl_df() )
   p1 <- plot_ly(x = ~arbrl_df()$Mheat$minutes_Q) %>% 
     add_lines(y = ~arbrl_df()$Mheat$Q_Q,name = "Total flux") %>% 
     add_lines(y = ~arbrl_df()$Mheat$qR_Q,name = "Radiation") %>% 
     add_lines(y = ~arbrl_df()$Mheat$qc_Q,name = "Convection") %>% 
     layout( title = paste("Heat flux", input$height_arbreal, "m above ground"),
             xaxis = list(title = "Minutes",
                          showgrid = TRUE,dtick = 1,
                          showline = TRUE,linewidth = 3,
                          mirror = "ticks",showticklabels  = FALSE),
             yaxis = list(title = "kW/m",
                          tickfont=list(family = "Arial",size=10),
                          showgrid = TRUE,showline = TRUE,zeroline = FALSE,
                          linewidth = 3,mirror = "ticks"),
             colorway = c("black","grey40","grey80")) 
   p1
 })
 
 output$arbreal_plt_2 <- renderPlotly({
   req(arbrl_df() )
   p2 <- plot_ly(x = ~arbrl_df()$Mheat$minutes_Q) %>% 
     add_lines(y = ~arbrl_df()$Mheat$Q_Q,name = "Air temperature (°C)") %>% 
     add_lines(y = ~arbrl_df()$Mheat$qR_Q,name = "Skin temperature (°C)") %>% 
     layout( title = paste("Temperatures", input$height_arbreal, "m above ground"),
             xaxis = list(title = "Minutes",
                          showgrid = TRUE,dtick = 1,
                          showline = TRUE,linewidth = 3,
                          mirror = "ticks",showticklabels  = FALSE),
             yaxis = list(title = "Temperatures (°C)",
                          tickfont=list(family = "Arial",size=10),
                          showgrid = TRUE,showline = TRUE,zeroline = FALSE,
                          linewidth = 3,mirror = "ticks"),
             colorway = c("black","grey40","grey80"))
   
   p2
   
 })
 

######
###underground
######
summary_undrgnd <-  reactive({
  if(is.null(input$runs_undrgrnd_file)){
    return(NULL)
  }else{
    read.csv(input$runs_undrgrnd_file$datapath)
  }
})


ip_undrgnd <-  reactive({
  if(is.null(input$ip_undrgrnd_file)){
    return(NULL)
  }else{
    read.csv(input$ip_undrgrnd_file$datapath)
  }
})
## error output in undrgrnd

undrgnd_errors_output <-  eventReactive(input$run_anls_undrgrnd,{
  if(as.integer(!is.null(summary_undrgnd())) + as.integer(!is.null(ip_undrgnd())) + 
                as.integer(isolate(site_anls_run_stat$status)) == 0){
    createAlert(session,"undrgrnd_error","undrgrnd_alert",title = "Oops",
                content ="You should either run contextual analysis or
                upload summary and IP data sets.",append = TRUE)
  }
  if(is.null(summary_undrgnd())){
    createAlert(session,"undrgrnd_error","undrgrnd_alert",title = "Oops",
                content ="You have not uploaded the summary file.",append = TRUE)
  }else if(is.null(ip_undrgnd())){
    createAlert(session,"undrgrnd_error","undrgrnd_alert",title = "Oops",
                content ="You have not uploaded the ignition path file.",append = TRUE)
  }else{
    closeAlert(session,"undrgrnd_alert")
  }
  
})

undrgnd_df <-  eventReactive(input$run_anls_undrgrnd,{
  req( site_anls_run_stat$status | (!is.null(summary_undrgnd()) & !is.null(ip_undrgnd())) )
  
  if( !is.null(summary_undrgnd()) & !is.null(ip_undrgnd()) ){
    arg1 <- summary_undrgnd()
    arg2 <- ip_undrgnd()
  }else{
    arg1 <- site_analysis()$runs
    arg2 <- site_analysis()$IP
  }
  progress <- shiny::Progress$new()
  progress$set(message = "Running the underground analysis \n", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  # calculating the time that the function goes thorough
  residence <- 0.871*input$diamtr_undrgrnd^1.875
  ROS <- mean(arg1$ros_kph)/3.6
  Ta <- round(input$dist_undrgrnd/ROS+residence)
  prg_steps <- Ta + 500
  
  updateProgress <- function(value = prg_steps, detail = NULL) {
    progress$inc(amount = 1/value, detail = detail)
  }
 

  
  Heat <-  underground(arg1, arg2, 
              diameter = input$diamtr_undrgrnd, surface = 677, 
              percentile = input$risk_per_undrgrnd,
              RH = input$rel_hum_undrgrnd, moisture = input$moist_undrgrnd,
              distance = input$dist_undrgrnd, trail = 500, var = 10,
              Pressure = input$sea_level_press_undrgrnd, 
              Altitude = input$alt_undrgrnd, texture = input$texture_undrgrnd, 
              peat = input$orgnic_temp_undrgrnd,
              grain = "fine", unfrozen = 1, depth = input$depth_undrgrnd, 
              soilTemp = input$strt_temp_undrgrnd,updateProgress = updateProgress)
 Mheat <- Heat %>%
   select(t, tempAir, tempSoil, mortality, qc, qR, Q)%>%
   mutate(qc = qc/1000,
          qR = qR/1000,
          Q = Q/1000,
          count = ceiling(t/2),
          minutes = t/60) %>%
   group_by(t) %>%
   summarise_all(list(Q = quantile), probs = input$risk_per_undrgrnd)
 
 mort <- Heat%>%
   group_by(t) %>%
   summarise_all(mean)
 
 fnl <-  list(Heat = Heat, Mheat = Mheat , mort = mort)
 return(fnl)

 })

## printing the summary
output$undrgrnd_text <- renderUI({
  undrgnd_errors_output()
  m <- 100*max(undrgnd_df()$mort$mortality)
  Tm <- round(max(undrgnd_df()$Mheat$tempSoil_Q),0)
  
  out <- HTML(paste(
    paste(" The soil was heated to", Tm, "degrees Celsius"),
    paste("Mortality was", m, "% likely"),sep = "<br/>"))
  return(out)
})
## plotting the underground analysis

output$undrgrnd_plt_1 <- renderPlotly({
  req(undrgnd_df())
  p1 <- plot_ly(x = ~undrgnd_df()$Mheat$minutes_Q) %>% 
    add_lines(y = ~undrgnd_df()$Mheat$Q_Q,name = "Total flux") %>% 
    add_lines(y = ~undrgnd_df()$Mheat$qR_Q,name = "Radiation") %>% 
    add_lines(y = ~undrgnd_df()$Mheat$qc_Q,name = "Convection") %>% 
    layout( 
      xaxis = list(title = "Minutes",
                   showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = "kW/m",
                   tickfont=list(family = "Arial",size=10),
                   showgrid = TRUE,showline = TRUE,zeroline = FALSE,
                   linewidth = 3,mirror = "ticks"),
      colorway = c("black","grey40","grey80")) 
  p1

  })


output$undrgrnd_plt_2 <- renderPlotly({
  req(undrgnd_df())
  p2 <- plot_ly(x = ~undrgnd_df()$Mheat$minutes_Q) %>% 
    add_lines(y = ~undrgnd_df()$Mheat$tempAir_Q,name = "Air temperature (°C)") %>% 
    add_lines(y = ~undrgnd_df()$Mheat$tempSoil_Q,name = "Soil temperature (°C)") %>% 
    layout(
      xaxis = list(title = "Minutes",
                   showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = "Temperatures (°C)",
                   tickfont=list(family = "Arial",size=10),
                   showgrid = TRUE,showline = TRUE,zeroline = FALSE,
                   linewidth = 3,mirror = "ticks"),
      colorway = c("black","grey40","grey80"))  
  
  p2
})
  
  
  
}

shinyApp(ui, server)