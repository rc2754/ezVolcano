library(shiny)
library(plotly)
library(data.table)
library(dplyr)
library(shinyjs)
library(readxl)

ui <- fluidPage(
  useShinyjs(),
  tags$style(type="text/css", "#plot.recalculating { opacity: 1.0 !important; }"),
  tags$head(
    tags$meta(name = "ezVolcano", content = "Interactive volcano plot tool"),
    tags$title("ezVolcano")
  ),
  tags$style(HTML("div.sticky {
  position: -webkit-sticky;
  position: sticky;
  top: 50px;
  z-index: 1;
}")),

  tabsetPanel(
    tabPanel("Plot",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Load a data file", accept = c(".csv", ".txt", ".tsv", ".xlsx")), verbatimTextOutput("error_message1"),
                 fluidRow(
                   column(12, tags$hr(style = "border-top: 2px solid black;")),

                   column(12, fileInput("load_state", "Load save file", accept = c(".rds"))), verbatimTextOutput("error_message2"),
                   column(12, downloadButton("save_state", "Save plot")),
                   column(6, textInput("prefix", "Save file prefix", value = "")),
                   column(6, textInput("suffix", "Save file suffix", value = "")),
                   column(12, tags$hr(style = "border-top: 2px solid black;")),
                   column(12, fileInput("load_config", "Load config file", accept = c(".rds"))), verbatimTextOutput("error_message3"),
                   column(12, downloadButton("save_config", "Save configuration")),
                   column(12, tags$hr(style = "border-top: 2px solid black;")),

                   column(12, selectInput('label', 'Select labels', choices = NULL, multiple = TRUE)),
                   column(6, actionButton("add_batch", "Add batch labels")),
                   column(6, actionButton("clear_label", "Clear labels")),
                   column(12, tags$hr(style = "border-top: 2px solid black;")),

                   column(12, textInput("plot_title", "Plot title", value = "")),
                   column(6, textInput("x_label", "x-axis label", value = "")),
                   column(6, textInput("y_label", "y-axis label", value = "-log<sub>10</sub>(P.adj)")),
                   column(6, checkboxInput("use_zscore", "Use Z-score", TRUE)),
                   column(6, checkboxInput("flip_x", "Flip x-axis", FALSE)),
                   column(6, checkboxInput("center_x", "Center x-axis", TRUE)),
                   column(6, checkboxInput("center_y", "Center y-axis", FALSE)),
                   column(12, tags$hr(style = "border-top: 2px solid black;")),

                   column(12, numericInput("stat_threshold", "Stat threshold", value = 3, min = 0, step = 0.1)),
                   column(6, numericInput("effect_size_left", "Left threshold", value = -1.5, step = 0.1)),
                   column(6, numericInput("effect_size_right", "Right threshold", value = 1.5, step = 0.1)),
                   column(6, numericInput("effect_size_left_z", "Left threshold (Z-score)", value = -1.96, step = 0.1)),
                   column(6, numericInput("effect_size_right_z", "Right threshold (Z-score)", value = 1.96, step = 0.1)),

                   column(6, checkboxInput("hyperbola", "Hyperbolic", TRUE)),
                   column(6, numericInput("curvature", "Curvature", value = 1, step = 0.1, min = 0.1)),
                   column(12, tags$hr(style = "border-top: 2px solid black;")),

                   column(6, numericInput("plot_width", "Plot width (px)", value = 1200)),
                   column(6, numericInput("plot_height", "Plot height (px)", value = 800)),
                   column(12, tags$hr(style = "border-top: 2px solid black;")),

                   column(4, textInput("left_color", "Left color", value = "#0000FF")),
                   column(4, textInput("middle_color", "Middle color", value = "#808080")),
                   column(4, textInput("right_color", "Right color", value = "#FF0000")),
                   column(6, numericInput("point_shape", "Point shape", value = 0)),
                   column(6, numericInput("point_size", "Point size", value = 8)),
                   column(6, textInput("point_outline_color", "Point border color")),
                   column(6, numericInput("point_outline_width", "Point border width", value = 0)),
                   column(6, numericInput("point_opacity", "Point opacity", min = 0, max = 1, value = 1, step = 0.05)),
                   column(12, tags$hr(style = "border-top: 2px solid black;")),

                   column(6, textInput("label1", "Label 1 (Top Left)", value = "")),
                   column(6, textInput("label2", "Label 2 (Top Right)", value = "")),
                   column(6, textInput("label3", "Label 3 (Bottom Left)", value = "")),
                   column(6, textInput("label4", "Label 4 (Bottom Right)", value = "")),
                   column(6, textInput("label1_color", "Label 1 color", value = "red")),
                   column(6, textInput("label2_color", "Label 2 color", value = "red")),
                   column(6, textInput("label3_color", "Label 3 color", value = "red")),
                   column(6, textInput("label4_color", "Label 4 color", value = "red")),
                   column(6, numericInput("label1_size", "Label 1 size", value = 14)),
                   column(6, numericInput("label2_size", "Label 2 size", value = 14)),
                   column(6, numericInput("label3_size", "Label 3 size", value = 14)),
                   column(6, numericInput("label4_size", "Label 4 size", value = 14)),
                   column(6, numericInput("label1_padding_x", "Label 1 padding_x", value = 0.001, max = 1, step = 0.001)),
                   column(6, numericInput("label1_padding_y", "Label 1 padding_y", value = 0.001, max = 1, step = 0.001)),
                   column(6, numericInput("label2_padding_x", "Label 2 padding_x", value = 0.001, max = 1, step = 0.001)),
                   column(6, numericInput("label2_padding_y", "Label 2 padding_y", value = 0.001, max = 1, step = 0.001)),
                   column(6, numericInput("label3_padding_x", "Label 3 padding_x", value = 0.001, max = 1, step = 0.001)),
                   column(6, numericInput("label3_padding_y", "Label 3 padding_y", value = 0.001, max = 1, step = 0.001)),
                   column(6, numericInput("label4_padding_x", "Label 4 padding_x", value = 0.001, max = 1, step = 0.001)),
                   column(6, numericInput("label4_padding_y", "Label 4 padding_y", value = 0.001, max = 1, step = 0.001)),
                   column(12, tags$hr(style = "border-top: 2px solid black;")),

                   column(12, numericInput("zero_pad", "Zeroline padding", value = 0.5, step = 0.05)),
                   column(6, numericInput("xlim1", "x-axis limit left", value = NA, step = 0.5)),
                   column(6, numericInput("xlim2", "x-axis limit right", value = NA, step = 0.5)),
                   column(6, numericInput("ylim1", "y-axis limit left", value = NA, step = 0.5)),
                   column(6, numericInput("ylim2", "y-axis limit right", value = NA, step = 0.5)),
                   column(6, numericInput("xlim_pad", "x-range padding", value = 1.1, step = 0.1)),
                   column(6, numericInput("ylim_pad", "y-range padding", value = 1.1, step = 0.1)),
                   column(12, tags$hr(style = "border-top: 2px solid black;")),

                   column(6, numericInput("annotlab_size", "Annotation label size", value = 12, step = 1)),
                   column(6, textInput("annotlab_col", "Annotation label color", value = "black")),
                   column(6, numericInput("annotarrow_size", "Annotation line width", value = 2, step = 1)),
                   column(6, textInput("annotarrow_col", "Annotation line color", value = "black")),
                   column(12, tags$hr(style = "border-top: 2px solid black;")),

                   column(12, textInput("stat_threshold_color", "Stat threshold line color", value = "gray")),
                   column(6, textInput("effect_size_left_color", "Left threshold line color", value = "gray")),
                   column(6, textInput("effect_size_right_color", "Right threshold line color", value = "gray")),
				   column(12, textInput("hyperbola_color", "Hyperbolic line color", value = "gray")),

                   column(12, numericInput("stat_threshold_size", "Stat threshold line width", value = 1)),
                   column(6, numericInput("effect_size_left_size", "Left threshold line width", value = 1)),
                   column(6, numericInput("effect_size_right_size", "Right threshold line width", value = 1)),
				   column(12, numericInput("hyperbola_size", "Hyperbolic line width", value = 1)),

                   column(12, textInput("stat_threshold_pattern", "Stat threshold line pattern", value = "dash")),
                   column(6, textInput("effect_size_left_pattern", "Left threshold line pattern", value = "dash")),
                   column(6, textInput("effect_size_right_pattern", "Right threshold line pattern", value = "dash")),
                   column(12, textInput("hyperbola_pattern", "Hyperbolic line pattern", value = "dash")),
                   column(12, tags$hr(style = "border-top: 2px solid black;")),

                   column(12, numericInput("title_size", "Title Size", value = 24)),
                   column(6, numericInput("title_x", "Title X position", value = 0.5, step = 0.01)),
                   column(6, numericInput("title_y", "Title Y position", value = 0.95, step = 0.01)),

                   column(6, numericInput("x_label_size", "x-axis label font size", value = 15)),
                   column(6, numericInput("y_label_size", "y-axis label font size", value = 15)),
                   column(6, numericInput("x_scale_size", "x-axis scale font size", value = 14)),
                   column(6, numericInput("y_scale_size", "y-axis scale font size", value = 14)),
                   column(6, numericInput("x_ticks_interval", "x-axis tick interval", value = 2)),
                   column(6, numericInput("y_ticks_interval", "y-axis tick interval", value = 1)),

                   column(6, numericInput("x_tick_length", "x-axis tickmark length", value = 5)),
                   column(6, numericInput("y_tick_length", "y-axis tickmark length", value = 5)),

                   column(6, numericInput("x_line_thickness", "x-axis line width", value = 2)),
                   column(6, numericInput("y_line_thickness", "y-axis line width", value = 2)),

                   column(12, tags$hr(style = "border-top: 2px solid black;")),

                   column(6, numericInput("l_margin", "Left margin", value = 0)),
                   column(6, numericInput("r_margin", "Right margin", value = 0)),
                   column(6, numericInput("t_margin", "Top margin", value = 100)),
                   column(6, numericInput("b_margin", "Bottom margin", value = 0)),
                   column(6, numericInput("x_standoff", "x-axis label standoff", value = 20)),
                   column(6, numericInput("y_standoff", "y-axis label standoff", value = 20)),

                   column(12, tags$hr(style = "border-top: 2px solid black;")),

                   column(6, checkboxInput("show_legend", "Show Legend", value = TRUE)),
                   column(6, checkboxInput("legend_traceorder", "Flip legend order", value = FALSE)),
                   column(6, textInput("legend_title", "Legend title", value = "Significance")),
                   column(6, numericInput("legend_title_size", "Legend title size", value = 15)),
                   column(6, textInput("legend_con_l", "Left condition label", value = "Downregulated")),
                   column(6, textInput("legend_con_r", "Right condition label", value = "Upregulated")),
                   column(6, textInput("legend_con_m", "Middle condition label", value = "Not significant")),
                   column(6, numericInput("legend_label_size", "Legend label size", value = 12)),
                   column(6, numericInput("legend_x", "Legend x position", value = 1.1, step = 0.01)),
                   column(6, numericInput("legend_y", "Legend y position", value = 1, step = 0.01)),
                   column(6, textInput("legend_box_color", "Legend box border color", value = "white")),
                   column(6, numericInput("legend_box_width", "Legend box border width", value = 1, step = 0.1)),
                   column(6, textInput("legend_box_bgcolor", "Legend box bckgrnd color", value = "white")),


                   column(12, tags$hr(style = "border-top: 2px solid black;")),

                   column(6, textInput("label_point_outline_color", "Labeled point outline color", value = "black")),
                   column(6, numericInput("label_point_outline_width", "Labeled point outline width", value = 0.5, step = 0.01)),

                   column(12, tags$hr(style = "border-top: 2px solid black;")),
                   column(6, textInput("font_title", "Title font", value = "Arial")),
                   column(6, textInput("font_axis", "Axis scale font", value = "Arial")),
                   column(6, textInput("font_axis_label", "Axis label font", value = "Arial")),
                   column(6, textInput("font_corner_lab", "Corner labels font", value = "Arial")),
                   column(6, textInput("font_legend", "Legend font", value = "Arial")),
                   column(6, textInput("font_legend_label", "Legend labels font", value = "Arial")),
                   column(6, textInput("font_annotlab", "Annotation labels font", value = "Arial")),

                   column(12, tags$hr(style = "border-top: 2px solid black;")),

                   column(6, textInput("color_title", "Title color")),
                   column(6, textInput("annotlab_col", "Annotation label color")),
                   column(6, textInput("color_axis", "Axis scale color")),
                   column(6, textInput("color_axis_label", "Axis label color")),
                   column(6, textInput("color_axisline", "Axis line color")),
                   column(6, textInput("color_tick", "Axis tick color")),
                   column(6, textInput("color_legend", "Legend color")),
                   column(6, textInput("color_legend_label", "Legend labels color"))
                 ),
                 width = 3
               ),
               tagAppendAttributes(mainPanel(class = "sticky",
                                             plotlyOutput('plot'),)
               )
             )
    ),


    tabPanel("INSTRUCTIONS",
             fluidPage(
               h3("Input Data Instructions"),
               p("ezVolcano accepts tabular data as TXT, CSV, TSV, and XLSX files."),
               br(),
               h4("Data Format Requirements"),
               p("The data file must be structured as follows:"),

               tags$ul(
                 tags$li(HTML("<b>The first column</b> must contain gene/protein names or some other identifier.")),
                 tags$li(HTML("<b>The second column</b> must represent the x-axis, such as effect size, fold change, or z-scores.")),
                 tags$li(HTML("<b>The third column</b> must represent the y-axis, such as p-value or FDR.")),
                 tags$li("Additional columns beyond the first three will be ignored.")
               ),

               br(),
               h4("Customizing Data Point Labels"),
               p("Once your data is loaded, you can label your data points the following ways:"),

               tags$ul(
                 tags$li("Click on data points to add and remove labels. You can drag these labels around to reposition them."),
                 tags$li(HTML("Use the <b>Select labels</b> selectizer to add and remove labels.")),
                 tags$li("Use the batch input button to paste in a list of names (space or comma delimited).")
               ),

               br(),
               h4("Special x-axis settings"),
               p("The x-axis can be swapped between two scales:"),

               tags$ul(
                 tags$li("ezVolcano automatically calculates the Z-Score of your fold changes and uses that by default as the x-axis scale. For example, a Z-Score of ±1.96 corresponds to the 95% confidence interval."),
                 tags$li("The x-axis cutoff thresholds for Z-Score and original fold change are set separately and do not interconvert.")
               ),

               br(),
               h4("Saving Your Work"),
               tags$ul(
                 tags$li(HTML("You can save the entire plot (data points, labels, and aesthetic settings), as a <b>save file</b>. This can be re-loaded later to restore all data point labels and plot customizations.")),
                 tags$li(HTML("You can also save only the aesthetic settings as a <b>configuration file</b>. This can be used as a template to quickly adjust plots in the future.")),

                 tags$li("To export your plot as a .PNG image, click the camera button in the Plotly toolbar.")
               ),

               br(),
               h4("Examples"),

               downloadButton("example_data", "Example data file"),
               downloadButton("example_save", "Example save file"),
               downloadButton("example_config", "Example config file"),
               downloadButton("param_description", "Description of all plot parameters")
             )
    ),


    tabPanel("Credits",
             fluidPage(
               h3("About ezVolcano"),
               p("ezVolcano is a Shiny R app tool for charting volcano plots. Aesthetics are highly customizable; click on datapoints to add labels and drag these labels around."),

               br(),
               h4("Contact"),
               p("Created and maintained by Richard Chen. For questions and bug reports:"),

               tags$ul(
                 tags$li(a(href = "https://github.com/rc2754/ezVolcano/issues", "https://github.com/rc2754/ezVolcano/issues"))
               ),

               br(),
               h4("Source"),
               p("Source code: ",
                 a(href = "https://github.com/rc2754/ezVolcano", "https://github.com/rc2754/ezVolcano")
               ),
               p("ezVolcano is easy to set up locally (see the GitHub tutorial)."),

               br(),
               h4("Acknowledgement"),
               p("To enable interactive labels, source code was adapted from the easylabel R package by Myles Lewis (https://github.com/myles-lewis/easylabel).")
             )
    )
  )
)


