library(shiny)
library(plotly)
library(data.table)
library(dplyr)
library(shinyjs)
library(readxl)

server <- function(input, output, session) {
  is_save <- reactiveVal(0)
  plot_filename <- reactiveVal("")

  output$example_data <- downloadHandler(
    filename = function() {
      "example_data.csv"
    },
    content = function(file) {
      file.copy("www/example_data.txt", file)}
  )

  output$example_save <- downloadHandler(
    filename = function() {
      "example_save.rds"
    },
    content = function(file) {
      file.copy("www/example_save.rds", file)}
  )

  output$example_config <- downloadHandler(
    filename = function() {
      "example_config.rds"
    },
    content = function(file) {
      file.copy("www/example_config.rds", file)}
  )

  # Reactive values for storing labels and their positions
  data <- reactiveVal()
  labels <- reactiveValues(list = list())
  labelsxy <- reactiveValues(list = list())

  # Define labelchoices as a reactive expression
  labelchoices <- reactive({
    unique(data()$Gene)
  })

  # Modal for batch gene input
  batchModal <- function() {
    modalDialog(
      textAreaInput("batchlabs", "Type or paste a list of genes",
                    height = '300px'),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("batch_ok", "OK")
      )
    )
  }

  # Open modal when button pressed
  observeEvent(input$add_batch, {
    showModal(batchModal())
  })

  # Add batch genes
  observeEvent(input$batch_ok, {
    req(input$batchlabs)
    if (!is.null(input$batchlabs)) {
      batch_genes <- unlist(strsplit(input$batchlabs, ",| |\n"))
      batch_genes <- batch_genes[batch_genes %in% labelchoices()]  # Check against available genes
      if (length(batch_genes) > 0) {
        current_labels <- lapply(labels$list, function(x) x$name)
        new_labels <- setdiff(batch_genes, current_labels)  # Only add new labels
        for (gene in new_labels) {
          gene_data <- data()[Gene == gene]
          x_val <- gene_data$x[1]
          labels$list <- c(labels$list, list(list(name = gene, x = x_val, y = -log10(gene_data$y[1]))))
          labelsxy$list <- c(labelsxy$list, list(list(ax = 50, ay = -50)))
        }
      }
      removeModal()
    }
  })

  observeEvent(input$clear_label, {
    updateSelectizeInput(session, "label", selected = character(0))
  })

  observeEvent(input$flip_x, {
    for (gene in names(labelsxy$list)) {
      labelsxy$list[[gene]]$ax <- -labelsxy$list[[gene]]$ax
    }
  })

  main_plotly <- function(data) {
    if (is.null(data)) {
      return(NULL)  # Avoid plotting when data is NULL
    }
    stat_threshold <- -log10(input$stat_threshold)

    # Determine the x-axis label. If user has set input$x_label, use that, otherwise default.
    x_label <- if (!is.null(input$x_label) && input$x_label != "") {
      input$x_label  # Use the user-provided label if available
    } else {
      if (input$use_zscore) {
        "FoldChange Z-Score"  # Default label for zscore
      } else {
        "log<sub>2</sub>(FoldChange)"  # Default label for log2 fold change
      }
    }

    if (input$use_zscore && input$flip_x) {
      temp_x <- data$zscore_flip_x  # If both zscore and flip_x are selected
    } else if (input$use_zscore) {
      temp_x <- data$zscore  # If only zscore is selected
    } else if (input$flip_x) {
      temp_x <- data$flip_x  # If only flip_x is selected
    } else {
      temp_x <- data$x  # If neither are selected, use the original x
    }

    # Only run the loop if labels$list has elements
    if (length(labels$list) > 0) {
      # Loop through the labels$list to match gene names and update x values
      for (i in 1:length(labels$list)) {
        gene_name <- labels$list[[i]]$name
        gene_index <- which(data$Gene == gene_name)  # Find the index of the gene in data$Gene

        if (input$use_zscore && input$flip_x) {
          labels$list[[i]]$x <- data$zscore_flip_x[gene_index]  # Update x with zscore_flip_x
        } else if (input$use_zscore) {
          labels$list[[i]]$x <- data$zscore[gene_index]  # Update x with zscore
        } else if (input$flip_x) {
          labels$list[[i]]$x <- data$flip_x[gene_index]  # Update x with flip_x
        } else {
          labels$list[[i]]$x <- data$x[gene_index]  # Update x with original x
        }
      }
    }

    if (input$center_x) {
      if (!is.na(input$xlim1) || !is.na(input$xlim2)) {
        max_x <- max(abs(c(input$xlim1, input$xlim2)), na.rm = TRUE)
      } else {
        max_x <- max(abs(temp_x), na.rm = TRUE)
      }
      xlim <- c(-max_x, max_x)
    } else {
      if (!is.na(input$xlim1) || !is.na(input$xlim2)) {
        xlim <- c(
          ifelse(!is.na(input$xlim1), input$xlim1, min(temp_x, na.rm = TRUE)),
          ifelse(!is.na(input$xlim2), input$xlim2, max(temp_x, na.rm = TRUE))
        )
      } else {
        xlim <- range(temp_x, na.rm = TRUE)
      }
    }


    xlim <- c(xlim[1] * input$xlim_pad, xlim[2] * input$xlim_pad)

    temp_y <- -log10(data$y)

    if (input$center_y) {
      if (!is.na(input$ylim1) || !is.na(input$ylim2)) {
        max_y <- max(abs(c(input$ylim1, input$ylim2)), na.rm = TRUE)
      } else {
        max_y <- max(abs(temp_y), na.rm = TRUE)
      }
      ylim <- c(-max_y, max_y)
    } else {
      if (!is.na(input$ylim1) || !is.na(input$ylim2)) {
        ylim <- c(
          ifelse(!is.na(input$ylim1), input$ylim1, min(temp_y, na.rm = TRUE)),
          ifelse(!is.na(input$ylim2), input$ylim2, max(temp_y, na.rm = TRUE))
        )
      } else {
        ylim <- range(temp_y, na.rm = TRUE)
        ylim <- c(ylim[1] - input$zero_pad, ylim[2])  # Apply zero padding
      }
    }

    ylim <- c(ylim[1] * input$ylim_pad, ylim[2] * input$ylim_pad)



    if (input$plot_title != "") {
      temp_title <- input$plot_title
    } else {
      temp_title <- plot_filename()
    }

    if (input$use_zscore) {
      effect_size_left_temp <- input$effect_size_left_z
      effect_size_right_temp <- input$effect_size_right_z
    } else {
      effect_size_left_temp <- input$effect_size_left
      effect_size_right_temp <- input$effect_size_right
    }


    # Convert the inputs to strings (in case they are not already)
    legend_con_m <- as.character(input$legend_con_m)
    legend_con_l <- as.character(input$legend_con_l)
    legend_con_r <- as.character(input$legend_con_r)

    # Create a separate condition label column for the legend
    data$condition <- factor(ifelse(-log10(data$y) < input$stat_threshold, legend_con_m,
                                    ifelse(temp_x < effect_size_left_temp, legend_con_l,
                                           ifelse(temp_x > effect_size_right_temp, legend_con_r, legend_con_m))),
                             levels = c(legend_con_m, legend_con_l, legend_con_r))


    # Define color mapping based on condition names using setNames()
    color_mapping <- setNames(c(input$middle_color, input$left_color, input$right_color),
                              c(legend_con_m, legend_con_l, legend_con_r))

    # Generate the base plot
    p <- plot_ly(data, x = temp_x, y = ~-log10(y), type = 'scatter', mode = 'markers', color = ~condition,  colors = color_mapping,
                 marker = list(opacity = input$point_opacity, symbol = input$point_shape, size = input$point_size, line = list(color = input$point_outline_color, width = input$point_outline_width)),
                 text = ~Gene, hoverinfo = 'x+y+text', key = ~Gene, source = "volcano_plot", width = input$plot_width, height = input$plot_height) %>%
      layout(
        title = list(
          text = temp_title,
          font = list(size = input$title_size, family = input$font_title, color = input$color_title),
          x = input$title_x,
          y = input$title_y,
          xref = "paper"
        ),
        xaxis = list(
          title = list(
            text = x_label,
            standoff = input$x_standoff,
            font = list(  # Correct placement for axis title font
              size = input$x_label_size,
              family = input$font_axis_label,
              color = input$color_axis_label
            )
          ),
          range = xlim,
          showgrid = FALSE,
          zeroline = FALSE,
          ticklen = input$x_tick_length,
          linecolor = input$color_axisline,
          tickfont = list(  # Correct placement for tick font
            size = input$x_scale_size,
            family = input$font_axis,
            color = input$color_axis
          ),
          tickcolor = input$color_tick,
          dtick = input$x_ticks_interval,
          linewidth = input$x_line_thickness
        ),
        yaxis = list(
          title = list(
            text = input$y_label,
            standoff = input$y_standoff,
            font = list(  # Correct placement for axis title font
              size = input$y_label_size,
              family = input$font_axis_label,
              color = input$color_axis_label
            )
          ),
          range = ylim,
          showgrid = FALSE,
          zeroline = FALSE,
          ticklen = input$y_tick_length,
          linecolor = input$color_axisline,
          tickfont = list(  # Correct placement for tick font
            size = input$y_scale_size,
            family = input$font_axis,
            color = input$color_axis
          ),
          tickcolor = input$color_tick,
          dtick = input$y_ticks_interval,
          linewidth = input$y_line_thickness
        ),
        shapes = list(
          list(type = "line",
               x0 = 0, x1 = 1, xref = "paper",
               y0 = stat_threshold, y1 = stat_threshold,
               line = list(color = input$stat_threshold_color, width = input$stat_threshold_size, dash = input$stat_threshold_pattern)),
          list(type = "line",
               x0 = effect_size_left_temp, x1 = effect_size_left_temp,
               y0 = 0, y1 = 1, yref = "paper",
               line = list(color = input$effect_size_left_color, width = input$effect_size_left_size, dash = input$effect_size_left_pattern)),
          list(type = "line",
               x0 = effect_size_right_temp, x1 = effect_size_right_temp,
               y0 = 0, y1 = 1, yref = "paper",
               line = list(color = input$effect_size_right_color, width = input$effect_size_right_size, dash = input$effect_size_right_pattern))
        ),
        annotations = c(
          create_annotations(labels$list, labelsxy$list),
          list(
            list(x = 0 + input$label1_padding_x, y = 1 - input$label1_padding_y, text = input$label1, showarrow = F, xref = 'paper', yref = 'paper', font = list(size = input$label1_size, color = input$label1_color)),
            list(x = 1 - input$label2_padding_x, y = 1 - input$label2_padding_y, text = input$label2, showarrow = F, xref = 'paper', yref = 'paper', font = list(size = input$label2_size, color = input$label2_color)),
            list(x = 0 + input$label3_padding_x, y = 0 + input$label3_padding_y, text = input$label3, showarrow = F, xref = 'paper', yref = 'paper', font = list(size = input$label3_size, color = input$label3_color)),
            list(x = 1 - input$label4_padding_x, y = 0 + input$label4_padding_y, text = input$label4, showarrow = F, xref = 'paper', yref = 'paper', font = list(size = input$label4_size, color = input$label4_color))
          )
        ),
        legend = list(
          traceorder = "normal",
          title = list(
            text = input$legend_title,  # Set the legend title here
            font = list(
              size = input$legend_title_size, family = input$font_legend, color = input$color_legend  # Set the font size of the title
            )
          ),
          x = input$legend_x,    # Horizontal position (0 = far left, 1 = far right)
          y = input$legend_y,    # Vertical position (0 = bottom, 1 = top)
          xanchor = "center",    # Anchor point for the legend box
          yanchor = "middle",    # Anchor point for the legend box
          font = list(            # Set font size for the legend labels
            size = input$legend_label_size, family = input$font_legend_label, color = input$color_legend_label  # Adjust this for label font size
          ),
          bordercolor = input$legend_box_color,  # Color of the legend box outline
          borderwidth = input$legend_box_width,  # Line width of the legend box outline
          bgcolor = input$legend_box_bgcolor
        ),
        showlegend = TRUE,
        margin = list(l = input$l_margin, t = input$t_margin, r = input$r_margin, b = input$b_margin)
      ) %>%
      config(edits = list(annotationTail = TRUE))


    if (!input$show_legend) {
      p <- p %>% layout(showlegend = FALSE)
    }

    if (!input$legend_traceorder) {
      p <- p %>% layout(
        legend = list(
          traceorder = "reversed"
        )
      )
    }

    p <- p %>%
      add_trace(
        x = unlist(lapply(labels$list, `[[`, "x")),
        y = unlist(lapply(labels$list, `[[`, "y")),
        type = 'scatter',
        mode = 'markers',
        marker = list(size = input$point_size,  color = 'rgba(0,0,0,0)', line = list(width = input$label_point_outline_width, color = input$label_point_outline_color)),
        inherit = FALSE,
        showlegend = FALSE
      )


    event_register(p, "plotly_click")  # Register event tracking for clicks
    return(p)
  }

  # Function to create annotations based on stored labels
  create_annotations <- function(label_list, label_xy_list) {
    if (length(label_list) == 0) {
      return(list())  # Return an empty list if no labels
    }

    annotations <- lapply(seq_along(label_list), function(i) {
      ax_val <- 50
      ay_val <- -50

      if (!is.null(label_xy_list[[i]])) {
        ax_val <- label_xy_list[[i]]$ax
        ay_val <- label_xy_list[[i]]$ay
      }

      # Default x position
      x_pos <- label_list[[i]]$x

      list(
        x = x_pos,
        y = label_list[[i]]$y,
        text = label_list[[i]]$name,
        showarrow = TRUE,
        arrowhead = 0,
        ax = ax_val,
        ay = ay_val,
        font = list(size = input$annotlab_size, color = input$annotlab_col, family = input$font_annotlab),
        arrowcolor = input$annotarrow_col,
        arrowwidth = input$annotarrow_size
      )
    })
    return(annotations)
  }

  observe({
    s <- event_data("plotly_click", source = "volcano_plot", priority = "event")
    req(s)

    s_key <- unlist(s$key)
    s_xy <- paste(s$x, s$y, sep = "_") # Concatenate x and y coordinates
    isolate({
      existing_index <- which(s_xy == sapply(labels$list, function(x) paste(x$x, x$y, sep = "_")))

      if (is.null(s$key)) {
        # If s$key is NULL, remove the label if it exists
        if (length(existing_index) > 0) {
          labels$list <- labels$list[-existing_index]
          labelsxy$list <- labelsxy$list[-existing_index]
        }
      } else {
        if (length(existing_index) > 0) {
          labels$list <- labels$list[-existing_index]
          labelsxy$list <- labelsxy$list[-existing_index]
        } else {
          x_pos <- s$x
          labels$list <- c(labels$list, list(list(name = s_key, x = x_pos, y = s$y)))
          labelsxy$list <- c(labelsxy$list, list(list(ax = 50, ay = -50)))
        }
      }
    })
  })

  # Store the location of moved labels by detecting Plotly relayout events
  observeEvent(event_data("plotly_relayout", source = 'volcano_plot'), {
    s <- event_data("plotly_relayout", source = 'volcano_plot')

    if (grepl("annotations\\[[0-9]+\\]\\.ax", names(s)[1])) {
      w <- as.numeric(gsub("annotations\\[|\\]\\.ax", "", names(s)[1])) + 1
      labelsxy$list[[w]] <- list(ax = s[[1]], ay = s[[2]])
    }
  })

  # Selectize genes
  input_label <- reactive({ input$label }) %>% debounce(500)

  # Update labels from selectize
  observeEvent(input_label(), {
    currentsel <- input_label()
    labelchoices <- unique(data()$Gene)
    convert_labs <- unlist(lapply(labels$list, function(x) x$name))

    if (length(currentsel) == 0) {
      labels$list <- list()
      labelsxy$list <- list()
    } else if (any(!currentsel %in% convert_labs)) {
      addsel <- currentsel[!currentsel %in% convert_labs]

      for (i in addsel) {
        gene_data <- data()[Gene == i]
        x_val <- gene_data$x[1]
        labels$list <- c(labels$list, list(list(name = i, x = x_val, y = -log10(gene_data$y[1]))))
        labelsxy$list <- c(labelsxy$list, list(list(ax = 50, ay = -50)))
      }
    } else if (any(!convert_labs %in% currentsel)) {
      removesel <- convert_labs[!convert_labs %in% currentsel]
      removenum <- unlist(lapply(removesel, function(i) which(unlist(lapply(labels$list, function(x) x$name)) == i)))

      labels$list <- labels$list[-removenum]
      labelsxy$list <- labelsxy$list[-removenum]
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)



  # update plotly labels
  observeEvent(labels$list, {
    labs <- unlist(lapply(labels$list, function(x) x$name))
    current_xy <- labelsxy$list
    # Annotate gene labels
    annot <- create_annotations(labels$list, labelsxy$list)
    labelsxy$list <- lapply(annot, function(i) list(ax = i$ax, ay = i$ay))
    names(labelsxy$list) <- labs
    plotlyProxy('plot', session) %>%
      plotlyProxyInvoke("relayout",
                        list(annotations = annot))
    sel_labels <- input$label
    sel_labels <- which(unique(data()$Gene) %in% sel_labels)
    if (any(!unique(unlist(lapply(labels$list, function(x) x$name))) %in% unique(data()$Gene)[sel_labels]) | any(!unique(data()$Gene)[sel_labels] %in% unique(unlist(lapply(labels$list, function(x) x$name))))) {
      updateSelectizeInput(session, 'label', choices = unique(data()$Gene),
                           selected = unlist(lapply(labels$list, function(x) x$name)), server = TRUE)
    }
  })

  # Render the volcano plot
  output$plot <- renderPlotly({
    req(data())
    df <- data()  # Access the latest dataframe from the reactive value
    main_plotly(df)  # Pass the dataframe to your plot function
  })


  output$save_state <- downloadHandler(
    filename = function() {

      if (is_save() == 2) {
        file_name <- tools::file_path_sans_ext(basename(input$load_state$name))  # Extract file name without extension for RDS
      } else if (is_save() == 1) {
        file_name <- tools::file_path_sans_ext(basename(input$file1$name))  # Extract file name without extension for CSV
      }

      # Append '_SaveState' to the file name
      paste0(input$prefix, file_name, input$suffix, ".rds")
    },
    content = function(file) {
      saveRDS(
        list(
          data = data(),
          input_values = reactiveValuesToList(input),  # Save all input values, including flip_x
          labels = labels$list,
          labelsxy = labelsxy$list
        ),
        file = file
      )
    }
  )

  output$save_config <- downloadHandler(
    filename = function() {
      file_name <- "_config.rds"  # Set your desired filename
      return(file_name)  # Return the filename
    },
    content = function(file) {
      saveRDS(
        list(
          input_values = reactiveValuesToList(input)  # Save all input values, including flip_x
        ),
        file = file
      )
    }
  )

  observeEvent(input$load_config, {
    req(input$load_config)
    state <- readRDS(input$load_config$datapath)  # Read the uploaded file

    # Restore input values
    for (name in names(state$input_values)) {
      if (name %in% names(input)) {  # Only update existing inputs
        updateTextInput(session, name, value = state$input_values[[name]])
        updateNumericInput(session, name, value = state$input_values[[name]])
        updateCheckboxInput(session, name, value = state$input_values[[name]])
      }
    }

    # Explicitly update flip_x
    if (!is.null(state$input_values$flip_x)) {
      updateCheckboxInput(session, "flip_x", value = state$input_values$flip_x)
    }

    # Update the plot
    output$plot <- renderPlotly({
      df <- data()  # Access the latest dataframe from the reactive value
      main_plotly(df)  # Pass the dataframe to your plot function
    })
  })


  observeEvent(input$load_state, {
    req(input$load_state)
    reset("file1")
    reset("load_config")
    state <- readRDS(input$load_state$datapath)  # Read the uploaded file

    # Restore data into the reactive data object
    data(state$data)  # Update the reactive data with state$data
    is_save(2)
    plot_filename (tools::file_path_sans_ext(basename(input$load_state$name)))

    # Restore input values
    for (name in names(state$input_values)) {
      if (name %in% names(input)) {  # Only update existing inputs
        updateTextInput(session, name, value = state$input_values[[name]])
        updateNumericInput(session, name, value = state$input_values[[name]])
        updateCheckboxInput(session, name, value = state$input_values[[name]])
      }
    }

    # Explicitly update flip_x
    if (!is.null(state$input_values$flip_x)) {
      updateCheckboxInput(session, "flip_x", value = state$input_values$flip_x)
    }

    # Restore labels and their positions
    labels$list <- state$labels
    labelsxy$list <- state$labelsxy

    # Ensure selectize input updates properly
    valid_labels <- unique(state$data$Gene)  # Get the available gene names
    selected_labels <- intersect(state$input_values$label, valid_labels)  # Keep only valid selections

    updateSelectizeInput(session, 'label',
                         choices = valid_labels,
                         selected = selected_labels,
                         server = TRUE)

    # Update the plot
    output$plot <- renderPlotly({
      df <- data()  # Access the latest dataframe from the reactive value
      main_plotly(df)  # Pass the dataframe to your plot function
    })
  })


  # Load the data file when the user uploads a new one
  observeEvent(input$file1, {
    req(input$file1)  # Ensure a file is selected
    ext <- tools::file_ext(input$file1$name)
    reset("load_state")
    reset("load_config")

    if (ext %in% c("csv", "txt", "tsv")) {
      df <- fread(input$file1$datapath, header = TRUE)
    } else if (ext == "xlsx") {
      df <- df <- as.data.table(read_excel(input$file1$datapath))
    } else {
      validate("Invalid file type.")
    }

    colnames(df)[c(1, 2, 3)] <- c("Gene", "x", "y")

    df$zscore <- scale(df$x)
    df$flip_x <- -df$x
    df$zscore_flip_x <- -df$zscore
    is_save(1)
    plot_filename (tools::file_path_sans_ext(basename(input$file1$name)))

    valid_labels <- unique(df$Gene)  # Get the available gene names
    selected_labels <- intersect(input$label, valid_labels)  # Keep only valid selections


    # Restore previous selection if still valid
    updateSelectizeInput(session, 'label',
                         choices = valid_labels,
                         selected = selected_labels,
                         server = TRUE)

    if (length(labels$list) > 0) {
      labels$list <- labels$list[sapply(labels$list, function(x) x$name %in% selected_labels)]
    }

    # Only run the loop if labels$list has elements
    if (length(labels$list) > 0) {

      # Loop through the labels$list to match gene names and update x values
      for (i in 1:length(labels$list)) {
        gene_name <- labels$list[[i]]$name
        gene_index <- which(df$Gene == gene_name)  # Find the index of the gene in data$Gene

        labels$list[[i]]$y <- -log10(pmax(df$y[gene_index], 1e-300))

        if (input$use_zscore && input$flip_x) {
          labels$list[[i]]$x <- df$zscore_flip_x[gene_index]
        } else if (input$use_zscore) {
          labels$list[[i]]$x <- df$zscore[gene_index]
        } else if (input$flip_x) {
          labels$list[[i]]$x <- df$flip_x[gene_index]
        } else {
          labels$list[[i]]$x <- df$x[gene_index]
        }
      }
    }

    data(df)
    # Update the plot
    output$plot <- renderPlotly({
      df <- data()  # Access the latest dataframe from the reactive value
      main_plotly(df)  # Pass the dataframe to your plot function
    })
  })
}
