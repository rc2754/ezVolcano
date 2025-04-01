library(shiny)
library(plotly)
library(data.table)
library(dplyr)
library(shinyjs)
library(readxl)

options(warn = -1)

server <- function(input, output, session) {
  #track filename and if data file or save file was loaded
  is_save <- reactiveVal(0)
  plot_filename <- reactiveVal("")

  #download examples
  output$example_data <- downloadHandler(
    filename = function() {
      "example_data.csv"
    },
    content = function(file) {
      file.copy("examples/example_data.txt", file)}
  )

  output$example_save <- downloadHandler(
    filename = function() {
      "example_save.rds"
    },
    content = function(file) {
      file.copy("examples/example_save.rds", file)}
  )

  output$example_config <- downloadHandler(
    filename = function() {
      "example_config.rds"
    },
    content = function(file) {
      file.copy("examples/example_config.rds", file)}
  )

  #Reactive values
  data <- reactiveVal()
  labels <- reactiveValues(list = list())
  labelsxy <- reactiveValues(list = list())

  labelchoices <- reactive({
    unique(data()$Gene)
  })

  #Batch gene input
  batchModal <- function() {
    modalDialog(
      textAreaInput("batchlabs", "Type or paste a list of genes", height = '300px'),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("batch_ok", "OK")
      )
    )
  }

  observeEvent(input$add_batch, {
    showModal(batchModal())
  })

  #add batch genes
  observeEvent(input$batch_ok, {
    req(input$batchlabs)
    if (!is.null(input$batchlabs)) {
      batch_genes <- unlist(strsplit(input$batchlabs, ",| |\n"))
      batch_genes <- batch_genes[batch_genes %in% labelchoices()]
      if (length(batch_genes) > 0) {
        current_labels <- lapply(labels$list, function(x) x$name)
        new_labels <- setdiff(batch_genes, current_labels)
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

  #clear all annotation labels
  observeEvent(input$clear_label, {
    updateSelectInput(session, "label", selected = character(0))
  })

  flip_ax_values <- function() {
    for (i in seq_along(labelsxy$list)) {
      labelsxy$list[[i]]$ax <- -labelsxy$list[[i]]$ax
    }
  }
  #flip ax value of labels
  observeEvent(input$flip_x, {
    flip_ax_values()
  })

  #function to draw the main plotly plot
  main_plotly <- function(data) {
    if (is.null(data)) {
      return(NULL)
    }

    #switch between regular fold change and z-scores for x-axis
    x_label <- if (!is.null(input$x_label) && input$x_label != "") {
      input$x_label
    } else {
      if (input$use_zscore) {
        "FoldChange Z-Score"
      } else {
        "log<sub>2</sub>(FoldChange)"
      }
    }

    #adjust x-axis values based on whether user wants it flipped and/or z-score
    if (input$use_zscore && input$flip_x) {
      temp_x <- data$zscore_flip_x
    } else if (input$use_zscore) {
      temp_x <- data$zscore
    } else if (input$flip_x) {
      temp_x <- data$flip_x
    } else {
      temp_x <- data$x
    }

    #adjust annotation x-axis values based on above
    if (length(labels$list) > 0) {
      for (i in 1:length(labels$list)) {
        gene_name <- labels$list[[i]]$name
        gene_index <- which(data$Gene == gene_name)

        if (input$use_zscore && input$flip_x) {
          labels$list[[i]]$x <- data$zscore_flip_x[gene_index]
        } else if (input$use_zscore) {
          labels$list[[i]]$x <- data$zscore[gene_index]
        } else if (input$flip_x) {
          labels$list[[i]]$x <- data$flip_x[gene_index]
        } else {
          labels$list[[i]]$x <- data$x[gene_index]
        }
      }
    }

    #adjust x-axis limits
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

    #adjust y-axis limits
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
        ylim <- c(ylim[1] - input$zero_pad, ylim[2])
      }
    }

    ylim <- c(ylim[1] * input$ylim_pad, ylim[2] * input$ylim_pad)

    #If no user input, plot title is the filename
    if (input$plot_title != "") {
      temp_title <- input$plot_title
    } else {
      temp_title <- plot_filename()
    }

    #Switch effect size threshold if using z-score
    if (input$use_zscore) {
      effect_size_left_temp <- input$effect_size_left_z
      effect_size_right_temp <- input$effect_size_right_z
    } else {
      effect_size_left_temp <- input$effect_size_left
      effect_size_right_temp <- input$effect_size_right
    }

    #legend and datapoint colors
    legend_con_m <- as.character(input$legend_con_m)
    legend_con_l <- as.character(input$legend_con_l)
    legend_con_r <- as.character(input$legend_con_r)

data$condition <- if (!input$hyperbola) {
  # Default threshold-based categorization (same as before)
  factor(
    ifelse(-log10(data$y) < input$stat_threshold, legend_con_m,
      ifelse(temp_x < effect_size_left_temp, legend_con_l,
        ifelse(temp_x > effect_size_right_temp, legend_con_r,
          legend_con_m
        )
      )
    ),
    levels = c(legend_con_m, legend_con_l, legend_con_r)
  )
} else {
  # Hyperbola-based thresholding
  hyperbola_threshold <- function(x, threshold, curvature, ref_point) {
    abs(curvature / (x - ref_point)) + threshold
  }

  # Compute threshold for left and right regions
  y_threshold_left  <- hyperbola_threshold(temp_x, input$stat_threshold, input$curvature, effect_size_left_temp)
  y_threshold_right <- hyperbola_threshold(temp_x, input$stat_threshold, input$curvature, effect_size_right_temp)

  # Assign colors based on hyperbola condition
  factor(
    ifelse(temp_x < effect_size_left_temp & -log10(data$y) > y_threshold_left, legend_con_l,
      ifelse(temp_x > effect_size_right_temp & -log10(data$y) > y_threshold_right, legend_con_r,
        legend_con_m
      )
    ),
    levels = c(legend_con_m, legend_con_l, legend_con_r)
  )
}


    color_mapping <- setNames(c(input$middle_color, input$left_color, input$right_color),
                              c(legend_con_m, legend_con_l, legend_con_r))

    #make plotly
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
            font = list(
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
          tickfont = list(
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
            font = list(
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
          tickfont = list(
            size = input$y_scale_size,
            family = input$font_axis,
            color = input$color_axis
          ),
          tickcolor = input$color_tick,
          dtick = input$y_ticks_interval,
          linewidth = input$y_line_thickness
        ),
        shapes = generate_shapes(input, effect_size_left_temp, effect_size_right_temp),
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
            text = input$legend_title,
            font = list(
              size = input$legend_title_size, family = input$font_legend, color = input$color_legend
            )
          ),
          x = input$legend_x,
          y = input$legend_y,
          xanchor = "center",
          yanchor = "middle",
          font = list(
            size = input$legend_label_size, family = input$font_legend_label, color = input$color_legend_label
          ),
          bordercolor = input$legend_box_color,
          borderwidth = input$legend_box_width,
          bgcolor = input$legend_box_bgcolor
        ),
        showlegend = TRUE,
        margin = list(l = input$l_margin, t = input$t_margin, r = input$r_margin, b = input$b_margin)
      ) %>%
      config(edits = list(annotationTail = TRUE))

    #add legend
    if (!input$show_legend) {
      p <- p %>% layout(showlegend = FALSE)
    }

    #flip legend order
    if (!input$legend_traceorder) {
      p <- p %>% layout(
        legend = list(
          traceorder = "reversed"
        )
      )
    }
if (input$hyperbola) {
  # Define the hyperbolic functions
  hyperbola_left <- function(x) abs(input$curvature / (x - effect_size_left_temp)) + input$stat_threshold
  hyperbola_right <- function(x) abs(input$curvature / (x - effect_size_right_temp)) + input$stat_threshold
  
  # Define the x-values for the left and right parts of the hyperbola
  x_left <- seq(effect_size_left_temp - 0.0000000000000000000001, effect_size_left_temp - 1000, length.out = 500)
  x_right <- seq(effect_size_right_temp + 0.0000000000000000000001, effect_size_right_temp + 1000, length.out = 500)
  print(input$effect_size_right_color)
  
  # Add lines for the hyperbolic lines
  p <- p %>% 
    add_lines(
      x = x_left,
      y = hyperbola_left(x_left),
      line = list(color = input$effect_size_left_color, width = input$effect_size_left_size, dash = input$effect_size_left_pattern),
      name = "Left Hyperbola"
    ) %>%
    add_lines(
      x = x_right,
      y = hyperbola_right(x_right),
      line = list(color = input$effect_size_right_color, width = input$effect_size_right_size, dash = input$effect_size_right_pattern),
      name = "Right Hyperbola"
    )
}


    #add outline trace for annotation labels
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
    event_register(p, "plotly_click")
    return(p)
  }
  
#generate lines for plotly
generate_shapes <- function(input, effect_size_left_temp, effect_size_right_temp) {
 shapes <- list()
  if (!input$hyperbola) {
    # Standard threshold-based lines
    shapes <- list(
      list(type = "line",
           x0 = 0, x1 = 1, xref = "paper",
           y0 = input$stat_threshold, y1 = input$stat_threshold,
           line = list(color = input$stat_threshold_color, width = input$stat_threshold_size, dash = input$stat_threshold_pattern)),
      list(type = "line",
           x0 = effect_size_left_temp, x1 = effect_size_left_temp,
           y0 = 0, y1 = 1, yref = "paper",
           line = list(color = input$effect_size_left_color, width = input$effect_size_left_size, dash = input$effect_size_left_pattern)),
      list(type = "line",
           x0 = effect_size_right_temp, x1 = effect_size_right_temp,
           y0 = 0, y1 = 1, yref = "paper",
           line = list(color = input$effect_size_right_color, width = input$effect_size_right_size, dash = input$effect_size_right_pattern))
    )
  }   
  return(shapes)
}



  #create annotation labels for plotly
  create_annotations <- function(label_list, label_xy_list) {
    if (length(label_list) == 0) {
      return(list())
    }

    annotations <- lapply(seq_along(label_list), function(i) {
      ax_val <- 50
      ay_val <- -50

      if (!is.null(label_xy_list[[i]])) {
        ax_val <- label_xy_list[[i]]$ax
        ay_val <- label_xy_list[[i]]$ay
      }

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

  #add/remove labels after user clicks
  #code is modified from the easylabel R package by Myles Lewis (https://github.com/myles-lewis/easylabel)
  observe({
    s <- event_data("plotly_click", source = "volcano_plot", priority = "event")
    req(s)

    s_key <- unlist(s$key)
    s_xy <- paste(s$x, s$y, sep = "_")
    isolate({
      existing_index <- which(s_xy == sapply(labels$list, function(x) paste(x$x, x$y, sep = "_")))

      if (is.null(s$key)) {
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

  #store the location of moved labels by detecting plotly relayout events of the type `annotations[1].ax`
  #code is from the easylabel R package by Myles Lewis (https://github.com/myles-lewis/easylabel)
  observeEvent(event_data("plotly_relayout", source = 'volcano_plot'), {
    s <- event_data("plotly_relayout", source = 'volcano_plot')

    if (grepl("annotations\\[[0-9]+\\]\\.ax", names(s)[1])) {
      w <- as.numeric(gsub("annotations\\[|\\]\\.ax", "", names(s)[1])) + 1
      labelsxy$list[[w]] <- list(ax = s[[1]], ay = s[[2]])
    }
  })

  #SelectInput
  observeEvent(input$label, {
    currentsel <- input$label
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

  #update plotly labels
  observeEvent(labels$list, {
    sel_labels <- input$label
    sel_labels <- which(unique(data()$Gene) %in% sel_labels)
    if (any(!unique(unlist(lapply(labels$list, function(x) x$name))) %in% unique(data()$Gene)[sel_labels]) | any(!unique(data()$Gene)[sel_labels] %in% unique(unlist(lapply(labels$list, function(x) x$name))))) {
      updateSelectInput(session, 'label', choices = unique(data()$Gene), selected = unlist(lapply(labels$list, function(x) x$name)))
    }
  })

  #output the plotly plot
  output$plot <- renderPlotly({
    req(data())
    df <- data()
    main_plotly(df)
  })

  #save plot
  output$save_state <- downloadHandler(
    filename = function() {

      if (is_save() == 2) {
        file_name <- tools::file_path_sans_ext(basename(input$load_state$name))
      } else if (is_save() == 1) {
        file_name <- tools::file_path_sans_ext(basename(input$file1$name))
      }
      paste0(input$prefix, file_name, input$suffix, ".rds")
    },
    content = function(file) {
      saveRDS(
        list(
          data = data(),
          input_values = reactiveValuesToList(input),
          labels = labels$list,
          labelsxy = labelsxy$list
        ),
        file = file
      )
    }
  )

  #save config file of aesthetic settings
  output$save_config <- downloadHandler(
    filename = function() {
      file_name <- "_config.rds"
      return(file_name)
    },
    content = function(file) {
      saveRDS(
        list(
          input_values = reactiveValuesToList(input)
        ),
        file = file
      )
    }
  )

  #load config file
  observeEvent(input$load_config, {
    req(input$load_config)
    state <- readRDS(input$load_config$datapath)

    if (!is.list(state$input_values) || length(state$input_values) != 107) {
      output$error_message3 <- renderText("Invalid file.")
      return()
    }
    output$error_message3 <- renderText("")

    for (name in names(state$input_values)) {
      if (name %in% names(input)) {
        updateTextInput(session, name, value = state$input_values[[name]])
        updateNumericInput(session, name, value = state$input_values[[name]])
        updateCheckboxInput(session, name, value = state$input_values[[name]])
      }
    }

    flip_ax_values()

    output$plot <- renderPlotly({
      df <- data()
      main_plotly(df)
    })
  })

  #load save
  observeEvent(input$load_state, {
    req(input$load_state)
    reset("file1")
    reset("load_config")
    state <- readRDS(input$load_state$datapath)

    if (!is.numeric(state$data$x) || !is.numeric(state$data$y)) {
      output$error_message2 <- renderText("Invalid file.")
      return()
    }
    output$error_message2 <- renderText("")

    data(state$data)
    is_save(2)
    plot_filename (tools::file_path_sans_ext(basename(input$load_state$name)))

    for (name in names(state$input_values)) {
      if (name %in% names(input)) {
        updateTextInput(session, name, value = state$input_values[[name]])
        updateNumericInput(session, name, value = state$input_values[[name]])
        updateCheckboxInput(session, name, value = state$input_values[[name]])
      }
    }
    valid_labels <- unique(state$data$Gene)
    selected_labels <- intersect(state$input_values$label, valid_labels)
    updateSelectInput(session, 'label', choices = valid_labels, selected = selected_labels)

    labels$list <- state$labels
    labelsxy$list <- state$labelsxy
    flip_ax_values()

    output$plot <- renderPlotly({
      df <- data()
      main_plotly(df)
    })
  })

  #load data file
  observeEvent(input$file1, {
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    reset("load_state")
    reset("load_config")

    if (ext %in% c("csv", "txt", "tsv")) {
      df <- fread(input$file1$datapath, header = TRUE)
    } else if (ext == "xlsx") {
      df <- df <- as.data.table(read_excel(input$file1$datapath))
    } else {
      output$error_message1 <- renderText("Invalid file type. Please upload a TXT, CSV, TSV, or XLSX file.")
      return()
    }

    if (ncol(df) < 3) {
      output$error_message1 <- renderText("The uploaded file must have at least 3 columns.")
      return()
    }

    colnames(df)[c(1, 2, 3)] <- c("Gene", "x", "y")

    if (!is.numeric(df$x) || !is.numeric(df$y)) {
      output$error_message1 <- renderText("Columns 'x' and 'y' must be numeric.")
      return()
    }

    output$error_message1 <- renderText("")

    df$zscore <- scale(df$x)
    df$flip_x <- -df$x
    df$zscore_flip_x <- -df$zscore
    is_save(1)
    plot_filename (tools::file_path_sans_ext(basename(input$file1$name)))

    valid_labels <- unique(df$Gene)
    selected_labels <- intersect(input$label, valid_labels)
    updateSelectInput(session, 'label', choices = valid_labels, selected = selected_labels)

    if (length(labels$list) > 0) {
      labels$list <- labels$list[sapply(labels$list, function(x) x$name %in% selected_labels)]
    }

    if (length(labels$list) > 0) {
      for (i in 1:length(labels$list)) {
        gene_name <- labels$list[[i]]$name
        gene_index <- which(df$Gene == gene_name)

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
    output$plot <- renderPlotly({
      df <- data()
      main_plotly(df)
    })
  })
}
