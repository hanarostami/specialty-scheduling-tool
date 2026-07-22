# Load required libraries
library(shiny)
library(shinyjs)
library(jsonlite)
library(dplyr)
library(colorspace)
library(tidyr)
library(plotly)
library(ggplot2)
library(tibble)
library(bslib)

MAX_ROOMS <- 100

# ---- Helper Functions ----

expand_capacity <- function(df_inputs) {
  md_rows <- list()
  app_rows <- list()
  
  for (i in seq_len(nrow(df_inputs))) {
    row <- df_inputs[i, ]
    
    specialty <- row$specialty
    spec_type <- as.integer(row$type)
    md_fte <- as.numeric(row$physician_fte)
    md_days <- as.numeric(row$clinic_days_per_fte)
    md_rooms <- as.integer(row$rooms_per_clinic_day)
    app_ratio <- as.numeric(row$app_ratio)
    app_days_per_fte <- as.numeric(row$app_days_per_fte)
    app_rooms <- as.integer(row$app_rooms_per_clinic_day)
    
    full_blocks <- floor(md_fte)
    rem_fte <- md_fte - full_blocks
    md_expanded <- list()
    
    for (j in seq_len(full_blocks)) {
      name <- paste(specialty, "Physician", j)
      md_rows[[length(md_rows) + 1]] <- data.frame(
        name = name, fte = 1, clinic_days = md_days,
        rooms = md_rooms, type = spec_type, specialty = specialty, role = "Physician"
      )
      md_expanded[[length(md_expanded) + 1]] <- list(name = name, fte = 1)
    }
    
    if (rem_fte > 0) {
      name <- paste(specialty, "Physician", full_blocks + 1)
      md_rows[[length(md_rows) + 1]] <- data.frame(
        name = name, fte = rem_fte, clinic_days = md_days,
        rooms = md_rooms, type = spec_type, specialty = specialty, role = "Physician"
      )
      md_expanded[[length(md_expanded) + 1]] <- list(name = name, fte = rem_fte)
    }
    
    for (md in md_expanded) {
      app_days <- app_ratio * md$fte * app_days_per_fte
      half_days <- round(app_days * 2)
      for (k in seq_len(half_days)) {
        app_name <- paste("APP for", md$name, k)
        app_rows[[length(app_rows) + 1]] <- data.frame(
          name = app_name, rooms = app_rooms, type = spec_type,
          specialty = specialty, role = "APP", mirrors = ifelse(spec_type == 1, md$name, NA)
        )
      }
    }
  }
  
  md_df <- do.call(rbind, md_rows)
  app_df <- do.call(rbind, app_rows)
  list(md_df = md_df, app_df = app_df)
}

assign_mirrored_apps_optimally <- function(app_df, md_df, assigned_md_slots, schedule) {
  mirrored_apps <- app_df[!is.na(app_df$mirrors) & app_df$type == 1, ]
  unmet <- character()
  if (nrow(mirrored_apps) == 0) return(list(schedule = schedule, unmet = unmet))

  md_room_map <- list()
  for (r in 1:nrow(schedule)) {
    for (c in 1:ncol(schedule)) {
      val <- schedule[r, c]
      if (val != "" && grepl("Physician", val)) {
        md_room_map[[val]] <- r
      }
    }
  }

  slot_names <- colnames(schedule)
  mirror_groups <- split(mirrored_apps, mirrored_apps$mirrors)

  for (md_name in names(mirror_groups)) {
    md_row <- md_room_map[[md_name]]
    md_slots <- assigned_md_slots[[md_name]]
    if (length(md_slots) == 0 || is.null(md_row)) {
      apps <- mirror_groups[[md_name]]
      unmet <- c(unmet, paste0(apps$name, ": mirrored physician ", md_name, " has no assigned slots"))
      next
    }

    apps <- mirror_groups[[md_name]]
    n_apps <- nrow(apps)
    n_slots <- length(md_slots)

    slot_indices <- rep(1:n_slots, length.out = n_apps)

    for (i in seq_len(n_apps)) {
      app <- apps[i, ]
      slot <- md_slots[slot_indices[i]]
      col_idx <- match(slot, colnames(schedule))

      assigned <- FALSE
      for (offset in 1:5) {
        candidate_row <- md_row + offset
        if (candidate_row > nrow(schedule)) {
          new_row <- rep("", length(slot_names))
          schedule <- rbind(schedule, setNames(as.list(new_row), slot_names))
        }
        if (schedule[candidate_row, col_idx] == "") {
          schedule[candidate_row, col_idx] <- app$name
          assigned <- TRUE
          break
        }
      }
      if (!assigned) unmet <- c(unmet, paste0(app$name, ": could not be placed near ", md_name))
    }
  }
  list(schedule = schedule, unmet = unmet)
}

schedule_all <- function(md_df, app_df) {
  days <- c("Mon", "Tue", "Wed", "Thu", "Fri")
  halves <- c("AM", "PM")
  slots <- outer(days, halves, paste)
  slot_names <- as.vector(t(slots))
  
  max_rooms <- MAX_ROOMS
  schedule <- matrix("", nrow = max_rooms, ncol = length(slot_names))
  rownames(schedule) <- paste("Room", 1:max_rooms)
  colnames(schedule) <- slot_names
  
  assign_provider_sequentially <- function(name, half_days_needed, rooms_needed) {
    days <- c("Mon AM", "Mon PM", "Tue AM", "Tue PM", 
              "Wed AM", "Wed PM", "Thu AM", "Thu PM", 
              "Fri AM", "Fri PM")
    
    assigned <- 0
    assigned_slots <- c()
    
    for (room_start in 1:(max_rooms - rooms_needed + 1)) {
      room_block <- room_start:(room_start + rooms_needed - 1)
      
      for (day in days) {
        if (all(schedule[room_block, day] == "")) {
          schedule[room_block, day] <<- name
          assigned <- assigned + 1
          assigned_slots <- c(assigned_slots, day)
        }
        
        if (assigned >= half_days_needed) {
          return(assigned_slots)
        }
      }
    }
    
    return(assigned_slots)  # partial or empty list if not all assigned
  }
  
  
  assign_person_single_room <- function(name, half_days_needed, rooms_needed = 1, avoid_name = NULL, restrict_slots = NULL) {
    for (r in 1:(max_rooms - rooms_needed + 1)) {
      room_block <- r:(r + rooms_needed - 1) # <- verify this line exactly
      
      # Avoid rooms occupied by avoid_name (for mirrored APPs)
      if (!is.null(avoid_name) && any(schedule[room_block, ] == avoid_name)) next
      
      free_slots <- which(colSums(schedule[room_block, , drop = FALSE] != "") == 0)
      
      if (!is.null(restrict_slots)) {
        free_slots <- free_slots[colnames(schedule)[free_slots] %in% restrict_slots]
      }
      
      if (length(free_slots) >= half_days_needed) {
        schedule[room_block, free_slots[1:half_days_needed]] <<- name
        return(colnames(schedule)[free_slots[1:half_days_needed]])
      }
    }
    return(NULL)
  }
  

  unmet <- character()

  assigned_md_slots <- list()
  for (i in seq_len(nrow(md_df))) {
    person <- md_df[i, ]
    hd <- as.integer(round(person$fte * person$clinic_days * 2))
    result <- assign_provider_sequentially(person$name, hd, person$rooms)
    assigned_md_slots[[person$name]] <- result
    if (length(result) < hd) {
      unmet <- c(unmet, sprintf(
        "%s: needed %d half-days, only %d could be scheduled (ran out of rooms/slots)",
        person$name, hd, length(result)
      ))
    }
  }

  for (i in seq_len(nrow(app_df))) {
    app <- app_df[i, ]
    is_mirrored <- !is.na(app$mirrors) && app$type == 1
    already_scheduled <- any(schedule == app$name)

    if (!already_scheduled) {
      # Calculate total half-days explicitly
      app_half_days <- 1 # Usually one half-day per each APP row, adjust if different

      # Assign all half-days consistently into a single room
      result <- assign_person_single_room(
        name = app$name,
        half_days_needed = app_half_days,
        rooms_needed = app$rooms,  # typically 1
        avoid_name = if (is_mirrored) app$mirrors else NULL,
        restrict_slots = if (is_mirrored) assigned_md_slots[[app$mirrors]] else NULL
      )
      if (is.null(result)) {
        unmet <- c(unmet, paste0(app$name, ": could not be scheduled (no available room/slot)"))
      }
    }
  }

  mirror_result <- assign_mirrored_apps_optimally(app_df, md_df, assigned_md_slots, schedule)
  schedule <- mirror_result$schedule
  unmet <- c(unmet, mirror_result$unmet)

  schedule <- schedule[rowSums(schedule != "") > 0, ]
  list(schedule = as.data.frame(schedule), unmet = unmet)
}

summarize_schedule <- function(schedule_df) {
  util <- summarize_utilization(schedule_df)
  paste0(util, collapse = "; ")
}

summarize_utilization <- function(schedule_df) {
  used <- schedule_df != ""
  room_usage_days <- rowSums(used) / 2
  usage_table <- table(room_usage_days)
  
  summary <- c()
  for (d in sort(as.numeric(names(usage_table)))) {
    count <- usage_table[[as.character(d)]]
    day_text <- if (d == 1) "day" else "days"
    room_text <- if (count == 1) "room" else "rooms"
    summary <- c(summary, paste(count, room_text, "used for", d, day_text))
  }
  return(summary)
}
# ---- UI ----

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(tags$style(HTML("
    .app-subtitle { color: #6c757d; margin-top: -8px; margin-bottom: 24px; max-width: 720px; }
    .well { border-radius: 12px; box-shadow: 0 1px 4px rgba(0,0,0,0.08); }
    .section-heading { margin-top: 4px; margin-bottom: 12px; }
    .type-help { margin-top: -8px; margin-bottom: 16px; font-size: 0.85em; }
  "))),

  titlePanel("Specialty Scheduling Tool"),
  p(class = "app-subtitle",
    "Plan exam-room and staffing needs for a multi-specialty practice: enter each specialty's ",
    "physician and APP requirements below, then generate a room-by-room weekly schedule."),

  sidebarLayout(
    sidebarPanel(
      selectInput("load_specialty", "Load Specialty Assumptions",
                  choices = c("Select specialty...", "ARJR", "Sports Med", "Hand & Wrist", "Foot & Ankle", "Spine", "Trauma", "Cardiac/Thoracic Surgeon", "Electrophysiologist", "Interventional Cardiologist", "General Cardiologist", "Advanced Heart Failure", "General GI", "Advanced Endoscopist", "Colorectal Surgeon", "OB/GYN", "Gyn-Only", "UroGyn", "MFM", "GynOnc", "MIGS", "Med Onc", "Hem Onc", "Plastics", "Urologist", "General Surgeon", "Acute Care Surgeon", "Vascular Surgeon")),
      hr(),
      textInput("specialty", "Specialty"),
      radioButtons("type", "Type", choices = list("Mirror" = 1, "Independent" = 2)),
      helpText(class = "type-help",
               "Mirror: APPs are in the office on the same days as their physician. ",
               "Independent: APPs run their own clinic on a separate schedule."),
      numericInput("physician_fte", "Physician FTE", value = 1, min = 0),
      numericInput("clinic_days", "Clinic Days/FTE", value = 4, min = 0),
      numericInput("rooms", "Physician Rooms/Day", value = 2, min = 1),
      numericInput("app_ratio", "APP Ratio", value = 1),
      numericInput("app_days", "APP Days/FTE", value = 2),
      numericInput("app_rooms", "APP Rooms/Day", value = 1),
      actionButton("add_entry", "Add/Update Specialty", icon = icon("plus"), class = "btn-primary w-100"),
      hr(),
      selectInput("entry_selector", "Edit Entry", choices = NULL),
      fluidRow(
        column(6, actionButton("edit_entry", "Load for Edit", icon = icon("pen"), class = "w-100")),
        column(6, actionButton("remove_entry", "Remove Selected", icon = icon("trash"), class = "btn-outline-danger w-100"))
      ),
      br(),
      actionButton("reset_entries", "Reset All", icon = icon("rotate-left"), class = "btn-outline-secondary w-100")
    ),
    mainPanel(
      h4("Current Entries", class = "section-heading"),
      tableOutput("entries_table"),
      hr(),
      actionButton("generate", "Generate Schedule", icon = icon("calendar-check"), class = "btn-success btn-lg"),
      conditionalPanel(
        condition = "output.hasSchedule",
        hr(),
        h4("Schedule Plot", class = "section-heading"),
        plotlyOutput("schedule_plot"),
        h4("Written Schedule Summary", class = "section-heading"),
        textOutput("written_summary")
      )
    )
  )
)

# ---- Server ----

server <- function(input, output, session) {
  observeEvent(input$load_specialty, {
    default_specialties <- data.frame(
      specialty = c("ARJR", "Sports Med", "Hand & Wrist", "Foot & Ankle", "Spine", "Trauma", "Cardiac/Thoracic Surgeon", "Electrophysiologist", "Interventional Cardiologist", "General Cardiologist", "Advanced Heart Failure", "General GI", "Advanced Endoscopist", "Colorectal Surgeon", "OB/GYN", "Gyn-Only", "UroGyn", "MFM", "GynOnc", "MIGS", "Med Onc", "Hem Onc", "Plastics", "Urologist", "General Surgeon", "Acute Care Surgeon", "Vascular Surgeon"),
      type = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 1, 2, 2, 1, 1, 1, 1, 1),
      physician_fte = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      clinic_days_per_fte = c(2.5, 2.5, 2.5, 2.5, 2.5, 1.5, 1, 2.5, 2.5, 5, 5, 2, 1, 2, 3, 3.5, 3.5, 5, 2.5, 3, 4, 4, 3, 3.5, 2, 1, 2),
      rooms_per_clinic_day = c(4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 2, 3, 3, 2, 2, 2, 2, 1, 1, 2 ),
      app_ratio = c(1.5, 1, 1, 1, 1.5, 1, 1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1, 1, 1),
      app_days_per_fte = c(2.5, 2.5, 2.5, 2.5, 2, 1, 1, 2, 2.5, 5, 5, 5, 1, 1, 4, 2, 3.5, 5, 2.5, 2, 5, 5, 2, 5, 2, 1, 2 ),
      app_rooms_per_clinic_day = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ),
      stringsAsFactors = FALSE
    )
    
    selected <- default_specialties %>% filter(specialty == input$load_specialty)
    
    if (nrow(selected) == 1) {
      updateTextInput(session, "specialty", value = selected$specialty)
      updateRadioButtons(session, "type", selected = as.character(selected$type))
      updateNumericInput(session, "physician_fte", value = selected$physician_fte)
      updateNumericInput(session, "clinic_days", value = selected$clinic_days_per_fte)
      updateNumericInput(session, "rooms", value = selected$rooms_per_clinic_day)
      updateNumericInput(session, "app_ratio", value = selected$app_ratio)
      updateNumericInput(session, "app_days", value = selected$app_days_per_fte)
      updateNumericInput(session, "app_rooms", value = selected$app_rooms_per_clinic_day)
    }
  })
  
  all_inputs <- reactiveVal(data.frame())
  schedule_ready <- reactiveVal(FALSE)

  output$hasSchedule <- reactive({ schedule_ready() })
  outputOptions(output, "hasSchedule", suspendWhenHidden = FALSE)

  observeEvent(input$add_entry, {
    if (is.null(input$specialty) || trimws(input$specialty) == "") {
      showNotification("Specialty name cannot be blank.", type = "error")
      return(NULL)
    }

    numeric_fields <- list(
      "Physician FTE" = input$physician_fte,
      "Clinic Days/FTE" = input$clinic_days,
      "Physician Rooms/Day" = input$rooms,
      "APP Ratio" = input$app_ratio,
      "APP Days/FTE" = input$app_days,
      "APP Rooms/Day" = input$app_rooms
    )
    missing_fields <- names(numeric_fields)[sapply(numeric_fields, function(x) is.null(x) || is.na(x))]
    if (length(missing_fields) > 0) {
      showNotification(
        paste("Please provide a value for:", paste(missing_fields, collapse = ", ")),
        type = "error"
      )
      return(NULL)
    }

    new_entry <- data.frame(
      specialty = input$specialty,
      type = as.integer(input$type),
      physician_fte = input$physician_fte,
      clinic_days_per_fte = input$clinic_days,
      rooms_per_clinic_day = input$rooms,
      app_ratio = input$app_ratio,
      app_days_per_fte = input$app_days,
      app_rooms_per_clinic_day = input$app_rooms,
      stringsAsFactors = FALSE
    )

    df <- all_inputs()
    idx <- match(tolower(trimws(input$specialty)), tolower(trimws(df$specialty)))
    if (!is.na(idx)) {
      df[idx, ] <- new_entry
    } else {
      df <- bind_rows(df, new_entry)
    }
    all_inputs(df)

    updateSelectInput(session, "entry_selector",
                      choices = paste(seq_len(nrow(df)), df$specialty, sep = ": "))
  })
  
  observeEvent(input$edit_entry, {
    index <- as.integer(strsplit(input$entry_selector, ":")[[1]][1])
    df <- all_inputs()
    entry <- df[index, ]
    updateTextInput(session, "specialty", value = entry$specialty)
    updateRadioButtons(session, "type", selected = as.character(entry$type))
    updateNumericInput(session, "physician_fte", value = entry$physician_fte)
    updateNumericInput(session, "clinic_days", value = entry$clinic_days_per_fte)
    updateNumericInput(session, "rooms", value = entry$rooms_per_clinic_day)
    updateNumericInput(session, "app_ratio", value = entry$app_ratio)
    updateNumericInput(session, "app_days", value = entry$app_days_per_fte)
    updateNumericInput(session, "app_rooms", value = entry$app_rooms_per_clinic_day)
  })
  
  observeEvent(input$remove_entry, {
    index <- as.integer(strsplit(input$entry_selector, ":")[[1]][1])
    df <- all_inputs()
    if (!is.na(index) && index <= nrow(df)) {
      df <- df[-index, ]
      all_inputs(df)
      updateSelectInput(session, "entry_selector",
                        choices = paste(seq_len(nrow(df)), df$specialty, sep = ": "))
    }
  })
  
  observeEvent(input$reset_entries, {
    all_inputs(data.frame())
    updateSelectInput(session, "entry_selector", choices = NULL)
    output$schedule_plot <- renderPlotly({ NULL })
    output$written_summary <- renderText({ "" })
    schedule_ready(FALSE)
  })
  
  output$entries_table <- renderTable({
    df <- all_inputs()
    if (nrow(df) == 0) return(NULL)
    colnames(df) <- c("Specialty", "Type", "Physician FTE", "Physician Clinic Days/FTE", "Physician Rooms/Day", "APP/Physician Ratio", "APP ClinicDays/FTE", "APP Rooms/Day")
    df
  })
  
  observeEvent(input$generate, {
    df <- all_inputs()
    if (nrow(df) == 0) {
      showNotification("No entries to schedule. Add a specialty first.", type = "error")
      return(NULL)
    }
    
    expanded <- expand_capacity(df)
    schedule_result <- schedule_all(expanded$md_df, expanded$app_df)
    final_schedule <- schedule_result$schedule
    unmet <- schedule_result$unmet

    if (length(unmet) > 0) {
      showNotification(
        paste0(
          "Some providers could not be fully scheduled (", MAX_ROOMS, "-room limit reached or no valid slot found):\n",
          paste(unmet, collapse = "\n")
        ),
        type = "warning",
        duration = NULL
      )
    }

    schedule_ready(TRUE)

    output$schedule_plot <- renderPlotly({
      schedule_df <- final_schedule %>%
        tibble::rownames_to_column(var = "Room") %>%
        pivot_longer(-Room, names_to = "Slot", values_to = "Name") %>%
        mutate(
          Day = sub(" .*", "", Slot),
          Half = sub(".* ", "", Slot),
          SlotID = match(Slot, colnames(final_schedule)),
          DayIndex = as.numeric(factor(Day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri"))),
          x = SlotID + 0.3 * (DayIndex - 1)
        )
      
      md_info <- expanded$md_df %>% select(name, specialty, role)
      app_info <- expanded$app_df %>% mutate(fte = NA) %>% select(name, specialty, role)
      name_info <- bind_rows(md_info, app_info)
      
      merged <- left_join(schedule_df, name_info, by = c("Name" = "name"))
      merged$Room <- factor(merged$Room, levels = rev(unique(merged$Room)))
      
      specialties <- unique(na.omit(merged$specialty))
      n_colors <- max(3, length(specialties))
      base_palette <- RColorBrewer::brewer.pal(n_colors, "Set2")[seq_along(specialties)]
      base_colors <- setNames(base_palette, specialties)
      
      merged <- merged %>%
        mutate(
          legend_label = case_when(
            role == "Physician" ~ paste0(specialty, " (Physician)"),
            role == "APP" ~ paste0(specialty, " (APP)"),
            TRUE ~ "Unassigned"
          ),
          fill_color = case_when(
            role == "Physician" ~ as.character(base_colors[specialty]),
            role == "APP" ~ as.character(colorspace::lighten(base_colors[specialty], amount = 0.4)),
            TRUE ~ "#ffffff"
          )
        )
      
      p <- ggplot(merged, aes(
        x = x,
        y = Room,
        fill = legend_label,
        text = paste("Name:", Name,
                     "<br>Specialty:", specialty,
                     "<br>Role:", role,
                     "<br>Slot:", Slot)
      )) +
        geom_tile(width = 1, height = 0.9, color = "white") +
        scale_fill_manual(values = setNames(merged$fill_color, merged$legend_label)) +
        scale_x_continuous(
          breaks = seq(2, 10, by = 2),
          labels = c("Mon", "Tue", "Wed", "Thu", "Fri"),
          expand = expansion(add = 0.5)
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
              panel.grid = element_blank()) +
        labs(title = "Schedule Grid", x = "Day", y = "Room", fill = "Specialty & Role")
      
      ggplotly(p, tooltip = "text")
    })
    
    output$written_summary <- renderText({
      summary_text <- summarize_schedule(final_schedule)
      if (length(unmet) > 0) {
        summary_text <- paste0(
          summary_text,
          " | WARNING: ", length(unmet), " provider(s) could not be fully scheduled - see notification for details."
        )
      }
      summary_text
    })
  })
}

# ---- Run App ----

shinyApp(ui = ui, server = server)
