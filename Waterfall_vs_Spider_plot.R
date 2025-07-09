############## WaterFall plot ########################

# Load necessary libraries
library(dplyr)
library(plotly)

# --- Data Preparation ---
# Recreate and expand the dataset from Table 1
waterfall_data <- tibble(
    subjid = c(192, 243, 126, 498, 257, 743, # Original data
               226, 314, 118,
               # Added subjects for a richer plot
               501, 502, 503, 504, 505, 506, 507, 508, 509, 510,
               511, 512, 513, 514, 515),
    maxchange = c(92, 80, 70, 66, 60, -93, # Original data (Note: 743 is PD despite -93% change, likely due to new lesions)
                  -88, -95, -100,
                  # Added subjects
                  110, 75, 45, 25, 10, 5, -5, -15, -28, -35,
                  -45, -58, -65, -79, -100),
    response = c("Progressive Disease", "Stable Disease", "Progressive Disease", "Progressive Disease", "Progressive Disease", "Progressive Disease",
                 "Partial Response", "Partial Response", "Complete Response",
                 # Added subjects' responses
                 "Progressive Disease", "Progressive Disease", "Progressive Disease", "Stable Disease", "Stable Disease", "Stable Disease", "Stable Disease", "Stable Disease", "Stable Disease", "Partial Response",
                 "Partial Response", "Partial Response", "Partial Response", "Partial Response", "Complete Response")
) %>%
    # IMPORTANT: Sort by maxchange for the waterfall effect and create a factor for plotting order
    arrange(desc(maxchange)) %>%
    mutate(
        subjid_ordered = factor(subjid, levels = subjid),
        response = factor(response, levels = c("Progressive Disease", "Stable Disease", "Partial Response", "Complete Response"))
    )

# Define a color palette for the responses
response_colors <- c(
    "Progressive Disease" = "#d73027",
    "Stable Disease" = "#fee090",
    "Partial Response" = "#4575b4",
    "Complete Response" = "#1a9850"
)

waterfall_plot <- plot_ly(
    data = waterfall_data,
    x = ~subjid_ordered,
    y = ~maxchange,
    color = ~response,
    colors = response_colors,
    type = 'bar',
    hoverinfo = 'text',
    text = ~paste(
        "<b>Subject ID:</b>", subjid,
        "<br><b>Max Change:</b>", maxchange, "%",
        "<br><b>Response:</b>", response
    )
) %>%
    layout(
        title = list(
            text = "<b>Waterfall Plot of Best Overall Response</b>",
            x = 0.05,
            font = list(size = 18)
        ),
        xaxis = list(
            title = "Subjects",
            showticklabels = FALSE,
            categoryorder = "array",
            categoryarray = ~subjid_ordered,
            linecolor = 'black',
            linewidth = 1,
            mirror = TRUE
        ),
        yaxis = list(
            title = "% Change from Baseline",
            gridcolor = "#e6e6e6",
            linecolor = 'black',
            linewidth = 1,
            mirror = TRUE
        ),
        legend = list(
            title = list(text = "<b>Best Overall Response</b>"),
            orientation = "v",
            x = 0.8,
            y = 1,  # Inside plot, above the top axis
            xanchor = "center",
            bgcolor = "rgba(255,255,255,0.85)",
            bordercolor = "rgba(200,200,200,0.5)",
            borderwidth = 1
        ),
        margin = list(
            l = 60,
            r = 60,
            b = 60,
            t = 100,  # Top space for the title and legend
            pad = 10
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        shapes = list(
            list(type = 'line', x0 = 0, x1 = 1, xref = 'paper',
                 y0 = 20, y1 = 20, line = list(color = 'grey', dash = 'dash')),
            list(type = 'line', x0 = 0, x1 = 1, xref = 'paper',
                 y0 = -30, y1 = -30, line = list(color = 'grey', dash = 'dash'))
        )
    )

# Display the plot
waterfall_plot


########### Spider Plot ####################

library(dplyr)
library(plotly)

# Custom color palette matching reference plot
response_colors <- c(
    "Progressive Disease" = "#8B0000",  # Dark Red
    "Stable Disease" = "#FFD700",      # Gold
    "Partial Response" = "#0000CD",    # Medium Blue
    "Complete Response" = "#228B22"    # Forest Green
)

# Enhanced sample data
spider_data <- tibble::tribble(
    ~subjid, ~month, ~change, ~response,
    201, 0, 0, "Complete Response", 201, 5, -35, "Complete Response", 201, 10, -40, "Complete Response",
    201, 15, -60, "Complete Response", 201, 20, -100, "Complete Response", 201, 30, -100, "Complete Response",
    
    202, 0, 0, "Partial Response", 202, 5, -25, "Partial Response", 202, 10, -40, "Partial Response",
    202, 15, -50, "Partial Response", 202, 20, -30, "Partial Response", 202, 30, -40, "Partial Response",
    
    203, 0, 0, "Stable Disease", 203, 5, 5, "Stable Disease", 203, 10, 15, "Stable Disease",
    203, 15, 10, "Stable Disease", 203, 20, 5, "Stable Disease", 203, 30, 10, "Stable Disease",
    
    204, 0, 0, "Progressive Disease", 204, 5, 10, "Progressive Disease", 204, 10, 30, "Progressive Disease",
    204, 15, 50, "Progressive Disease", 204, 20, 70, "Progressive Disease", 204, 30, 90, "Progressive Disease",
    
    205, 0, 0, "Progressive Disease", 205, 5, -10, "Progressive Disease", 205, 10, 0, "Progressive Disease",
    205, 15, 20, "Progressive Disease", 205, 20, 40, "Progressive Disease", 205, 30, 58, "Progressive Disease"
) %>%
    mutate(response = factor(response, levels = c("Progressive Disease", "Stable Disease", "Partial Response", "Complete Response")))

# Create plot
spider_plot <- plot_ly(
    data = spider_data,
    x = ~month,
    y = ~change,
    color = ~response,
    colors = response_colors,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(width = 3),
    marker = list(size = 6),
    hoverinfo = 'text',
    text = ~paste(
        "<b>Subject ID:</b>", subjid,
        "<br><b>Month:</b>", month,
        "<br><b>Change:</b>", change, "%"
    )
) %>%
    layout(
        title = list(
            text = "<b>Spider Plot of % Change from Baseline in Tumor Size</b>",
            x = 0.05,
            font = list(size = 18)
        ),
        xaxis = list(
            title = "Study Duration (months)",
            showgrid = FALSE,
            zeroline = FALSE,
            linecolor = 'black',
            linewidth = 1,
            mirror = TRUE
        ),
        yaxis = list(
            title = "% Change from Baseline",
            range = c(-110, 110),
            showgrid = TRUE,
            gridcolor = "#e6e6e6",
            zeroline = FALSE,
            linecolor = 'black',
            linewidth = 1,
            mirror = TRUE
        ),
        legend = list(
            title = list(text = "<b>Best Overall Response</b>"),
            orientation = "h",
            x = 0.5,
            y = -0.15,
            xanchor = "center",
            bgcolor = "rgba(255,255,255,0.9)",
            bordercolor = "rgba(200,200,200,0.5)",
            borderwidth = 1
        ), margin = list(
            l = 60,  # left
            r = 60,  # right
            b = 100, # bottom (increased for legend space)
            t = 80,  # top
            pad = 10
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        shapes = list(
            list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 20, y1 = 20,
                 line = list(color = 'gray', dash = 'dash')),
            list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = -30, y1 = -30,
                 line = list(color = 'gray', dash = 'dash')),
            list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0, y1 = 0,
                 line = list(color = 'black', width = 1))
        )
    )

# Display the plot
spider_plot

