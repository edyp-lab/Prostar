callModule(modulePopover, "modulePopover_numPrecision",
    data = reactive(
        list(
            title = "Numerical precisions",
            content = "Set the number of decimals to display for
            numerical values."
        )
    )
)

observe({
    shinyjs::toggle("defineColorsUI", condition = !is.null(rv$current.obj))
    shinyjs::toggle("showInfoColorOptions", condition = is.null(rv$current.obj))
})


output$settings_nDigits_UI <- renderUI({
    numericInput("settings_nDigits", "",
        value = rv$settings_nDigits,
        min = 0,
        width = "100px"
    )
})

observeEvent(input$settings_nDigits, {
    rv$settings_nDigits <- input$settings_nDigits
})


observe({
    shinyjs::onclick("btn_configConditionsColors", {
        shinyjs::toggle(id = "defineColorsForConditionsUI", anim = TRUE)
    })

    shinyjs::onclick("btn_configMVColors", {
        shinyjs::toggle(id = "defineColorsForMVUI", anim = TRUE)
    })


    shinyjs::onclick("btn_configVolcanoColors", {
        shinyjs::toggle(id = "defineColorsForVolcanoUI", anim = TRUE)
    })


    shinyjs::onclick("btn_configFCColors", {
        shinyjs::toggle(id = "defineColorsForFCUI", anim = TRUE)
    })
})



##########
output$defineColorsUI <- renderUI({
    shinyBS::bsCollapse(
        id = "collapseExample", open = "",
        shinyBS::bsCollapsePanel("Colors for conditions",
            uiOutput("defineColorsForConditionsUI"),
            style = "primary"
        ),
        shinyBS::bsCollapsePanel("Colors for missing values",
            tagList(
                colourpicker::colourInput("colMEC", "Select colour for MEC",
                    orangeProstar,
                    showColour = "background"
                ),
                colourpicker::colourInput("colPOV", "Select colour for POV",
                    "lightblue",
                    showColour = "background"
                )
            ),
            style = "primary"
        ),
        shinyBS::bsCollapsePanel("Colors for volcanoplots",
            colourpicker::colourInput("colVolcanoIn",
                "Select colour for selected entities",
                rv$colorsVolcanoplot$In,
                showColour = "background"
            ),
            colourpicker::colourInput("colVolcanoOut",
                "Select colour for filtered out entities",
                rv$colorsVolcanoplot$Out,
                showColour = "background"
            ),
            style = "primary"
        ),
        shinyBS::bsCollapsePanel("logFC distribution",
            "todo",
            style = "primary"
        )
    )
})




output$defineColorsForConditionsUI <- renderUI({
    tagList(
        fluidRow(
            column(
                width = 3,
                radioButtons("typeOfPalette",
                    "Type of palette for conditions",
                    choices = c(
                        "predefined" = "predefined",
                        "custom" = "custom"
                    ),
                    selected = rv$typeOfPalette
                )
            ),
            column(
                width = 6,
                highchartOutput("displayPalette",
                    width = "300px",
                    height = "200px"
                )
            )
        ),
        uiOutput("choosePalette_UI"),
        uiOutput("customPaletteUI"),
        hr()
    )
})


output$choosePalette_UI <- renderUI({
    rv$typeOfPalette
    if (rv$typeOfPalette != "predefined") {
        return(NULL)
    }
    selectInput("choosePalette", "Palette",
        choices = listBrewerPalettes,
        selected = rv$typeOfPalette,
        width = "200px"
    )
})

observeEvent(input$choosePalette, {
    rv$choosePalette <- input$choosePalette
})


GetTest <- reactive({
    req(rv$current.obj)
    rv$whichGroup2Color

    nbConds <- length(unique(pData(rv$current.obj)$Condition))
    pal <- rep("#000000", length(pData(rv$current.obj)$Condition))


    nbColors <- NULL
    temp <- NULL
    if (is.null(rv$whichGroup2Color) || (rv$whichGroup2Color == "Condition")) {
        nbColors <- length(unique(pData(rv$current.obj)$Condition))
        nbColors <- brewer.pal.info[listBrewerPalettes[1], ]$mincolors
        nbColors <- max(nbColors, nbConds)
        pal <- NULL
        for (i in 1:nbConds) {
            pal <- c(pal, input[[paste0("customColorCondition_", i)]])
        }
        for (i in 1:ncol(exprs(rv$current.obj))) {
            .cond <- pData(rv$current.obj)$Condition
            temp[i] <- pal[which(.cond[i] == unique(.cond))]
        }
    } else if (rv$whichGroup2Color == "Replicate") {
        nbColors <- length((pData(rv$current.obj)$Condition))
        for (i in 1:nbColors) {
            temp <- c(temp, input[[paste0("customColorCondition_", i)]])
        }
    }

    temp
})


GetWhichGroup2Color <- reactive({
    rv$whichGroup2Color
})

observeEvent(input$whichGroup2Color, {
    rv$whichGroup2Color <- input$whichGroup2Color
    rv$PlotParams$paletteForConditions <- GetPaletteForConditions()
})



GetPaletteForConditions <- reactive({
    req(rv$current.obj)
    rv$typeOfPalette

    nbConds <- length(pData(rv$current.obj)$Condition)
    nbUniqueConds <- length(unique(pData(rv$current.obj)$Condition))

    pal <- rep("#000000", nbUniqueConds)

    switch(rv$typeOfPalette,
        predefined = {
            temp <- DAPAR::ExtendPalette(nbUniqueConds, rv$choosePalette)
        },
        custom = {
            pal <- NULL
            for (i in 1:nbUniqueConds) {
                pal <- c(pal, input[[paste0("customColorCondition_", i)]])
            }
        }
    )

    if (!is.null(temp)) {
        pal[1:length(temp)] <- temp
    }

    pal
})


observeEvent(input$typeOfPalette, {
    rv$typeOfPalette <- input$typeOfPalette
})

output$customPaletteUI <- renderUI({
    rv$typeOfPalette
    if (rv$typeOfPalette != "custom") {
        return(NULL)
    }
    rv$whichGroup2Color
    ll <- list()
    nbColors <- NULL
    switch(rv$whichGroup2Color,
        Condition = {
            nbColors <- length(unique(pData(rv$current.obj)$Condition))
            labels <- unique(pData(rv$current.obj)$Condition)
        },
        Replicate = {
            nbColors <- length((pData(rv$current.obj)$Condition))
            labels <- pData(rv$current.obj)$Condition
        }
    )

    for (i in 1:nbColors) {
        ll <- list(
            ll,
            colourpicker::colourInput(paste0("customColorCondition_", i),
                labels[i],
                "#000000",
                showColour = "background"
            )
        )
    }

    ll
})



observeEvent(
    c(
        rv$choosePalette,
        rv$typeOfPalette,
        rv$current.obj, GetTest(),
        rv$whichGroup2Color
    ),
    {
        rv$PlotParams$paletteForConditions <- GetPaletteForConditions()
    }
)

observeEvent(input$colMEC, {
    rv$colorsTypeMV$MEC <- input$colMEC
})
observeEvent(input$colPOV, {
    rv$colorsTypeMV$POV <- input$colPOV
})
observeEvent(input$colVolcanoIn, {
    rv$colorsVolcanoplot$In <- input$colVolcanoIn
})
observeEvent(input$colVolcanoOut, {
    rv$colorsVolcanoplot$Out <- input$colVolcanoOut
})

output$displayPalette <- renderHighchart({
    # req(input$chooseNbColors)
    # GetTest()
    rv$PlotParams$paletteForConditions
    nbConds <- length(unique(pData(rv$current.obj)$Condition))

    highchart() %>%
        my_hc_chart(chartType = "column") %>%
        hc_add_series(
            data = data.frame(
                y = abs(1 + rnorm(ncol(exprs(rv$current.obj))))
            ),
            type = "column",
            colorByPoint = TRUE
        ) %>%
        hc_colors(rv$PlotParams$paletteForConditions) %>%
        hc_plotOptions(
            column = list(stacking = "normal"),
            animation = list(duration = 1)
        ) %>%
        hc_legend(enabled = FALSE) %>%
        hc_yAxis(labels = FALSE, title = list(text = "")) %>%
        hc_xAxis(categories = 1:nbConds, title = list(text = ""))
})
