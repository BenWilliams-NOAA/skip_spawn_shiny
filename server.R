source("global.R")

shinyServer(function(input, output, session) {
  # inputs ----
  # auto update slider
  observeEvent(input$ages,{
    updateSliderInput(session = session, inputId = "bin", value = input$ages, max = input$ages)
    updateSliderInput(session = session, inputId = "mat_bin", value = input$ages, min = input$bin, max = input$ages)
    updateSliderInput(session = session, inputId = "minskip", max = input$ages-1)
    updateSliderInput(session = session, inputId = "maxskip", max = input$ages)
  })

  # container for changing sample size and weights
  db <- reactiveValues(database = NULL)

  # to store the modified values
  # edited_vals <- reactiveVal(tibble(row = numeric(),
  #                                   col = numeric(),
  #                                   value = numeric()))

  # create a new table each time the sliders are changed
  observeEvent(c(input$ages, input$nsamp), {
    df <- data.frame(
      age = 1:input$ages,
      samples = input$nsamp,
      weighting = 1
    )
    db$database <- df
  })

  # record changes to spreadsheet
  observeEvent(input$sample_tbl_cell_edit, {
    db$database[as.numeric(input$sample_tbl_cell_edit$row), as.numeric(input$sample_tbl_cell_edit$col + 1)] <- as.numeric(input$sample_tbl_cell_edit$value)
  })

  # create output object
  output$sample_tbl <- renderDT(
    {
      input$ages
      input$nsamp

      datatable(
        isolate(db$database),
        selection = "none",
        editable = TRUE,
        rownames = FALSE,
        options = list(stateSave = TRUE)
      )
    },
    server = TRUE
  )

  # length-weight dataframe
  lw <- reactive({
    expand.grid(age = 1:input$ages, sims = 1:input$nsim) |>
      mutate(length = round(purrr::pmap_dbl(list(age, input$linf, input$k, input$t0, 1), vonb), 0),
             weight = purrr::pmap_dbl(list(length, input$wa1, input$wa2), waa) / 1000)
  })

  lw2 <-  reactive({
    data.frame(age = 1:input$ages) |>
      mutate(length = round(purrr::pmap_dbl(list(age, input$linf, input$k, input$t0, 1), vonb), 0),
             weight = purrr::pmap_dbl(list(length, input$wa1, input$wa2), waa) / 1000)
  })


  # skip spawning
  skips <- reactive({
    data.frame(age = input$minskip:input$maxskip) |>
      mutate(s = c(input$skip, rep(NA, length(age) - 2 ), input$shigh),
             s = approx(age, s, age)$y) -> newd

    data.frame(age = 1:input$ages) %>%
      left_join(newd)  |>
      replace_na(list(s = 0))
  })

  # biological
  bios <- reactive({
    biological(age = input$ages, lw = lw(),
               b0 = input$b0, b1 = input$b1, b0_l = input$b0_l, b1_l = input$b1_l,
               minskip = input$minskip, maxskip = input$maxskip, skip = input$skip,
               shigh = input$shigh, sims = input$nsim)
  })

  # population
  pops <- reactive({
    population(input$ages, input$mean_pop, input$sigr, m = input$m, fish = input$fish, s50 = input$s50, bin = input$bin)
  })

  # estimation models ----
  mod_a <- reactive({
    model_a(bios(), n = input$nsamp, mat_bin = input$mat_bin, input$a_k)
  })

  # plots ----

  output$alplot <- renderPlot({
    dat = lw()
    plot_al(dat)
  })

  output$wtplot <- renderPlot({

    dat = lw()
    plot_wl(dat)
  })

  output$skipplot <- renderPlot({
    plot_skip(skips())
  })

  output$popplot <- renderPlot({
    plot_pop(pops()) + theme(legend.position = "none")
  })

  output$slxplot <- renderPlot({
    plot_slx(input$s50, input$ages)
  })

  # maturity plots ----
  output$matplot <- renderPlot({
    plot_mature(mod_a(), type = NULL)
  })
  # SSB plots ----
  ssb_a <- reactive({
    ssb(mod_a(), pops(), lw2())
  })


  output$ssb_plot_a <- renderPlot({
    plot_ssb(ssb_a(), age = input$ages, bin = input$bin)
  })

  # percent difference
  output$ssb_apd <- renderPlot({
    plot_pd(ssb_a(), age = input$ages, bin = input$bin)
  })


})
