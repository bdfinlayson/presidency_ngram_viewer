build_ngram_detail_column <-
  function(title = '',
           president_name = '',
           n_times_said = 0,
           start_year = 1900,
           end_year = 1900) {
    column(
      3,
      h5(title),
      tags$image(
        src = name_to_url(president_name),
        height = '100px',
        width = '80px'
      ),
      br(),
      em(president_name),
      br(),
      strong(str_interp("Times said: ${n_times_said}")),
      br(),
      strong(str_interp(
        "Years said: ${start_year} - ${end_year}"
      ))
    )
  }