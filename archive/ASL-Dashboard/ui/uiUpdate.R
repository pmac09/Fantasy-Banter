fluidPage(
  
  h2('Update Data'),
  
  fluidRow(
    box(
      textInput("txtAccessCode", label="Enter Access Code:", width='1000%'),
      actionButton("btnUpdateData", label="Update")
    )
  ),
  
  fluidRow(
    column(12,
           "TEST"
           )
  )
)







