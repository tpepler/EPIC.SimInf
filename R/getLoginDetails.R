#***********************************************************
# Function to interactively get login details for EPIC server
## Based on code by Barry Rowlingson
## http://r.789695.n4.nabble.com/tkentry-that-exits-after-RETURN-tt854721.html#none

getLoginDetails <- function(){
  #require(tcltk)
  cat('Please supply your login details to extract data from the EPIC server.\n')
  tt <- tktoplevel()
  tkwm.title(tt, "Get login details")
  Name <- tclVar("username")
  Password <- tclVar("Password")
  entry.Name <- tkentry(tt, width = "20", textvariable = Name)
  entry.Password <- tkentry(tt, width = "20", show = "*",
                            textvariable = Password)
  tkgrid(tklabel(tt, text = "EPIC server login details:"))
  tkgrid(entry.Name)
  tkgrid(entry.Password)

  OnOK <- function()
  {
    tkdestroy(tt)
  }
  OK.but <- tkbutton(tt, text = " OK ", command = OnOK)
  tkbind(entry.Password, "<Return>", OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)

  invisible(c(uid = tclvalue(Name), pwd = tclvalue(Password)))
}

#***********************************************************
