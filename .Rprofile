source("renv/activate.R")

# Set print width
options(width = 80)

# Set significant figures
options(digits = 4)

# Set max print length
options(max.print = 100)

# Don't ask to save workspace on exit
utils::assignInNamespace(
  "q", 
  function(save = "no", status = 0, runLast = TRUE) 
  {
    .Internal(quit(save, status, runLast))
  }, 
  "base"
)
utils::assignInNamespace(
  "quit", 
  function(save = "no", status = 0, runLast = TRUE) 
  {
    .Internal(quit(save, status, runLast))
  }, 
  "base"
)

# Permanently set mirror
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org"
  options(repos = r)
})

# Disable completion from the language server - handled by cmp-nvim-r 
options(languageserver.server_capabilities =
        list(completionProvider = FALSE, completionItemResolve = FALSE))
