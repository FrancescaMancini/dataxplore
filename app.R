# Required to
library(devtools)

if (!require("occAssess")){
  install_github("https://github.com/robboyd/occAssess.git")
}

load_all()
run_app()
