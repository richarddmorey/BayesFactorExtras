# add globalVariables to please R CMD check for NSE ...
if (getRversion() >= "2.15.1")  utils::globalVariables(c("group", "Alt"))