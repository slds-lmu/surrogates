load_from_csv = function(filepath) {
  data = as.data.frame(fread(filepath))

  return(data)
}

# TODO: implement loading different file formats
load_rds = function(filepath) {}
load_arff = function(filepath) {}
load_oml = function(){}
