config <- function() {
  config_path <- system.file("inst", "config.yaml", package = "Pandora")
  yaml::yaml.load_file(config_path)
}
