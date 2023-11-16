config <- function() {
  config_path <- system.file("config", "config.yaml", package = "Pandora")
  yaml::yaml.load_file(config_path)
}
