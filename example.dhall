let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "example/src/**/*.purs" ]
}
