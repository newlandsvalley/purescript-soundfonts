let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs" ],
  dependencies = conf.dependencies # [  "node-fs-aff", "test-unit"  ]
}
