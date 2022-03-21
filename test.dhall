let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs" ],
  dependencies = conf.dependencies # [  "free", "node-buffer", "node-fs-aff", "test-unit"  ]
}
