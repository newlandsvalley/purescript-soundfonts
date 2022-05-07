let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs" ],
  dependencies = conf.dependencies # [  "node-buffer", "node-fs-aff", "spec"  ]
}
