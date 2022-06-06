let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "example/src/**/*.purs" ],
  dependencies = conf.dependencies # [ "newtype"
                                     , "unfoldable"
                                     , "unsafe-coerce"
                                     , "web-dom"
                                     , "web-events"
                                     , "web-html" 
                                     ]
  
}
