class Config()
def renderWebsite(path: String)(using config: Config): String = ???
def renderWidget(using Config): Unit = renderWebsite("/tmp")(Config())
