cbf.setwd.currentpath = setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

cbf.dropboxpath = function() {

  if (Sys.info()['sysname'] != "Windows") {
    stop("CBF Dropbox path currently only availble on Windows")
  }

  locations = list(
    LOCALAPPDATA = file.path(Sys.getenv("LOCALAPPDATA"), "Dropbox\\info.json", fsep = "\\"),
    APPDATA = file.path(Sys.getenv("APPDATA"), "Dropbox\\info.json", fsep = "\\")
  )

  LOCATION = NULL
  for (loc in locations) {
    if (file.exists(loc)) {
      LOCATION <- loc
    }
  }

  if(is.null(LOCATION)) {stop("Dropbox config file not found (sorry we tried!)")}

  dropboxpath = jsonlite::fromJSON(txt = LOCATION)$personal$path
  return(normalizePath(dropboxpath, winslash = "/"))
}
