library(reticulate)

virtualenv_create(envname = "pyEnv",python = "python3")
virtualenv_install(envname = "pyEnv", packages = c('oauthlib', 'requests_oauthlib'))
use_virtualenv("pyEnv", required = TRUE)

source_python('./ASL-Dashboard/functions/getToken.py')

getToken(
  cid= Sys.getenv("SC_CID"),
  usr= Sys.getenv("SC_USR"),
  pwd= Sys.getenv("SC_PWD")
)
