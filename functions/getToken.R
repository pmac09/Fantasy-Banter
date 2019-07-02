library(reticulate)


## Source Python Functions
#virtualenv_create(envname = "pyEnv", python = "python3")
#virtualenv_install(envname = "pyEnv", packages = c('oauthlib', 'requests_oauthlib'))
#use_virtualenv("pyEnv", required = TRUE)

py_install(packages= c('oauthlib', 'requests_oauthlib'))
source_python('./functions/getToken.py')