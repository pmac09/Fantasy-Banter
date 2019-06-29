import oauthlib
import requests_oauthlib


def getToken(cid, usr, pwd):
    url = 'https://supercoach.heraldsun.com.au/2019/api/afl/classic/v1/access_token'

    oauth = requests_oauthlib.OAuth2Session(client=oauthlib.oauth2.LegacyApplicationClient(client_id=cid))
    token = oauth.fetch_token(token_url=url,
                              username=usr,
                              password=pwd,
                              client_id=cid,
                              client_secret='')
    
    return(token["access_token"])


