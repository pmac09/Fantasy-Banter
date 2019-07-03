data <- list()

data <- download(projectURL= DATABASE_URL, fileName= "ASL_DASHBOARD")
patch(data, DATABASE_URL, directory = "ASL_DASHBOARD")
delete(x=mtcars, DATABASE_URL, directory = "ASL_DASHBOARD/SC_COACHES")

data <- list()

data[['SC_COACH']] <- list(
  "1" = list('SC_COACH_ID' = 1, 'FIRST_NAME' = 'James',   'LAST_NAME' = 'Garretto', 'NICKNAME' = 'Garter'),
  "2" = list('SC_COACH_ID' = 2, 'FIRST_NAME' = 'Lester',  'LAST_NAME' = 'Ilagan',   'NICKNAME' = 'Lester'),
  "3" = list('SC_COACH_ID' = 3, 'FIRST_NAME' = 'Luke',    'LAST_NAME' = 'Kapulica', 'NICKNAME' = 'Kappaz'),
  "4" = list('SC_COACH_ID' = 4, 'FIRST_NAME' = 'Paul',    'LAST_NAME' = 'McGrath',  'NICKNAME' = 'Pmac'),
  "5" = list('SC_COACH_ID' = 5, 'FIRST_NAME' = 'Anthony', 'LAST_NAME' = 'Melino',   'NICKNAME' = 'Melons'),
  "6" = list('SC_COACH_ID' = 6, 'FIRST_NAME' = 'Jordan',  'LAST_NAME' = 'Merceica', 'NICKNAME' = 'Jmerc'),
  "7" = list('SC_COACH_ID' = 7, 'FIRST_NAME' = 'Simon',   'LAST_NAME' = 'Minness',  'NICKNAME' = 'Chief'),
  "8" = list('SC_COACH_ID' = 8, 'FIRST_NAME' = 'Mark',    'LAST_NAME' = 'Richards', 'NICKNAME' = 'Spoonie'),
  "9" = list('SC_COACH_ID' = 9, 'FIRST_NAME' = 'Lachie',  'LAST_NAME' = 'Nunn',     'NICKNAME' = 'Autobot')
)

data[['SC_TEAM']] <- list(
  "2019-1" = list("SEASON"= 2019, "SC_COACH_ID"= 1, "SC_TEAM_NAME"= "Fire The Worpedo"),
  "2019-2" = list("SEASON"= 2019, "SC_COACH_ID"= 2, "SC_TEAM_NAME"= "DEVQON.1"),
  "2019-3" = list("SEASON"= 2019, "SC_COACH_ID"= 3, "SC_TEAM_NAME"= "Man of Steele"),
  "2019-4" = list("SEASON"= 2019, "SC_COACH_ID"= 4, "SC_TEAM_NAME"= "TryingAnewStratton"),
  "2019-5" = list("SEASON"= 2019, "SC_COACH_ID"= 5, "SC_TEAM_NAME"= "Salt&VinegarCripps"),
  "2019-6" = list("SEASON"= 2019, "SC_COACH_ID"= 6, "SC_TEAM_NAME"= "Casting Crouch"),
  "2019-7" = list("SEASON"= 2019, "SC_COACH_ID"= 7, "SC_TEAM_NAME"= "Barbeques Treloar"),
  "2019-8" = list("SEASON"= 2019, "SC_COACH_ID"= 8, "SC_TEAM_NAME"= "Swimming n Titch")
)

data[['SC_GAME']] <- list(
  "2019-1-1" = list("SEASON"= 2019, "ROUND"= 1, "SC_COACH_ID"= 1, "SC_OPP_ID"= 2)
)










