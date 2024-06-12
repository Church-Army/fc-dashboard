library(Microsoft365R)
library(AzureAuth)
library(AzureGraph)

app      <- "cf81189c-b1be-492e-929e-6e47c3706346"
tenant   <- "ChurchArmy787"
redirect <- "https://church-army.shinyapps.io/FCtest"
resource <- c("https://graph.microsoft.com/.default", "openid")
secret <- readLines("fc-dashboard/app-secrets/microsoft-app-secret")

token <- get_azure_token(resource, tenant, app, password = secret)

gr <- create_graph_login()

gr$list_users()

me <- gr$get_user(email = "david.lovell@churcharmy.org")

head(me$list_group_memberships())
me$list_sharepoint_sites()

ru <- gr$get_sharepoint_site("https://churcharmy787.sharepoint.com/sites/ResearchUnit")

app_files <-
  ru$
  get_drive()$
  get_item("Project - Internal - Fundraising reporting/files-for-app")

ru$get_drive(drive_id = "b!FBa-5Y9Qw0iz5k2Naz0eVg8dJm_6qmdFqIQgce_w0yO1bfQBIQ5UQ53Df-ootpzr") -> foo
