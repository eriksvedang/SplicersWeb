# Daily backups
# heroku pg:backups schedule DATABASE_URL --at '00:00 Europe/Stockholm' --app splicers

# Backup and download
heroku pg:backups capture
curl -o latest.dump `heroku pg:backups public-url`

# Restore
pg_restore --verbose --clean --no-acl --no-owner -h localhost -U erik -d splicers latest.dump
