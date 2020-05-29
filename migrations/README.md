We use the migrate tool for version control of database schemas.

# To set up the database for our application
1. Install the [migrate](https://hackage.haskell.org/package/postgresql-simple-migration) tool (refer to Dockerfile if required)
2. `cd migrations`
3. Ensure the required connection environment variables are present (see comments in _runMigration.sh_)
4. Run `./runMigration.sh` to create the resources

# Updating existing resource schemas or creating new resources
1. `cd migrations`
2. Run `./newTemplate.sh` to get a timestamped sql template
3. Modify the sql template to your needs
4. Run `./runMigration.sh` to apply the new changes
5. Commit the new sql file to the code repository

# Migration tracking
Migration tracking is stored in _public.schema_migrations_ table in the postgres database

# Using Docker to build migrate and run migrations
1. `cd migrations`
2. `docker build -t migrate .` (this might take a while) 
3. Replace env vars as appropriate: `docker run --rm -v $PWD:/workdir -e PG_USER=postgres -e PG_PASSWORD=pw -e PG_HOST=127.0.0.1 -e PG_PORT=5432 migrate ./runMigration.sh`