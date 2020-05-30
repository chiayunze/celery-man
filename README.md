# Introduction
Celery Man is an employee salary management web application for [this task](./resources/task.pdf). We implement user stories 1, 2, 4.

## Endpoints
- `/` contains the web ui
- `/users/upload` for importing users (user story 1)
- `/users` for retrieving users (user story 2)
- `/users/largeupload` for large file upload (user story 4)

Websocket support is required for the application to work properly. For demo purposes we use the ws protocol instead of wss.

## User story 1: Upload
- Atomicity - we utilize database transactions to commit our imports, if imports result in an invalid state then no changes are made. This also means that concurrent uploads are supported, the first file to be received will be imported and the subsequent file will overwrite the first file if there are any overlapping rows.
- Unpolished UI, obscure error messages, lack of logging, unpooled db connections - we deprioritize these issues for now, instead focusing on a working demo.

## User Story 2: Dashboard
- UI is server rendered, hence API is for automated testing purposes only. Both routes call the same core functions.
- For the sort field, prefix, "+" is a reserved character that decodes to the space character " ". Instead, "%2B" decodes to "+" on the backend. However, we allow all mentioned examples in the query string.

## User Story 4: Large File Upload
This is an interesting task. Our goal will be to 1) have responsive user feedback, 2) ingest the file in constant memory. We do this using a streaming approach.
1. Open a websocket connection from the client (user's browser) to the server.
2. Send the file over the websocket connection in chunks. For now we have set it to be 1MB chunks. By rendering on the UI the number of bytes we have sent across, the user can get an idea of how long the upload will take. We can also make it such that the server sends a "number of bytes received" message and render this in real time due to our duplex websocket connection.
3. On the server end, we receive the chunks and start our ingestion in a streaming fashion. We decode and parse the bytes into records as we receive it.
4. As each record is being built from bytes, we also validate them (i.e. salary cannot be negative).
5. Every n records, we flush our memory into the database by performing an insert into a temporary table. For now we have set n to be 500. Constraints set up in the database will inform us whether that set of records is valid (i.e. non-unique ids or logins).
6. Once all the data has been streamed and inserted into the temp table, we copy the temp table into the main table in a single transaction. At this point we can know whether the new records will clash with the existing records, if it does, the copy is aborted.
7. At any point in step 3, 4, 5, 6, if an error occurs we send an appropriate error message to the client and abort the entire process, including cleaning up of resources like deleting the temp table and closing the websocket connection.

The streaming approach allows us to keep the process within constant memory. We can tune the chunk size (step 2) or the record cache (step 5) to meet our requirements. Streaming also allows us to handle any errors and halt the import process once it is found, instead of waiting for the entire file to be uploaded. The user can be informed promptly when this happens, saving time.

An additional benefit of our streaming approach is that we can eventually build in a feature for resuming of partial uploads (for instance, if the user disconnects halfway).

Since we utilize the atomic semantics of database transactions here as well, we get free concurrency built in. By using unique temp tables, multiple users can upload at once. We handle concurrency on the server end with green threads (that can be multiplexed to multiple OS threads if scaling up is required, thanks to GHC). The only limitation will be the database instance.

# Quickstart with docker
```
docker build -t . celery-man
docker run -p 8081:8081 -e PG_USER=postgres PG_PASSWORD=pw PG_HOST=127.0.0.1 PG_PORT=5432 celery-man
```
The build process may be pretty lengthy depending on compute power since GHC needs to build all dependencies from scratch. Parallel building is supported if more cpus are available. Make sure sufficient memory is available.

Access the application in your browser on localhost:8081. The browser must support websockets.

# Setting up the database
Refer to [this document](./migrations/README.md) for details.

Or just run the sql queries in `./migrations/scripts`.

# Repository structure
```
.
├── migrations          -- additional tools for managing db resources
│   └── scripts         -- sql for creating db resources
├── resources           -- non-code resources
├── src
│   ├── API             -- servant api
│   ├── Core            -- base types, validation, core logic
│   ├── Database        -- interfacing with postgres
│   └── Interface       -- for rendering the UI
├── static              -- html and js for ui
└── test                -- for testing,  mirrors src/
```

# Building, running, testing the application
```
cabal update
cabal v2-build
cabal v2-run celery-man
cabal v2-test
```
The application is exposed on port 8081 by default.

# Contributing
We use cabal as the build tool. Make sure it is version >=3.0
```
cabal --version
```

Set up hlint for lilnting and stylish-haskell for additional code formatting
```
cabal update; cabal install hlint stylish-haskell
```

Environment variables required (set these to point to your dev postgres instance)
- `PG_USER`
- `PG_PASSWORD`
- `PG_HOST`
- `PG_PORT`

Run migrations on your dev postgres instance to create the required tables. See [here](./migrations/README.md) for more details.

The web user interface requires websockets, check that your browser supports it.

Code should be `-Wall` and `hlint` clean. Imports to be `stylish-haskell` formatted.