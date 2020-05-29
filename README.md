# Introduction
Celery Man is an employee salary management web application for [the task](./resources/task.pdf) described in `./resources/task.pdf`. We implement user stories 1 & 2.

## Endpoints
- `/` contains the web ui
- `/users/upload` for importing users (user story 1)
- `/users` for retrieving users (user story 2)

## User story 1: Upload
- Upload button sends a post request to the backend.
- Unpolished UI, obscure error messages, lack of logging - we deprioritize these issues for now, instead focusing on a working demo.
- Atomicity - we utilize database transactions to commit our imports, if imports result in an invalid state then no changes are made. This also means that concurrent uploads are supported, the first file to be received will be imported and the subsequent file will overwrite the first file if there are any overlapping rows.

## User Story 2: Dashboard
- UI is server rendered, hence API is for automated testing purposes only. Both routes call the same core functions.
- For the sort field, prefix, "+" is a reserved character that decodes to the space character " ". Instead, "%2B" decodes to "+" on the backend. However, we allow all mentioned examples in the query string.

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
├── js                  -- javascript for UI
├── migrations          -- additional tools for managing db resources
│   └── scripts         -- sql for creating db resources
├── resources           -- non-code resources
├── src
│   ├── API             -- servant api
│   ├── Core            -- base types, validation, core logic
│   ├── Database        -- interfacing with postgres
│   └── Interface       -- for rendering the UI
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