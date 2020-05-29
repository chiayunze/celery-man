CREATE TABLE celery_man.employees (
    id VARCHAR(255) PRIMARY KEY,
    login VARCHAR(255) UNIQUE,
    name VARCHAR(255),
    salary NUMERIC(16, 2)
)