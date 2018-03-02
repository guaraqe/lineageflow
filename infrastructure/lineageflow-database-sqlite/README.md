# lineageflow-database-sqlite

This is an implementation of a LineageFlow database using SQLite.
Measurements are stored in a folder called `lf-store` using SHA256 hashes as names.

The columns of the database are:

- species;
- specimen;
- tracking;
- domain;
- subdomain;
- codomain;
- measurement.
