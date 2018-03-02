# lineageflow-database

This package defines the `Database` type, which is a record that allow
different database implementations to be used, provided they obey the
interface:

```haskell
data Database = Database
  { db_get :: MQuery -> IO FilePath
  , db_put :: AQuery -> MQuery -> FilePath -> IO ()
  , db_delete :: MQuery -> IO ()
  , db_fields :: IO [Text]
  , db_search :: Assoc Text -> IO [MInfo]
  , db_match :: Assoc Text -> Text -> IO [Text]
  }
```

Therefore, this interface allows to:

- get measurements from the database;
- put measurements in the database;
- delete measurements from the database;
- declare the fields (columns) it uses for storage;
- search for matching measurements;
- get possible column values matching search parameters.
