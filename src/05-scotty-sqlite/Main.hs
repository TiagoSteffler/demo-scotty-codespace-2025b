{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Network.HTTP.Types.Status (status404)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

-- Define the User data type
data User = User
  { userId   :: Int
  , name     :: String
  , email    :: String
  } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance ToRow User where
  toRow (User id_ name_ email_) = toRow (id_, name_, email_)

-- Main entry point
main :: IO ()
main = do
  conn <- open "users.db"
  initDB conn
  putStrLn "Server running on http://localhost:3000"
  scotty 3000 $ do

    -- GET /users - list all users
    get "/users" $ do
      users <- liftAndCatchIO $ query_ conn "SELECT id, name, email FROM users" :: ActionM [User]
      json users

    -- GET /users/:id - get user by id
    get "/users/:id" $ do
      idParam <- param "id"
      result <- liftAndCatchIO $ query conn "SELECT id, name, email FROM users WHERE id = ?" (Only (idParam :: Int)) :: ActionM [User]
      if null result
        then status status404 >> json ("User not found" :: String)
        else json (head result)

    -- POST /users - create new user (expects JSON)
    post "/users" $ do
      user <- jsonData :: ActionM User
      liftAndCatchIO $ execute conn "INSERT INTO users (id, name, email) VALUES (?, ?, ?)" user
      json ("User created" :: String)

    -- PUT /users/:id - update user (expects JSON)
    put "/users/:id" $ do
      idParam <- param "id"
      user <- jsonData :: ActionM User
      let updatedUser = user { userId = idParam }
      liftAndCatchIO $ execute conn "UPDATE users SET name = ?, email = ? WHERE id = ?" (name updatedUser, email updatedUser, userId updatedUser)
      json ("User updated" :: String)

    -- DELETE /users/:id - delete user
    delete "/users/:id" $ do
      idParam <- param "id"
      liftAndCatchIO $ execute conn "DELETE FROM users WHERE id = ?" (Only (idParam :: Int))
      json ("User deleted" :: String)

-- Initialize database
initDB :: Connection -> IO ()
initDB conn = execute_ conn
  "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT, email TEXT)"

