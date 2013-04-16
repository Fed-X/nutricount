{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Debug.Trace
import           Control.Applicative
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Snap.Core
import           Snap.Extras
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application

instance ToJSON AuthFailure where
    toJSON UserNotFound      = object ["error" .= ("User not found" :: String)]
    toJSON IncorrectPassword = object ["error" .= ("Incorrect password" :: String)]
    toJSON PasswordMissing   = object ["error" .= ("Missing password" :: String)]
    toJSON (AuthError e)     = object ["error" .= e]

maybeCreds = runMaybeT $ do
    Just email <- lift $ getParam "email"
    Just pass  <- lift $ getParam "password"
    return (email, pass)

authHandler = do
    user <- with auth $ currentUser
    writeJSON $ toJSON user

registerHandler = do
    creds <- maybeCreds
    maybe failure register creds

  where
    failure = writeJSON $ Object $ HM.singleton "message" "Enter your email and password."

    register creds = do
        exists <- with auth $ usernameExists $ E.decodeUtf8 $ fst creds
        if exists 
          then writeJSON $ Object $ HM.singleton "message" "Email already exists."
          else do
              user <- with auth $ registerUser "email" "password"
              case user of
                  Right u -> do
                      with auth $ forceLogin u
                      writeJSON $ Object $ HM.singleton "user" $ toJSON u
                  Left e ->
                      writeJSON $ Object $ HM.singleton "user" $ toJSON e


loginHandler = do
    creds <- maybeCreds
    maybe failure login creds

  where
    failure = writeJSON $ Object $ HM.singleton "message" "Enter your email and password."

    login creds = do
        rsp <- with auth $ loginByUsername (fst creds) (ClearText $ snd creds) True
        case rsp of
            Right u -> writeJSON $ Object $ HM.singleton "user"    $ toJSON u
            Left  e -> writeJSON $ Object $ HM.singleton "message" $ toJSON e

logoutHandler = with auth logout >> writeText "over 'n out"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [("", serveDirectory "static"),
          ("/register", registerHandler),
          ("/login", loginHandler),
          ("/logout", logoutHandler),
          ("/auth", authHandler)
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    addRoutes routes
    addAuthSplices auth
    return $ App h s a d

