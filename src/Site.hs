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
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import           Snap.Core
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

maybeCreds = runMaybeT $ do
  Just email <- lift $ getParam "email"
  Just pass <- lift $ getParam "password"
  return (email, pass)

authHandler = do
    loggedIn <- with auth $ isLoggedIn
    if loggedIn then writeText "" else modifyResponse $ setResponseCode 401

registerHandler = do
    creds <- maybeCreds
    maybe failure register creds

  where
    failure = do
      modifyResponse $ setResponseCode 401
      writeText "Must specify email and pass"

    register creds = do
      rsp <- with auth $ registerUser "email" "password"
      traceShow rsp (writeText "user registered")


loginHandler = do
    creds <- maybeCreds
    maybe failure login creds

  where
    failure = modifyResponse $ setResponseCode 401
    login creds = do
      rsp <- with auth $ loginByUsername (fst creds) (ClearText $ snd creds) True
      traceShow rsp (writeText "")


logoutHandler = logout >> putResponse emptyResponse


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [("", serveDirectory "static"),
          ("/register", registerHandler),
          ("/login", loginHandler),
          ("/logout", with auth logoutHandler),
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

