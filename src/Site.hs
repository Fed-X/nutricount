{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
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

authHandler :: Handler App App ()
authHandler = modifyResponse $ setResponseCode 401


registerHandler :: Handler App App ()
registerHandler = do
    -- check if exists
    email' <- email
    pass'  <- pass
    return $ registerUser <$> email' <*> pass'
    --loginUser email pass "9999"
    writeText ""

  where
    email = withRequest (\r -> return (head <$> (rqParam "email" r)))
    pass  = withRequest (\r -> return (head <$> (rqParam "pass" r)))


loginHandler :: Handler App App ()
loginHandler = writeText ""
  -- email
  -- password
  -- check creds against db
  -- forward to login || not authorized


logoutHandler :: Handler App (AuthManager App) ()
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

