{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans (liftIO, lift)
import           Control.Monad.Trans.Either
import           Control.Error.Safe (tryJust)
import           Control.Lens
import           Data.ByteString (ByteString)
import           Data.Maybe
import           Data.Pool (withResource)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.SqliteSimple
import           Snap.Util.FileServe
import           Heist()
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
import qualified Db
import           Util

type H = Handler App (AuthManager App)

maybeWhen :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeWhen Nothing _  = return ()
maybeWhen (Just a) f = f a

-- | Render login form
handleLogin :: Maybe T.Text -> H ()
handleLogin authError =
  heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = [("loginError", I.textSplice c) | c <- maybeToList authError]

-- | Handle login submit.  Either redirect to '/' on success or give
-- an error.  We deliberately do NOT show the AuthFailure on the login
-- error, as we don't want to reveal to visitors whether or not the
-- login exists in the user database.
handleLoginSubmit :: H ()
handleLoginSubmit =
  loginUser "login" "password" Nothing
    (const . handleLogin . Just $ "Unknown login or incorrect password")
    (redirect "/")

-- | Logs out and redirects the user to the site index.
handleLogout :: H ()
handleLogout = logout >> redirect "/"

-- | Handle new user form submit
handleNewUser :: H ()
handleNewUser =
  method GET (renderNewUserForm Nothing) <|> method POST handleFormSubmit
  where
    handleFormSubmit = do
      authUser <- registerUser "login" "password"
      either (renderNewUserForm . Just) login authUser

    renderNewUserForm (err :: Maybe AuthFailure) =
      heistLocal (I.bindSplices errs) $ render "new_user"
      where
        errs = [("newUserError", I.textSplice . T.pack . show $ c) | c <- maybeToList err]

    login user = logRunEitherT $ do
      lift $ forceLogin user >> (return $ redirect "/")

-- | Run actions with a logged in user or go back to the login screen
withLoggedInUser :: (Db.User -> H ()) -> H ()
withLoggedInUser action =
  currentUser >>= go
  where
    go Nothing  = handleLogin (Just "Must be logged in to view the main page")
    go (Just u) = logRunEitherT $ do
      uid  <- tryJust "withLoggedInUser: missing uid" (userId u)
      uid' <- hoistEither (reader T.decimal (unUid uid))
      return $ action (Db.User uid' (userLogin u))

handleCommentSubmit :: H ()
handleCommentSubmit = method POST (withLoggedInUser go)
  where
    go user = do
      c <- getParam "comment"
      maybeWhen c (withTop db . Db.saveComment user . T.decodeUtf8)
      redirect "/"

handleTodos :: H ()
handleTodos = method GET (withLoggedInUser go)
  where
    go user = do
      modifyResponse $ setContentType "application/json"
      writeText "[ { \"id\":1, \"text\":\"haloo\", \"done\":\"false\" } ]"

-- | Render main page
mainPage :: H ()
mainPage = withLoggedInUser go
  where
    go :: Db.User -> H ()
    go _user = do
      serveDirectory "static"

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",        with auth handleLoginSubmit)
         , ("/logout",       with auth handleLogout)
         , ("/new_user",     with auth handleNewUser)
         , ("/save_comment", with auth handleCommentSubmit)
         , ("/api/todo",     with auth handleTodos)
         , ("/",             with auth mainPage)
         , ("/static",       serveDirectory "static")
         ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    -- addRoutes must be called before heistInit - heist wants to
    -- serve "" itself which means our mainPage handler never gets a
    -- chance to get called.
    addRoutes routes
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- Initialize auth that's backed by an sqlite database
    d <- nestSnaplet "db" db sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d

    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let connPool = sqlitePool $ d ^# snapletValue
    liftIO $ withResource connPool $ \conn -> Db.createTables conn

    addAuthSplices auth
    return $ App h s d a

