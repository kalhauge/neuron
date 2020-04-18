{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module should have been in Rib.
-- it allows for creation and management of safe LocalUrl's.
module Rib.Site 
  ( LocalUrl
  , rref_
  , localUrlRel

  -- Generate LocalUrl's 
  , SiteGen 
  , liftAction

  , runSiteGen
  , makeLocalUrl
  ) where

-- lucid
import Lucid

-- shake
import Development.Shake

-- mtl
import Control.Monad.Writer

-- text
import Data.Text (Text) 
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- rib 
import qualified Rib

-- | A type-safe url that is guranteed to exist.
newtype LocalUrl = LocalUrl 
  { getLocalUrl :: FilePath 
  }

-- | A safe reference to a `LocalUrl
rref_ :: LocalUrl -> Attribute
rref_ url = href_ ("/" <> localUrlRel url)

-- | Get a relative url
localUrlRel :: LocalUrl -> Text
localUrlRel = T.pack . getLocalUrl

-- | A top-level monad to control the generation of a Site.
-- It is essentially a Write monad which stores all the routes needed 
-- to generate the site. 
newtype SiteGen s a = 
  SiteGen (WriterT [(LocalUrl, (s -> Action TL.Text))] Action a)
  deriving newtype (Functor, Applicative, Monad, MonadFail)  

liftAction :: Action a -> SiteGen s a
liftAction m = SiteGen (lift m)

-- | Create a localUrl
makeLocalUrl :: 
  FilePath
  -- | The relative name of the route
  -> (s -> Action TL.Text) 
  -- | The action required to make the route
  -> SiteGen s LocalUrl
makeLocalUrl rel act = do
  -- At SomePoint
  -- Just (SiteConfig { baseUrl }) <- liftAction getShakeExtra
  let url = LocalUrl rel
  SiteGen (tell [(url, act)])
  return $ url

runSiteGen :: SiteGen s s -> Action ()
runSiteGen (SiteGen gen) = do
  (site, routes) <- runWriterT gen
  void $ forP routes $ \(LocalUrl path, generator) -> do
    txt <- generator site
    Rib.writeFileCached path (TL.unpack txt)


