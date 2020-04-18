{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Main module for using neuron as a library, instead of as a CLI tool.
module Neuron.Web.Generate
  ( generateSite,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import Development.Shake (Action)
import qualified Neuron.Config as Z
import Neuron.Version (neuronVersion, olderThan)
import qualified Neuron.Web.Route as Z
import qualified Neuron.Zettelkasten.Graph as Z
import qualified Neuron.Zettelkasten.Store as Z
import Options.Applicative
import Relude
import qualified Rib
import qualified Rib.Site as Rib

-- | Generate the Zettelkasten site
generateSite ::
  Z.Config ->
  (forall a. 
    Z.Site
    -> Z.Route Z.ZettelStore Z.ZettelGraph a 
    -> (Z.ZettelStore, Z.ZettelGraph, a) 
    -> Action TL.Text) ->
  [FilePath] ->
  Action (Z.ZettelStore, Z.ZettelGraph)
generateSite config writeHtmlRoute' zettelsPat = do
  when (olderThan $ Z.minVersion config) $ do
    error $ "Require neuron mininum version " <> Z.minVersion config 
          <> ", but your neuron version is " <> neuronVersion
  zettelStore <- Z.mkZettelStore =<< Rib.forEvery zettelsPat pure
 
  let zettelGraph = Z.mkZettelGraph zettelStore
  
  Rib.runSiteGen $ do 
    let makeLocalRoute v r = Rib.makeLocalUrl (Z.routeFile r) $ \site -> 
          writeHtmlRoute' site r (zettelStore, zettelGraph, v)

    -- Generate HTML for every zettel
    siteZettel' <- forM (Map.keys zettelStore) $ \zid -> 
      (zid,) <$> makeLocalRoute () (Z.Route_Zettel zid)
    -- Generate the z-index
    siteZIndex <- makeLocalRoute () Z.Route_ZIndex
    -- Generate search page
    siteSearch <- makeLocalRoute () Z.Route_Search
    -- Write alias redirects, unless a zettel with that name exists.
    aliases <- Z.getAliases config zettelStore
    
    siteZettelAliases' <- forM aliases $ \Z.Alias {..} ->
      (aliasZettel,) <$> makeLocalRoute targetZettel (Z.Route_Redirect aliasZettel)

    pure $ Z.Site
      { siteZIndex = siteZIndex
      , siteZettel = Map.fromList (siteZettel' <> siteZettelAliases')
      , siteSearch = siteSearch
      }

  pure (zettelStore, zettelGraph)


