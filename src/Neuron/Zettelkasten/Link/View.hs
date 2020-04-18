{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Special Zettel links in Markdown
module Neuron.Zettelkasten.Link.View where

import Lucid
import Neuron.Web.Route (Site (..))
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Link.Action
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Rib.Site as Rib
import qualified Data.Map as M

linkActionRender :: Monad m => Site -> ZettelStore -> MarkdownLink -> LinkAction -> HtmlT m ()
linkActionRender site store _ = \case
  LinkAction_ConnectZettel _conn zid -> do
    renderZettelLink site LinkTheme_Default store zid
  LinkAction_QueryZettels _conn linkTheme q -> do
    toHtml q
    let zettels = reverse $ sort $ zettelID <$> runQuery store q
    ul_ $ do
      forM_ zettels $ \zid -> do
        li_ $ renderZettelLink site linkTheme store zid

renderZettelLink :: forall m. Monad m => Site -> LinkTheme -> ZettelStore -> ZettelID -> HtmlT m ()
renderZettelLink Site { siteZettel } ltheme store zid = do
  let Zettel {..} = lookupStore zid store
      zurl = siteZettel M.! zid
      renderDefault :: ToHtml a => a -> HtmlT m ()
      renderDefault linkInline = do
        span_ [class_ "zettel-link"] $ do
          span_ [class_ "zettel-link-idlink"] $ do
            a_ [Rib.rref_ zurl] $ toHtml linkInline
          span_ [class_ "zettel-link-title"] $ do
            toHtml $ zettelTitle
  case ltheme of
    LinkTheme_Default -> do
      -- Special consistent styling for Zettel links
      -- Uses ZettelID as link text. Title is displayed aside.
      renderDefault zid
    LinkTheme_WithDate -> do
      case zettelIDDay zid of
        Just day ->
          renderDefault $ show @Text day
        Nothing ->
          -- Fallback to using zid
          renderDefault zid
    LinkTheme_Simple -> do
      renderZettelLinkSimpleWith zurl (zettelIDText zid) zettelTitle

-- | Render a normal looking zettel link with a custom body.
renderZettelLinkSimpleWith :: forall m a. (Monad m, ToHtml a) => Rib.LocalUrl -> Text -> a -> HtmlT m ()
renderZettelLinkSimpleWith url title body =
  a_ [class_ "zettel-link item", Rib.rref_ url, title_ title] $ do
    span_ [class_ "zettel-link-title"] $ do
      toHtml body
