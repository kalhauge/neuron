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

-- | Special Zettel links in Markdown
module Neuron.Zettelkasten.Link where

import Neuron.Zettelkasten.Link.Action
import Neuron.Zettelkasten.Link.View
import Neuron.Zettelkasten.Store
import Neuron.Web.Route (Site)
import Relude
import qualified Text.MMark.Extension as Ext
import Text.MMark.Extension (Extension, Inline (..))


-- | MMark extension to transform @z:/@ links in Markdown
linkActionExt :: Site -> ZettelStore -> Extension
linkActionExt site store =
  Ext.inlineRender $ \f -> \case
    inline@(Link inner uri _title) ->
      let mlink = MarkdownLink (Ext.asPlainText inner) uri
       in case linkActionFromLink mlink of
            Just lact ->
              linkActionRender site store mlink lact
            Nothing ->
              f inline
    inline ->
      f inline
