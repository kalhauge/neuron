{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Clay hiding (s, style, type_)
import qualified Clay as C
import qualified Data.Text.Lazy as TL
import Development.Shake
import Lucid
import Main.Utf8
import Neuron.CLI (run)
import Neuron.Config (Config)
import qualified Neuron.Config as Config
import Neuron.Web.Generate (generateSite)
import Neuron.Web.Route (Site (..), Route (..))
import Neuron.Web.View (renderRouteBody, renderRouteHead, style)
import Relude
import qualified Rib
import qualified Rib.Site as Rib
import Rib.Extra.CSS (googleFonts, stylesheet)

main :: IO ()
main = withUtf8 $ run generateMainSite

generateMainSite :: Action ()
generateMainSite = do
  Rib.buildStaticFiles ["static/**"]
  config <- Config.getConfig
  let writeHtmlRoute :: Site -> Route s g -> (s, g) -> Action TL.Text
      writeHtmlRoute site r = pure . Lucid.renderText . renderPage site config r
  void $ generateSite config writeHtmlRoute ["*.md"]

renderPage :: Site -> Config -> Route s g -> (s, g) -> Html ()
renderPage site config r val@(s, _) = html_ [lang_ "en"] $ do
  head_ $ do
    renderRouteHead config r s
    case r of
      Route_Redirect _ _ ->
        mempty
      _ -> do
        stylesheet "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"
        stylesheet "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css"
        style_ [type_ "text/css"] $ C.renderWith C.compact [] $ mainStyle config
        googleFonts [headerFont, bodyFont, monoFont]
        when (Config.mathJaxSupport config) $
          with (script_ mempty) [id_ "MathJax-script", src_ "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js", async_ ""]
  body_ $
    div_ [class_ "ui text container", id_ "thesite"] $
      renderRouteBody site config r val

headerFont :: Text
headerFont = "Oswald"

bodyFont :: Text
bodyFont = "Open Sans"

monoFont :: Text
monoFont = "Roboto Mono"

mainStyle :: Config -> Css
mainStyle cfg = "div#thesite" ? do
  C.fontFamily [bodyFont] [C.serif]
  C.paddingTop $ em 1
  C.paddingBottom $ em 1
  "p" ? do
    C.lineHeight $ pct 150
  "h1, h2, h3, h4, h5, h6, .ui.header" ? do
    C.fontFamily [headerFont] [C.sansSerif]
  "img" ? do
    C.maxWidth $ pct 50
    C.display C.block
    C.marginLeft C.auto
    C.marginRight C.auto
  style cfg
