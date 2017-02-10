{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad       (filterM, forM_)
import           Data.Monoid         ((<>))
import qualified Data.Set            as S
import           Data.Time.Clock
import           Data.Time.Format
import           Debug.Trace
import           Hakyll
import           System.FilePath
import           System.Process
import           Text.Pandoc.Options

config :: Configuration
config = defaultConfiguration { deployCommand = deployCmd
                              , inMemoryCache = True
                              }

deployCmd :: String
deployCmd = "./build.sh && rsync --checksum -ave 'ssh' --delete _site/* calisto:anler.me"

main :: IO ()
main = do
  lastCommit <- readProcess "git" ["rev-parse", "--short", "master"] ""
  lastUpdate <- getCurrentTime >>= return . formatTime defaultTimeLocale "%d/%m/%Y"

  let defaultCtx = constField "lastUpdate" lastUpdate
                   <> constField "lastCommit" lastCommit
                   <> defaultContext
  hakyllWith config (hakyllRules defaultCtx)

hakyllRules :: Context String -> Rules ()
hakyllRules defaultCtx = do
  forM_ ["images/*", "fonts/*", "js/*.js"] $ \pattern -> do
    match pattern $ do
      route idRoute
      compile copyFileCompiler

  match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler

  match "css/*.less" $ do
    route $ setExtension "css"
    compile $ getResourceString >>= withItemBody (unixFilter "lessc" ["-"])

  match "static/*" $ do
    route $ composeRoutes (gsubRoute "static/" (const "")) idRoute
    compile copyFileCompiler

  match "pages/*" $ do
    route $ stripRoute "pages/"
      `composeRoutes` setExtension "html"
      `composeRoutes` cleanRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultCtx
      >>= relativizeUrls
      >>= useCleanUrls

   -- match "sicp/*" $ do
   --   route $ setExtension "html"
   --   compile $ pandocMathCompiler
   --     >>= loadAndApplyTemplate "templates/exercise.html" postCtx
   --     >>= loadAndApplyTemplate "templates/default.html" postCtx
   --     >>= relativizeUrls

   -- match "cis194/*" $ do
   --   route $ setExtension "html"
   --   compile $ pandocMathCompiler
   --     >>= loadAndApplyTemplate "templates/exercise.html" postCtx
   --     >>= loadAndApplyTemplate "templates/default.html" postCtx
   --     >>= relativizeUrls

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  match "posts/*" $ do
    let postPublished meta = const (maybe "" (++ "-") (lookupString "published" meta))
        postTitle meta = gsubRoute "posts/" $ postPublished meta
    route $ setExtension "html"
      `composeRoutes` metadataRoute postTitle
      `composeRoutes` cleanRoute
    compile $ pandocMathCompiler
      >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
      >>= loadAndApplyTemplate "templates/default.html" defaultCtx
      >>= relativizeUrls
      >>= useCleanUrls

  tagsRules tags $ \tag pattern -> do
    let title = "Entradas etiquetadas \"" ++ tag ++ "\""
    route $ setExtension "html"
      `composeRoutes` gsubRoute "tags/" (const "etiqueta-")
      `composeRoutes` cleanRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title
                <> listField "posts" postCtx (return posts)
                <> defaultCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag-list.html" (constField "tag" tag <> ctx)
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= useCleanUrls

  create ["archivo.html"] $ do
    route $ idRoute `composeRoutes` cleanRoute
    compile $ do
      posts <- recentFirst =<< publishedOnly =<< loadAll "posts/*"
      let archiveCtx = listField "posts" postCtx (return posts)
                       <> constField "title" "Archivo"
                       <> defaultCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" defaultCtx
        >>= relativizeUrls
        >>= useCleanUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< publishedOnly =<< loadAll "posts/*"
      let indexCtx = listField "posts" postCtx (return posts)
                     <> constField "title" "Inicio"
                     <> defaultCtx

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls
        >>= useCleanUrls

  match "templates/*" $ compile templateCompiler

publishedOnly :: MonadMetadata m => [Item a] -> m [Item a]
publishedOnly items = filterM isNotDraft items

postCtx :: Context String
postCtx =
    dateField "date" "%d/%m/%Y" <>
    field "id" ((`getMetadataField'` "id") . itemIdentifier) <>
    field "teaser" ((`getMetadataField'` "teaser") . itemIdentifier) <>
    defaultContext

isDraft :: MonadMetadata m => Item a -> m Bool
isDraft item = do
  maybeRawValue <- getMetadataField (itemIdentifier item) "draft"
  return $ case maybeRawValue of
             Just "true" -> True
             _           -> False

isNotDraft :: MonadMetadata m => Item a -> m Bool
isNotDraft = fmap not . isDraft

postCtxWithTags :: Tags -> Context String
postCtxWithTags = (<> postCtx) . tagsField "tags"

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
  pandocCompilerWith defaultHakyllReaderOptions writerOptions
  where
    mathExtensions = [ Ext_tex_math_dollars
                     , Ext_tex_math_double_backslash
                     , Ext_latex_macros
                     ]
    defaultExtensions = writerExtensions defaultHakyllWriterOptions
    newExtensions = foldr S.insert defaultExtensions mathExtensions
    writerOptions = defaultHakyllWriterOptions {
      writerExtensions = newExtensions
      , writerHTMLMathMethod = MathJax ""
      }

stripRoute :: String -> Routes
stripRoute = (`gsubRoute` const "")

cleanRoute :: Routes
cleanRoute = customRoute filenameToIndex

filenameToIndex :: Identifier -> FilePath
filenameToIndex identifier =
  let (path, ext) = splitExtensions $ toFilePath identifier
  in
    path </> "index" <.> ext

useCleanUrls :: Item String -> Compiler (Item String)
useCleanUrls = return . fmap (withUrls cleanUrl)

cleanUrl :: String -> String
cleanUrl url =
  let (path, ext) = splitExtensions url
  in
    if ext == ".html"
    then
      if takeBaseName path == "index"
      then take (length url - 10) url
      else take (length url - 5) url </> ""
    else url
