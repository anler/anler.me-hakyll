{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Set            as S
import           Hakyll
import           Text.Pandoc.Options

config :: Configuration
config = defaultConfiguration { deployCommand = deployCmd
                              , providerDirectory = "src/"}

deployCmd :: String
deployCmd = "./site build && rsync --checksum -ave 'ssh' --delete _site/* calisto:anler.me"

main :: IO ()
main = hakyllWith config $ do
    match "images/*" (route idRoute >> compile copyFileCompiler)
    match "fonts/*" (route idRoute >> compile copyFileCompiler)
    match "css/*" (route idRoute >> compile compressCssCompiler)

    match "static/*" $ do
      route $ composeRoutes (gsubRoute "static/" (const "")) idRoute
      compile copyFileCompiler

    match "pages/*" $ do
      route $ composeRoutes
        (gsubRoute "pages/" (const ""))
        (setExtension "html")
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    match "sicp/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/exercise.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "cis194/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/exercise.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged \""++tag++"\""
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title
                  `mappend` listField "posts" postCtx (return posts)
                  `mappend` defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/tag-list.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    match "drafts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/draft.html"   postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "About me"            `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

pandocMathCompiler =
  let mathExtensions = [Ext_tex_math_dollars,
                        Ext_tex_math_double_backslash,
                        Ext_latex_macros]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = foldr S.insert defaultExtensions mathExtensions
      writerOptions = defaultHakyllWriterOptions {
        writerExtensions = newExtensions,
        writerHTMLMathMethod = MathJax ""
        }
  in pandocCompilerWith defaultHakyllReaderOptions writerOptions

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx
