{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll

config :: Configuration
config = defaultConfiguration { deployCommand = deployCmd
                              , providerDirectory = "src/"}

deployCmd :: String
deployCmd = "./site build && rsync --checksum -ave 'ssh' --delete _site/* calisto:anler.me"

main :: IO ()
main = hakyllWith config $ do
    match "images/*" (route idRoute >> compile copyFileCompiler)
    match "css/*" (route idRoute >> compile compressCssCompiler)

    match "static/*" $ do
      route $ composeRoutes (gsubRoute "static/" (const "")) idRoute
      compile copyFileCompiler

    match (fromList ["pages/about.org"]) $ do
      route $ composeRoutes
        (gsubRoute "pages/" (const ""))
        (setExtension "html")
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
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
