--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections #-}
import           Data.Monoid ((<>))
import           Data.List (isInfixOf,sortBy)
import           Data.Time.Clock (UTCTime (..))
import           Data.Time.Format (parseTime)
import           Data.Ord (comparing)
import           Data.List (intercalate)
import           Hakyll
import           Hakyll.Web.Html
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           System.FilePath (takeDirectory,splitFileName,splitPath,joinDrive,isDrive,isPathSeparator,hasDrive)
import           System.Locale (TimeLocale, defaultTimeLocale)
import           Control.Applicative (Alternative (..), (<$>))
import           Control.Monad (msum,liftM)
import           GHC.IO.Encoding


--------------------------------------------------------------------------------
main :: IO ()
main = do
  setLocaleEncoding utf8        -- These 3 lines deal with an encoding problem on Windows:
  setFileSystemEncoding utf8    --   when having some characters like ' in markdown content, site couldn't build
  setForeignEncoding utf8
  hakyll $ do

    tags <- buildTags "articles/**.md" (fromCapture "galerie/*/index.html")

    tagsRules tags $ \tag pattern -> do
        let title = tag
        route idRoute
        compile $ do
            list <- articleList tags pattern alphabetical
            makeItem ""
                >>= loadAndApplyTemplate "templates/tags-articles.html"
                        (constField "title" title <>
                            constField "body" list <>
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" (field "tags" (\_ -> renderTagListCustom tags) <> defaultContext)
                >>= removeIndexHtml
                >>= relativizeUrlsFix

    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "articles/**.md" $ do
        route $ niceRoute "article" `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/article.html" (articleContext tags)
            >>= loadAndApplyTemplate "templates/default.html" (field "tags" (\_ -> renderTagListCustom tags) <> defaultContext)
            >>= relativizeUrlsFix

    match "articles/**.jpg" $ do
        route $ niceRoute "article"
        compile copyFileCompiler

    match "articles/**.jpg" $ version "thumb" $ do
        route $ niceRoute "article" `composeRoutes` setExtension "thumb.jpg"
        compile $ getResourceLBS
            >>= withItemBody (unixFilterLBS "convert" ["-resize", "100x100", "-", "-"])

    match "articles/**/index.jpg" $ version "cover" $ do
        route $ niceRoute "article" `composeRoutes` setExtension "cover.jpg"
        compile $ getResourceLBS
            >>= withItemBody (unixFilterLBS "convert" ["-resize", "500x250", "-", "-"])

    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            articles <- loadAll "articles/**.md"
            let indexCtx =
                    listField "articles" (articleContext tags) (return articles) <>
                    field "tags" (\_ -> renderTagListCustom tags) <>
                    defaultContext
            pandocCompiler
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (field "tags" (\_ -> renderTagListCustom tags) <> defaultContext)
                >>= removeIndexHtml
                >>= relativizeUrlsFix

    create ["galerie/index.html"] $ do
        route idRoute
        compile $ do
            articles <- loadAll "articles/**.md"
            let indexCtx =
                    listField "articles" (articleContext tags) (return articles) <>
                    field "tags" (\_ -> renderTagListCustom tags) <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/galerie.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (field "tags" (\_ -> renderTagListCustom tags) <> defaultContext)
                >>= removeIndexHtml
                >>= relativizeUrlsFix

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
-- ARTICLE CONTEXT

getPicsInDir :: Compiler [Item CopyFile]
getPicsInDir = do
    postPath <- toFilePath <$> getUnderlying
    let pattern = fromGlob $ takeDirectory postPath ++ "/*.jpg"
    loadAll (pattern .&&. hasNoVersion)

getFirstPicInDir :: Compiler [Item CopyFile]
getFirstPicInDir = do
    postPath <- toFilePath <$> getUnderlying
    let pattern = fromGlob $ takeDirectory postPath ++ "/index.jpg"
    loadAll (pattern .&&. hasNoVersion)

picContext :: Context CopyFile
picContext =
    urlField "url" <>
    (field "thumb" $ \item -> do
        pic <- fmap (maybe empty toUrl) . getRoute $ itemIdentifier item
        let thumb = (foldl1 (++) (splitAll ".jpg" pic)) ++ ".thumb.jpg"
        return thumb)

coverPicContext :: Context CopyFile
coverPicContext =
    urlField "url" <>
    (field "cover" $ \item -> do
        pic <- fmap (maybe empty toUrl) . getRoute $ itemIdentifier item
        let cover = (foldl1 (++) (splitAll ".jpg" pic)) ++ ".cover.jpg"
        return cover)

articleContext :: Tags -> Context String
articleContext tags =
    listField "photos" picContext getPicsInDir <>
    listField "cover" coverPicContext getFirstPicInDir <>
    tagsField "prettytags" tags <>
    defaultContext

articleList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
articleList tags pattern preprocess' = do
    articleItemTpl <- loadBody "templates/tags-article.html"
    articles <- loadAll pattern
    processed <- preprocess' articles
    applyTemplateList articleItemTpl (articleContext tags) processed

--------------------------------------------------------------------------------

renderTagListCustom :: Tags -> Compiler (String)
renderTagListCustom = renderTags makeLink (intercalate "")
  where
    makeLink tag url count _ _ = renderHtml $
        H.li $ H.a ! A.href (toValue url) $ toHtml (tag)

--------------------------------------------------------------------------------
-- USE NICE ROUTES
-- Inspired by: http://hub.darcs.net/DarkFox/DarkFox-blog/browse/site.hs

niceRoute :: String -> Routes
niceRoute top = gsubRoute (top ++ "/") (const "")

removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml = return . (withUrls removeIndexStr <$>)
  where removeIndexStr :: String -> String
        removeIndexStr u = case splitFileName u of
            (d, "index.html") | isLocal d -> d
            _                             -> u
        isLocal :: String -> Bool
        isLocal = not . (isInfixOf "://")

--------------------------------------------------------------------------------

fakeGetItemUTC :: MonadMetadata m
           => TimeLocale        -- ^ Output time locale
           -> Identifier        -- ^ Input page
           -> m UTCTime         -- ^ Parsed UTCTime
fakeGetItemUTC locale id' = do
    maybe empty' return $ msum $ [parseTime' "%Y" "0000"]
  where
    empty'     = fail $ "Hakyll.Web.Template.Context.fakeGetItemUTC: " ++
        "could not parse time for " ++ show id'
    parseTime' = parseTime locale

alphabetical :: MonadMetadata m => [Item a] -> m [Item a]
alphabetical =
    sortByM $ fakeGetItemUTC defaultTimeLocale . itemIdentifier
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs

--------------------------------------------------------------------------------
-- FIX RELATIVE URL BUG ON WINDOWS

relativizeUrlsFix :: Item String -> Compiler (Item String)
relativizeUrlsFix item = do
    route <- getRoute $ itemIdentifier item
    return $ case route of
        Nothing -> item
        Just r -> fmap (relativizeUrlsWith $ toSiteRootFix r) item

toSiteRootFix :: String -> String
toSiteRootFix = emptyException . joinPathFix . map parent
              . filter relevant . splitPath . takeDirectory
  where
    parent = const ".."
    emptyException [] = "."
    emptyException x = x
    relevant "." = False
    relevant "/" = False
    relevant _ = True

joinPathFix :: [FilePath] -> FilePath
joinPathFix x = foldr combineFix "" x

combineFix :: FilePath -> FilePath -> FilePath
combineFix a b | hasDrive b || (not (null b) && isPathSeparator (head b)) = b
               | otherwise = combineAlwaysFix a b

combineAlwaysFix :: FilePath -> FilePath -> FilePath
combineAlwaysFix a b | null a = b
                     | null b = a
                     | isPathSeparator (last a) = a ++ b
                     | isDrive a = joinDrive a b
                     | otherwise = a ++ "/" ++ b
