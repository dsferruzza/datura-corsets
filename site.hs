--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll
import           System.FilePath (takeBaseName,takeDirectory,(</>),splitFileName,splitPath,joinDrive,isDrive,isPathSeparator,hasDrive)
import           Control.Applicative (Alternative (..), (<$>))
import           GHC.IO.Encoding


--------------------------------------------------------------------------------
main :: IO ()
main = do
  setLocaleEncoding utf8        -- These 3 lines deal with an encoding problem on Windows:
  setFileSystemEncoding utf8    --   when having some characters like ' in markdown content, site couldn't build
  setForeignEncoding utf8
  hakyll $ do

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "articles/**.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/article.html" articleContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrlsFix

    match "articles/**.jpg" $ do
        route   idRoute
        compile copyFileCompiler

    match "articles/**.jpg" $ version "thumb" $ do
        route $ setExtension "thumb.jpg"
        compile $ getResourceLBS
            >>= withItemBody (unixFilterLBS "convert" ["-resize", "100x100", "-", "-"])

    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            articles <- loadAll "articles/**.md"
            let indexCtx =
                    listField "articles" articleContext (return articles) <>
                    defaultContext
            pandocCompiler
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
getPicsInDir :: Compiler [Item CopyFile]
getPicsInDir = do
    postPath <- toFilePath <$> getUnderlying
    let pattern = fromGlob $ takeDirectory postPath ++ "/*.jpg"
    loadAll (pattern .&&. hasNoVersion)

picContext :: Context CopyFile
picContext =
    urlField "url" <>
    (field "thumb" $ \item -> do
        pic <- fmap (maybe empty toUrl) . getRoute $ itemIdentifier item
        let thumb = (foldl1 (++) (splitAll ".jpg" pic)) ++ ".thumb.jpg"
        return thumb)

articleContext :: Context String
articleContext =
    listField "photos" picContext getPicsInDir <>
    defaultContext

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
