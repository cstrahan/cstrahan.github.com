--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.List                       (concat, intercalate)
import           Data.Maybe
import           Data.Monoid                     (mappend, (<>))
import qualified Data.Set
import           Hakyll
import           System.IO.Unsafe                (unsafePerformIO)
import           System.Process                  (readProcess)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5                as H
import           Text.Blaze.XHtml5.Attributes    as A
import           Text.Pandoc                     (Block (..), Extension (Ext_markdown_in_html_blocks),
                                                  HTMLMathMethod (MathML),
                                                  Inline (..), ObfuscationMethod (NoObfuscation),
                                                  Pandoc (..),
                                                  ReaderOptions (..),
                                                  WriterOptions (..), bottomUp,
                                                  bottomUpM)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/**/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        --route $ setExtension "html"
        route $ setExtension "html"
        compile $ pandocCompilerWithTransform readerOptions writerOptions pandocTransform
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
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
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Home"                <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    --modificationTimeField "modified" "%d %b %Y" <>
    defaultContext


--------------------------------------------------------------------------------
readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions{ readerExtensions = Data.Set.filter (/=Ext_markdown_in_html_blocks) $ readerExtensions defaultHakyllReaderOptions }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions{ --writerStandalone = True,
                                            --writerTemplate = "<div id=\"TOC\">$toc$</div>\n$body$",
                                            --writerTableOfContents = True,
                                            writerSectionDivs = True,
                                            writerColumns = 120,
                                            writerHtml5 = True,
                                            writerHTMLMathMethod = Text.Pandoc.MathML Nothing,
                                            writerEmailObfuscation = NoObfuscation }


--------------------------------------------------------------------------------
pandocTransform :: Pandoc -> Pandoc
pandocTransform = bottomUp pygments

pygments :: Block -> Block
pygments block@(CodeBlock (_, _, namevals) contents) =
  --let lang = (unsafePerformIO $ putStrLn (show block)) `seq` fromMaybe "text" $ lookup "lang" namevals
  let lang = languageFor block
      colored = pygmentize lang contents
      -- might consider using figure and figcaption
      --colored = renderHtml $ H.div ! A.class_ "code-container" $ do
      --            preEscapedToHtml $ pygmentize lang contents
      --captionText = fromMaybe "" $ lookup "caption" namevals
      --caption = if captionText /= ""
      --          then renderHtml $ H.figcaption $ H.span $ H.toHtml captionText
      --          else ""
      --composed = renderHtml $ H.figure ! A.class_ "code" $ do
      --             preEscapedToHtml $ colored ++ caption
  in RawBlock "html" colored
pygments block = block

pygmentize :: String -> String -> String
pygmentize lang contents = unsafePerformIO $
    let cssClass = "highlight"
    in readProcess "pygmentize" ["-f", "html", "-l", lang, "-O", cssClass] contents

languageFor :: Block -> String
languageFor (CodeBlock (_, klass:_, _) _) = klass
languageFor _                             = "text"


--------------------------------------------------------------------------------
--datedRoute :: Routes
--datedRoute = customRoute $ \ident ->
--    case splitList '-' (toFilePath ident) of
--        year:month:day:titleParts ->
--            intercalate "/" $ year:month:day:(intercalate "-" titleParts):[]
--
--splitList :: Eq a => a -> [a] -> [[a]]
--splitList _   [] = []
--splitList sep list = h:splitList sep t
--        where (h,t)=split (==sep) list
--
---- | Split is like break, but the matching element is dropped.
--split :: (a -> Bool) -> [a] -> ([a], [a])
--split f s = (left,right)
--        where
--        (left,right')=break f s
--        right = if null right' then [] else tail right'


--------------------------------------------------------------------------------
-- Inspiration:
--   http://www.blaenkdenum.com/posts/the-switch-to-hakyll
--   http://geekplace.eu/flow/posts/2014-09-24-draft-posts-with-hakyll.html
--   http://vapaus.org/text/hakyll-configuration.html
--   https://github.com/gwern/gwern.net/blob/master/hakyll.hs
