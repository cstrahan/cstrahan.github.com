--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad                   (liftM)
import           Data.List                       (concat, intercalate)
import           Data.Maybe
import           Data.Monoid                     (mappend, (<>))
import qualified Data.Set
import           System.Environment              (getArgs)
import           System.IO.Unsafe                (unsafePerformIO)
import           System.Process                  (readProcess)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5                as H hiding (main)
import           Text.Blaze.XHtml5.Attributes    as A
import           Text.Pandoc                     (Block (..), Extension (Ext_markdown_in_html_blocks),
                                                  HTMLMathMethod (MathML),
                                                  Inline (..), ObfuscationMethod (NoObfuscation),
                                                  Pandoc (..),
                                                  ReaderOptions (..),
                                                  WriterOptions (..), bottomUp,
                                                  bottomUpM)

import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- Static files
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Compress CSS
    match ("css/*" .||. "css/**/*") $ do
        route   idRoute
        compile compressCssCompiler

    -- Render some static pages
    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Render each and every post
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ do
            pandocCompilerWithTransform readerOptions writerOptions pandocTransform
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                >>= loadAndApplyTemplate "templates/content.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Post list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx =
                    listField "posts" (postCtx tags) (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 3) . recentFirst =<< loadAll "posts/*"
            let indexContext =
                    listField "posts" (postCtx tags) (return posts) <>
                    field "tags" (\_ -> renderTagList tags) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/content.html" indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Render the 404 page, we don't relativize URL's here.
    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext


--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]


--------------------------------------------------------------------------------
readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions{ readerExtensions = Data.Set.filter (/=Ext_markdown_in_html_blocks) $ readerExtensions defaultHakyllReaderOptions }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions{ --writerStandalone = True,
                                            --writerTemplate = "$if(toc)$\n$toc$\n$endif$\n$body$",
                                            --writerTableOfContents = True,
                                            writerSectionDivs = False,
                                            writerColumns = 120,
                                            writerHtml5 = True,
                                            writerHTMLMathMethod = Text.Pandoc.MathML Nothing,
                                            writerEmailObfuscation = NoObfuscation }


--------------------------------------------------------------------------------
pandocTransform :: Pandoc -> Pandoc
pandocTransform = bottomUp pygments

pygments :: Block -> Block
pygments block@(CodeBlock (_, _, namevals) contents) =
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
-- Inspiration:
--   http://www.blaenkdenum.com/posts/the-switch-to-hakyll
--   http://geekplace.eu/flow/posts/2014-09-24-draft-posts-with-hakyll.html
--   http://vapaus.org/text/hakyll-configuration.html
--   https://github.com/gwern/gwern.net/blob/master/hakyll.hs
--   http://blog.scottlowe.org/
