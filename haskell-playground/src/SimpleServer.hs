module SimpleServer (startSimpleServer) where

import           Control.Concurrent
import           Control.Exception  (finally)
import           Control.Monad      (forever)
import           Data.Char          (isAlpha)
import           Data.List          (isPrefixOf, partition)
import           Data.Maybe         (listToMaybe)
import           GHC.Conc           (par)
import           Network.Socket
import           System.IO



startSimpleServer :: IO ()
startSimpleServer = withSocketsDo $ do
  addr <- resolve "3000"
  sock <- open addr
  putStrLn "Listening on port 3000"
  acceptLoop sock
  where

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 10
  return sock

resolve :: ServiceName -> IO AddrInfo
resolve port = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  infos <- getAddrInfo (Just hints) Nothing (Just port)
  case listToMaybe infos of
    Just addr -> return addr
    Nothing   -> ioError $ userError $ "getAddrInfo returned no results for port " ++ port

acceptLoop :: Socket -> IO ()
acceptLoop sock = forever $ do
  (conn, peer) <- accept sock
  putStrLn $ "Accepted connection from " ++ show peer
  forkIO $ handleConn conn `finally` close conn

handleConn :: Socket -> IO ()
handleConn conn = do
  h <- socketToHandle conn ReadWriteMode
  hSetBuffering h NoBuffering
  reqLine <- safeGetLine h
  putStrLn $ "Request: " ++ reqLine
  consumeHeaders h
  let (method, path) = parseRequestLine reqLine
      response = route method path
  hPutStr h response
  hFlush h
  hClose h

safeGetLine :: Handle -> IO String
safeGetLine h = do
  eof <- hIsEOF h
  if eof then return "" else hGetLine h

consumeHeaders :: Handle -> IO ()
consumeHeaders h = do
  line <- safeGetLine h
  if line == "" || line == "\r" then return () else consumeHeaders h

data Method = GET | POST | PUT | DELETE deriving (Show, Eq)

-- Routing logic
route :: Method -> String -> String

-- GET /
route GET "/" =
  mkResponse "<html><body><h1>Hello from Haskell</h1></body></html>"

-- GET /about
route GET "/about" =
  mkResponse $ renderDOM $
    elHtml [("lang", "en")] [
      elBody `elChild` Fragment [
        elH1 `elChild` Text "About Page",
        elP `elChild` Text "This is a simple Haskell web server."
      ]
    ]

-- GET /hello/{name}
route GET path | "/hello/" `isPrefixOf` path =
  let name = drop (length "/hello/") path
      body = renderDOM $
        elHtml [("lang", "en")] [
          elBody `elChildren` [
            elH1 `elChild` Text ("Hello, " ++ name ++ "!")
          ]
        ]
  in mkResponse body

-- GET /{fallback}
route _ _ =
  mkResponse404

mkResponse :: String -> String
mkResponse body = concat
  [ "HTTP/1.0 200 OK\r\n"
  , "Content-Type: text/html; charset=utf-8\r\n"
  , "Content-Length: ", show (length body), "\r\n"
  , "\r\n"
  , body
  ]

mkResponse404 :: String
mkResponse404 = concat
  [ "HTTP/1.0 404 Not Found\r\n"
  , "Content-Type: text/html; charset=utf-8\r\n"
  , "Content-Length: 48\r\n"
  , "\r\n"
  -- , "<html><body><h1>404 - Not Found</h1></body></html>"
  , renderDOM $
      elHtml [("lang", "en")] [
        elBody [] [
          elH1 `elChild` Text "404 - Not Found"
        ]
      ]
  ]

-- Parse request line: "GET /path HTTP/1.1"
parseRequestLine :: String -> (Method, String)
parseRequestLine line =
  case words line of
    (method:path:_) -> (parseMethod method, path)
    _               -> (GET, "")

parseMethod :: String -> Method
parseMethod "GET"    = GET
parseMethod "POST"   = POST
parseMethod "PUT"    = PUT
parseMethod "DELETE" = DELETE
parseMethod _        = GET

-- DomElement: Element "TagName" [(Attribute, Value)] [Children] | Text "..."
data HtmlTag = Html | Body | Div | Span | P | H1 | H2 | H3 | Section | Article | Header | Ul | Li | A | Custom String 
  deriving (Show, Eq)

fromShow :: String -> HtmlTag
fromShow x = case x of
  "Html"    -> Html
  "Body"    -> Body
  "Div"     -> Div
  "Span"    -> Span
  "P"       -> P
  "H1"      -> H1
  "H2"      -> H2
  "H3"      -> H3
  "Section" -> Section
  "Article" -> Article
  "Header"  -> Header
  "Ul"      -> Ul
  "Li"      -> Li
  "A"       -> A
  custom    -> Custom custom

type Attribute = (String, String)
data DOM =  Tag HtmlTag [Attribute] [DOM] | Fragment [DOM] | Text String deriving (Show, Eq)

instance Semigroup (DOM) where
  -- Tag <> Tag = Fragment
  Tag t attrs children1 <> Tag _ _ children2 = Fragment [Tag t attrs children1, Tag t attrs children2]
  -- Tag <> _ = insert into tag
  Tag t attrs children1 <> Fragment children2 = Tag t attrs (children1 ++ children2)
  Tag t attrs children <> rhs = Tag t attrs (children ++ [rhs])
  -- Fragment <> _ = insert into Fragment
  Fragment children1 <> any = Fragment (children1 ++ [any])
  -- Text <> _ = Fragment
  Text content1 <> rhs = Fragment [Text content1, rhs]

(<|) :: ([Attribute] -> [DOM] -> DOM) -> [DOM] -> DOM
infixr 5 <|
partial <| children =  partial [] children

elChild :: ([Attribute] -> [DOM] -> DOM) -> DOM -> DOM
infixr 5 `elChild`
partial `elChild` children =  partial [] [children]

elChildren :: ([Attribute] -> [DOM] -> DOM) -> [DOM] -> DOM
infixr 5 `elChildren`
partial `elChildren` children =  partial [] [Fragment children]

-- (|<) :: ([DOM] -> DOM) -> DOM -> DOM
-- infixr 5 |<
-- partial |< children =  partial [children]

elHtml = Tag Html
elBody = Tag Body
elDiv = Tag Div
elSpan = Tag Span
elP = Tag P
elH1 = Tag H1
elH2 = Tag H2
elH3 = Tag H3
elSection = Tag Section
elArticle = Tag Article
elHeader = Tag Header
elUl = Tag Ul
elLi = Tag Li
el = Tag . Custom

renderDOM :: DOM -> String
renderDOM (Tag name attrs children) =
  "<" ++ show name ++ concatMap renderAttr attrs ++ ">" ++ concatMap renderDOM children ++ "</" ++ show name ++ ">"
renderDOM (Fragment children) = concatMap renderDOM children
renderDOM (Text content) = content

renderAttr :: (String, String) -> String
renderAttr (name, value) = " " ++ name ++ "=\"" ++ value ++ "\""
