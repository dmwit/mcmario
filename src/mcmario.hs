module Main where

import Control.Monad.IO.Class
import Crypto.Argon2
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Functor.Identity
import Data.String
import Database.PostgreSQL.Simple
import Lucid
import Snap
import Snap.Util.FileServe
import System.IO

import qualified Data.ByteString as BS
import qualified UI

main :: IO ()
main = do
	db <- connectPostgreSQL (fromString "dbname=mcmario")
	httpServe mempty . asum $
		[ method GET . route $ tail [undefined
			, "" .> writeHtml UI.welcome
			, "login" .> login db
			]
		, method POST . route $ tail [undefined
			, "create-account" .> createAccount db
			, "login" .> loginFirstTime db
			]
		, serveDirectory "static"
		]

createAccount :: Connection -> Snap ()
createAccount db = do
	h <- liftIO (openFile "/dev/urandom" ReadMode)
	pw <- liftIO (BS.hGet h 16)
	case hash defaultHashOptions mempty pw of
		Left err -> problem
			$ "Argon2 hashing of generated password failed: " ++ show err
		Right hashed -> undefined

loginFirstTime :: Connection -> Snap ()
loginFirstTime = undefined

login :: Connection -> Snap ()
login = undefined

problem :: String -> Snap ()
problem s = do
	modifyResponse (setResponseCode 500)
	writeHtml (UI.problem s)

writeHtml :: Html () -> Snap ()
writeHtml = writeBuilder . runIdentity . execHtmlT

(.>) :: String -> Snap () -> (ByteString, Snap ())
s .> act = (fromString s, ifTop act)
