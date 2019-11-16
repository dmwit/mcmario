{-# LANGUAGE OverloadedStrings #-}
module UI where

import Data.String
import Lucid

preamble :: Html () -> Html () -> Html ()
preamble title body = do
	doctype_
	html_ $ do
		head_ $ do
			meta_ [charset_ "utf-8"]
			title_ title
			link_ [rel_ "stylesheet", href_ "style.css"]
		body_ body

welcome :: Html ()
welcome = preamble "MC Mario in the hizzouse" $ do
	h1_ "MC Mario"
	p_ [class_ "teaser"] "Get handicap recommendations for 2-player Dr. Mario games."
	id
		. form_ [method_ "post", action_ "login", class_ "create-account"]
		. button_ [type_ "submit", name_ "ignored", value_ "ignored", class_ "link-button"]
		$ "Create a new account"

problem :: String -> Html ()
problem s = preamble "Whoopsy!" $ do
	h1_ "Something went wrong."
	p_ "Here's some info you can use when you complain to the webmaster:"
	p_ (fromString s)
	p_ "If you can reliably trigger this problem, please also give the webmaster detailed instructions to follow so they can see this error themselves. Thanks!"
