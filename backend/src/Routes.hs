module Routes where

import Import.NoFoundation

import AppType

mkYesodData "App" [parseRoutes|
/static StaticR Static appStatic

/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/           HomeR    GET POST
/login      LoginR   GET POST
/signup     SignupR  GET POST
/signout    SignoutR GET

/admin      AdminR       GET
/admin/deck AdminDeckR   GET POST
/admin/card AdminCardR   GET POST
|]
