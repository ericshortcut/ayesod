{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
 
pRoutes = [parseRoutes|
   / CadastroR GET POST
   /listar ListarR GET
   /pessoa/#PessoaId PessoaR GET
   /depto DeptoR GET POST
   /user UserR GET POST
   /auto AutR GET POST
   /listuser ListarUserR GET
   /bye ByeR GET
|]