{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Import
import Yesod
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio { connPool :: ConnectionPool }
-- CRIAR UMA TABELA Usuario
-- Campos: login, senha
-- Listar Usuarios
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuario
   nome Text
   pass Text

Departamento
   nome Text
   sigla Text sqltype=varchar(2)
   deriving Show

Pessoa
   nome Text
   idade Int
   salario Double
   deptoid DepartamentoId
   deriving Show
|]

mkYesodData "Sitio" pRoutes

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
   authRoute _ = Just AutR
   isAuthorized CadastroR _ = isAdmin
   isAuthorized DeptoR _ = isAdmin
   isAuthorized _ _ = return Authorized

isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "You must be an admin"

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage

