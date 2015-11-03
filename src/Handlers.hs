{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Handlers where
import Import
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes

formDepto :: Form Departamento
formDepto = renderDivs $ Departamento <$>
            areq textField "Nome" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident22",
                           fsLabel="Sigla",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","2")]} Nothing

formPessoa :: Form Pessoa
formPessoa = renderDivs $ Pessoa <$>
             areq textField "Nome" Nothing <*>
             areq intField "Idade" Nothing <*>
             areq doubleField "Salario" Nothing <*>
             areq (selectField dptos) "Depto" Nothing

dptos = do
       entidades <- runDB $ selectList [] [Asc DepartamentoNome] 
       optionsPairs $ fmap (\ent -> (departamentoSigla $ entityVal ent, entityKey ent)) entidades

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = [whamlet|
            <h1>
                #{y}
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
|] >> toWidget [lucius|
       label{
          color:blue;
       }
|]

getCadastroR :: Handler Html
getCadastroR = do
             (widget, enctype) <- generateFormPost formPessoa
             defaultLayout $ widgetForm CadastroR enctype widget "Pessoas"

getDeptoR :: Handler Html
getDeptoR = do
             (widget, enctype) <- generateFormPost formDepto
             defaultLayout $ widgetForm DeptoR enctype widget "Departamentos"

getPessoaR :: PessoaId -> Handler Html
getPessoaR pid = do
             pessoa <- runDB $ get404 pid
             depto <- runDB $ get $ pessoaDeptoid pessoa
             defaultLayout [whamlet| 
                 <h1> Seja bem-vindx #{pessoaNome pessoa}
                 <p> Salario: #{pessoaSalario pessoa}
                 <p> Idade: #{pessoaIdade pessoa}
                 <p> Depto: #{show $ fmap departamentoNome depto}
             |]

getListarR :: Handler Html
getListarR = do
             listaP <- runDB $ selectList [] [Asc PessoaNome]
             defaultLayout [whamlet|
                 <h1> Pessoas cadastradas:
                 $forall Entity pid pessoa <- listaP
                     <a href=@{PessoaR pid}> #{pessoaNome pessoa} <br>
             |]

postCadastroR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formPessoa
                case result of
                    FormSuccess pessoa -> do
                       runDB $ insert pessoa 
                       defaultLayout [whamlet| 
                           <h1> #{pessoaNome pessoa} Inseridx com sucesso. 
                       |]
                    _ -> redirect CadastroR

postDeptoR :: Handler Html
postDeptoR = do
                ((result, _), _) <- runFormPost formDepto
                case result of
                    FormSuccess depto -> do
                       runDB $ insert depto
                       defaultLayout [whamlet|
                           <h1> #{departamentoNome depto} Inserido com sucesso. 
                       |]
                    _ -> redirect DeptoR

formUser :: Form Usuario
formUser = renderDivs $ Usuario <$>
           areq textField "User" Nothing <*>
           areq passwordField "Pass" Nothing

getUserR :: Handler Html
getUserR = do
             (widget, enctype) <- generateFormPost formUser
             defaultLayout $ widgetForm UserR enctype widget "Usuarios"

postUserR :: Handler Html
postUserR = do
                ((result, _), _) <- runFormPost formUser
                case result of
                    FormSuccess user -> do
                       runDB $ insert user
                       defaultLayout [whamlet|
                           <h1> #{usuarioNome user} Inserido com sucesso. 
                       |]
                    _ -> redirect DeptoR

getListarUserR :: Handler Html
getListarUserR = do
               listaP <- runDB $ selectList [] [Asc UsuarioNome]
               defaultLayout [whamlet|
                 <h1> Pessoas cadastradas:
                 $forall Entity pid u <- listaP
                     <p #{usuarioNome u}> <br>
               |]

getAutR :: Handler Html
getAutR = do
          (widget, enctype) <- generateFormPost formUser
          defaultLayout $ widgetForm AutR enctype widget "Login"

postAutR :: Handler Html
postAutR = do
           ((result, _), _) <- runFormPost formUser
           case result of
                    FormSuccess user -> do
                       setSession "_ID" (usuarioNome user)
                       redirect CadastroR
                    _ -> redirect AutR

getByeR :: Handler Html
getByeR = do
          deleteSession "_ID"
          defaultLayout [whamlet| BYE! |]

connStr = "dbname=dd9en8l5q4hh2a host=ec2-107-21-219-201.compute-1.amazonaws.com user=kpuwtbqndoeyqb password=aCROh525uugAWF1l7kahlNN3E0 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warpEnv (Sitio pool)