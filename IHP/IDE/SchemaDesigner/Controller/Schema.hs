module IHP.IDE.SchemaDesigner.Controller.Schema where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.ViewContext

import IHP.IDE.SchemaDesigner.View.Schema.Code
import IHP.IDE.SchemaDesigner.View.Schema.Error
import IHP.IDE.SchemaDesigner.View.Schema.GeneratedCode

import IHP.IDE.SchemaDesigner.Parser
import IHP.IDE.SchemaDesigner.Compiler
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.SchemaDesigner.View.Layout (findTableByName, findEnumByName, removeQuotes, replace)
import qualified IHP.SchemaCompiler as SchemaCompiler
import qualified System.Process as Process
import IHP.IDE.SchemaDesigner.Parser (schemaFilePath)
import qualified Data.Text.IO as Text

instance Controller SchemaController where
    
    action ShowCodeAction = do
        schema <- Text.readFile schemaFilePath
        error <- getSqlError
        putStrLn (tshow (fromMaybe "" (error)))
        render CodeView { .. }

    action SaveCodeAction = do
        let schema = param "schemaSql"
        Text.writeFile schemaFilePath schema
        redirectTo ShowCodeAction

    -- DB
    action PushToDbAction = do
        Process.system "make db"
        redirectTo TablesAction

    action DumpDbAction = do
        Process.system "make dumpdb"
        redirectTo TablesAction


    -- GENERATED HASKELL CODE
    action ShowGeneratedCodeAction { tableName } = do
        statements <- readSchema
        let (Just table) = findTableByName tableName statements
        let generatedHaskellCode = SchemaCompiler.compileStatementPreview statements table
        render GeneratedCodeView { .. }

readSchema :: _ => _
readSchema = parseSchemaSql >>= \case
        Left error -> do render ErrorView { error }; pure []
        Right statements -> pure statements

getSqlError :: _ => IO (Maybe ByteString)
getSqlError = parseSchemaSql >>= \case
        Left error -> do pure (Just error)
        Right statements -> do pure Nothing

updateSchema :: _ => _
updateSchema updateFn = do
    statements <- readSchema
    let statements' = updateFn statements
    writeSchema statements'