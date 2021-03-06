module IHP.IDE.CodeGen.Controller where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.ViewContext
import IHP.IDE.CodeGen.View.Generators
import IHP.IDE.CodeGen.View.NewController
import IHP.IDE.CodeGen.View.NewScript
import IHP.IDE.CodeGen.Types
import IHP.IDE.CodeGen.ControllerGenerator as ControllerGenerator
import IHP.IDE.CodeGen.ScriptGenerator as ScriptGenerator
import IHP.IDE.ToolServer.Helper.Controller
import qualified System.Process as Process
import qualified System.Directory as Directory
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Text.Inflections as Inflector
import Control.Exception

instance Controller CodeGenController where
    action GeneratorsAction = do
        render GeneratorsView

    action NewControllerAction = do
        let controllerName = paramOrDefault "" "name"
        plan <- ControllerGenerator.buildPlan controllerName
        render NewControllerView { .. }

    action CreateControllerAction = do
        let controllerName = param "name"
        (Right plan) <- ControllerGenerator.buildPlan controllerName
        executePlan plan
        setSuccessMessage "Controller generated"
        redirectTo GeneratorsAction

    action NewScriptAction = do
        let scriptName = paramOrDefault "" "name"
        let plan = ScriptGenerator.buildPlan scriptName
        render NewScriptView { .. }

    action CreateScriptAction = do
        let scriptName = paramOrDefault "" "name"
        let (Right plan) = ScriptGenerator.buildPlan scriptName
        executePlan plan
        setSuccessMessage "Script generated"
        redirectTo GeneratorsAction

    action OpenControllerAction = do
        let name = param "name"
        case name |> Inflector.toCamelCased True of
            Left error -> renderPlain "Failed to transform name to camel case"
            Right indexActionName-> redirectToUrl ("http://localhost:" <> tshow appPort <> "/" <> indexActionName)


executePlan :: [GeneratorAction] -> IO ()
executePlan actions = forEach actions evalAction
    where
        evalAction CreateFile { filePath, fileContent } = do
            Text.writeFile (cs filePath) (cs fileContent)
            putStrLn ("+ " <> filePath)
        evalAction AppendToFile { filePath, fileContent } = do
            Text.appendFile (cs filePath) fileContent
            putStrLn ("* " <> filePath)
        evalAction AppendToMarker { marker, filePath, fileContent } = do
            content <- Text.readFile (cs filePath)
            let newContent = Text.replace marker (marker <> "\n" <> cs fileContent) (cs content)
            Text.writeFile (cs filePath) (cs newContent)
            putStrLn ("* " <> filePath <> " (import)")
        evalAction EnsureDirectory { directory } = do
            Directory.createDirectoryIfMissing True (cs directory)
        evalAction RunShellCommand { shellCommand } = do
            _ <- Process.system (cs shellCommand)
            putStrLn ("* " <> shellCommand)

undoPlan :: [GeneratorAction] -> IO()
undoPlan actions = forEach actions evalAction
    where
        evalAction CreateFile { filePath, fileContent } = do
            (Directory.removeFile (cs filePath)) `catch` handleError
            putStrLn ("- " <> filePath)
        evalAction AppendToFile { filePath, fileContent } = do
            deleteTextFromFile (cs filePath) fileContent `catch` handleError
            putStrLn ("* " <> filePath)
        evalAction AppendToMarker { marker, filePath, fileContent } = do
            (deleteTextFromFile (cs filePath) (fileContent <> "\n")) `catch` handleError
            putStrLn ("* " <> filePath <> " (import)")
        evalAction EnsureDirectory { directory } = do
            (Directory.removeDirectory (cs directory)) `catch` handleError
        evalAction RunShellCommand { shellCommand } = pure ()
        handleError :: SomeException -> IO ()
        handleError ex = putStrLn (tshow ex)

deleteTextFromFile :: Text -> Text -> IO ()
deleteTextFromFile filePath lineContent = do
    fileContent <- Text.readFile (cs filePath)
    let replacedContent = Text.replace lineContent "" fileContent
    Text.writeFile (cs filePath) replacedContent
