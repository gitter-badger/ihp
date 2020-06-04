module IHP.IDE.CodeGen.ViewGenerator (buildPlan) where

import IHP.Prelude
import IHP.HaskellSupport
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import IHP.ViewSupport
import qualified System.Process as Process
import IHP.IDE.CodeGen.Types

buildPlan :: Text -> Text -> Either Text [GeneratorAction]
buildPlan viewName controllerName =
    if (null viewName || null controllerName)
        then Left "View name and controller name cannot be empty"
        else do 
            schema <- SchemaDesigner.parseSchemaSql >>= \case
                Left parserError -> pure []
                Right statements -> pure statements

generateGenericView :: [Statement] -> ViewConfig -> [GeneratorAction]
generateGenericView schema config = 
        let 
            controllerName = get #controllerName config
            name = get #viewName config
            singularName = config |> get #modelName
            singularVariableName = lcfirst singularName
            pluralVariableName = lcfirst controllerName
            nameWithSuffix = name <> "View" --e.g. "TestView"

            viewHeader =
                ""
                <> "module " <> qualifiedViewModuleName config name <> " where\n"
                <> "import " <> get #applicationName config <> ".View.Prelude\n"
                <> "\n"
            
            genericView = 
                viewHeader
                <> "data " <> nameWithSuffix <> " = " <> nameWithSuffix <> "\n"
                <> "\n"
                <> "instance View " <> nameWithSuffix <> " ViewContext where\n"
                <> "    html " <> nameWithSuffix <> " { .. } = [hsx|\n"
                <> "        <nav>\n"
                <> "            <ol class=\"breadcrumb\">\n"
                <> "                <li class=\"breadcrumb-item\"><a href={" <> indexAction <> "}>" <> Countable.pluralize name <> "</a></li>\n"
                <> "                <li class=\"breadcrumb-item active\">" <> nameWithSuffix <> "</li>\n"
                <> "            </ol>\n"
                <> "        </nav>\n"
                <> "        <h1>" <> nameWithSuffix <> "</h1>\n"
                <> "    |]\n"
        in
            [ EnsureDirectory { directory = get #applicationName config <> "/View/" <> name }
            , CreateFile { filePath = get #applicationName config <> "/View/" <> name <> "/" <> name <> ".hs", fileContent = genericView }
            ]