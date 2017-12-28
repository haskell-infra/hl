-- | Packages page controller.

module HL.Controller.Packages where

import HL.Controller
import HL.View.Packages
import HL.Model.Markdown
import HL.View

-- | Packages controller.

getPackagesR :: C (Html ())
getPackagesR =
  lucid . packagesFromMarkdown =<< io (getMarkdown "packages-main.md")