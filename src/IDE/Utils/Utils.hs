{-# OPTIONS_GHC -XDeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.Utils
-- Copyright   :  2007-2009 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Utils.Utils (

    leksahSessionFileExtension
,   standardSessionFilename
,   packageSessionFilename
,   leksahWorkspaceFileExtension
,   leksahPreferencesFileExtension
,   standardPreferencesFilename
,   strippedPreferencesFilename
,   leksahCandyFileExtension
,   standardCandyFilename
,   leksahKeymapFileExtension
,   standardKeymapFilename
,   leksahSourcesFileExtension
,   standardSourcesFilename
,   leksahMetadataSystemFileExtension
,   leksahMetadataDebugExtension
,   leksahMetadataWorkspaceFileExtension
,   leksahTemplateFileExtension
,   standardModuleTemplateFilename
,   leksahFlagFileExtension

) where



leksahSessionFileExtension           = ".lkshs"
leksahWorkspaceFileExtension         = ".lkshw"
leksahPreferencesFileExtension       = ".lkshp"
leksahCandyFileExtension             = ".lkshc"
leksahKeymapFileExtension            = ".lkshk"
leksahSourcesFileExtension           = ".lksho"
leksahMetadataSystemFileExtension    = ".lkshm"
leksahMetadataWorkspaceFileExtension = ".lkshe"
leksahMetadataDebugExtension         = ".lkshd"
leksahTemplateFileExtension          = ".lksht"
leksahFlagFileExtension              = ".lkshf"

standardSessionFilename              =   "current" ++ leksahSessionFileExtension
packageSessionFilename               =   "leksah" ++ leksahSessionFileExtension
standardKeymapFilename               =   "keymap" ++ leksahKeymapFileExtension
standardCandyFilename                =   "candy" ++ leksahCandyFileExtension
standardPreferencesFilename          =   "prefs" ++ leksahPreferencesFileExtension
strippedPreferencesFilename          =   "prefscoll" ++ leksahPreferencesFileExtension
standardSourcesFilename              =   "sources" ++ leksahSourcesFileExtension
standardModuleTemplateFilename       =   "module" ++ leksahTemplateFileExtension




