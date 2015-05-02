{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}


-- cannibalised from `cabal-install`'s Main.hs

module Main (main) where

import Distribution.Client.Setup
         ( GlobalFlags(..), globalCommand, globalRepos
         , ConfigFlags(..)
         , ConfigExFlags(..), defaultConfigExFlags, configureExCommand
         , InstallFlags(..), defaultInstallFlags, installCommand
         , FreezeFlags(..), freezeCommand
         , ListFlags(..)
         , InfoFlags(..), infoCommand
         , SandboxFlags(..), sandboxCommand
         )
import Distribution.Simple.Setup
         ( defaultHaddockFlags, HaddockFlags(..)
         , Flag(..), fromFlag, fromFlagOrDefault, flagToMaybe, toFlag
         , configAbsolutePaths
         , optionVerbosity
         , trueArg, readPackageDbList, showPackageDbList
         )

import Distribution.Client.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import Distribution.Client.Config
         ( SavedConfig(..), loadConfig, defaultConfigFile, userConfigDiff
         , userConfigUpdate )
import Distribution.Client.Targets
         ( readUserTargets )
import qualified Distribution.Client.List as List
         ( list, info )

import Distribution.Client.Install            (install)
import Distribution.Client.Sandbox            (sandboxInit
                                              ,sandboxAddSource
                                              ,sandboxDelete
                                              ,sandboxDeleteSource
                                              ,sandboxListSources
                                              ,sandboxHcPkg
                                              ,dumpPackageEnvironment

                                              ,getSandboxConfigFilePath
                                              ,loadConfigOrSandboxConfig
                                              ,initPackageDBIfNeeded
                                              ,maybeWithSandboxDirOnSearchPath
                                              ,maybeWithSandboxPackageInfo
                                              ,WereDepsReinstalled(..)
                                              ,maybeReinstallAddSourceDeps
                                              ,tryGetIndexFilePath
                                              ,sandboxBuildDir
                                              ,updateSandboxConfigFileFlag

                                              ,configCompilerAux'
                                              ,configPackageDB')
import Distribution.Client.Sandbox.PackageEnvironment
                                              (setPackageDB
                                              ,userPackageEnvironmentFile)
import Distribution.Client.Sandbox.Timestamp  (maybeAddCompilerTimestampRecord)
import Distribution.Client.Sandbox.Types      (UseSandbox(..), whenUsingSandbox)
import Distribution.Client.Utils              (determineNumJobs
                                              ,relaxEncodingErrors
                                              ,existsAndIsMoreRecentThan)

import Distribution.PackageDescription
         ( Executable(..), benchmarkName, benchmarkBuildInfo, testName
         , testBuildInfo, buildable )
import Distribution.PackageDescription.Parse
         ( readPackageDescription )
import Distribution.PackageDescription.PrettyPrint
         ( writeGenericPackageDescription )
import Distribution.Simple.Command
         ( CommandParse(..), CommandUI(..), Command
         , commandsRun, commandAddAction, hiddenCommand, usageAlternatives, option, reqArg' )
import Distribution.Simple.Compiler
         ( Compiler(..) )
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Program (defaultProgramConfiguration
                                   ,configureAllKnownPrograms)
import Distribution.Simple.Utils
         ( die, notice, info, topHandler
         , findPackageDesc, tryFindPackageDesc, wrapText, cabalVersion )
import Distribution.Text
         ( display )
import Distribution.Verbosity as Verbosity
         ( Verbosity, normal )
import Distribution.Version
         ( Version(..), orLaterVersion )
import qualified Paths_cabal_install (version)

import Distribution.Client.Config
import Distribution.Client.Dependency
import Distribution.Client.Dependency
import Distribution.Client.IndexUtils
import Distribution.Client.Install
import Distribution.Client.PackageIndex
import Distribution.Client.Sandbox
import Distribution.Client.Targets
import Distribution.Client.Targets
import Distribution.Client.Types

import           Distribution.Package as P
import qualified Distribution.PackageDescription as PD
import           Distribution.Text
import           Distribution.Verbosity
import           Distribution.Version


import System.Environment       (getArgs, getProgName)
import System.Exit              (exitFailure)
import System.FilePath          (splitExtension, takeExtension)
import System.IO                ( BufferMode(LineBuffering), hSetBuffering, stdout )
import System.Directory         (doesFileExist, getCurrentDirectory)
import Data.List                (intercalate)
import Data.Maybe               (mapMaybe)
import Data.Monoid
import Control.Monad
import Prelude
import qualified Data.Map.Strict as Map

-- | Entry point
--
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  getArgs >>= mainWorker

mainWorker :: [String] -> IO ()
mainWorker args = topHandler $
  case commandsRun (globalCommand commands) commands args of
    CommandHelp   help                 -> printGlobalHelp help
    CommandList   opts                 -> printOptionsList opts
    CommandErrors errs                 -> printErrors errs
    CommandReadyToGo (globalFlags, commandParse)  ->
      case commandParse of
        _ | fromFlagOrDefault False (globalVersion globalFlags)
            -> printVersion
          | fromFlagOrDefault False (globalNumericVersion globalFlags)
            -> printNumericVersion
        CommandHelp     help           -> printCommandHelp help
        CommandList     opts           -> printOptionsList opts
        CommandErrors   errs           -> printErrors errs
        CommandReadyToGo action        -> do
          globalFlags' <- updateSandboxConfigFileFlag globalFlags
          action globalFlags'

  where
    printCommandHelp help = do
      pname <- getProgName
      putStr (help pname)
    printGlobalHelp help = do
      pname <- getProgName
      configFile <- defaultConfigFile
      putStr (help pname)
      putStr $ "\nYou can edit the cabal configuration file to set defaults:\n"
            ++ "  " ++ configFile ++ "\n"
      exists <- doesFileExist configFile
      when (not exists) $
          putStrLn $ "This file will be generated with sensible "
                  ++ "defaults if you run 'cabal update'."
    printOptionsList = putStr . unlines
    printErrors errs = die $ intercalate "\n" errs
    printNumericVersion = putStrLn $ display Paths_cabal_install.version
    printVersion        = putStrLn $ "cabal-install version "
                                  ++ display Paths_cabal_install.version
                                  ++ "\nusing version "
                                  ++ display cabalVersion
                                  ++ " of the Cabal library "

    commands =
        [ xlistCommand   `commandAddAction` xlistAction
        , installCommand `commandAddAction` installAction
        ]

----------------------------------------------------------------------------
----------------------------------------------------------------------------
----------------------------------------------------------------------------

installAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
              -> [String] -> GlobalFlags -> IO ()
installAction (configFlags, _, installFlags, _) _ _globalFlags
  | fromFlagOrDefault False (installOnly installFlags)
  = let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    in setupWrapper verbosity defaultSetupScriptOptions Nothing
         installCommand (const mempty) []

installAction (configFlags, configExFlags, installFlags, haddockFlags)
              extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
  (useSandbox, config) <- loadConfigOrSandboxConfig verbosity
                          globalFlags (configUserInstall configFlags)
  targets <- readUserTargets verbosity extraArgs

  -- TODO: It'd be nice if 'cabal install' picked up the '-w' flag passed to
  -- 'configure' when run inside a sandbox.  Right now, running
  --
  -- $ cabal sandbox init && cabal configure -w /path/to/ghc
  --   && cabal build && cabal install
  --
  -- performs the compilation twice unless you also pass -w to 'install'.
  -- However, this is the same behaviour that 'cabal install' has in the normal
  -- mode of operation, so we stick to it for consistency.

  let sandboxDistPref = case useSandbox of
        NoSandbox             -> NoFlag
        UseSandbox sandboxDir -> Flag $ sandboxBuildDir sandboxDir
      configFlags'    = maybeForceTests installFlags' $
                        savedConfigureFlags   config `mappend` configFlags
      configExFlags'  = defaultConfigExFlags         `mappend`
                        savedConfigureExFlags config `mappend` configExFlags
      installFlags'   = defaultInstallFlags          `mappend`
                        savedInstallFlags     config `mappend` installFlags
      haddockFlags'   = defaultHaddockFlags          `mappend`
                        savedHaddockFlags     config `mappend` haddockFlags
      globalFlags'    = savedGlobalFlags      config `mappend` globalFlags
  (comp, platform, conf) <- configCompilerAux' configFlags'
  -- TODO: Redesign ProgramDB API to prevent such problems as #2241 in the future.
  conf' <- configureAllKnownPrograms verbosity conf

  -- If we're working inside a sandbox and the user has set the -w option, we
  -- may need to create a sandbox-local package DB for this compiler and add a
  -- timestamp record for this compiler to the timestamp file.
  configFlags'' <- case useSandbox of
        NoSandbox               -> configAbsolutePaths $ configFlags'
        (UseSandbox sandboxDir) ->
          return $ (setPackageDB sandboxDir comp platform configFlags') {
            configDistPref = sandboxDistPref
            }

  whenUsingSandbox useSandbox $ \sandboxDir -> do
    initPackageDBIfNeeded verbosity configFlags'' comp conf'

    indexFile     <- tryGetIndexFilePath config
    maybeAddCompilerTimestampRecord verbosity sandboxDir indexFile
      (compilerId comp) platform

  -- FIXME: Passing 'SandboxPackageInfo' to install unconditionally here means
  -- that 'cabal install some-package' inside a sandbox will sometimes reinstall
  -- modified add-source deps, even if they are not among the dependencies of
  -- 'some-package'. This can also prevent packages that depend on older
  -- versions of add-source'd packages from building (see #1362).
  maybeWithSandboxPackageInfo verbosity configFlags'' globalFlags'
                              comp platform conf useSandbox $ \mSandboxPkgInfo ->
                              maybeWithSandboxDirOnSearchPath useSandbox $
      install verbosity
              (configPackageDB' configFlags'')
              (globalRepos globalFlags')
              comp platform conf'
              useSandbox mSandboxPkgInfo
              globalFlags' configFlags'' configExFlags'
              installFlags' haddockFlags'
              targets

    where
      -- '--run-tests' implies '--enable-tests'.
      maybeForceTests installFlags' configFlags' =
        if fromFlagOrDefault False (installRunTests installFlags')
        then configFlags' { configTests = toFlag True }
        else configFlags'

----------------------------------------------------------------------------
----------------------------------------------------------------------------
----------------------------------------------------------------------------

xlistAction :: ListFlags -> [String] -> GlobalFlags -> IO ()
xlistAction listFlags extraArgs globalFlags = do
    let verbosity = fromFlag (listVerbosity listFlags)
    (_useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags mempty
    let configFlags' = savedConfigureFlags config
        configFlags  = configFlags' {
          configPackageDBs = configPackageDBs configFlags'
                             `mappend` listPackageDBs listFlags
          }
        globalFlags' = savedGlobalFlags    config `mappend` globalFlags

    -- ???
    (comp, _, conf) <- configCompilerAux' configFlags

    let repos = globalRepos $ globalFlags'
    SourcePackageDb pix pp <- getSourcePackages verbosity repos

    targets <- readUserTargets verbosity extraArgs
    let deps = [ (p,c) | UserTargetNamed (Dependency p c) <- targets ]

        inDep spkg = or [ pn == n && withinRange pv vrg | (n,vrg) <- deps ]
          where
            pn = P.packageName    spkg
            pv = P.packageVersion spkg

    unless (length targets == length deps) $ do
        die "xlist: invalid/unsupported targets given"

    let pkgs | null deps = allPackages pix
             | otherwise = filter inDep $ allPackages pix

    forM_ pkgs $ \spkg@(SourcePackage _ pd _ _) -> do
        let pn                = P.packageName spkg
            pv@(Version _ []) = P.packageVersion spkg
            xrev              = maybe (0::Word) read $ lookup "x-revision"
                                $ PD.customFieldsPD $ PD.packageDescription $ pd
            pref | maybe True (withinRange pv) $ Map.lookup pn pp = "N" -- normal/preferred
                 | otherwise = "U" -- unpreferred
            autoflags         = [ (if flagDefault then '+' else '-') : n
                                | PD.MkFlag {..} <- PD.genPackageFlags pd
                                , not flagManual
                                , let PD.FlagName n = flagName ]

        -- print out line
        --   <pkg-name> <pkg-version> <pkg-x-revision> <pkg-is-undesired>
        putStrLn $ unwords ([ display pn, display pv, show xrev, pref ] ++ autoflags)

defaultListFlags :: ListFlags
defaultListFlags = ListFlags {
    listInstalled    = Flag False,
    listSimpleOutput = Flag False,
    listVerbosity    = toFlag normal,
    listPackageDBs   = []
  }

xlistCommand :: CommandUI ListFlags
xlistCommand = CommandUI {
    commandName         = "xlist",
    commandSynopsis     = "Extended listing of packages",
    commandDescription  = Just $ \_ -> wrapText $
         "List all packages, or all packages matching one of the search"
      ++ " strings.\n"
      ++ "\n"
      ++ "If there is a sandbox in the current directory and "
      ++ "config:ignore-sandbox is False, use the sandbox package database. "
      ++ "Otherwise, use the package database specified with --package-db. "
      ++ "If not specified, use the user package database.\n",
    commandNotes        = Nothing,
    commandUsage        = usageAlternatives "xlist" [ "[FLAGS]"
                                                    , "[FLAGS] STRINGS"],
    commandDefaultFlags = defaultListFlags,
    commandOptions      = \_ -> [
        optionVerbosity listVerbosity (\v flags -> flags { listVerbosity = v })

        , option [] ["installed"]
            "Only print installed packages"
            listInstalled (\v flags -> flags { listInstalled = v })
            trueArg

        , option [] ["simple-output"]
            "Print in a easy-to-parse format"
            listSimpleOutput (\v flags -> flags { listSimpleOutput = v })
            trueArg

        , option "" ["package-db"]
          "Use a given package database. May be a specific file, 'global', 'user' or 'clear'."
          listPackageDBs (\v flags -> flags { listPackageDBs = v })
          (reqArg' "DB" readPackageDbList showPackageDbList)

        ]
  }


{-

freezeAction :: FreezeFlags -> [String] -> GlobalFlags -> IO ()
freezeAction freezeFlags _extraArgs globalFlags = do
  let verbosity = fromFlag (freezeVerbosity freezeFlags)
  (useSandbox, config) <- loadConfigOrSandboxConfig verbosity globalFlags mempty
  let configFlags  = savedConfigureFlags config
      globalFlags' = savedGlobalFlags config `mappend` globalFlags
  (comp, platform, conf) <- configCompilerAux' configFlags

  maybeWithSandboxPackageInfo verbosity configFlags globalFlags'
                              comp platform conf useSandbox $ \mSandboxPkgInfo ->
                              maybeWithSandboxDirOnSearchPath useSandbox $
      freeze verbosity
            (configPackageDB' configFlags)
            (globalRepos globalFlags')
            comp platform conf
            mSandboxPkgInfo
            globalFlags' freezeFlags


-}
