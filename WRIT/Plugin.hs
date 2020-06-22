-- Copyright (c) 2020 Matthías Páll Gissurarson
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
module WRIT.Plugin ( plugin, module WRIT.Configure ) where

import WRIT.Configure

import Control.Monad (when, guard, foldM, zipWithM, msum)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, fromJust, listToMaybe, isJust)
import Data.Either
import Data.IORef
import Data.List (intersperse)
import Data.Function (on)
import Data.Kind (Constraint)
import Data.Data (Data, toConstr)
import Prelude hiding ((<>))
import qualified Data.Set as Set
import Data.Set (Set)

import GHC.TypeLits(TypeError(..), ErrorMessage(..))

#if __GLASGOW_HASKELL__ > 810
import GHC.Plugins hiding (TcPlugin)
import GHC.Tc.Plugin

import GHC.Tc.Types
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint
import GHC.Tc.Utils.TcMType hiding (newWanted, newFlexiTyVar, zonkTcType)


import GHC.Core.TyCo.Rep
import GHC.Core.Predicate
import GHC.Core.Class

import GHC.Utils.Error

import GHC.Builtin.Types.Prim
import GHC.Builtin.Names

import GHC.Types.Id.Make
#else
import GhcPlugins hiding (TcPlugin)
import TcRnTypes
import TcPluginM
import ErrUtils (Severity(SevWarning))
import TcEvidence


import TysPrim
import PrelNames
import TyCoRep

import ClsInst
import Class
import Inst hiding (newWanted)

import MkId
import TcMType hiding (newWanted, newFlexiTyVar, zonkTcType)

import TcType
import CoAxiom
import Unify

#if __GLASGOW_HASKELL__ < 810

-- Backported from 8.10
isEqPrimPred = isCoVarType
instance Outputable SDoc where
  ppr x = x

#else

import Constraint
import Predicate

#endif

#endif


--------------------------------------------------------------------------------
-- Exported

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . gritPlugin
                       , pluginRecompile = purePlugin }


--------------------------------------------------------------------------------

data Log = Log { log_pred_ty :: Type, log_loc :: CtLoc}
         | LogDefault { log_pred_ty :: Type, log_loc :: CtLoc,
                        log_var :: Var, log_kind :: Kind, log_res :: Type }

logSrc :: Log -> RealSrcSpan
logSrc = ctLocSpan . log_loc

instance Ord Log where
  compare a b = if logSrc a == logSrc b
                then case (a,b) of
                  (Log{}, LogDefault{}) -> GT
                  (LogDefault{}, Log{}) -> LT
                  (_, _) -> EQ
                else compare (logSrc a) (logSrc b)

instance Eq Log where
   Log{} == LogDefault{} = False
   LogDefault{} == Log{} = False
   a@Log{} == b@Log{} =
       ((==) `on` logSrc) a b && (eqType `on` log_pred_ty) a b
   a@LogDefault{} == b@LogDefault{} =
       ((==) `on` logSrc) a b && (eqType `on` log_pred_ty) a b
                              && ((==) `on` log_var) a b

instance Outputable Log where
   -- We do some extra work to pretty print the Defaulting messages
   ppr Log{..} =
        case userTypeError_maybe log_pred_ty of
           Just msg -> pprUserTypeErrorTy msg
           _ -> text "WRIT" <+> ppr log_pred_ty
   ppr LogDefault{..} = fsep [ text "Defaulting"
                               -- We want to print a instead of a0
                             , quotes (ppr (mkTyVarTy log_var)
                                       <+> dcolon <+> ppr log_kind)
                             , text "to"
                             , quotes (ppr log_res)
                             , text "in"
                             , quotes (ppr log_pred_ty)]
      where printFlav Given = "Will default"
            printFlav _ = "Defaulting"

zonkLog :: Log -> TcPluginM Log
zonkLog log@Log{..} = do zonked <- zonkTcType log_pred_ty
                         return $ log{log_pred_ty=zonked}
-- We don't want to zonk LogDefault, since then we can't see what variable was
-- being defaulted.
zonkLog log = return log

logToErr :: Log -> TcPluginM Ct
logToErr Log{..} = mkWanted log_loc log_pred_ty
logToErr LogDefault{..} =
   do txtCon <- promoteDataCon <$> tcLookupDataCon typeErrorTextDataConName
      appCon <- promoteDataCon <$> tcLookupDataCon typeErrorAppendDataConName
      dflags <- unsafeTcPluginTcM getDynFlags
      let txt str = mkTyConApp txtCon [mkStrLitTy $ fsLit str]
          sppr = txt . showSDoc dflags . ppr
          app ty1 ty2 = mkTyConApp appCon [ty1, ty2]
          msg = foldl1 app $
                  map sppr $ intersperse (text " ") $
                    [ text "Defaulting"
                    , quotes (ppr (mkTyVarTy log_var)
                              <+> dcolon <+> ppr log_kind)
                    , text "to"
                    , quotes (ppr log_res)
                    , text "in"
                    , quotes (ppr log_pred_ty)]
      mkTyErr msg >>= mkWanted log_loc

addWarning :: DynFlags -> Log -> TcPluginM ()
addWarning dflags log = tcPluginIO $ warn (ppr log)
  where warn = putLogMsg dflags NoReason SevWarning
#if __GLASGOW_HASKELL__ > 810
                 (RealSrcSpan (logSrc log) Nothing)
#else
                 (RealSrcSpan (logSrc log)) (defaultErrStyle dflags)
#endif

data Flags = Flags { f_debug        :: Bool
                   , f_quiet        :: Bool
                   , f_keep_errors  :: Bool
                   , f_no_default   :: Bool
                   , f_no_ignore    :: Bool
                   , f_no_promote   :: Bool
                   , f_no_discharge :: Bool
                   , f_no_only_if   :: Bool } deriving (Show)

getFlags :: [CommandLineOption] -> Flags
getFlags opts = Flags { f_debug        = "debug"          `elem` opts
                      , f_quiet        = "quiet"          `elem` opts
                      , f_keep_errors  = "keep-errors"    `elem` opts
                      , f_no_default   = "no-default"     `elem` opts
                      , f_no_ignore    = "no-ignore"      `elem` opts
                      , f_no_discharge = "no-discharge"   `elem` opts
                      , f_no_promote   = "no-promote"     `elem` opts
                      , f_no_only_if   = "no-only-if"     `elem` opts }

gritPlugin :: [CommandLineOption] -> TcPlugin
gritPlugin opts = TcPlugin initialize solve stop
  where
    flags@Flags{..} = getFlags opts
    initialize = do
      when f_debug $ tcPluginIO $ putStrLn "Starting WRIT in debug mode..."
      when f_debug $ tcPluginIO $ print flags
      tcPluginIO $ newIORef Set.empty
    solve :: IORef (Set Log) -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
    solve warns given derived wanted = do
       dflags <- unsafeTcPluginTcM getDynFlags
       let pprDebug :: Outputable a => String -> a -> TcPluginM ()
           pprDebug str a = when f_debug $
               tcPluginIO $ putStrLn (str ++ " " ++ showSDoc dflags (ppr a))
       pprDebug "Solving" empty
       pprDebug "-------" empty
       mapM_ (pprDebug "Given:") given
       mapM_ (pprDebug "Derived:") derived
       mapM_ (pprDebug "Wanted:") wanted
       pprDebug "-------" empty
       pluginTyCons <- getPluginTyCons
       let solveWFun :: ([Ct], ([(EvTerm, Ct)],[Ct], Set Log)) -> (SolveFun, String)
                     -> TcPluginM ([Ct], ([(EvTerm, Ct)],[Ct], Set Log))
           solveWFun (unsolved, (solved, more, logs)) (solveFun, explain) = do
             (still_unsolved, (new_solved, new_more, new_logs)) <-
               inspectSol <$> mapM (solveFun flags pluginTyCons) unsolved
             mapM_ (pprDebug (explain ++ "-sols")) new_solved
             mapM_ (pprDebug (explain ++ "-more")) new_more
             return (still_unsolved, (solved ++ new_solved,
                                      more ++ new_more,
                                      logs `Set.union` new_logs))
           order :: [(SolveFun, String)]
           order = [ (solveIgnore,    "Ignoring")
                   , (solveDefault,   "Defaulting")
                   , (solveOnlyIf,    "OnlyIf")
                   , (solveDischarge, "Discharging") ]
           to_check = wanted ++ derived
       (_, (solved_wanteds, more_cts, logs)) <-
          foldM solveWFun (to_check, ([],[],Set.empty)) order
       errs <- if f_keep_errors
               then mapM logToErr (Set.toAscList logs)
               else tcPluginIO $ modifyIORef warns (logs `Set.union`) >> mempty
       return $ TcPluginOk solved_wanteds (errs ++ more_cts)
    stop warns = do dflags <- unsafeTcPluginTcM getDynFlags
                    logs <- Set.toAscList <$> tcPluginIO (readIORef warns)
                    zonked_logs <- mapM zonkLog logs
                    when (not f_quiet) $ mapM_ (addWarning dflags) zonked_logs


data PluginTyCons = PTC { ptc_default :: TyCon
                        , ptc_promote :: TyCon
                        , ptc_only_if :: TyCon
                        , ptc_ignore  :: TyCon
                        , ptc_discharge  :: TyCon
                        , ptc_msg :: TyCon }

getPluginTyCons :: TcPluginM PluginTyCons
getPluginTyCons =
   do fpmRes <- findImportedModule (mkModuleName "WRIT.Configure") Nothing
      case fpmRes of
         Found _ mod  ->
             do ptc_default <- getTyCon mod "Default"
                ptc_discharge  <- getTyCon mod "Discharge"
                ptc_promote <- getTyCon mod "Promote"
                ptc_ignore  <- getTyCon mod "Ignore"
                ptc_only_if <- getTyCon mod "OnlyIf"
                ptc_msg     <- getPromDataCon mod "Msg"
                return PTC{..}
         NoPackage uid -> pprPanic "Plugin module not found (no package)!" (ppr uid)
         FoundMultiple ms -> pprPanic "Multiple plugin modules found!" (ppr ms)
         NotFound{..} -> pprPanic "Plugin module not found!" empty
  where getTyCon mod name = lookupOrig mod (mkTcOcc name) >>= tcLookupTyCon
        getPromDataCon mod name = promoteDataCon <$>
           (lookupOrig mod (mkDataOcc name) >>= tcLookupDataCon)


type Solution = Either Ct (Maybe (EvTerm, Ct), -- The solution to the Ct
                           [Ct],               -- Possible additional work
                           Set Log)              -- What we did

type SolveFun = Flags -> PluginTyCons -> Ct -> TcPluginM Solution

wontSolve :: Ct -> TcPluginM Solution
wontSolve = return . Left

couldSolve :: Maybe (EvTerm,Ct) -> [Ct] -> Set Log -> TcPluginM Solution
couldSolve ev work logs = return (Right (ev,work,logs))


-- Defaults any ambiguous type variables of kind k to l if Default k = l
-- Ignores any variables of kind (*)
solveDefault :: SolveFun
solveDefault Flags{..} _ ct | f_no_default = wontSolve ct
solveDefault Flags{..} ptc@PTC{..} ct =
  do defaults <- catMaybes <$> mapM getDefault (nonStar $ tyCoVarsOfCtList ct)
     if null defaults
     then wontSolve ct
     else
      -- We make assertions that `a ~ def` for all free a in pred_ty of ct. and
      -- add these as new assertions. For meta type variables (i.e. ones that
      -- have been instantiated with a `forall`, e.g. `forall a. Less H a`), an
      -- assert is a derived, meaning that we emit a wanted that requires no
      -- evidence . E.g. when checking  `forall (a :: Label) . Less H a` and we
      -- have `type instance Default Label = L`, we emit a `a0 ~ L`.
      -- For skolems ("rigid" type variables like the a in `True :: F a Bool`),
      -- we cannot touch the variable so we cannot unify them with a derived. In
      -- that case, we emit a given, saying that `a ~ L` i.e. we essentially
      -- change the type of `True :: F a Bool` to `True :: a ~ L => F a Bool`.
      -- Note that we cannot simply emit a given for both, since we cannot
      -- mention a meta type variable in a given.
      do let (eq_tys, logs) = unzip $ map mkTyEq defaults
         assert_eqs <- mapM mkAssert eq_tys
         couldSolve Nothing assert_eqs (Set.fromList logs)
   where mkAssert :: Either PredType (Type, EvExpr) -> TcPluginM Ct
         mkAssert = either (mkDerived bump) (uncurry (mkGiven bump))
         nonStar = filter (not . tcIsLiftedTypeKind . varType)
         bump = bumpCtLocDepth $ ctLoc ct
         getDefault var = do
           res <- matchFam ptc_default [varType var]
           case res of
             Just (_, rhs) -> return $ Just (var, rhs)
             _ -> return Nothing
         mkTyEq (var,def) = ( if isMetaTyVar var
                              then Left pred_ty
                              else Right (pred_ty, proof),
                              LogDefault{log_pred_ty = ctPred ct,
                                         log_var = var, log_kind = varType var,
                                         log_res = def, log_loc =ctLoc ct})
           where EvExpr proof = mkProof "grit-default" (mkTyVarTy var) def
                 pred_ty = mkPrimEqPredRole Nominal (mkTyVarTy var) def

-- Solves Γ |- c :: Constraint if Γ |- Ignore c ~ m and  Γ |- m,
-- *where c is an empty class*
solveIgnore :: SolveFun
solveIgnore Flags{..} _ ct | f_no_ignore = wontSolve ct
solveIgnore _ _ ct@CDictCan{..} | not (null $ classMethods cc_class) = wontSolve ct
solveIgnore _ ptc@PTC{..} ct@CDictCan{..} = do
  res <- matchFam ptc_ignore [ctPred ct]
  case res of
    Nothing -> wontSolve ct
    Just (_, rhs) -> do
      let classCon = tyConSingleDataCon (classTyCon cc_class)
      (msg_check, msg_var) <- checkMsg ptc ct rhs
      let log = Set.singleton (Log msg_var (ctLoc ct))
      couldSolve (Just (evDataConApp classCon cc_tyargs [], ct)) [msg_check] log
solveIgnore _ _ ct = wontSolve ct

-- Solves Γ |- (a :: k) ~ (b :: k) if Γ |- Discharge a b ~ m and  Γ |- m.
-- Promote is the same as Discharge, except we also require that a and b be
-- Coercible.
solveDischarge :: SolveFun
solveDischarge flags@Flags{..} ptc@PTC{..} ct =
  case splitEquality (ctPred ct) of
    Just (k1,ty1,ty2) -> do
      famRes <- matchFam ptc_discharge [k1, ty1, ty2]
      let (might, can't) = (return famRes, return Nothing)
      canSolve <-
        case famRes of
          Nothing -> can't
          -- If k is *, then we are doing a promotion, since the only
          -- instance of Discharge (a :: *) (b ::*) is for Promote.
          _ | tcIsLiftedTypeKind k1 && f_no_promote -> can't
          _ | tcIsLiftedTypeKind k1 -> do
               -- We only solve (a :: *) ~ (b :: *) if Promote a b
               hasProm <- isJust <$> matchFam ptc_promote [ty1, ty2]
               -- Note that we don't use the rhs here, since it is already
               -- taken care of by the Discharge a b instance (and the)
               -- Coercible constraint as well.
               if hasProm then might else can't
          -- Otherwise, it's a regular discharge
          _ | f_no_discharge -> can't
          _ -> might
      case canSolve of
        Nothing -> wontSolve ct
        Just (_, msg) -> do
          (msg_check, msg_var) <- checkMsg ptc ct msg
          let log = Set.singleton (Log msg_var (ctLoc ct))
          couldSolve (Just (mkProof "grit-discharge" ty1 ty2, ct)) [msg_check] log
    _ -> wontSolve ct

-- checkMsg generates a `m ~ Msg m0` constraint that we can solve, which unifies
-- the type variable m0 with whatever the resulting type error message is.
checkMsg :: PluginTyCons -> Ct -> Type -> TcPluginM (Ct, Type)
checkMsg PTC{..} ct msg =  do
  err_msg_kind <- flip mkTyConApp [] <$> getErrMsgCon
  ty_var <- mkTyVarTy <$> newFlexiTyVar err_msg_kind
  let eq_ty = mkCoercionType Nominal msg (mkTyConApp ptc_msg [ty_var])
  ct <- mkWanted (ctLoc ct) eq_ty
  ty_err <- mkTyErr ty_var
  return (ct, ty_err)


-- Solve only if solves equalities of the for OnlyIf C m_a ~ m_b, by checking
-- that C holds and that m_a ~ m_b.
solveOnlyIf :: SolveFun
solveOnlyIf _ PTC{..} ct =
  case splitEquality (ctPred ct) of
    Just (k1,ty1,ty2) -> do
        -- As an optimization to avoid the constraint solver having to do too
        -- many loops, we unwrap any nested OnlyIfs here, and gather all the
        -- constraints.
        case reverse (unwrapOnlyIfs ty1) of
          [_] -> wontSolve ct
          (msg:cons) -> do
            let eq_ty = mkCoercionType Nominal msg ty2
                ev = mkProof "grit-only-if" ty1 ty2
            check_msg <- mkWanted (ctLoc ct) eq_ty
            check_cons <- mapM (mkWanted (ctLoc ct)) cons
            couldSolve (Just (ev, ct)) (check_msg:check_cons) Set.empty
          _ -> wontSolve ct
    _ -> wontSolve ct
  where unwrapOnlyIfs msg = case splitTyConApp_maybe msg of
                             Just (tc, [con, nested]) | tc == ptc_only_if ->
                                (con:unwrapOnlyIfs nested)
                             _ -> [msg]

mkTyErr ::  Type -> TcPluginM Type
mkTyErr msg = flip mkTyConApp [typeKind msg, msg] <$>
                 tcLookupTyCon errorMessageTypeErrorFamName

getErrMsgCon :: TcPluginM TyCon
getErrMsgCon = lookupOrig gHC_TYPELITS (mkTcOcc "ErrorMessage") >>= tcLookupTyCon
-- Utils
mkDerived :: CtLoc -> PredType -> TcPluginM Ct
mkDerived loc eq_ty = flip setCtLoc loc . CNonCanonical <$> newDerived loc eq_ty

mkWanted :: CtLoc -> PredType -> TcPluginM Ct
mkWanted loc eq_ty = flip setCtLoc loc . CNonCanonical <$> newWanted loc eq_ty

mkGiven :: CtLoc -> PredType -> EvExpr -> TcPluginM Ct
mkGiven loc eq_ty ev = flip setCtLoc loc . CNonCanonical <$> newGiven loc eq_ty ev

mkProof :: String -> Type -> Type -> EvTerm
mkProof str ty1 ty2 = evCoercion $ mkUnivCo (PluginProv str) Nominal ty1 ty2

splitEquality :: Type -> Maybe (Kind, Type, Type)
splitEquality pred =
  do (tyCon, [k1, k2, ty1,ty2]) <- splitTyConApp_maybe pred
     guard (tyCon == eqPrimTyCon)
     guard (k1 `eqType` k2)
     return (k1, ty1,ty2)

inspectSol :: Ord d => [Either a (Maybe b, [c], Set d)]
           -> ([a], ([b], [c], Set d))
inspectSol xs = (ls, (catMaybes sols, concat more, Set.unions logs))
  where (ls, rs) = partitionEithers xs
        (sols, more, logs) = unzip3 rs