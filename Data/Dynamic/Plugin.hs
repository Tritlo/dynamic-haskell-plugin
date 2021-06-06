-- Copyright (c) 2020-2021 Matthías Páll Gissurarson
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Dynamic.Plugin ( plugin, Default, TypeError(..), ErrorMessage(..),
                             castDyn, dynDispatch, pattern Is) where

import Control.Monad
    ( when, unless, guard, foldM, zipWithM, msum, filterM, replicateM )
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, fromJust, listToMaybe, isJust)
import Data.Either
import Data.IORef
import Data.List (nubBy, sortOn, intersperse, or, partition, minimumBy, maximumBy, sort, find)
import Control.Arrow ((&&&))
import Data.Function (on)
import Data.Kind (Constraint)
import Data.Data (Data, toConstr)
import Prelude hiding ((<>))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Proxy
import Data.Dynamic
import Text.Read (readMaybe)

import GHC.TypeLits(TypeError(..), ErrorMessage(..))

import Data.Coerce

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.IO.Unsafe (unsafePerformIO)

import Bag
import FV (fvVarListVarSet, fvVarSet)

import qualified TcEnv as Tc (tcLookup)
import DsUtils



























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
import qualified TcMType as TcM

import TcType
import CoAxiom
import Unify
import TcHsSyn

import InstEnv

-- Holefits
import RdrName (globalRdrEnvElts)
import TcRnMonad (keepAlive, getLclEnv, getGlobalRdrEnv, getGblEnv, newSysName, setGblEnv)
import TcHoleErrors
import PrelInfo (knownKeyNames)
import Data.Graph (graphFromEdges, topSort, scc)
import DsBinds (dsHsWrapper)
import DsMonad (initDsTc)

import TcEvTerm (evCallStack)


import GHC.Hs.Expr





















import Constraint
import Predicate



import GHC.TypeLits (TypeError(..),ErrorMessage(..))
--import Data.Typeable
import Type.Reflection (SomeTypeRep(..), someTypeRep)
import Data.Dynamic
import GHC.Stack

--------------------------------------------------------------------------------
-- Exported

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . dynamicPlugin
                       , pluginRecompile = purePlugin
                       , installCoreToDos = coreDyn }

-- | The Default family allows us to 'default' free type variables of a given
-- kind in a constraint to the given value, i.e. if there is an instance
-- Default k for and a is a free type variable of kind k in constraint c,
-- then a ~ Default k will be added to the context of c, and
-- Γ, a ~ Defaul k |- c : Constraint checked for validity.
type family Default k :: k

-- | castDyn casts a Dynamic to any typeable value, and fails with a descriptive
-- error if the types dont match. Automatically inserted for casting Dynamic
-- values back to static values.
castDyn :: forall a . (Typeable a, HasCallStack) => Dynamic -> a
castDyn arg = fromDyn arg err
  where err = error ("Couldn't match expected type '" ++ target
                     ++ "' with actual dynamic type '" ++ actual  ++ "'")
        target = show (someTypeRep (Proxy :: Proxy a))
        actual = show (dynTypeRep arg)

dynDispatch :: forall b . (Typeable b)
            => [(SomeTypeRep, Dynamic)] -- ^ Provided by the plugin
            -> String                   -- ^ The name of the function
            -> String                   -- ^ The name of the class
            -> Dynamic -> b
dynDispatch insts fun_name class_name dispatcher =
    case lookup argt insts of
      Just f ->
         fromDyn f
         (error $ "Type mismatch when dispatching '"
         ++ fun_name
         ++ "' expecting '" ++ show targett
         ++"' but got '" ++ show (dynTypeRep f)
         ++ "' using dispatch table for '"
         ++ class_name ++ "'!")
      _ -> error $ "No instance of '" ++ class_name ++ " " ++ show argt ++ "'"
                  ++ " found when dispatching for '"
                  ++ fun_name ++ " :: " ++ show targett
                  ++ "', with 'Dynamic ~ " ++ show argt
                  ++ "' in this context."
 where argt = dynTypeRep dispatcher
       targett = someTypeRep (Proxy :: Proxy b)

pattern Is :: forall a. (Typeable a) => a -> Dynamic
pattern Is res <- (fromDynamic @a -> Just res)

--------------------------------------------------------------------------------

data Log = Log { log_pred_ty :: Type, log_loc :: CtLoc}
         | LogDefault { log_pred_ty :: Type, log_loc :: CtLoc,
                        log_var :: Var, log_kind :: Kind, log_res :: Type }
         | LogMarshal { log_pred_ty :: Type, log_loc :: CtLoc, log_to_dyn :: Bool}
         | LogSDoc {log_pred_ty :: Type, log_loc :: CtLoc, log_msg :: SDoc}

logSrc :: Log -> RealSrcSpan
logSrc = ctLocSpan . log_loc

instance Ord Log where
  compare a@Log{} b@Log{} =
      if logSrc a == logSrc b
      then (compare `on` showSDocUnsafe . ppr) a b
      else (compare `on` logSrc) a b
  compare Log{} _ = LT
  compare _ Log{} = GT
  compare a@LogDefault{} b@LogDefault{} =
      if logSrc a == logSrc b
      then (compare `on` showSDocUnsafe . ppr) a b
      else (compare `on` logSrc) a b
  compare LogDefault{} _ = LT
  compare _ LogDefault{} = GT
  compare a@LogMarshal{} b@LogMarshal{} =
      if logSrc a == logSrc b
      then (compare `on` showSDocUnsafe . ppr) a b
      else (compare `on` logSrc) a b
  compare LogMarshal{} _ = LT
  compare _ LogMarshal{} = GT
  compare a@LogSDoc{} b@LogSDoc{} =
      if logSrc a == logSrc b
      then (compare `on` showSDocUnsafe . ppr) a b
      else (compare `on` logSrc) a b

instance Eq Log where
   a@Log{} == b@Log{} =
       ((==) `on` logSrc) a b && (eqType `on` log_pred_ty) a b
   Log{} == _ = False
   a@LogDefault{} == b@LogDefault{} =
       ((==) `on` logSrc) a b && (eqType `on` log_pred_ty) a b
                              && ((==) `on` log_var) a b
   LogDefault{} == _ = False
   a@LogMarshal{} == b@LogMarshal{} =
       ((==) `on` logSrc) a b && (eqType `on` log_pred_ty) a b
   LogMarshal{} == _ = False
   a@LogSDoc{} == b@LogSDoc{} =
       ((==) `on` logSrc) a b
       && (eqType `on` log_pred_ty) a b
       && ((==) `on` showSDocUnsafe . log_msg) a b
   LogSDoc{} == _ = False

instance Outputable Log where
   -- We do some extra work to pretty print the Defaulting messages
   ppr Log{..}
     | Just msg <- userTypeError_maybe log_pred_ty = pprUserTypeErrorTy msg
     | otherwise = text "DataDynamicPlugin" <+> ppr log_pred_ty
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
   ppr LogMarshal{..} = fsep [ text "Marshalling"
                             , quotes (ppr log_pred_ty)
                             , text (if log_to_dyn
                                     then "to Dynamic"
                                     else "from Dynamic") ]
   ppr LogSDoc{..} = log_msg

zonkLog :: Log -> TcPluginM Log
zonkLog log@Log{..} = do zonked <- zonkTcType log_pred_ty
                         return $ log{log_pred_ty=zonked}
-- We don't want to zonk LogDefault, since then we can't see what variable was
-- being defaulted.
zonkLog log = return log

logToErr :: Log -> TcPluginM Ct
logToErr Log{..} = mkWanted log_loc log_pred_ty
logToErr LogDefault{..} =
   sDocToTyErr [ text "Defaulting"
            , quotes (ppr (mkTyVarTy log_var)
                      <+> dcolon <+> ppr log_kind)
            , text "to"
            , quotes (ppr log_res)
            , text "in"
            , quotes (ppr log_pred_ty)] >>= mkWanted log_loc
logToErr LogMarshal{..} =
   sDocToTyErr [ text "Marshalling"
            , quotes (ppr log_pred_ty)
            , text (if log_to_dyn
                    then "to Dynamic"
                    else "from Dynamic") ] >>= mkWanted log_loc
logToErr LogSDoc{..} = sDocToTyErr [log_msg] >>= mkWanted log_loc

sDocToTyErr :: [SDoc] -> TcPluginM Type
sDocToTyErr docs =
  do txtCon <- promoteDataCon <$> tcLookupDataCon typeErrorTextDataConName
     appCon <- promoteDataCon <$> tcLookupDataCon typeErrorAppendDataConName
     dflags <- unsafeTcPluginTcM getDynFlags
     let txt str = mkTyConApp txtCon [mkStrLitTy $ fsLit str]
         sppr = txt . showSDoc dflags . ppr
         app ty1 ty2 = mkTyConApp appCon [ty1, ty2]
     mkTyErr $ foldl1 app $ map sppr $ intersperse (text " ") docs

addWarning :: DynFlags -> Log -> TcPluginM ()
addWarning dflags log = tcPluginIO $ warn (ppr log)
  where warn = putLogMsg dflags NoReason SevWarning



                 (RealSrcSpan (logSrc log)) (defaultErrStyle dflags)


data Flags = Flags { f_debug            :: Bool
                   , f_quiet            :: Bool
                   , f_keep_errors      :: Bool
                    } deriving (Show)

getFlags :: [CommandLineOption] -> Flags
getFlags opts = Flags { f_debug        = "debug"        `elem` opts
                      , f_quiet        = "quiet"        `elem` opts
                      , f_keep_errors  = "keep_errors"  `elem` opts
                      }


pprOut :: Outputable a => String -> a -> TcPluginM ()
pprOut str a = do dflags <- unsafeTcPluginTcM getDynFlags
                  tcPluginIO $ putStrLn (str ++ " " ++ showSDoc dflags (ppr a))

dynamicPlugin :: [CommandLineOption] -> TcPlugin
dynamicPlugin opts = TcPlugin initialize solve stop
  where
    flags@Flags{..} = getFlags opts
    initialize = do
      when f_debug $ tcPluginIO $ putStrLn "Starting DataDynamicPlugin in debug mode..."
      when f_debug $ tcPluginIO $ print flags
      tcPluginIO $ newIORef Set.empty
    solve :: IORef (Set Log) -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
    solve warns given derived wanted = do
       dflags <- unsafeTcPluginTcM getDynFlags
       let pprDebug :: Outputable a => String -> a -> TcPluginM ()
           pprDebug str a = when f_debug $ pprOut str a
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
               inspectSol <$> mapM (solveFun pluginTyCons) unsolved
             mapM_ (pprDebug (explain ++ "-sols")) new_solved
             mapM_ (pprDebug (explain ++ "-more")) new_more
             return (still_unsolved, (solved ++ new_solved,
                                      more ++ new_more,
                                      logs `Set.union` new_logs))
           order :: [(SolveFun, String)]
           order = [ (solveDynamic, "Discharging")
                   , (solveDefault,   "Defaulting")
                   , (solveDynamicTypeables,   "SDTs")
                   , (solveDynDispatch,   "Checking Dynamic Dispatch") ]
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
                    unless f_quiet $ mapM_ (addWarning dflags) zonked_logs


data PluginTyCons = PTC { ptc_default :: TyCon
                        , ptc_dc :: DynCasts }

data DynCasts = DC { dc_typeable :: Class
                   , dc_dynamic :: TyCon
                   , dc_to_dyn :: Id
                   , dc_cast_dyn :: Id
                   , dc_has_call_stack :: TyCon
                   , dc_dyn_dispatch :: Id
                   , dc_sometyperep :: TyCon
                   , dc_sometyperep_dc :: DataCon
                   , dc_typerep :: Id }

getPluginTyCons :: TcPluginM PluginTyCons
getPluginTyCons =
   do fpmRes <- findImportedModule (mkModuleName "Data.Dynamic.Plugin") Nothing
      dc_dynamic <- getTyCon dYNAMIC "Dynamic"
      dc_typeable <- getClass tYPEABLE_INTERNAL "Typeable"
      dc_sometyperep <- getTyCon tYPEABLE_INTERNAL "SomeTypeRep"
      dc_sometyperep_dc <- getDataCon tYPEABLE_INTERNAL "SomeTypeRep"
      dc_typerep <- getId tYPEABLE_INTERNAL "typeRep"
      dc_to_dyn <- getId dYNAMIC "toDyn"
      dc_has_call_stack <- getTyCon gHC_STACK_TYPES "HasCallStack"

      case fpmRes of
         Found _ mod  ->
             do ptc_default <- getTyCon mod "Default"
                dc_cast_dyn <- getId mod "castDyn"
                dc_dyn_dispatch <- getId mod "dynDispatch"
                let ptc_dc = DC {..}
                return PTC{..}
         NoPackage uid -> pprPanic "Plugin module not found (no package)!" (ppr uid)
         FoundMultiple ms -> pprPanic "Multiple plugin modules found!" (ppr ms)
         NotFound{..} -> pprPanic "Plugin module not found!" empty
  where getTyCon mod name = lookupOrig mod (mkTcOcc name) >>= tcLookupTyCon
        getDataCon mod name = lookupOrig mod (mkDataOcc name) >>= tcLookupDataCon
        getPromDataCon mod name = promoteDataCon <$> getDataCon mod name
        getClass mod name = lookupOrig mod (mkClsOcc name) >>= tcLookupClass
        getId mod name = lookupOrig mod (mkVarOcc name) >>= tcLookupId


type Solution = Either Ct (Maybe (EvTerm, Ct), -- The solution to the Ct
                           [Ct],               -- Possible additional work
                           Set Log)              -- What we did

type SolveFun = PluginTyCons -> Ct -> TcPluginM Solution

wontSolve :: Ct -> TcPluginM Solution
wontSolve = return . Left

couldSolve :: Maybe (EvTerm,Ct) -> [Ct] -> Set Log -> TcPluginM Solution
couldSolve ev work logs = return (Right (ev,work,logs))


-- Defaults any ambiguous type variables of kind k to l if Default k = l
solveDefault :: SolveFun
solveDefault ptc@PTC{..} ct =
  do defaults <- catMaybes <$> mapM getDefault (tyCoVarsOfCtList ct)
     if null defaults then wontSolve ct
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
     else do let (eq_tys, logs) = unzip $ map mkTyEq defaults
             assert_eqs <- mapM mkAssert eq_tys
             couldSolve Nothing assert_eqs (Set.fromList logs)
   where mkAssert = either (mkDerived bump) (uncurry (mkGiven bump))
         bump = bumpCtLocDepth $ ctLoc ct
         getDefault var = fmap ((var,) . snd) <$> matchFam ptc_default [varType var]
         mkTyEq (var,def) = ( if isMetaTyVar var then Left pred_ty
                              else Right (pred_ty, proof),
                              LogDefault{log_pred_ty = ctPred ct,
                                         log_var = var, log_kind = varType var,
                                         log_res = def, log_loc =ctLoc ct})
           where EvExpr proof = mkProof "data-dynamic-default" (mkTyVarTy var) defApp
                 pred_ty = mkPrimEqPredRole Nominal (mkTyVarTy var) defApp
                 defApp = mkTyConApp ptc_default [varType var]


mkTyErr ::  Type -> TcPluginM Type
mkTyErr msg = flip mkTyConApp [typeKind msg, msg] <$>
                 tcLookupTyCon errorMessageTypeErrorFamName

-- | Creates a type error with the given string at the given loc.
mkTypeErrorCt :: CtLoc -> String -> TcPluginM Ct
mkTypeErrorCt loc str =
  do txtCon <- promoteDataCon <$> tcLookupDataCon typeErrorTextDataConName
     appCon <- promoteDataCon <$> tcLookupDataCon typeErrorAppendDataConName
     vappCon <- promoteDataCon <$> tcLookupDataCon  typeErrorVAppendDataConName
     let txt str = mkTyConApp txtCon [mkStrLitTy $ fsLit str]
         app ty1 ty2 = mkTyConApp appCon [ty1, ty2]
         vapp ty1 ty2 = mkTyConApp vappCon [ty1, ty2]
         unwty = foldr1 app . map txt . intersperse " "
         ty_err_ty = foldr1 vapp $ map (unwty . words) $ lines str
     te <- mkTyErr ty_err_ty
     mkWanted loc te

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

----------------------------------------------------------------
-- Marshalling to and from Dynamic
----------------------------------------------------------------
-- | Solves Γ |- (a :: Type) ~ (b :: Type) if a ~ Dynamic or b ~ Dynamic
solveDynamic :: SolveFun
solveDynamic ptc@PTC{..} ct
 | Just (k1,ty1,ty2) <- splitEquality (ctPred ct) = do
    let DC {..} = ptc_dc
        dynamic = mkTyConApp dc_dynamic []
        kIsType = tcIsLiftedTypeKind k1
        isDyn ty = ty `tcEqType` dynamic
    if kIsType && (isDyn ty1 || isDyn ty2)
    then marshalDynamic k1 ty1 ty2 ptc ct
    else wontSolve ct
  | otherwise = wontSolve ct


dYNAMICPLUGINPROV :: String
dYNAMICPLUGINPROV = "data-dynamic"

marshalDynamic :: Kind -> Type -> Type -> SolveFun
marshalDynamic k1 ty1 ty2 PTC{..} ct@(CIrredCan CtWanted{ctev_dest = HoleDest coho} _) =
   do let DC {..} = ptc_dc
          dynamic = mkTyConApp dc_dynamic []
          isDyn ty = ty `tcEqType` dynamic
          relTy = if isDyn ty1 then ty2 else ty1
          log = Set.singleton (LogMarshal relTy (ctLoc ct) (isDyn ty2))
          hasTypeable = mkTyConApp (classTyCon dc_typeable) [k1, relTy]
          hasCallStack = mkTyConApp dc_has_call_stack []
      checks@[check_typeable, check_call_stack] <- mapM (mkWanted (ctLoc ct)) [hasTypeable, hasCallStack]
      call_stack <- mkFromDynErrCallStack dc_cast_dyn ct $ ctEvEvId $ ctEvidence check_call_stack
      let typeableDict = ctEvEvId $ ctEvidence check_typeable
          evExpr = if isDyn ty1
                   then mkApps (Var dc_cast_dyn) [Type relTy, Var typeableDict, call_stack]
                   else mkApps (Var dc_to_dyn) [Type relTy, Var typeableDict]
          (at1,at2) = if isDyn ty1 then (dynamic, relTy) else (relTy, dynamic)
      deb <- unsafeTcPluginTcM $ mkSysLocalM (fsLit dYNAMICPLUGINPROV) (exprType evExpr)

      let mkProof prov = mkUnivCo (PluginProv prov) Nominal at1 at2
      if isTopTcLevel (ctLocLevel $ ctLoc ct)
      then do -- setEvBind allows us to emit the evExpr we built, and since
              -- we're at the top, it will be emitted as an exported variable
              let prov = marshalVarToString deb
              setEvBind $ mkGivenEvBind (setIdExported deb) (EvExpr evExpr)
              couldSolve (Just (evCoercion (mkProof prov), ct)) checks log
      else do -- we're within a function, so setting the evBinds won't actually
              -- put it within scope.
              let prov = dYNAMICPLUGINPROV
                  let_b = Let (NonRec deb evExpr)
                          -- By binding and seqing, we ensure that the evExpr
                          -- doesn't get erased.
                          (seqVar deb $ Coercion $ mkProof prov)
              couldSolve (Just (EvExpr let_b, ct)) checks log



marshalDynamic _ _ _ _  ct = wontSolve ct

-- By applying the same function when generating the provinence and for the
-- lookup of the variable name later, we know we will find the corresponding
-- variable.
marshalVarToString :: Var -> String
marshalVarToString var = nstr ++ "_" ++ ustr
  where nstr = occNameString (occName var)
        ustr = show (varUnique var)

mkFromDynErrCallStack :: Id -> Ct -> EvVar -> TcPluginM EvExpr
mkFromDynErrCallStack fdid ct csDict =
  flip mkCast coercion <$>
     unsafeTcPluginTcM (evCallStack (EvCsPushCall name loc var))
  where name = idName fdid
        loc = ctLocSpan (ctLoc ct)
        var = Var csDict
        coercion = mkSymCo (unwrapIP (exprType var))

-- | Post-processing for Dynamics

type DynExprMap  = Map (Either String Var) (Expr Var) -- These we need to find from case exprs.

-- | Here we replace the "proofs" of the casts with te actual calls to toDyn
-- and castDyn.
coreDyn :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
coreDyn clo tds = return $ CoreDoPluginPass "DataDynamicPlugin" (bindsOnlyPass addDyn):tds
  where
    Flags {..} = getFlags clo
    found var expr = Map.singleton var expr
    addDyn :: CoreProgram -> CoreM CoreProgram
    addDyn program = mapM (addDynToBind dexprs) program
      where
        dexprs = Map.fromList $ concatMap getDynamicCastsBind program

    -- We need to find the two types of expressions, either the exported globals
    -- (which we can then directly use, or the seq'd ones buried within cases
    -- for locals).
    -- We grab the `data-dynamic_a1bK :: A -> Dynamics` from the binds, and
    -- the `case case <dyn_expr> of {DEFAULT -> <UnivCo proof>} of <covar>` from
    -- the expressions, where Left <data-dynamic_var_name> and Right <covar>.
    getDynamicCastsBind :: CoreBind -> [(Either String Var, Expr Var)]
    getDynamicCastsBind (NonRec var expr) |
      occNameString (occName var) == dYNAMICPLUGINPROV =
        (Left $ marshalVarToString var, Var var):getDynamicCastsExpr expr
    getDynamicCastsBind (NonRec _ expr) = getDynamicCastsExpr expr
    getDynamicCastsBind (Rec as) =
      -- The top level ones will never be recursive.
      concatMap (getDynamicCastsExpr . snd) as

    getDynamicCastsExpr :: Expr Var -> [(Either String Var, Expr Var)]
    getDynamicCastsExpr (Var _) = []
    getDynamicCastsExpr (Lit _) = []
    getDynamicCastsExpr (App expr arg) =
      concatMap getDynamicCastsExpr [expr, arg]
    getDynamicCastsExpr (Lam _ expr) = getDynamicCastsExpr expr
    getDynamicCastsExpr (Let bind expr) =
      getDynamicCastsBind bind ++ getDynamicCastsExpr expr
    getDynamicCastsExpr c@(Case expr covar _ alts) =
       ecasts ++ concatMap gdcAlts alts
      where gdcAlts (_,_,e) = getDynamicCastsExpr e
            ecasts = case expr of
              -- This is the expression build by the seqVar, though unfortunately,
              -- the var itself isn't preserved. It's OK though, since we have
              -- to replace the covar itself and not from the variable name.
              Case dexpr _ _ [(DEFAULT, [], Coercion (UnivCo (PluginProv prov) _ _ _))] |
                prov == dYNAMICPLUGINPROV -> [(Right covar, dexpr)]
              _ -> getDynamicCastsExpr expr
    getDynamicCastsExpr (Cast expr _) = getDynamicCastsExpr expr
    getDynamicCastsExpr (Tick _ expr) = getDynamicCastsExpr expr
    getDynamicCastsExpr (Type _) = []
    getDynamicCastsExpr (Coercion _) = []



    addDynToBind :: DynExprMap -> CoreBind -> CoreM CoreBind
    addDynToBind dexprs (NonRec b expr) = NonRec b <$> addDynToExpr dexprs expr
    addDynToBind dexprs (Rec as) = do
      let (vs, exprs) = unzip as
      nexprs <- mapM (addDynToExpr dexprs) exprs
      return (Rec $ zip vs nexprs)


    addDynToExpr :: DynExprMap -> Expr Var -> CoreM (Expr Var)
    addDynToExpr _ e@(Var _) = pure e
    addDynToExpr _ e@(Lit _) = pure e
    addDynToExpr dexprs (App expr arg) =
      App <$> addDynToExpr dexprs expr <*> addDynToExpr dexprs arg
    addDynToExpr dexprs (Lam b expr) = Lam b <$> addDynToExpr dexprs expr
    addDynToExpr dexprs (Let binds expr) = Let <$> addDynToBind dexprs binds
                                               <*> addDynToExpr dexprs expr
    addDynToExpr dexprs (Case expr b ty alts) =
             (\ne na -> Case ne b ty na) <$> addDynToExpr dexprs expr
                                         <*> mapM addDynToAlt alts
      where addDynToAlt (c, bs, expr) = (c, bs,) <$> addDynToExpr dexprs expr
    -- Cast is the only place that we do any work beyond just recursing over
    -- the sub-expressions. Here we replace the
    -- (A `cast` UnivCo (PluginProv <data-dynamic_var_name>) Nominal A Dynamic)
    -- and (B `cast` SubCo <covar>) that was generated in the TcPlugin with
    -- the respective (data-dynamic_var_name A) (i.e. apply the function to A)
    -- and (toDyn @B ... B).
    addDynToExpr dexprs orig@(Cast expr coercion) = do
      nexpr <- addDynToExpr dexprs expr
      case coercion of
        UnivCo (PluginProv prov) _ _ _ |
           Just expr <- dexprs Map.!? Left prov -> found expr nexpr
        SubCo (CoVarCo co) | Just expr <- dexprs Map.!? Right co -> found expr nexpr
        UnivCo (PluginProv _) _ _ _ -> pprPanic "Unfound var" $ ppr coercion
        _ -> return (Cast nexpr coercion)
      where found expr nexpr = do
               let res = App expr nexpr
               when f_debug $
                 liftIO $ putStrLn $ showSDocUnsafe $
                 text "Replacing" <+> parens (ppr orig)
                 <+> text "with" <+> parens (ppr res)
               return res
    addDynToExpr dexprs (Tick t expr) = Tick t <$> addDynToExpr dexprs expr
    addDynToExpr _ e@(Type _) = pure e
    addDynToExpr _ e@(Coercion _) = pure e

-- | Solves Γ |- C Dynamic
solveDynDispatch :: SolveFun
solveDynDispatch ptc@PTC{..} ct | CDictCan{..} <- ct
                                , [arg] <- cc_tyargs
                                , arg `tcEqType` dynamic = do
  class_insts <- flip classInstances cc_class <$> getInstEnvs
  let (unsaturated, saturated) = partition (not . null . is_tvs) class_insts
      class_tys = map is_tys saturated
  -- We can only dispatch on singe argument classes
  if not (all ((1 ==) . length) class_tys) then wontSolve ct
  else do
    -- Make sure we check any superclasses
    scChecks <- mapM (mkWanted (ctLoc ct) .
                      flip piResultTys cc_tyargs .
                      mkSpecForAllTys (classTyVars cc_class))
                      $ classSCTheta cc_class
    let scEvIds = map (evId . ctEvId) scChecks
    args_n_checks <- mapM (methodToDynDispatch cc_class class_tys)
                          (classMethods cc_class)
    let logs = Set.fromList $ [LogSDoc (ctPred ct) (ctLoc ct) $
                fsep ([text "Building dispatch table for"
                , quotes $ ppr $ ctPred ct
                , text "based on"
                , fsep $ map (quotes . ppr) saturated
                ] ++ if null unsaturated then []
                     else [ text "Skipping unsaturated instances"
                     , fsep $ map (quotes . ppr) unsaturated ])]
        classCon = tyConSingleDataCon (classTyCon cc_class)
        (args, checks) = unzip args_n_checks
        proof = evDataConApp classCon cc_tyargs $ scEvIds ++ args
    couldSolve (Just (proof, ct)) (scChecks ++ concat checks) logs
                               | otherwise = wontSolve ct
  where
     DC {..} = ptc_dc
     dynamic = mkTyConApp dc_dynamic []
     sometyperep = mkTyConApp dc_sometyperep []
     -- | The workhorse. Creates the dictonary for C Dynamic on the fly.
     methodToDynDispatch :: Class
                         -> [[Type]]
                         -> Id
                         -> TcPluginM (EvExpr, [Ct])
     -- For method 'loo :: Show a => Int -> a -> Int -> Int' in Foo with instances
     -- Foo A and Foo B, this will generate the following (in Core):
     -- Notation: {Foo A} = The dictionary for Foo A
     -- (\ (k :: Show Dynamic) (l :: Int) (m :: Dynamic) ->
     --   dynDispatch @(Show Dynamic => Int -> Dynamic -> Int -> Int)
     --               {Typeable (Show Dynamic => Int -> Dynamic -> Int -> Int)}
     --               -- ^ Only used too lookup in the table
     --               [ (SomeTypeRep (typeRep :: TypeRep A), -- In core
     --                  toDyn @(Show Dynamic => Int -> Dynamic -> Int -> Int)
     --                        {Typeable (Show Dynamic => Int -> Dynamic -> Int -> Int)}
     --                        (\ (k :: Show Dynamic) (l :: Int) (m :: Dynamic) ->
     --                           loo @A {Foo A} {Show A} l (castDyn m)))
     --               , (SomeTypeRep (typeRep :: TypeRep B), -- In core
     --                  toDyn @(Show Dynamic => Int -> Dynamic -> Int -> Int)
     --                        {Typeable (Show Dynamic => Int -> Dynamic -> Int -> Int)}
     --                        (\ (k :: Show Dynamic) (l :: Int) (m :: Dynamic) ->
     --                           loo @B {Foo B} {Show B} l (castDyn m)))]
     --               -- ^ The dynamic dispatch table
     --               "loo"
     --               -- ^ The name of the function. Used for better error messages.
     --               "Foo"
     --               -- ^ The name of the class. Used for better error messages.
     --               (m :: Dynamic)
     --               -- ^ The dynamic value to dispatch on
     --               (runtimeError @(Show Dynamic) "Should never be evaluated!")
     --               -- ^ Provided to please the type gods. This dictionary
     --               --   is just thrown away by the function after dispatch.
     --               (l :: Int)
     --               -- ^ The first argument to the function, captured before
     --               --   we had the dynamic we could use to know which type
     --               --   to dispatch on.
     --               (m :: Dynamic)
     --               -- ^ The dynamic again. This will go to a castDyn to the
     --               --   proper type before being evaluated at the function.
     -- )
     -- And similar entries for each function in the Foo class.
     --
     -- When given a dynamic (Dynamic (tr :: TypeRep a) (v :: a)), dynDispatch
     -- looks up  (SomeTypeRep tr :: SomeTypeRep) in the dispatch table.
     -- If it finds a function 'f' that matches, it converts it to the expected
     -- value with 'fromDyn f', if possible, and emits a runtime error otherwise.
     -- If a function with the matching type is not found, it also emits a
     -- runtime error, saying that no matching instance was found.
     methodToDynDispatch cc_class class_tys fid = do
       -- Names included for better error messages.
       let fname = occNameFS (getOccName fid)
           cname = occNameFS (getOccName cc_class)
       fun_name <- unsafeTcPluginTcM $ mkStringExprFS fname
       class_name <- unsafeTcPluginTcM $ mkStringExprFS cname
       let (tvs, ty) = tcSplitForAllVarBndrs (varType fid)
           (res, preds) = splitPreds ty
           bound_preds = map (mkForAllTys tvs) preds
           dpt_ty = mkBoxedTupleTy [sometyperep, dynamic]
           fill_ty = piResultTys (mkForAllTys tvs res)
           enough_dynamics = replicate (length $ head class_tys) dynamic
           dyn_ty = fill_ty enough_dynamics
           -- Whole ty is the type minus the Foo a in the beginning
           whole_ty = funResultTy $ piResultTys (varType fid) enough_dynamics
           unsatisfied_preds = map (`piResultTy` dynamic) $  drop 1 bound_preds
           mkMissingDict t =
              mkRuntimeErrorApp rUNTIME_ERROR_ID t "Dynamic dictonary shouldn't be evaluated!"
           dynb_pred_dicts = map mkMissingDict unsatisfied_preds
       dyn_pred_vars <- unsafeTcPluginTcM $ mapM (mkSysLocalM (getOccFS fid)) unsatisfied_preds
       let -- | The workhorse that constructs the dispatch tables.
           mkDpEl :: Type -> [CoreBndr]  -> [Type] -> TcPluginM (CoreExpr, [Ct])
           mkDpEl res_ty revl dts@[dp_ty] =
              do (tev, check_typeable) <- checkTypeable whole_ty
                 (dptev, check_typeable_dp) <- checkTypeable dp_ty
                 check_preds <- mapM (mkWanted (ctLoc ct) . flip piResultTys dts) bound_preds
                 let dyn_app = mkCoreApps (Var dc_to_dyn) [Type whole_ty, Var tev]
                     pevs = map ctEvId check_preds
                     fapp = mkCoreApps (Var fid) $ Type dp_ty : map Var pevs
                     toFappArg :: (Type, Type, CoreBndr) -> TcPluginM (CoreExpr, [Ct])
                     toFappArg (t1,t2,b) | tcEqType t1 t2 = return (Var b, [])
                                         |  otherwise  = do
                         (tev, check_typeable) <- checkTypeable t2
                         ccs <- mkWanted (ctLoc ct) $ mkTyConApp dc_has_call_stack []
                         cs <- mkFromDynErrCallStack dc_cast_dyn ct $ ctEvEvId $ ctEvidence ccs
                         let app = mkCoreApps (Var dc_cast_dyn)
                                       [Type t2, Var tev, cs, Var b]
                         return (app,[check_typeable, ccs])
                     matches :: [CoreBndr] -> Type -> [(Type, Type, CoreBndr)]
                     matches [] _ = []
                     matches (b:bs) ty = (varType b, t, b):matches bs r
                       where (t,r) = splitFunTy ty -- Safe, binders are as long or longer.
                 (fappArgs, fappChecks) <- unzip <$> mapM toFappArg (matches revl res_ty)
                 let fapp_app = mkCoreApps fapp fappArgs
                     -- If the result is dependent on the type, we must wrap it in
                     -- a toDyn. I.e. for Ord Dynamic,
                     -- max :: a -> a -> a  must have the type Dynamic -> Dynamic -> Dynamic
                     -- so we must cast the result to
                     --
                     -- NOTE BREAKS, i.e. max (A :: Dynamic) (B :: Dynamic)
                     --             is just the latter argument.
                     dfapp_arg = if (exprType (lambda fapp_app) `tcEqType` whole_ty)
                                 then lambda fapp_app
                                 else lambda (td fapp_app)
                        where dfapp_arg_mb = lambda fapp_app
                              lambda = mkCoreLams (dyn_pred_vars ++ revl)
                              td x = mkCoreApps (Var dc_to_dyn) [Type dp_ty, Var dptev, x]

                     dfapp = mkCoreApps dyn_app [dfapp_arg]
                     trapp = mkCoreApps (Var dc_typerep) [Type (tcTypeKind dp_ty), Type dp_ty, Var dptev]
                     strapp = mkCoreApps
                                 (Var (dataConWrapId dc_sometyperep_dc))
                                 [Type (tcTypeKind dp_ty), Type dp_ty, trapp]
                     checks = [check_typeable, check_typeable_dp] ++ check_preds ++ concat fappChecks
                     tup = mkCoreTup [strapp, dfapp]
                 return (tup, checks)
           mkDpEl _ _ tys = pprPanic "Multi-param typeclasses not supported!" $ ppr tys

           finalize (dp:lams) res_ty = do
             let revl = reverse (dp:lams)
                 mkFunApp a b = mkTyConApp funTyCon [tcTypeKind a,tcTypeKind b, a, b]
             (tev, check_typeable) <- checkTypeable whole_ty
             let saturated = filter is_saturated class_tys
                 is_saturated = all (not . isPredTy)
             dpt_els_n_checks <- mapM (\ct -> mkDpEl (fill_ty ct) revl ct) saturated
             -- To make the types match up, we must make a dictionary for each of
             -- the predicates, even though these will never be used.
             let (dpt_els, dpt_checks) = unzip dpt_els_n_checks
                 app = mkCoreApps (Var dc_dyn_dispatch)
                         ([ Type whole_ty, evId tev, mkListExpr dpt_ty dpt_els
                         , fun_name, class_name, Var dp]
                         ++ dynb_pred_dicts
                         ++ map Var revl)
                 checks = check_typeable:concat dpt_checks
                 -- TODO app to pred dicts
                 lamApp = mkCoreLams (dyn_pred_vars ++ revl) app
             return (lamApp, checks)

           -- We figure out all the arguments to the functions first from the type.
           loop lams ty = do
             case splitFunTy_maybe ty of
                Just (t,r) -> do
                  bid <- unsafeTcPluginTcM $ mkSysLocalM (getOccFS fid) t
                  loop (bid:lams) r
                _ -> finalize lams ty
       loop [] dyn_ty

     checkTypeable :: Type -> TcPluginM (EvId, Ct)
     checkTypeable ty = do
        c <- mkWanted (ctLoc ct) $ mkTyConApp (classTyCon dc_typeable) [tcTypeKind ty, ty]
        return (ctEvId c, c)

splitPreds :: Type -> (Type, [PredType])
splitPreds ty =
  case tcSplitPredFunTy_maybe ty of
    Just (pt, t) -> (pt:) <$> splitPreds t
    _ -> (ty, [])

-- | GHC doesn't know how to solve Typeable (Show Dynamic => Dynamic -> Int),
-- but in core it's the same as Show Dynamic -> Dynamic -> Int. So we simply
-- show that 'Show Dynamic' and 'Dynamic -> Int' are both typeable, and
-- construct the evidence that 'Show Dynamic => Dynamic -> Int' is thus
-- typeable.
solveDynamicTypeables :: SolveFun
solveDynamicTypeables ptc@PTC{..}
   ct | CDictCan{..} <- ct
      , cc_class == dc_typeable
      , [kind, ty] <- cc_tyargs
      , tcIsLiftedTypeKind kind
      , (res_ty, preds@(p:ps)) <- splitPreds ty
      , pts <- mapMaybe splitTyConApp_maybe preds
      , all (tcEqType dynamic) $ concatMap snd pts =
     do (r_typable_ev, r_typeable_ct) <- checkTypeable res_ty
        -- We don't want to check the constraints here, since we won't need
        -- them for the actual e.g. Show Dynamic, since we'll never
        -- call the function at Dynamic.
        -- constrs <- mapM (mkWanted (ctLoc ct)) preds
        t_preds <- mapM checkTypeablePred pts
        let (p_evs, p_cts) = unzip t_preds
            checks = r_typeable_ct:concat p_cts
            classCon = tyConSingleDataCon (classTyCon cc_class)
            r_ty_ev = EvExpr $ evId r_typable_ev
            (final_ty, proof) = foldr conTypeable (res_ty, r_ty_ev) p_evs
        couldSolve (Just (proof, ct)) checks Set.empty
     | otherwise = wontSolve ct
  where
     DC {..} = ptc_dc
     dynamic = mkTyConApp dc_dynamic []
     checkTypeablePred :: (TyCon, [Type]) -> TcPluginM ((Type, EvTerm), [Ct])
     checkTypeablePred (tc, tys) = do
       args_typeable <- mapM checkTypeable tys
       let (_, evcts) = unzip args_typeable
           ev = EvTypeableTyCon tc (map (EvExpr . evId . ctEvId) evcts)
           ty = mkTyConApp tc tys
       return ((ty, evTypeable ty ev), evcts)
     conTypeable :: (Type, EvTerm) -> (Type, EvTerm) -> (Type, EvTerm)
     conTypeable (fty, fterm) (argty, argterm) =
       let res_ty = mkTyConApp funTyCon [tcTypeKind fty, tcTypeKind argty, fty, argty]
           r_term = evTypeable res_ty $ EvTypeableTrFun fterm argterm
       in (res_ty, r_term)

     checkTypeable :: Type -> TcPluginM (EvId, Ct)
     checkTypeable ty = do
        c <- mkWanted (ctLoc ct) $ mkTyConApp (classTyCon dc_typeable) [tcTypeKind ty, ty]
        return (ctEvId c, c)
