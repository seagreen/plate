
module Plate.Validation where

import           Plate.Prelude hiding (evaluate, exp)

import qualified Data.HashMap.Strict as HM

import           Plate.Schema
import           Plate.Value

data Invalid
  = VariableNotFound      Text
  | AbstractionUnapplied  Text Expression
  | NonAbstractionApplied Expression Expression
  | SumTypeSizeNotOne     (HashMap Text Expression) (HashMap Text Plate)
  | SumTypeNoMatch        (HashMap Text Expression) (HashMap Text Plate)
  | MissingFields         (HashMap Text Expression) (HashMap Text Plate)
  | BuiltinMismatch       Schema Plate
  deriving (Eq, Show)

validate :: HashMap Text Expression -> Expression -> Plate -> Either Invalid ()
validate bound expression plate =
  case expression of
    Variable t -> case evaluateVariable bound t of
                    Nothing  -> Left (VariableNotFound t)
                    Just exp -> validate bound exp plate
    Abstraction t exp -> Left (AbstractionUnapplied t exp)
    Application exp1 exp2 ->
      case exp1 of
        Abstraction t abstractedExp -> validate (addBinding bound t exp2)
                                                abstractedExp plate
        _ -> Left (NonAbstractionApplied exp1 exp2)
    Builtin schema -> validateBuiltin bound schema plate

-- | Internal. Used by 'validate'.
addBinding
  :: HashMap Text Expression
  -> Text
  -> Expression
  -> HashMap Text Expression
addBinding hm t exp = HM.insert t exp hm

-- | Internal. Used by 'validate'.
evaluateVariable
  :: HashMap Text Expression
  -> Text
  -> Maybe Expression
evaluateVariable hm t = HM.lookup t hm

-- | Internal. Used by 'validate'.
validateBuiltin
  :: HashMap Text Expression
  -> Schema
  -> Plate
  -> Either Invalid ()
validateBuiltin bound schema plate =
  case (schema, plate) of
    (SumType st, PPrimative hm) -> do
      when (HM.size hm /= 1) (Left (SumTypeSizeNotOne st hm))
      case HM.elems (HM.intersectionWith (,) st hm) of
        [(exp, p2)] -> validate bound exp p2
        _           -> Left (SumTypeNoMatch st hm)
    (ProductType pt, PPrimative hm) -> do
      let b = HM.intersectionWith (,) pt hm
      when (HM.size b < HM.size pt) (Left (MissingFields pt hm))
      traverse_
        (\(exp, p2) -> validate bound exp p2)
        (HM.elems b)
    (SPrimative exp, PPrimative hm) -> traverse_ (validate bound exp) hm
    (SInteger, PInteger _) -> pure ()
    (SSet exp, PSet xs) -> traverse_ (validate bound exp) xs
    (SDictionary exp1 exp2, PDictionary hm) ->
      traverse_
        (\(k, p2) -> validate bound exp1 k *> validate bound exp2 p2)
        (HM.toList hm)
    (SSequence exp, PSequence xs) -> traverse_ (validate bound exp) xs
    (SString, PString _) -> pure ()
    _ -> Left (BuiltinMismatch schema plate)

-- | 'Left' means an expression was applied to a non-abstraction.
--
-- Not actually used in this library, just exposed in case other code needs it.
whnf :: Expression -> Either (Expression, Expression) Expression
whnf expression =
  case expression of
    Application exp1 exp2 -> handleApplication exp1 exp2
    _                     -> Right expression
  where
    handleApplication
      :: Expression
      -> Expression
      -> Either (Expression, Expression) Expression
    handleApplication exp1 exp2 =
      case exp1 of
        Abstraction t abstractedExp -> whnf (replace t exp2 abstractedExp)
        Application e1 e2 -> (\a -> handleApplication a exp2)
                         =<< handleApplication e1 e2
        _ -> Left (exp1, exp2)

    -- PERFORMANCE: Not efficient to replace variables one-by-one like this.
    replace :: Text -> Expression -> Expression -> Expression
    replace t bound target =
      case target of
        Variable v -> if t == v
                        then bound
                        else target
        Abstraction param abstractedExp ->
          if t == param
            then target
            else Abstraction param (replace t bound abstractedExp)
        Application e1 e2 -> Application (replace t bound e1)
                                         (replace t bound e2)
        Builtin schema -> Builtin (mapSchema (replace t bound) schema)
