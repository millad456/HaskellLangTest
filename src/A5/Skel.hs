
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-name-shadowing -Wno-unused-matches #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use fromMaybe" #-}
module A5.Skel where


import Control.Applicative


import Data.Functor
import Data.List
import Debug.Trace


-- * Core Data Types
-- TODO: Implement the following data types, following
-- the relevant sections from "Types and Programming Languages" by Benjamin Pierce.


-- | Untyped expressions
data Expr
   = Var String
   -- ^ Variables
   | Lam String Expr | App Expr Expr
   -- ^ Lambda and application (See figure 9-1)
   | Unit
   -- ^ Unit (See 11.2 in TAPL)
   | Let String Expr Expr
   -- ^ Let-bindings (See 11.5 in TAPL)
   | Pair Expr Expr | Fst Expr | Snd Expr
   -- ^ Pairs (See 11.6 in TAPL)
   | TT | FF | If Expr Expr Expr
   -- ^ Booleans (See 8.2 in TAPL)
   | Zero | Succ Expr | Pred Expr | IsZero Expr
   -- ^ Natural Numbers (See 8.2 in TAPL)
   deriving (Show)


-- | Types.
data Type
 = TpVar String
 -- ^ Type variables
 | FnTp Type Type
 -- ^ Function types
 | UnitTp
 -- ^ Unit (See 11.2 in TAPL)
 | PairTp Type Type
 -- ^ Pair Types (See 11.6 in TAPL)
 | BoolTp
 -- ^ Booleans (See 8.2 in TAPL)
 | NatTp
 -- ^ Booleans (See 8.2 in TAPL)
 deriving (Eq, Show)


-- | Typed expressions
data TypedExpr
   = TVar String
   -- ^ Variables
   | TLam String Type TypedExpr | TApp TypedExpr TypedExpr
   -- ^ Lambda and application (See figure 9-1)
   | TUnit
   -- ^ Unit (See figure 11-2 in TAPL)
   | TLet String Scheme TypedExpr TypedExpr
   -- ^ Let-bindings (See figure 11-4 in TAPL)
   | TPair TypedExpr TypedExpr | TFst TypedExpr | TSnd TypedExpr
   -- ^ Pairs (See figure 11-5 in TAPL)
   | TTT | TFF | TIf TypedExpr TypedExpr TypedExpr
   -- ^ Booleans (See figure 8-2 in TAPL)
   | TZero | TSucc TypedExpr | TPred TypedExpr | TIsZero TypedExpr
   -- ^ Natural Numbers (See figure 8-2 in TAPL)
   deriving (Show)


data Scheme = Poly [String] Type | Mono Type
   deriving (Show)


-- | Contexts
type Ctx = [(String, Scheme)]


-- | Substitutions
type Substitution = [(String, Type)]

-- | Find the free variables in an expression.
freeVars :: Expr -> [String]
freeVars (Var x) = [x]
freeVars (Lam x e) = freeVars e \\ [x]
freeVars (App e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Let x e1 e2) = freeVars e1 `union` (freeVars e2 \\ [x])
freeVars (Pair e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Fst e) = freeVars e
freeVars (Snd e) = freeVars e
freeVars (If e1 e2 e3) = freeVars e1 `union` freeVars e2 `union` freeVars e3
freeVars (Succ e) = freeVars e
freeVars (Pred e) = freeVars e
freeVars (IsZero e) = freeVars e
-- Constants like Unit, True, False, Zero do not contain any free variables
freeVars Unit = []
freeVars TT = []
freeVars FF = []
freeVars Zero = []

-- | Find the free type variables in a type.
freeTpVars :: Type -> [String]
freeTpVars (TpVar v) = [v]
freeTpVars (FnTp t1 t2) = nub (freeTpVars t1 ++ freeTpVars t2)
freeTpVars (PairTp t1 t2) = nub $ freeTpVars t1 ++ freeTpVars t2
freeTpVars BoolTp = []
freeTpVars NatTp = []
freeTpVars _ = []

-- | Find the free type variables in a type scheme.
freeSchemeVars :: Scheme -> [String]
freeSchemeVars (Mono tp) = freeTpVars tp
freeSchemeVars (Poly vars tp) = freeTpVars tp \\ vars

-- | Find the free type variables in a context.
freeCtxVars :: Ctx -> [String]
freeCtxVars ctx = nub $ concatMap (freeSchemeVars . snd) ctx


-- * Substitution
-- | Compose two substitutions.
composeSub :: Substitution -> Substitution -> Substitution
composeSub s1 s2 = nub $ map (apply s2) s1 ++ s2
  where
    apply :: Substitution -> (String, Type) -> (String, Type)
    apply sub (var, typ) = (var, substTp sub typ)

-- | Compose a list of substitutions into a single substitution.
composeSubs :: [Substitution] -> Substitution
composeSubs = foldr composeSub []

-- | Remove a set of variables from a substitution.
thinSub :: [String] -> Substitution -> Substitution
thinSub avoid = filter (\(var, _) -> notElem var avoid)


-- | Pick a name that isn't found in the provided list of names.
freshen :: String -> [String] -> String
freshen name avoid = head $ dropWhile (`elem` avoid) (iterate (++ "'") name)
-- | Rename a term to not use the names in the provided list.
rename :: Expr -> [String] -> Expr
-- If the expression is a variable, check if it's in the avoid list.
-- If it is, rename it using freshen; otherwise, leave it unchanged.
rename (Var x) avoid = if x `elem` avoid then Var (freshen x avoid) else Var x
-- If the expression is a lambda abstraction, introduce a fresh name for the parameter
-- and recursively rename the body, updating the avoid list.
rename (Lam x e) avoid = Lam newX (rename e (newX : avoid))
  where
    newX = freshen x avoid
-- If the expression is an application, recursively rename the function and argument.
rename (App e1 e2) avoid = App (rename e1 avoid) (rename e2 avoid)
-- If the expression is Unit, TT, FF, Zero, leave it unchanged.
rename Unit _ = Unit
rename TT _ = TT
rename FF _ = FF
rename Zero _ = Zero
-- If the expression is a let binding, introduce a fresh name for the variable,
-- recursively rename the bound expression and body, updating the avoid list.
rename (Let x e1 e2) avoid = Let newX (rename e1 avoid) (rename e2 (newX : avoid))
  where
    newX = freshen x avoid
-- If the expression is a pair, recursively rename its components.
rename (Pair e1 e2) avoid = Pair (rename e1 avoid) (rename e2 avoid)
-- If the expression is a projection (Fst or Snd), recursively rename its operand.
rename (Fst e) avoid = Fst (rename e avoid)
rename (Snd e) avoid = Snd (rename e avoid)
-- If the expression is an if-then-else, recursively rename its condition and branches.
rename (If e1 e2 e3) avoid = If (rename e1 avoid) (rename e2 avoid) (rename e3 avoid)
-- If the expression is a successor or predecessor, recursively rename its operand.
rename (Succ e) avoid = Succ (rename e avoid)
rename (Pred e) avoid = Pred (rename e avoid)
-- If the expression is an is-zero, recursively rename its operand.
rename (IsZero e) avoid = IsZero (rename e avoid)




-- | 'subst e1 e2 x' is 'e1[e2/x]'
subst :: Expr -> Expr -> String -> Expr
subst = undefined


-- | Substitute for type variables in a type.
substTp :: Substitution -> Type -> Type
substTp sub (TpVar x) = maybe (TpVar x) id (lookup x sub)
substTp sub (FnTp t1 t2) = FnTp (substTp sub t1) (substTp sub t2)
substTp sub (PairTp t1 t2) = PairTp (substTp sub t1) (substTp sub t2)
substTp _ UnitTp = UnitTp
substTp _ BoolTp = BoolTp
substTp _ NatTp = NatTp

-- | Substitute for type variables in a type scheme.
substScheme :: Substitution -> Scheme -> Scheme
substScheme = undefined

-- | Substitute for type variables in a typed expression.
substTypedExpr :: Substitution -> TypedExpr -> TypedExpr
substTypedExpr = undefined
-- | Substitute for type variables in a context.
substCtx :: Substitution -> Ctx -> Ctx
substCtx = undefined


-- | Generalize a type to a scheme by quantifying all type variables
-- that are not mentioned in the given context.
generalize :: Ctx -> Type -> Scheme
generalize = undefined


instantiate :: Scheme -> Infer Type
instantiate = undefined

-- | Type synthesis function
synth :: Ctx -> Expr -> Infer (TypedExpr, Type, Substitution)
-- If the expression is a variable, look up its type scheme in the context (gamma).
synth = undefined 

-- | Unify two types, returning a substitution that makes them equal, if possible.
unify :: Type -> Type -> Infer Substitution
unify = undefined

-- | Bind a type variable to a type, unless they are already identical,
-- or it would create a recursive type.
bind :: String -> Type -> Infer Substitution
bind = undefined

-- | Infer the type of an expression, returning
-- it's typed counterpart, along with it's type.
infer :: Ctx -> Expr -> Either String (TypedExpr, Type)
infer ctx expr = case runInfer (synth ctx expr) genNames of
    Left err -> Left err
    Right ((typedExpr, tp, _), _) -> Right (typedExpr, tp)

-- | Erase a typed expression.
erase :: TypedExpr -> Expr
erase = undefined


step :: Expr -> Maybe Expr
step = undefined


eval :: Expr -> Expr
eval expr = maybe expr eval (step expr)

-- * Inference Monad
-- When we are inferring types, we need to be able to generate fresh names
-- and throw errors. Both of these effects can be captured by monads, but
-- combining monads is a bit tricky. Therefore, we just define a custom
-- monad that does both!


-- | The 'Infer' monad is used for type inference. It combines the ability
-- to generate fresh names and handle errors.
newtype Infer a = Infer { runInfer :: [String] -> Either String (a, [String]) }
   deriving (Functor)

-- | 'Applicative' instance for the 'Infer' monad.
instance Applicative Infer where
   -- | Lifts a value into the 'Infer' monad.
   pure a = Infer $ \s -> Right (a, s)

   -- | Sequentially compose two 'Infer' actions, passing the result of the first
   -- to the second.
   mf <*> ma = Infer $ \s -> do
       -- Run the first 'Infer' action and get its result and new state.
       (f, s') <- runInfer mf s
       -- Run the second 'Infer' action with the new state and get the final result.
       (a, s'') <- runInfer ma s'
       -- Combine the results using the provided function.
       pure (f a, s'')


--given from skel
-- | 'Monad' instance for the 'Infer' monad.
instance Monad Infer where
   -- | Lifts a value into the 'Infer' monad.
   return = pure
   -- | Sequentially compose two 'Infer' actions, passing the result of the first
   -- to the second.
   mf >>= k = Infer $ \s -> do
       -- Run the first 'Infer' action and get its result and new state.
       (a, s') <- runInfer mf s
       -- Run the continuation function with the result and the new state.
       runInfer (k a) s'

-- | Generate a fresh type variable.
freshTpVar :: Infer Type
freshTpVar = Infer $ \names ->
 let name = head names
 in Right (TpVar name, tail names)


-- | Emit a type error.
typeError :: String -> Infer a
typeError err = Infer $ \_ -> Left err

-- | Infinite list of fresh names.
genNames :: [String]
genNames = map pure ['a' .. 'z'] ++ map (++ "'") genNames

execInfer :: Infer a -> Either String a
execInfer m = fst <$> runInfer m genNames

-- * Misc. Helpers

unionWith :: (Eq b) => (a -> [b]) -> [a] -> [b]
unionWith k = foldr (union . k) []

remove :: (Eq a) => a -> [(a,b)] -> [(a,b)]
remove a [] = []
remove a ((a', x):xs) | a == a' = remove a xs
                     | otherwise = (a', x) : remove a xs

