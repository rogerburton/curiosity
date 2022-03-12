{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DataKinds          #-}
{- |
Module: Prototype.Backend.InteractiveState
Description: Live state server

We introduce three type classes: InteractiveState, InteractiveDisp, and
InteractiveStateOnDisp.

The first one describes the operations supported by our "database", the second
one describes how those operations can be communicated to the system and how to
render their results (e.g. in a REPL), and the third one simply glues them
together (this corresponds to the REP part of the REPL).

TODO: I think this file should start with InteractiveState, instead of
InteractiveDisp.

TODO: It seems InteractiveState should be named something like
InteractiveOperations or similar.

Discussion: I'm wondering if the structure here with the notions of DispInput,
DispInput and parsing will work outside of the textual REPL case. I think it's
right to have layers, with the state at the bottom, then a notion of operations
that can interact with the state, but for me, the UI, be it a REPL, a web
server, or a TUI, is more of a concrete thing that is responsible of
constructing the appropriate operations (and handling the results). The way it
constructs such an operation is too specific to be abstracted (even more by a
notion of parsing). Imagine an OpenGL application where you can use a mouse, a
keayboard, shortcuts, but that has also a text console embedded within the GUI.

-}
module Prototype.Backend.InteractiveState.Class
  (
    -- * Main typeclasses 
    InteractiveDisp(..)
  , InteractiveState(..)
  , InteractiveStateOnDisp(..)
  -- * Supported display types. 
  , DispType(..)
  -- * Common functions
  , execAnyInputOnState
  -- * Types 
  , AnyStateInput(..)
  , AnyStateOutput(..)
  -- ** Instance definitions
  , DispInput(..)
  , DispOutput(..)
  ) where

import qualified Data.String                   as String
import qualified Prototype.Runtime.Errors      as Errs
import "this"    PrototypeHsLibPrelude
import qualified Text.Pretty.Simple            as Pretty

-- | The "Repl" output medium for the live state. 
data DispType = Repl

-- | A typeclass indicating how an interactive state's DispType handles the common display operations,
-- and the types of values it provides as inputs and outputs. 
class InteractiveDisp (dispType :: DispType) where

  -- | The data type of input the given display has. 
  data DispInput (dispType :: DispType) :: Type

  -- | The data type of output the given display has. 
  data DispOutput (dispType :: DispType) :: Type

-- | The instance for @Repl@ is straightforward. The inputs are Text and the outputs either @Text@ or @LText@. 
instance InteractiveDisp 'Repl where
  
  newtype DispInput 'Repl = ReplInputStrict { replInputStrict :: Text }
                          deriving (Show)
                          deriving IsString via Text
  
  data DispOutput 'Repl = ReplOutputStrict Text
                        | ReplOutputLazy LText
                        | ReplRuntimeErr Errs.RuntimeErr

instance IsString (DispOutput 'Repl) where
  fromString = ReplOutputStrict . String.fromString

-- | A sum-type that encodes any operation on an interactive state. 
data AnyStateInput state =
  -- | Operation to visualise the state: this is usually querying the state for some data, but not modifying it. 
  AnyStateVisualisationInput (StateVisualisation state)
  -- | Operation to _modify_ the state: this is when we execute operations to modify the state. 
  | AnyStateModificationInput (StateModification state)


-- | A sum-type that encodes any operation on an interactive state. 
data AnyStateOutput state =
  AnyStateVisualisationOutput (StateVisualisationOutput state)
  | AnyStateModificationOutput (StateModificationOutput state)

deriving instance ( Show (StateVisualisation state)
                  , Show (StateModification state)
                  )
                  => Show (AnyStateInput state)

{- |

= Synopsis:

The live state of the backend offers a way to visualise and alter the "state" of the application. By "state" here, we refer to the
storage state. This lets the /executor/ of the service, modify & visualise the stored values on the fly, irrespective of the UI.

This greatly aids in the process of rapid prototyping where the executing party doesn't need to fiddle around with the UI. 

= Note:

In the future, we'd also like to support additional modes like a @Brick@ etc., if needed. 

= Goals:

An InteractiveState should enable the user to perform the following operations on the state.

1. Query (i.e. visualise) the state.

2. Modify the state, without having to restart the application.

3. Generate state snapshots, optionally saving them. 

4. Revert the state to a saved snapshot. 
-}
class InteractiveState state where

  -- | Operations to modify state, represented as an ADT. 
  data StateModification state :: Type

  -- | The result of modifying the state. 
  data StateModificationOutput state :: Type

  -- | Operations to visualise state, represented as an ADT. 
  data StateVisualisation state :: Type

  -- | The result of visualising the state. 
  data StateVisualisationOutput state :: Type

  -- | Associated errors that can be generated by the state. 
  data InteractiveStateErr state :: Type

  -- | Constraints needed to be satisfied in which state modifications can take place. 
  type StateModificationC state (m :: Type -> Type) :: Constraint

  -- | Constraints needed to be satisfied in which state visualisations can take place. 
  type StateVisualisationC state (m :: Type -> Type) :: Constraint

  -- | Execute a modification.
  execModification
    :: (StateModificationC state m)
    => StateModification state
    -> m (StateModificationOutput state)

  -- | Execute a visualisation. 
  execVisualisation
    :: (StateVisualisationC state m)
    => StateVisualisation state
    -> m (StateVisualisationOutput state)

  execAny
    :: ( StateVisualisationC state m
       , StateModificationC state m
       , Applicative m
       )
    => AnyStateInput state
    -> m (AnyStateOutput state)
  execAny = \case
    AnyStateVisualisationInput vis -> AnyStateVisualisationOutput <$> execVisualisation vis
    AnyStateModificationInput mod' -> AnyStateModificationOutput <$> execModification mod'

{- ** 
Tying up the `InteractiveState` instance with `InteractiveDisp` 
-}

-- | Given we have an instance of `InteractiveState` and a @dispType@, which is a known instance of `InteractiveDisp`,
-- this typeclass encodes how operations are parsed and outputs are displayed on the given `DispType`.
class ( InteractiveDisp dispType
      , InteractiveState state
      )
      => InteractiveStateOnDisp state (dispType :: DispType) where

  -- | Constraints needed to be able to parse state inputs. 
  type StateParseInputC state dispType (m :: Type -> Type) :: Constraint

  -- | Parse either the modofication or visualisation inputs. 
  parseAnyStateInput
    :: (StateParseInputC state dispType m)
    => DispInput dispType
    -> m ( Either (InteractiveStateErr state)
                  (AnyStateInput state)
         )
  default parseAnyStateInput
    :: ( StateParseInputC state dispType m
       , Applicative m
       )
    => DispInput dispType
    -> m ( Either (InteractiveStateErr state)
                  (AnyStateInput state)
         )
  parseAnyStateInput input = (<>) <$> parseModInput <*> parseVisInput
    where
      parseModInput = second AnyStateModificationInput <$> parseModificationInput input
      parseVisInput = second AnyStateVisualisationInput <$> parseVisualisationInput input

  -- | Parse the state modification input in an environment @m@ such that all parsing constraints are satsified. 
  parseModificationInput
    :: (StateParseInputC state dispType m)
    => DispInput dispType -- ^ Raw input 
    -> m ( Either (InteractiveStateErr state)
                  (StateModification state)
         ) -- ^ We eventually return a successfully parsed modification.

  -- | Parse the state visualisation input in an environment @m@ such that all parsing constraints are satsified. 
  parseVisualisationInput
    :: (StateParseInputC state dispType m)
    => DispInput dispType -- ^ Raw input 
    -> m ( Either (InteractiveStateErr state)
                  (StateVisualisation state)
         ) -- ^ We eventually return a successfully parsed visualisation.

  {- | Map a modification result to the output.

  The default implementation outputs as pretty text, should the modification result be an instance of `Show`. 
  -}
  displayModificationOutput :: StateModificationOutput state -> DispOutput dispType
  default displayModificationOutput
    :: ( dispType ~ 'Repl
       , Show (StateModificationOutput state)
       )
    => StateModificationOutput state
    -> DispOutput dispType
  displayModificationOutput = ReplOutputLazy . Pretty.pShow

  {- | Map a visualisation result to the output.

   The default implementation outputs as pretty text, should the visualisation result be an instance of `Show`. 
  -}
  displayVisualisationOutput :: StateVisualisationOutput state -> DispOutput dispType
  default displayVisualisationOutput
    :: ( dispType ~ 'Repl
       , Show (StateVisualisationOutput state)
       )
    => StateVisualisationOutput state
    -> DispOutput dispType
  displayVisualisationOutput = ReplOutputLazy . Pretty.pShow

-- | Execute some input on a state.
-- The state, like all other exec functions, is supplied within @m@; via a `MonadReader` mechanism, or equivalent. 
execAnyInputOnState
  :: forall state dispType m
   . ( InteractiveDisp dispType
     , InteractiveState state
     , InteractiveStateOnDisp state dispType
     , StateParseInputC state dispType m
     , StateVisualisationC state m
     , StateModificationC state m
     , Monad m
     )
  => DispInput dispType
  -> m (Either (InteractiveStateErr state) (DispOutput dispType))
execAnyInputOnState input = parseAnyStateInput @state input >>= \case
  Left  err    -> pure . Left $ err
  Right anyInp -> execAny anyInp <&> Right . \case
    AnyStateVisualisationOutput visOut ->
      displayVisualisationOutput @state @dispType visOut
    AnyStateModificationOutput modOut ->
      displayModificationOutput @state @dispType modOut
