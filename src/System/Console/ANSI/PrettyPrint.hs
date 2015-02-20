{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
--
--
--
module System.Console.ANSI.PrettyPrint (
  -- * Raw Effect (requires the effect be present)
    ScopedEffect(..)
  , with
  , Effect(..) -- unpaired effects
  -- ** Graceful degradation
  , soft
  -- ** Effects (built with soft)
  , blink -- with (soft Blink)
  , bold  -- with (soft Bold)
  , underline -- with (soft Underline)
  , standout -- with (soft Standout)
  , reversed -- with (soft Reversed)
  , protected -- with (soft Protected)
  , invisible -- with (soft Invisible)
  , dim -- with (soft Dim)
  -- ** Colors (built with soft)
  , red
  , black
  , green
  , blue
  , yellow
  , magenta
  , cyan
  , white
  , foreground
  , background
  -- ** Ringing bells
  , Bell(..)
--  , ring
  -- * A Color Pretty Printer
  , TermDoc
  , display
--  , displayLn
  -- ** Progressively less magical formatting
--  , displayDoc
--  , displayDoc'
--  , displayDoc''
  -- ** A Classy Interface
  , PrettyTerm(..)
  -- ** Evaluation
  , SimpleTermDoc
--  , evalTermState
  , displayCap
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Traversable
import Data.Foldable (toList)
import Text.PrettyPrint.Free
import qualified System.Console.ANSI as ANSI
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Int
import Data.Word
import Data.Sequence (Seq)
import Numeric.Natural (Natural)
import Data.List.NonEmpty (NonEmpty)



data ScopedEffect
  = Bold
  | Standout
  | Underline
  | Reverse
  | Blink
  | Dim
  | Invisible
  | Protected
  | Foreground ANSI.Color
  | Background ANSI.Color
  | Else ScopedEffect ScopedEffect
  | Nop
  deriving (Eq)

data Bell
  = VisibleBellOnly
  | AudibleBellOnly
  | VisibleBellPreferred
  | AudibleBellPreferred
  deriving (Eq,Ord,Show,Enum)

data Effect
  = Push ScopedEffect
  | Pop
  | Ring Bell -- visual bell ok, audible bell ok,
  deriving (Eq)

type TermState = [ScopedEffect]

--ring :: Bell -> TermDoc
--ring b = pure (Ring b)

eval :: Effect -> State TermState String
eval (Push Blink)          = modify (Blink:) *> pure (ANSI.setSGRCode [ANSI.SetBlinkSpeed ANSI.SlowBlink])
eval (Push Reverse)        = modify (Reverse:) *> pure (ANSI.setSGRCode [ANSI.SetSwapForegroundBackground True])
eval (Push Protected)      = modify (Protected:) *> pure ""
eval (Push Bold)           = modify (Bold:) *> pure (ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.BoldIntensity])
eval (Push (Foreground n)) = modify (Foreground n:) *> pure (ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull n])
eval (Push (Background n)) = modify (Background n:) *> pure (ANSI.setSGRCode [ANSI.SetColor ANSI.Background ANSI.Dull n])
eval (Push Invisible)      = modify (Invisible:) *> pure (ANSI.setSGRCode [ANSI.SetVisible False])
eval (Push Dim)            = modify (Dim:) *> pure ""
eval (Push Underline)      = modify (Underline:) *> pure (ANSI.setSGRCode [ANSI.SetUnderlining ANSI.SingleUnderline])
eval (Push Standout)       = modify (Standout:) *> pure ""
eval (Push Nop)            = modify (Nop:) *> pure ""
eval (Push (Else l r))     = do { x <- eval (Push l); if null x then eval (Push r) else pure x }
eval (Ring _)              = pure ""
eval Pop = do
  ts <- get
  let ts' = drop 1 ts
  r <- concat <$> traverse  (eval . Push) (reverse ts')
  put ts'
  pure $ ANSI.setSGRCode [ANSI.Reset] ++ r


type TermDoc = Doc Effect
type SimpleTermDoc = SimpleDoc Effect

with :: ScopedEffect -> TermDoc -> TermDoc
with cmd = pure (Push cmd) `enclose` pure Pop

soft :: ScopedEffect -> ScopedEffect
soft l = Else l Nop

foreground, background :: ANSI.Color -> TermDoc -> TermDoc
foreground n = with (soft (Foreground n))
background n = with (soft (Background n))

red, black, green, yellow, blue, magenta, cyan, white, blink, bold, underline,
 standout, reversed, protected, invisible, dim :: TermDoc -> TermDoc

blink      = with (soft Blink)
bold       = with (soft Bold)
underline  = with (soft Underline)
reversed   = with (soft Reverse)
protected  = with (soft Protected)
invisible  = with (soft Invisible)
dim        = with (soft Dim)
standout   = with (soft Standout)

red = foreground ANSI.Red
black = foreground ANSI.Black
green = foreground ANSI.Green
yellow = foreground ANSI.Yellow
blue = foreground ANSI.Blue
magenta = foreground ANSI.Magenta
cyan = foreground ANSI.Cyan
white = foreground ANSI.White

displayCap :: SimpleTermDoc -> State TermState String
displayCap = go where
  go (SChar c x)   = ([c] ++) <$> go x
  go (SText _ s x) = (s ++) <$> go x
  go (SLine i x)   = (('\n': spaces i) ++) <$> go x
  go (SEffect e t) = (++) <$> eval e <*> go t
  go _            = return ""

spaces :: Int -> String
spaces n | n <= 0    = ""
         | otherwise = replicate n ' '


-- kludgeWindowSize :: IO Int
-- kludgeWindowSize = fail "missing ncurses"


display :: (MonadIO m, PrettyTerm t) => Float -> Int -> t -> m ()
display ribbon cols doc = liftIO $ putStr $ fst $ flip runState [] $ displayCap sdoc
  where sdoc = renderPretty ribbon cols (prettyTerm doc)



class Pretty t => PrettyTerm t where
  prettyTerm :: t -> TermDoc
  prettyTerm = pretty
  prettyTermList :: [t] -> TermDoc
  prettyTermList = list . map prettyTerm

instance PrettyTerm t => PrettyTerm [t] where
  prettyTerm = prettyTermList

instance PrettyTerm Char where
  prettyTerm = char
  prettyTermList = prettyList

instance e ~ Effect => PrettyTerm (Doc e) where
  prettyTerm = id
  prettyTermList = list

instance PrettyTerm B.ByteString
instance PrettyTerm BL.ByteString
instance PrettyTerm T.Text
instance PrettyTerm TL.Text
instance PrettyTerm Int
instance PrettyTerm Int8
instance PrettyTerm Int16
instance PrettyTerm Int32
instance PrettyTerm Int64
instance PrettyTerm Word
instance PrettyTerm Word8
instance PrettyTerm Word16
instance PrettyTerm Word32
instance PrettyTerm Word64
instance PrettyTerm Bool
instance PrettyTerm Integer
instance PrettyTerm Float
instance PrettyTerm Double
instance PrettyTerm ()
instance PrettyTerm Natural

instance PrettyTerm a => PrettyTerm (Seq a) where
  prettyTerm = prettyTermList . toList

instance PrettyTerm a => PrettyTerm (NonEmpty a) where
  prettyTerm = prettyTermList . toList

instance (PrettyTerm a,PrettyTerm b) => PrettyTerm (a,b) where
  prettyTerm (x,y) = tupled [prettyTerm x, prettyTerm y]

instance (PrettyTerm a,PrettyTerm b,PrettyTerm c) => PrettyTerm (a,b,c) where
  prettyTerm (x,y,z) = tupled [prettyTerm x, prettyTerm y, prettyTerm z]

instance PrettyTerm a => PrettyTerm (Maybe a) where
  prettyTerm Nothing  = empty
  prettyTerm (Just x) = prettyTerm x
