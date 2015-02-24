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
  , displayLn
  -- ** Progressively less magical formatting
  , displayDoc
  , displayDoc'
  , displayDoc''
  , displaySimpleTermDoc
  , hDisplaySimpleTermDoc
  -- ** A Classy Interface
  , PrettyTerm(..)
  -- ** Evaluation
  , SimpleTermDoc
--  , evalTermState
--  , displayCap
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Foldable (toList)
import Text.PrettyPrint.Free
import qualified System.Console.ANSI as ANSI
import System.IO

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


--ring :: Bell -> TermDoc
--ring b = pure (Ring b)



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


-- kludgeWindowSize :: IO Int
-- kludgeWindowSize = fail "missing ncurses"

displayLn :: (MonadIO m, PrettyTerm t) => t -> m ()
displayLn t = displayDoc 0.6 (prettyTerm t <> linebreak)

display :: (MonadIO m, PrettyTerm t) => t -> m ()
display = displayDoc 0.6

displayDoc :: (MonadIO m, PrettyTerm t) => Float -> t -> m ()
displayDoc = displayDoc' stdout

displayDoc' :: (MonadIO m, PrettyTerm t) => Handle -> Float -> t -> m ()
displayDoc' h ribbon doc = displayDoc'' h ribbon 80 doc

displayDoc'' :: (MonadIO m, PrettyTerm t) => Handle -> Float -> Int -> t -> m ()
displayDoc'' h ribbon cols doc = hDisplaySimpleTermDoc h $ renderPretty ribbon cols (prettyTerm doc)

displaySimpleTermDoc :: MonadIO m => SimpleTermDoc -> m ()
displaySimpleTermDoc = hDisplaySimpleTermDoc stdout

hDisplaySimpleTermDoc :: MonadIO m => Handle -> SimpleTermDoc -> m ()
hDisplaySimpleTermDoc h = liftIO . go [] where
  spaces :: Int -> String
  spaces n | n <= 0    = ""
          | otherwise = replicate n ' '

  go :: [ANSI.SGR] -> SimpleTermDoc -> IO ()
  go st (SChar c x) = hPutChar h c >> go st x
  go st (SText _ s x) = hPutStr h s >> go st x
  go st (SLine i x) = hPutStr h ('\n':spaces i) >> go st x
  go st (SEffect Pop x) = do
    let st' = drop 1 st
    ANSI.hSetSGR h $ [ANSI.Reset] ++ reverse st'
    go st' x
  go st (SEffect (Ring _) x) = go st x
  go st (SEffect (Push e) x) = maybe (go st x) (\sgr -> ANSI.hSetSGR h [sgr] >> go (sgr:st) x) $ effToSGR e
  go _ _ = pure ()

  effToSGR :: ScopedEffect -> Maybe ANSI.SGR
  effToSGR e =
    case e of
     Blink -> Just $ ANSI.SetBlinkSpeed ANSI.SlowBlink
     Reverse -> Just $ ANSI.SetSwapForegroundBackground True
     Protected -> Nothing
     Bold -> Just $ ANSI.SetConsoleIntensity ANSI.BoldIntensity
     Foreground n -> Just $ ANSI.SetColor ANSI.Foreground ANSI.Dull n
     Background n -> Just $ ANSI.SetColor ANSI.Background ANSI.Dull n
     Invisible -> Just $ ANSI.SetVisible False
     Dim -> Nothing
     Underline -> Just $ ANSI.SetUnderlining ANSI.SingleUnderline
     Standout -> Nothing
     Nop -> Nothing
     Else l r -> effToSGR l <|> effToSGR r



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
