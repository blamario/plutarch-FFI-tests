-- -Wno-name-shadowing enabled as workaround for now
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Temporarily copied here from plutus-extra
module Plutus.PAB.PrettyLogger (
  module Plutus.PAB.PrettyLogger,
  module System.Console.ANSI,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Kind (Type)
import System.Console.ANSI
import Prelude

-------------------------------------------------------------------------------

data LogStyle = LogStyle
  { bgColor :: LogColor
  , color :: LogColor
  , isBold :: Bool
  }

data LogColor
  = Vibrant Color
  | Standard Color
  | DefaultColor

defLogStyle :: LogStyle
defLogStyle =
  LogStyle {bgColor = DefaultColor, color = DefaultColor, isBold = False}

-------------------------------------------------------------------------------

logPretty :: forall (m :: Type -> Type). MonadIO m => String -> m ()
logPretty = logPrettyStyled defLogStyle

logPrettyStyled :: forall (m :: Type -> Type). MonadIO m => LogStyle -> String -> m ()
logPrettyStyled style string = liftIO $ do
  setSGR
    ( getColorList (color style)
        <> getBgColorList (bgColor style)
        <> getConsoleIntensityList (isBold style)
    )
  putStr string
  setSGR [Reset]
  where
    getColorList :: LogColor -> [SGR]
    getColorList color = case color of
      Vibrant x -> [SetColor Foreground Vivid x]
      Standard x -> [SetColor Foreground Dull x]
      _ -> []
    getBgColorList :: LogColor -> [SGR]
    getBgColorList bgColor = case bgColor of
      Vibrant x -> [SetColor Background Vivid x]
      Standard x -> [SetColor Background Dull x]
      _ -> []
    getConsoleIntensityList :: Bool -> [SGR]
    getConsoleIntensityList isBold =
      [SetConsoleIntensity BoldIntensity | isBold]

-- Convenience functions ------------------------------------------------------

logPrettyColor :: forall (m :: Type -> Type). MonadIO m => LogColor -> String -> m ()
logPrettyColor color = logPrettyStyled defLogStyle {color = color}

logPrettyBgColor ::
  forall (m :: Type -> Type).
  MonadIO m =>
  Int ->
  LogColor ->
  LogColor ->
  String ->
  m ()
logPrettyBgColor minWidth bgColor color str =
  logPrettyStyled
    defLogStyle {bgColor = bgColor, color = color}
    (padRight ' ' minWidth str)

logPrettyColorBold ::
  forall (m :: Type -> Type).
  MonadIO m =>
  LogColor ->
  String ->
  m ()
logPrettyColorBold color =
  logPrettyStyled defLogStyle {color = color, isBold = True}

withNewLines :: String -> String
withNewLines string = "\n" ++ string ++ "\n"

logNewLine :: forall (m :: Type -> Type). MonadIO m => m ()
logNewLine = logPretty "\n"

logDivider :: forall (m :: Type -> Type). MonadIO m => m ()
logDivider =
  logPretty $
    "-----------------------------------------------------------"
      ++ "\n"

padLeft :: Char -> Int -> String -> String
padLeft char len txt = replicate (len - length txt) char <> txt

padRight :: Char -> Int -> String -> String
padRight char len txt = txt <> replicate (len - length txt) char
