module Graphics.Drawing2D
  ( Drawing
  , Point
  , Shape
  , FillStyle
  , OutlineStyle
  , rectangle
  , closed
  , filled
  , outlined
  , fillColor
  , outlineColor
  , lineWidth
  , render
  ) where

import Prelude

import Color (Color, toHexString)
import Control.Alt ((<|>))
import Data.Array (uncons)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (Context2D)
import Graphics.Canvas as Canvas

type Point = { x :: Number, y :: Number }

data Shape
  = Rectangle Number Number Number Number
  | ClosedPath (Array Point)

newtype FillStyle = FillStyle Color

newtype OutlineStyle = OutlineStyle
  { color     :: Maybe Color
  , lineWidth :: Maybe Number
  }

instance semigroupOutlineStyle :: Semigroup OutlineStyle where
  append (OutlineStyle a) (OutlineStyle b) = OutlineStyle
    { color:     b.color     <|> a.color
    , lineWidth: b.lineWidth <|> a.lineWidth
    }

instance monoidOutlineStyle :: Monoid OutlineStyle where
  mempty = OutlineStyle { color: Nothing, lineWidth: Nothing }

newtype Drawing = Drawing (Context2D -> Effect Unit)

instance semigroupDrawing :: Semigroup Drawing where
  append (Drawing a) (Drawing b) = Drawing \ctx -> a ctx *> b ctx

instance monoidDrawing :: Monoid Drawing where
  mempty = Drawing \_ -> pure unit

rectangle :: Number -> Number -> Number -> Number -> Shape
rectangle = Rectangle

closed :: Array Point -> Shape
closed = ClosedPath

fillColor :: Color -> FillStyle
fillColor = FillStyle

outlineColor :: Color -> OutlineStyle
outlineColor c = OutlineStyle { color: Just c, lineWidth: Nothing }

lineWidth :: Number -> OutlineStyle
lineWidth w = OutlineStyle { color: Nothing, lineWidth: Just w }

filled :: FillStyle -> Shape -> Drawing
filled (FillStyle c) shape = Drawing \ctx -> do
  _ <- Canvas.setFillStyle ctx (toHexString c)
  Canvas.beginPath ctx
  traceShape ctx shape
  Canvas.fill ctx

outlined :: OutlineStyle -> Shape -> Drawing
outlined (OutlineStyle s) shape = Drawing \ctx -> do
  for_ s.color \c -> Canvas.setStrokeStyle ctx (toHexString c)
  for_ s.lineWidth \w -> Canvas.setLineWidth ctx w
  Canvas.beginPath ctx
  traceShape ctx shape
  Canvas.stroke ctx

traceShape :: Context2D -> Shape -> Effect Unit
traceShape ctx (Rectangle x y w h) =
  Canvas.rect ctx { x, y, width: w, height: h }
traceShape ctx (ClosedPath points) =
  case uncons points of
    Nothing -> pure unit
    Just { head, tail } -> do
      Canvas.moveTo ctx head.x head.y
      for_ tail \p -> Canvas.lineTo ctx p.x p.y
      Canvas.closePath ctx

render :: Context2D -> Drawing -> Effect Unit
render ctx (Drawing f) = f ctx
