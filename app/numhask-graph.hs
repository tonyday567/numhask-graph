{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import qualified Control.Foldl as L
import Control.Monad.Primitive (unsafeInlineIO)
import qualified Data.GraphViz as GV
import qualified Data.Map as Map
import qualified Data.Text as Text
import Diagrams.Backend.SVG (SVG, renderSVG)
import Diagrams.Prelude hiding (Loop, (<>))
import Diagrams.TwoD ()
import qualified Diagrams.TwoD.GraphViz as DGV
import qualified Diagrams.TwoD.Text
import Protolude as P

data Class
  = Magma
  | Unital
  | Associative
  | Commutative
  | Invertible
  | Idempotent
  | Absorbing
  | Group
  | AbelianGroup
  | Additive
  | Subtractive
  | Multiplicative
  | Divisive
  | Distributive
  | Semiring
  | Ring
  | CommutativeRing
  | IntegralDomain
  | Field
  | ExpField
  | QuotientField
  | UpperBoundedField
  | LowerBoundedField
  | TrigField
  | Actor
  | Module
  | Integral
  | ToIntegral
  | FromIntegral
  | Metric
  | Normed
  | Signed
  | Epsilon
  deriving (Show, Eq, Ord)

data Family
  = Addition
  | Multiplication
  deriving (Show, Eq, Ord)

data Dependency = Dependency
  { _class :: Class
  , _dep :: Class
  , _op :: Maybe Family
  } deriving (Show, Eq, Ord)

dependencies :: [Dependency]
dependencies =
  [ Dependency Unital Magma Nothing
  , Dependency Associative Magma Nothing
  , Dependency Commutative Magma Nothing
  , Dependency Invertible Magma Nothing
  , Dependency Idempotent Magma Nothing
  , Dependency Absorbing Magma Nothing
  , Dependency Group Unital Nothing
  , Dependency Group Invertible Nothing
  , Dependency Group Associative Nothing
  , Dependency AbelianGroup Unital Nothing
  , Dependency AbelianGroup Invertible Nothing
  , Dependency AbelianGroup Associative Nothing
  , Dependency AbelianGroup Commutative Nothing
  , Dependency Additive Commutative (Just Addition)
  , Dependency Additive Unital (Just Addition)
  , Dependency Additive Associative (Just Addition)
  , Dependency Subtractive Invertible (Just Addition)
  , Dependency Subtractive Additive (Just Addition)
  , Dependency Multiplicative Unital (Just Multiplication)
  , Dependency Multiplicative Associative (Just Multiplication)
  , Dependency Divisive Invertible (Just Multiplication)
  , Dependency Divisive Multiplicative (Just Multiplication)
  , Dependency Distributive Additive (Just Addition)
  , Dependency Distributive Multiplicative (Just Multiplication)
  , Dependency Distributive Absorbing Nothing
  , Dependency Semiring Additive (Just Addition)
  , Dependency Semiring Distributive Nothing
  , Dependency Semiring Associative (Just Multiplication)
  , Dependency Semiring Unital (Just Multiplication)
  , Dependency Ring Distributive Nothing
  , Dependency Ring Subtractive (Just Addition)
  , Dependency CommutativeRing Commutative (Just Multiplication)
  , Dependency CommutativeRing Ring Nothing
  , Dependency IntegralDomain Ring Nothing
  , Dependency Field CommutativeRing Nothing
  , Dependency Field Divisive (Just Multiplication)
  ]

fileSvg f s = renderSVG f (mkSizeSpec (Just <$> r2 s))

data Config = Config
  { _rectX :: Double
  , _rectY :: Double
  , _textScale :: Double
  , _arrowScale :: Double
  }

-- | Render an annotated graph as a diagram, given functions
--   controlling the drawing of vertices and of edges.  The first
--   function is given the label and location of each vertex. The
--   second function, for each edge, is given the label and location
--   of the first vertex, the label and location of the second vertex,
--   and the label and path corresponding to the edge.
boxes ps =
  zipWith
    (\p c ->
       place
         ((unitSquare # scaleX 50.0 # scaleY 25.0 # lc (sRGB 0.33 0.33 0.33) .
           opacity 0.3 <>
           (Diagrams.Prelude.text (Text.unpack $ show c) # scale 5.0)) #
          named c)
         p)
    (Map.elems ps :: [P2 Double])
    (Map.keys ps)

edge ::
     (Renderable (Path V2 Double) b)
  => Dependency
  -> QDiagram b V2 Double Any
  -> QDiagram b V2 Double Any
edge (Dependency to from Nothing) =
  connectOutside'
    (headStyle %~ fc (sRGB 0.33 0.33 0.33) . opacity 0.3 $ shaftStyle %~
     lc (sRGB 0.33 0.33 0.33) .
     opacity 0.3 $
     arrowHead .~
     dart $
     headLength .~
     8 $
     def)
    from
    to
edge (Dependency to from (Just Addition)) =
  connectOutside'
    (headStyle %~ fc red . opacity 0.5 $ shaftStyle %~ lc red . opacity 0.5 $
     arrowHead .~
     dart $
     headLength .~
     8 $
     def)
    from
    to
edge (Dependency to from (Just Multiplication)) =
  connectOutside'
    (headStyle %~ fc blue . opacity 0.5 $ shaftStyle %~ lc blue . opacity 0.5 $
     arrowHead .~
     dart $
     headLength .~
     8 $
     def)
    from
    to

ps cs ds =
  fst $ DGV.getGraph $ unsafeInlineIO $
  DGV.layoutGraph GV.Dot (DGV.mkGraph cs (toEdge <$> ds))
  where
    toEdge (Dependency to from wrapper) = (from, to, wrapper)

instance IsName Class

makeGraph ::
     ( (Renderable (Path V2 Double) b)
     , Renderable (Diagrams.TwoD.Text.Text Double) b
     )
  => Config
  -> [Class]
  -> [Dependency]
  -> QDiagram b V2 Double Any
makeGraph Config {} cs ds =
  L.fold (L.Fold (flip edge) (mconcat $ boxes (ps cs ds)) identity) ds

fieldClasses =
  [ Magma
  , Unital
  , Associative
  , Commutative
  , Invertible
  , Absorbing
  , Additive
  , Subtractive
  , Multiplicative
  , Divisive
  , Distributive
  , Ring
  , CommutativeRing
  , Field
  ]

main :: IO ()
main = do
  let gField = makeGraph (Config 3 30 10 1) fieldClasses dependencies
  fileSvg "other/field.svg" (600, 600) (gField :: QDiagram SVG V2 Double Any)
