{-# LANGUAGE StandaloneDeriving #-}

module GlossExtras (
    Binary(..)
    , pictureToFile
    , pictureFromFile
    , Read (..)
    , readPicture
    , showPicture
    ) where

import Graphics.Gloss


import Data.Binary
import Data.Data
import Data.ByteString (ByteString(..))
import qualified Data.ByteString as B
import Data.Generics
import Foreign.ForeignPtr
import Data.ByteString.Unsafe
import System.IO.Unsafe

pictureToFile :: FilePath -> Picture -> IO ()
pictureToFile f p = encodeFile f p

pictureFromFile :: FilePath -> IO Picture
pictureFromFile f = decodeFile f

putInt :: Int -> Put
putInt i = put i
getInt :: Get Int
getInt = get

instance Binary Picture where
    put Blank = putInt 1
    put (Polygon p) = putInt 2 >> put p
    put (Line p) = putInt 3 >> put p
    put (Circle f) = putInt 4 >> put f
    put (ThickCircle f1 f2) = putInt 5 >> put f1 >> put f2
    put (Arc f1 f2 f3) = putInt 6 >> put f1 >> put f2 >> put f3
    put (ThickArc f1 f2 f3 f4) = putInt 7 >> put f1 >> put f2 >> put f3 >> put f4
    put (Text s) = putInt 8 >> put s
    put (Bitmap w h dta b) = putInt 9 >> put w >> put h >> put (bitmapDataToByteString dta) >> put b
    put (Color c p) = putInt 10 >> put c >> put p
    put (Translate f1 f2 p) = putInt 11 >> put f1 >> put f2 >> put p
    put (Rotate f p) = putInt 12 >> put f >> put p
    put (Scale f1 f2 p) = putInt 13 >> put f1 >> put f2 >> put p
    put (Pictures ps) = putInt 14 >> put ps
    
    get = getInt >>= \i -> case i of
        1 -> return Blank
        2 -> get >>= return . Polygon
        3 -> get >>= return . Line
        4 -> get >>= return . Circle
        5 -> get >>= \f1 -> get >>= \f2 -> return (ThickCircle f1 f2)
        6 -> get >>= \f1 -> get >>= \f2 -> get >>= \f3 -> return (Arc f1 f2 f3)
        7 -> get >>= \f1 -> get >>= \f2 -> get >>= \f3 -> get >>= \f4 -> return (ThickArc f1 f2 f3 f4)
        8 -> get >>= return . Text
        9 -> get >>= \w -> get >>= \h -> get >>= \d -> get >>= \b -> return  (bitmapOfByteString w h d b)
        10 -> get >>= \c -> get >>= \p -> return (Color c p)
        11 -> get >>= \f1 -> get >>= \f2 -> get >>= \p -> return (Translate f1 f2 p)
        12 -> get >>= \f -> get >>= \p -> return (Rotate f p)
        13 -> get >>= \f1 -> get >>= \f2 -> get >>= \p -> return (Scale f1 f2 p)
        14 -> get >>= return . Pictures

bitmapDataToByteString :: BitmapData -> ByteString
bitmapDataToByteString dta = unsafePerformIO $ withForeignPtr ptr $ \p -> unsafePackCStringFinalizer p l (return ())
    where (l,ptr) = unfoldBitmapData dta

unfoldBitmapData :: BitmapData -> (Int,ForeignPtr Word8)
unfoldBitmapData dta = (head is,head $ tail ps)
        where
        is = gmapQ (mkQ undefined (id :: Int -> Int)) dta
        ps = gmapQ (mkQ undefined (id :: ForeignPtr Word8 -> ForeignPtr Word8)) dta

instance Binary Color where
    put c = put (rgbaOfColor c)
    get = do
        (f1,f2,f3,f4) <- get
        return (makeColor f1 f2 f3 f4)

readPicture :: String -> Picture
readPicture = read

deriving instance Read Picture

showPicture :: Picture -> String
showPicture x = showsPicture x ""

showsPicture :: Picture -> ShowS
showsPicture = showsPrecPicture 0

showsPrecPicture :: Int -> Picture -> ShowS
showsPrecPicture _ Blank = showString "Blank"
showsPrecPicture p (Polygon x1)
  = ((showParen (p > 10))
     $ ((showString "Polygon ") . (showsPrec 11 x1)))
showsPrecPicture p (Line x1)
  = ((showParen (p > 10))
     $ ((showString "Line ") . (showsPrec 11 x1)))
showsPrecPicture p (Circle x1)
  = ((showParen (p > 10))
     $ ((showString "Circle ") . (showsPrec 11 x1)))
showsPrecPicture p (ThickCircle x1 x2)
  = ((showParen (p > 10))
     $ ((showString "ThickCircle ")
        . ((showsPrec 11 x1) . ((showChar ' ') . (showsPrec 11 x2)))))
showsPrecPicture p (Arc x1 x2 x3)
  = ((showParen (p > 10))
     $ ((showString "Arc ")
        . ((showsPrec 11 x1)
           . ((showChar ' ')
              . ((showsPrec 11 x2) . ((showChar ' ') . (showsPrec 11 x3)))))))
showsPrecPicture p (ThickArc x1 x2 x3 x4)
  = ((showParen (p > 10))
     $ ((showString "ThickArc ")
        . ((showsPrec 11 x1)
           . ((showChar ' ')
              . ((showsPrec 11 x2)
                 . ((showChar ' ')
                    . ((showsPrec 11 x3) . ((showChar ' ') . (showsPrec 11 x4)))))))))
showsPrecPicture p (Text x1)
  = ((showParen (p > 10))
     $ ((showString "Text ") . (showsPrec 11 x1)))
showsPrecPicture p (Bitmap x1 x2 x3 x4)
  = ((showParen (p > 10))
     $ ((showString "Bitmap ")
        . ((showsPrec 11 x1)
           . ((showChar ' ')
              . ((showsPrec 11 x2)
                 . ((showChar ' ')
                    . ((showsPrecBitmapData 11 x3) . ((showChar ' ') . (showsPrec 11 x4)))))))))
showsPrecPicture p (Color x1 x2)
  = ((showParen (p > 10))
     $ ((showString "Color ")
        . ((showsPrec 11 x1) . ((showChar ' ') . (showsPrecPicture 11 x2)))))
showsPrecPicture p (Translate x1 x2 x3)
  = ((showParen (p > 10))
     $ ((showString "Translate ")
        . ((showsPrec 11 x1)
           . ((showChar ' ')
              . ((showsPrec 11 x2) . ((showChar ' ') . (showsPrecPicture 11 x3)))))))
showsPrecPicture p (Rotate x1 x2)
  = ((showParen (p > 10))
     $ ((showString "Rotate ")
        . ((showsPrec 11 x1) . ((showChar ' ') . (showsPrecPicture 11 x2)))))
showsPrecPicture p (Scale x1 x2 x3)
  = ((showParen (p > 10))
     $ ((showString "Scale ")
        . ((showsPrec 11 x1)
           . ((showChar ' ')
              . ((showsPrec 11 x2) . ((showChar ' ') . (showsPrecPicture 11 x3)))))))
showsPrecPicture p (Pictures x1)
  = ((showParen (p > 10))
     $ ((showString "Pictures ") . (showListPicture showsPicture x1)))

showListPicture :: (Picture -> ShowS) ->  [Picture] -> ShowS
showListPicture _     []     s = "[]" ++ s
showListPicture showx (x:xs) s = '[' : showx x (showl xs)
  where
    showl []     = ']' : s
    showl (y:ys) = ',' : showx y (showl ys)

showsPrecBitmapData :: Int -> BitmapData -> ShowS
showsPrecBitmapData p bitmap =
    showParen (p > 10)
     $ showString "BitmapData "
      . showsPrec 11 len
       . showChar ' '
        . showsPrec 11 (bitmapDataToByteString bitmap)
  where (len,ptr) = unfoldBitmapData bitmap

instance Read BitmapData where
    readsPrec p0 r
        = readParen (p0 > 10)
            (\ r0 -> [(mkBitMapData x1 x2, r3) |
                ("BitmapData", r1) <- lex r0,
                (x1, r2) <- readsPrec 11 r1,
                (x2, r3) <- readsPrec 11 r2])
            r
      where
        mkBitMapData :: Int -> ByteString -> BitmapData
        mkBitMapData len bytes =
            let square = toEnum len / 4
                axis = fromEnum (square / 2)
            in case bitmapOfByteString axis axis bytes False of
                Bitmap _ _ bitmap _ -> bitmap
    
instance Read Color where
    readsPrec p0 r
      = readParen
          (p0 > 10)
          (\ r0
             -> [(makeColor x1 x2 x3 x4, r5) |
                   ("RGBA", r1) <- lex r0,
                   (x1, r2) <- readsPrec 11 r1,
                   (x2, r3) <- readsPrec 11 r2,
                   (x3, r4) <- readsPrec 11 r3,
                   (x4, r5) <- readsPrec 11 r4])
          r
    
