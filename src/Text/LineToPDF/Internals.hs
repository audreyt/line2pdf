{-# LANGUAGE ImplicitParams, ExistentialQuantification #-}

module Text.LineToPDF.Internals where
import Data.IORef
import System.IO
import System.IO.Unsafe
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntMap as IM

{-# NOINLINE __POS__ #-}
__POS__ :: IORef Int
__POS__ = unsafePerformIO (newRef 0)

{-# NOINLINE __OBJ__ #-}
__OBJ__ :: IORef Int
__OBJ__ = unsafePerformIO (newRef 0)

{-# NOINLINE __LOC__ #-}
__LOC__ :: IORef (IM.IntMap String)
__LOC__ = unsafePerformIO (newRef IM.empty)

{-# NOINLINE __PAGE__ #-}
__PAGE__ :: IORef [Obj]
__PAGE__ = unsafePerformIO (newRef [])

data Encoding
    = Latin
    | Big5
    | GBK
    | EUC_JP
    | EUC_KR
    | ShiftJIS

data AppConfig = forall a. MkAppConfig
    { pageWidth  :: Int
    , pageHeight :: Int
    , ptSize     :: Float
    , encodings  :: [Encoding]
    , srcLines   :: [a]
    , srcPut     :: a -> IO ()
    , srcEscape  :: Char -> a -> a
    , srcLength  :: a -> Int
    , srcHead    :: a -> Char
    }

defaultConfig :: Encoding -> L.ByteString -> AppConfig
defaultConfig enc txt = MkAppConfig
    { pageWidth  = 842
    , pageHeight = 595
    , ptSize     = 12
    , encodings  = [enc]
    , srcLines   = L.lines txt
    , srcPut     = L.putStr
    , srcEscape  = \ch -> L.intercalate (L.pack ['\\', ch]) . L.split ch
    , srcLength  = fromEnum . L.length
    , srcHead    = L.head
    }

type M = IO
type Obj = Int

lineToPDF :: AppConfig -> IO ()
lineToPDF appConfig = do
    hSetBinaryMode stdout True

    pr$ "%PDF-1.2\n" ++ "%\xE2\xE3\xCF\xD3\n"

    (info, root, tPages, resources) <- writeHeader $ encodings appConfig

    pageObjs <- let ?tPages = tPages
                    ?resources = resources in writePages appConfig

    markLocation root
    writeObj root $ do
        pr$ "/Type/Catalog" ++ "/Pages "
        pr$ show tPages ++ " 0 R" 

    markLocation tPages
    writeObj tPages $ do
        pr$ "/Type/Pages" ++ "/Count "
        pr$ show (length pageObjs)
         ++ "/MediaBox[0 0 "
         ++ show (pageWidth appConfig) ++ " " ++ show (pageHeight appConfig)
        pr$ "]" ++ "/Kids["
        pr$ concatMap ((++ " 0 R ") . show) pageObjs
        pr$ "]"

    xfer <- currentLocation

    objCount <- incrObj
    pr$ "xref\n" ++ "0 " ++ show objCount ++ "\n"
     ++ "0000000000 65535 f \r"

    writeLocations

    pr$ "trailer\n" ++ "<<" ++ "/Size "
    pr$ show objCount

    pr$ "/Root " ++ show root ++ " 0 R"
    pr$ "/Info " ++ show info ++ " 0 R"
    pr$ ">>\n" ++ "startxref\n"
    pr$ show xfer ++ "\n" ++ "%%EOF\n"

writeObj :: Obj -> M a -> M a
writeObj obj f = do
    pr$ show obj ++ " 0 obj" ++ "<<"
    rv <- f
    pr$ ">>" ++ "endobj\n"
    return rv

writeLocations :: M ()
writeLocations = do
    locs <- IM.elems <$> readRef __LOC__
    pr$ concatMap fmt locs
    where
    fmt x = pad ++ x ++ " 00000 n \r"
        where
        pad = replicate (10-l) '0'
        l = length x 

printObj :: String -> M Obj
printObj str = markObj $ (pr str >>) . return

writeHeader :: [Encoding] -> M (Obj, Obj, Obj, Obj)
writeHeader encs = do
    info <- printObj $ "/CreationDate(D:20080707163949+08'00')"
                    ++ "/Producer(line2pdf.hs)"
                    ++ "/Title(Untitled)"
    encoding    <- printObj strDefaultEncoding
    latinFonts  <- (`mapM` ([1..] `zip` baseFonts)) $ \(n, font) -> do
        markObj $ \obj -> do
            pr$ "/Type/Font" ++ "/Subtype/Type1" ++ "/Name/F"
            pr$ show (n :: Int)
            pr$ "/Encoding " ++ show encoding ++ " 0 R"
            pr$ font
            return $ "/F" ++ show n ++ " " ++ show obj ++ " 0 R"
    cjkFonts    <- (`mapM` encs) $ \enc -> case enc of
        Latin    -> return []
        Big5     -> writeFontsBig5
        GBK      -> writeFontsGBK
        EUC_JP   -> writeFontsEUC_JP
        EUC_KR   -> writeFontsEUC_KR
        ShiftJIS -> writeFontsShiftJIS
    root        <- incrObj
    tPages      <- incrObj
    markObj $ \resources -> do
        pr$ "/Font<<" ++ concat latinFonts ++ concat (concat cjkFonts) ++ ">>"
            ++ "/ProcSet[/PDF/Text]" ++ "/XObject<<>>"
        return (info, root, tPages, resources)

baseFonts :: [String]
baseFonts =
    [ "/BaseFont/Courier"
    , "/BaseFont/Courier-Oblique"
    , "/BaseFont/Courier-Bold"
    , "/BaseFont/Courier-BoldOblique"
    , "/BaseFont/Helvetica"
    , "/BaseFont/Helvetica-Oblique"
    , "/BaseFont/Helvetica-Bold"
    , "/BaseFont/Helvetica-BoldOblique"
    , "/BaseFont/Times-Roman"
    , "/BaseFont/Times-Italic"
    , "/BaseFont/Times-Bold"
    , "/BaseFont/Times-BoldItalic"
    , "/BaseFont/Symbol"
    , "/BaseFont/ZapfDingbats"
    ]
    
pr :: String -> M ()
pr str = do
    putStr str
    modifyRef __POS__ (+ (length str))

currentLocation :: IO Int
currentLocation = readRef __POS__

newRef :: a -> M (IORef a)
newRef = newIORef

readRef :: IORef a -> M a
readRef = readIORef

writeRef :: IORef a -> a -> M ()
writeRef = writeIORef

modifyRef :: IORef a -> (a -> a) -> M ()
modifyRef = modifyIORef

incrObj :: M Obj
incrObj = do
    obj <- succ <$> readRef __OBJ__
    writeRef __OBJ__ obj
    return obj

markObj :: (Obj -> M a) -> M a
markObj f = do
    obj <- incrObj
    markLocation obj
    writeObj obj (f obj)

markLocation :: Obj -> M ()
markLocation obj = do
    loc <- currentLocation
    modifyRef __LOC__ $ IM.insert obj (show loc)

fontOf :: Encoding -> String
fontOf Latin    = "1"
fontOf EUC_JP   = "20"
fontOf ShiftJIS = "25"
fontOf Big5     = "30"
fontOf GBK      = "35"
fontOf EUC_KR   = "40"

startPage :: (?tPages :: Obj, ?resources :: Obj) => AppConfig -> M Int
startPage MkAppConfig{ pageHeight = height, ptSize = pt, encodings = encs } = do
    markObj $ \obj -> do
        modifyRef __PAGE__ (obj:)
        pr$ "/Type/Page"
        pr$ "/Parent " ++ show ?tPages ++ " 0 R"
        pr$ "/Resources " ++ show ?resources ++ " 0 R"
        pr$ "/Contents " ++ show (succ obj) ++ " 0 R"
        pr$ "/Rotate 0"

    obj <- incrObj
    markLocation obj
    pr$ show obj ++ " 0 obj" ++ "<<"
    pr$ "/Length " ++ show (succ obj) ++ " 0 R"
    pr$ ">>" ++ "stream\n"

    streamPos <- currentLocation
    pr$ "BT\n";
    let font = fontOf $ case encs of
            (l:_)   -> l
            _       -> Latin
    pr$ "/F" ++ font ++ " " ++ show pt ++ " Tf\n"
    pr$ "1 0 0 1 50 " ++ show (height - 40) ++ " Tm\n"
    pr$ show pt ++ " TL\n"

    return streamPos

endPage :: Int -> M ()
endPage streamStart = do
    pr$ "ET\n"
    streamEnd <- currentLocation
    pr$ "endstream\n"
     ++ "endobj\n"

    obj <- incrObj
    markLocation obj

    pr$ show obj ++ " 0 obj\n"
     ++ show (streamEnd - streamStart) ++ "\n"
     ++ "endobj\n"

writePages :: (?tPages :: Obj, ?resources :: Obj) => AppConfig -> M [Obj]
writePages appConfig@MkAppConfig
    { srcLength  = len
    , srcPut     = put
    , srcHead    = hd
    , srcLines   = lns
    , srcEscape  = esc
    , pageHeight = height
    } = do
    pos <- newRef =<< startPage appConfig

    (`mapM_` lns) $ \ln -> do
        case len ln of
            1 | hd ln == '\f' -> do
                endPage =<< readRef pos
                writeRef pos =<< startPage appConfig
            _ -> do
                pr$ "T*("
                let ln' = (esc '(' . esc ')' . esc '\\') ln
                put ln'
                modifyRef __POS__ (+ len ln')
                pr$ ")Tj\n"

    endPage =<< readRef pos
    reverse <$> readRef __PAGE__

strDefaultEncoding :: String
strDefaultEncoding = "/Type/Encoding"
    ++ "/Differences[0 /.notdef/.notdef/.notdef/.notdef"
    ++ "/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef"
    ++ "/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef"
    ++ "/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef"
    ++ "/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef"
    ++ "/.notdef/.notdef/.notdef/.notdef/space/exclam"
    ++ "/quotedbl/numbersign/dollar/percent/ampersand"
    ++ "/quoteright/parenleft/parenright/asterisk/plus/comma"
    ++ "/hyphen/period/slash/zero/one/two/three/four/five"
    ++ "/six/seven/eight/nine/colon/semicolon/less/equal"
    ++ "/greater/question/at/A/B/C/D/E/F/G/H/I/J/K/L"
    ++ "/M/N/O/P/Q/R/S/T/U/V/W/X/Y/Z/bracketleft"
    ++ "/backslash/bracketright/asciicircum/underscore"
    ++ "/quoteleft/a/b/c/d/e/f/g/h/i/j/k/l/m/n/o/p"
    ++ "/q/r/s/t/u/v/w/x/y/z/braceleft/bar/braceright"
    ++ "/asciitilde/.notdef/.notdef/.notdef/.notdef/.notdef"
    ++ "/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef"
    ++ "/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef"
    ++ "/dotlessi/grave/acute/circumflex/tilde/macron/breve"
    ++ "/dotaccent/dieresis/.notdef/ring/cedilla/.notdef"
    ++ "/hungarumlaut/ogonek/caron/space/exclamdown/cent"
    ++ "/sterling/currency/yen/brokenbar/section/dieresis"
    ++ "/copyright/ordfeminine/guillemotleft/logicalnot/hyphen"
    ++ "/registered/macron/degree/plusminus/twosuperior"
    ++ "/threesuperior/acute/mu/paragraph/periodcentered"
    ++ "/cedilla/onesuperior/ordmasculine/guillemotright"
    ++ "/onequarter/onehalf/threequarters/questiondown/Agrave"
    ++ "/Aacute/Acircumflex/Atilde/Adieresis/Aring/AE"
    ++ "/Ccedilla/Egrave/Eacute/Ecircumflex/Edieresis/Igrave"
    ++ "/Iacute/Icircumflex/Idieresis/Eth/Ntilde/Ograve"
    ++ "/Oacute/Ocircumflex/Otilde/Odieresis/multiply/Oslash"
    ++ "/Ugrave/Uacute/Ucircumflex/Udieresis/Yacute/Thorn"
    ++ "/germandbls/agrave/aacute/acircumflex/atilde/adieresis"
    ++ "/aring/ae/ccedilla/egrave/eacute/ecircumflex"
    ++ "/edieresis/igrave/iacute/icircumflex/idieresis/eth"
    ++ "/ntilde/ograve/oacute/ocircumflex/otilde/odieresis"
    ++ "/divide/oslash/ugrave/uacute/ucircumflex/udieresis"
    ++ "/yacute/thorn/ydieresis]"

data FontConfig = MkFontConfig
    { encoding     :: String
    , cidFontType  :: String
    , ordering     :: String
    , supplement   :: String
    , wBox         :: String
    , descriptor   :: String
    }

writeFontsShiftJIS :: M [String]
writeFontsShiftJIS = writeFonts fontsShiftJIS $ MkFontConfig
    { encoding      = "90ms-RKSJ-H"
    , cidFontType   = "0"
    , ordering      = "Japan1"
    , supplement    = "2"
    , wBox          = "50 500 500"
    , descriptor    = "/Ascent 723"
                   ++ "/CapHeight 709"
                   ++ "/Descent -241"
                   ++ "/Flags 6"
                   ++ "/FontBBox[-123 -257 1001 910]"
                   ++ "/ItalicAngle 0"
                   ++ "/StemV 69"
                   ++ "/XHeight 450"
                   ++ "/Style<</Panose<010502020400000000000000>>>>>"
    }


writeFontsEUC_JP :: M [String]
writeFontsEUC_JP = writeFonts fontsEUC_JP $ MkFontConfig
    { encoding      = "EUC-H/WinCharSet 128"
    , cidFontType   = "2"
    , ordering      = "Japan1"
    , supplement    = "2"
    , wBox          = "50 500 500"
    , descriptor    = "/Ascent 859"
                   ++ "/CapHeight 859"
                   ++ "/Descent -141"
                   ++ "/Flags 5"
                   ++ "/Leading 0"
                   ++ "/MaxWidth 1000"
                   ++ "/AvgWidth 500"
                   ++ "/FontBBox[-100 -141 1100 1100]"
                   ++ "/MissingWidth 500"
                   ++ "/ItalicAngle 0"
                   ++ "/StemV 91"
                   ++ "/StemH 91"
                   ++ "/XHeight 430"
                   ++ "/Style<</Panose<000000000000000000000000>>>"
    }

writeFontsEUC_KR :: M [String]
writeFontsEUC_KR = writeFonts fontsEUC_KR $ MkFontConfig
    { encoding      = "KSCms-UHC-H/WinCharSet 129"
    , cidFontType   = "2"
    , ordering      = "Korea1"
    , supplement    = "0"
    , wBox          = "0 1000 500"
    , descriptor    = "/Flags 7"
                   ++ "/FontBBox[-100 -142 102 1000]"
                   ++ "/MissingWidth 500"
                   ++ "/StemV 91"
                   ++ "/StemH 91"
                   ++ "/ItalicAngle 0"
                   ++ "/CapHeight 858"
                   ++ "/XHeight 429"
                   ++ "/Ascent 858"
                   ++ "/Descent -142"
                   ++ "/Leading 148"
                   ++ "/MaxWidth 2"
                   ++ "/AvgWidth 500"
                   ++ "/Style<</Panose<000000400000000000000000>>>"
    }

writeFontsGBK :: M [String]
writeFontsGBK = writeFonts fontsGBK $ MkFontConfig
    { encoding      = "GB-EUC-H"
    , cidFontType   = "2"
    , ordering      = "GB1"
    , supplement    = "0"
    , wBox          = "500 1000 500 7716 [500]"
    , descriptor    = "/FontBBox[0 -199 1000 801]"
                   ++ "/Flags 6"
                   ++ "/CapHeight 658"
                   ++ "/Ascent 801"
                   ++ "/Descent -199"
                   ++ "/StemV 56"
                   ++ "/XHeight 429"
                   ++ "/ItalicAngle 0"
    }

writeFontsBig5 :: M [String]
writeFontsBig5 = writeFonts fontsBig5 $ MkFontConfig
    { encoding      = "ETen-B5-H"
    , cidFontType   = "2"
    , ordering      = "CNS1"
    , supplement    = "0"
    , wBox          = "13500 14000 500"
    , descriptor    = "/FontBBox[0 -199 1000 801]"
                   ++ "/Flags 7"
                   ++ "/CapHeight 0"
                   ++ "/Ascent 800"
                   ++ "/Descent -199"
                   ++ "/StemV 0"
                   ++ "/ItalicAngle 0"
    }

writeFonts fonts cfg = do 
    ref <- newRef []
    let addFont o fn = modifyRef ref
            (("/F" ++ show fn ++ " " ++ show o ++ " 0 R"):)
        markFont fn name = do
            markObj $ \obj -> do
                addFont obj fn
                pr$ "/Type/Font"
                 ++ "/Subtype/Type0"
                 ++ "/Name/F"
                pr$ show fn ++ "/BaseFont/"
                pr$ name ++ "/Encoding/" ++ encoding cfg
                pr$ "/DescendantFonts[" ++ show (succ obj) ++ " 0 R]"
            markObj $ \obj -> do
                pr$ "/Type/Font"
                 ++ "/Subtype/CIDFontType"
                pr$ cidFontType cfg
                 ++ "/BaseFont/"
                pr$ name ++ "/FontDescriptor " ++ show (succ obj) ++ " 0 R"
                 ++ "/CIDSystemInfo<<"
                 ++ "/Registry(Adobe)"
                 ++ "/Ordering("
                pr$ ordering cfg
                 ++ ")"
                 ++ "/Supplement "
                pr$ supplement cfg
                 ++ ">>"
                 ++ "/DW 1000"
                 ++ "/W["
                pr$ wBox cfg
                 ++ "]"
            printObj $ "/Type/FontDescriptor"
                    ++ "/FontName/"
                    ++ takeWhile (/= ',') name
                    ++ descriptor cfg
    mapM_ (uncurry markFont) fonts
    readRef ref

fontFamily :: Int -> String -> [(Int, String)]
fontFamily n f =
    [ (n,   f)
    , (n+1, f ++ ",Italic")
    , (n+2, f ++ ",Bold")
    , (n+3, f ++ ",BoldItalic")
    ]

fontsEUC_JP :: [(Int, String)]
fontsEUC_JP = fontFamily 20 "HeiseiKakuGo-W5"

fontsShiftJIS :: [(Int, String)]
fontsShiftJIS = fontFamily 25 "HeiseiKakuGo-W5"

fontsBig5 :: [(Int, String)]
fontsBig5 = fontFamily 30 "MingLiU"

fontsGBK :: [(Int, String)]
fontsGBK = fontFamily 35 "STSong"

fontsEUC_KR :: [(Int, String)]
fontsEUC_KR = fontFamily 40 "#B5#B8#BF#F2#C3#BC"

-- Target size = AFP size / 3
