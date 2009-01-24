module Main where
import System.Exit
import System.Environment
import Text.LineToPDF
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
    args <- getArgs

    (enc, input) <- case args of
        []      -> do
            putStrLn "Usage: line2pdf [-big5|-gbk|-shiftjis|-euc-jp|-euc-kr] input.txt > output.pdf"
            putStrLn "  (Form feed (^L) in input denotes a pagebreak.)"
            exitWith ExitSuccess
        ("-big5":i:_)     -> return (Big5, i)
        ("-gbk":i:_)      -> return (GBK, i)
        ("-euc-jp":i:_)   -> return (EUC_JP, i)
        ("-euc-kr":i:_)   -> return (EUC_KR, i)
        ("-shiftjis":i:_) -> return (ShiftJIS, i)
        (i:_)             -> return (Latin, i)

    src <- L.readFile input
    lineToPDF $ defaultConfig enc src
