module FormatPangoMarkup (formatPangoMarkup) where
import Text.Highlighting.Kate
import Graphics.Rendering.Pango

-- TODO: should use blaze-builder

tagTok :: Token -> String
tagTok (KeywordTok, s)        = "<span foreground=\"#007020\" font_weight=\"bold\">" ++ escapeMarkup s ++ "</span>"
tagTok (DataTypeTok, s)       = "<span foreground=\"#902000\">" ++ escapeMarkup s ++ "</span>"
tagTok (DecValTok, s)         = "<span foreground=\"#40a070\">" ++ escapeMarkup s ++ "</span>"
tagTok (BaseNTok, s)          = "<span foreground=\"#40a070\">" ++ escapeMarkup s ++ "</span>"
tagTok (FloatTok, s)          = "<span foreground=\"#40a070\">" ++ escapeMarkup s ++ "</span>"
tagTok (CharTok, s)           = "<span foreground=\"#4070a0\">" ++ escapeMarkup s ++ "</span>"
tagTok (StringTok, s)         = "<span foreground=\"#4070a0\">" ++ escapeMarkup s ++ "</span>"
tagTok (CommentTok, s)        = "<span foreground=\"#60a0b0\" background=\"lightgray\" font_style=\"italic\">" ++ escapeMarkup s ++ "</span>"
tagTok (OtherTok, s)          = "<span foreground=\"#007020\">" ++ escapeMarkup s ++ "</span>"
tagTok (AlertTok, s)          = "<span foreground=\"red\" font_weight=\"bold\">" ++ escapeMarkup s ++ "</span>"
tagTok (FunctionTok, s)       = "<span foreground=\"#06287e\">" ++ escapeMarkup s ++ "</span>"
tagTok (RegionMarkerTok, s)   = escapeMarkup s
tagTok (ErrorTok, s)          = "<span foreground=\"red\" font_weight=\"bold\">" ++ escapeMarkup s ++ "</span>"
tagTok (NormalTok, s)         = escapeMarkup s

tagLine :: SourceLine -> [String]
tagLine = fmap tagTok

formatPangoMarkup :: String -> String -> String
formatPangoMarkup lang = unlines . fmap (concat . tagLine) . highlightAs lang
