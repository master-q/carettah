module FormatPangoMarkup (formatPangoMarkup, formatPangoMarkupWhite) where
import Text.Highlighting.Kate
import Graphics.Rendering.Pango

-- TODO: should use blaze-builder


tokColor :: TokenType -> String
tokColor KeywordTok        = "<span foreground=\"#007020\">"
tokColor DataTypeTok       = "<span foreground=\"#902000\">"
tokColor DecValTok         = "<span foreground=\"#40a070\">"
tokColor BaseNTok          = "<span foreground=\"#40a070\">"
tokColor FloatTok          = "<span foreground=\"#40a070\">"
tokColor CharTok           = "<span foreground=\"#4070a0\">"
tokColor StringTok         = "<span foreground=\"#4070a0\">"
tokColor CommentTok        = "<span foreground=\"#60a0b0\">"
tokColor OtherTok          = "<span foreground=\"#007020\">"
tokColor AlertTok          = "<span foreground=\"red\">"
tokColor FunctionTok       = "<span foreground=\"#06287e\">"
tokColor RegionMarkerTok   = "<span>"
tokColor ErrorTok          = "<span foreground=\"red\">"
tokColor NormalTok         = "<span>"

tokShape :: TokenType -> String
tokShape KeywordTok        = "<span font_weight=\"bold\">"
tokShape DataTypeTok       = "<span>"
tokShape DecValTok         = "<span>"
tokShape BaseNTok          = "<span>"
tokShape FloatTok          = "<span>"
tokShape CharTok           = "<span>"
tokShape StringTok         = "<span>"
tokShape CommentTok        = "<span font_style=\"italic\">"
tokShape OtherTok          = "<span>"
tokShape AlertTok          = "<span font_weight=\"bold\">"
tokShape FunctionTok       = "<span>"
tokShape RegionMarkerTok   = "<span>"
tokShape ErrorTok          = "<span font_weight=\"bold\">"
tokShape NormalTok         = "<span>"

tagTok, tagTokShape :: Token -> String
tagTok (t, s) = tokColor t ++ tokShape t ++ escapeMarkup s ++ "</span></span>"
tagTokShape (t, s) = tokShape t ++ escapeMarkup s ++ "</span>"

formatPangoMarkup :: String -> String -> String
formatPangoMarkup lang = unlines . fmap (concat . fmap tagTok) . highlightAs lang

formatPangoMarkupWhite :: String -> String -> String
formatPangoMarkupWhite lang text =
  "<span foreground=\"white\">" ++ 
  (unlines . fmap (concat . fmap tagTokShape) . highlightAs lang) text ++ 
  "</span>"
