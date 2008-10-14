{-# OPTIONS_GHC -fallow-overlapping-instances #-}
module Screen(
    Packing(..),
    Attribute(..),
    Widget(..),
    Canvas(..),
    doRender,
    Key(..),
    RenderContext,
    newRenderContext,
    setRenderContext,
    tryDrawRenderContext,
    resizeRenderContext,
    setRenderWidget,
    keyRenderContext,
    keyCatcherWidget,
    newSVarWidget,
    dynamicWidget,
    widgetAttr,
    widgetCenter,
    widgetEmpty,
    widgetHorizontalBox,
    widgetScroll,
    widgetSimpleFrame,
    widgetText,
    widgetVerticalBox,
    stackedWidgets

    ) where

import Control.Concurrent
import Control.Exception as E
import Data.Bits
import Monad

import CacheIO
import Curses
import ErrorLog
import Format
import GenUtil
import Doc.Chars
import Doc.DocLike()


data Canvas = Canvas {window :: !Window, origin :: !(Int,Int), widgetOrigin :: !(Int,Int), bounds :: !(Int,Int)  }

data Widget = Widget {
    render :: Canvas -> IO (),
    getBounds :: IO (Int,Int),
    processKey :: Key -> IO Bool
    }


data Packing = NoExpand | Expand | ExpandFill
    deriving(Show,Eq,Enum)
data Attribute = AttrBold | AttrBlink | AttrDim | AttrReverse | AttrUnderline | AttrColor String String
    deriving(Show,Eq)

instance Show Canvas where
    show c = fmtSs "<canvas:origin=%s,bounds=%s>" [show (origin c), show (bounds c)]

widgetEmpty = emptyWidget
--widgetEmptySized x y = emptySizedWidget (floor (x + 0.5)) (floor (y + 0.5))
widgetCenter w = centerWidget w
widgetSimpleFrame w = boxWidget w
widgetVerticalBox True ws = homVerticalBox ws
widgetVerticalBox False ws = verticalBox ws []
widgetHorizontalBox True ws = homHorizontalBox ws
widgetHorizontalBox False ws = horizontalBox ws []
widgetText s = staticText s
widgetAttr al w = attrWidget (foldl (flip ($)) attr0 (map ma al)) w where
    ma AttrBlink a = a `setBlink` True
    ma AttrBold a = a  `setBold` True
    ma AttrDim a = a  `setDim` True
    ma AttrReverse a = a  `setReverse` True
    ma AttrUnderline a = a `setUnderline` True
    ma _ a = a
widgetScroll = newScrollWidget


isExpand NoExpand = False
isExpand _ = True

isFill ExpandFill = True
isFill _ = False

{-
data Attribute = Attribute [String] String String
parseAttr :: String -> Attribute
parseAttr s = Attribute as fg bg where
    rs = filter (not . f . head) $ groupBy (\x y -> f x && f y) (map toLower s)
    as = filter (`elem` attributes) rs
    col x = if isJust (color x) then return x else Nothing
    fg = fromJust $ msum (map (cGet "fg") rs)  `mplus` msum (map col rs) `mplus` return "default"
    bg = fromJust $ msum (map (cGet "bg") rs) `mplus` return "default"
    f ',' = True
    f c | isSpace c = True
    f _ = False
    cGet p r | (p ++ ":") `isPrefixOf` r = col (drop (length p + 1) r)
    cGet _ _ = Nothing
    attributes = ["normal", "bold", "blink", "dim", "reverse", "underline" ]
-}

data RenderContext = RenderContext {
    needsResize :: MVar Bool,
    drawingStuff :: MVar (IO (), Key -> IO Bool, IO ())
    }

newRenderContext = do
    y <- newMVar False
    z <- newMVar (return (), \_ -> return False, return ())
    return RenderContext {needsResize = y, drawingStuff = z }


resizeRenderContext rc action = swapMVar (needsResize rc) True >>= \x -> unless x action >> return ()

tryDrawRenderContext rc = do
    y <- swapMVar (needsResize rc) False
    withMVar (drawingStuff rc) $ \(z,_,_) -> when (True) $ do
	erase
	when y (attempt endWin >> refresh)
	z
	refresh

setRenderContext rc dr = do
    modifyMVar_ (drawingStuff rc)  $ \(_,_,final) -> do
    final
    --touchRenderContext rc
    return (dr,\_ -> return False, return ())

setRenderWidget rc w = do
    modifyMVar_ (drawingStuff rc)  $ \(_,_,final) -> do
    final
    --touchRenderContext rc
    return (wdr, processKey w, return (){-, nf-}) where
	wdr = do
	    c <- newCanvas
	    eannM ("doRender " ++ show c) $ render w c

keyRenderContext :: RenderContext -> Key -> IO Bool
keyRenderContext _ KeyResize = return True
keyRenderContext rc k = do
    (_,pks,_) <- readMVar (drawingStuff rc)
    pks k

doRender w = do
    erase
    c <- newCanvas
    eannM ("doRender " ++ show c) $ render w c
    refresh

newCanvas = do
    (ys,xs) <- scrSize
    return $  (Canvas {window = stdScr, origin = (0,0), widgetOrigin = (0,0), bounds = (xs,ys)})




-------------------
-- Drawing Routines
-------------------

canvasTranslate :: Canvas -> (Int,Int) -> Canvas
-- canvasTranslate canvas@(Canvas {origin = (x,y), bounds = (xb,yb)}) (xt,yt) = assert (xt > 0 && yt > 0) $ canvas {origin = (x + xt, y + yt), bounds = (max (xb - xt) 0, max (yb - yt) 0)}
canvasTranslate canvas@(Canvas {origin = (x,y), bounds = (xb,yb), widgetOrigin = (wx,wy)}) (xt,yt) = canvas {origin = (x + xt, y + yt), widgetOrigin = (wx, wy), bounds = (max (xb - xt) 0, max (yb - yt) 0)}


canvasWTranslate :: Canvas -> (Int,Int) -> Canvas
canvasWTranslate canvas@(Canvas {widgetOrigin = (x,y)}) (xt,yt) = canvas {widgetOrigin = (x + xt, y + yt)}

withAttr :: Canvas -> Attr -> IO a -> IO a
withAttr canvas attr action = E.bracket (wAttrGet w) (wAttrSet w) f where
    f (a,p) = wAttrSet w (a .|. attr,p) >> action
    w = window canvas


drawString :: Canvas -> String -> IO ()
drawString canvas s = eannM (fmtSs "drawString %s %s" [(show canvas), s]) $ ds ml yb (origin canvas) where
	ds (s:ls) yb (x,y) | yb > 0 = E.try (mvWAddStr (window canvas) y x (take xb s)) >> ds ls (yb - 1) (x,y + 1)
	ds _ _ _ = return ()
	(xb,yb) = bounds canvas
	ls = lines s
	(wx,wy) = widgetOrigin canvas
	ml = map (mv ' ' wx) (mv "" wy ls)
	mv _ 0 ss = ss
	mv _ n ss | n > 0 = drop n ss
	mv ec n ss = replicate (0 - n) ec ++ ss


boundCanvas :: Maybe Int -> Maybe Int -> Canvas -> Canvas
boundCanvas mx my canvas = (canvas {bounds = (nx,ny)}) where
    (cx,cy) = bounds canvas
    nx = maybe cx (min cx) mx
    ny = maybe cy (min cy) my


-- rendering routines

renderCentered :: Widget -> Canvas -> IO ()
renderCentered w canvas = do
	(xs,ys) <- getBounds w
	let (xs',ys') =  bounds canvas
	    f cs ws | ws >= cs = 0
	    f cs ws = (cs - ws) `div` 2
	    nc = (canvasTranslate canvas (f xs' xs,f ys' ys))
	renderChild w nc {bounds=(min xs $ fst $ bounds nc ,min ys $ snd $ bounds nc)}


renderChild :: Widget -> Canvas -> IO ()
renderChild _ (Canvas {bounds = (xb,yb)}) | xb == 0 || yb == 0 = return ()
renderChild w c = render w c

----------
-- Widgets
----------



emptyWidget = Widget {
    render = const (return ()),
    getBounds = return (0,0),
    processKey = \_ -> return False
    }


childrenWidget ws = emptyWidget {processKey = pk} where
    pk k = tk ws k
    tk [] _ = return False
    tk (w:ws) k = do
	b <- processKey w k
	if b then return True else tk ws k
-- layout widgets

-- | centers child widget, causes child to not use any extra space.
centerWidget :: Widget -> Widget
centerWidget w = w {render = rst} where
    rst canvas = do
	(xs,ys) <- getBounds w
	let (xs',ys') =  bounds canvas
	renderChild w (canvasTranslate canvas (f xs' xs,f ys' ys)) {bounds=(min xs $ fst $ bounds canvas ,min ys $ snd $ bounds canvas)} where
	    f cs ws | ws >= cs = 0
	    f cs ws = (cs - ws) `div` 2


-- General Widgets


newScrollWidget :: Widget -> IO Widget
newScrollWidget w = do
    yr <- newMVar 0
    let rst canvas = do
	    y <- readMVar yr
	    renderChild w (canvasWTranslate canvas (0,y))
	pk (KeyChar 'j') = modifyMVar_ yr (\x -> return $ x + 1) >> keyDone
	pk KeyDown = modifyMVar_ yr (\x -> return $ x + 1) >> keyDone
	pk (KeyChar 'k') = modifyMVar_ yr (\x -> return $ x - 1) >> keyDone
	pk KeyUp = modifyMVar_ yr (\x -> return $ x - 1) >> keyDone
	pk KeyHome = swapMVar yr 0 >> keyDone
	pk (KeyChar 'g') = swapMVar yr 0 >> keyDone
	pk KeyNPage = modifyMVar_ yr (\x -> return $ x + 25) >> keyDone
	pk KeyPPage = modifyMVar_ yr (\x -> return $ x - 25) >> keyDone
	pk k = processKey w k
	keyDone = modifyMVar_ yr (\x -> return  (max 0 x)) >> return True
    return w {processKey = pk, render = rst}


-- | draw box around child widget
boxWidget :: Widget -> Widget
boxWidget w = w {render = rst, getBounds = gb} where
    gb = getBounds w >>= \(x,y) -> return (x + 2, y + 2)
    rst canvas@(Canvas {bounds = (xb,yb)}) = do
	-- let tb = '+' : (replicate (xb - 2) '-' ++ "+")
	let tb = ulCorner ++ (replicate (xb - 2) hLine ++ urCorner)
	    bb = llCorner ++ (replicate (xb - 2) hLine ++ lrCorner)
	    bc c@(Canvas {bounds = (xb,yb)}) = c {bounds = (max (xb - 1) 0, max (yb - 1) 0)}
	renderChild w (canvasTranslate (bc canvas) (1,1))	
	drawString canvas tb
	drawString (canvasTranslate canvas (0,yb - 1)) bb
	sequence_ [drawString (canvasTranslate canvas (xt,yt)) (vLine :: String) | yt <- [1..yb - 2], xt <- [0,xb - 1]]


{-
widgetMinBBox :: Maybe Int -> Maybe Int -> Widget -> Widget
widgetMaxBBox :: Maybe Int -> Maybe Int -> Widget -> Widget
widgetExactBBox :: Maybe Int -> Maybe Int -> Widget -> Widget
-}

staticText s = emptyWidget {render = rst, getBounds = return bounds } where
    bounds = (maximum (0:map length ls),length ls)
    ls = lines s
    rst canvas = drawString canvas s


	


verticalBox :: [(Packing,Widget)] -> [(Packing,Widget)] -> Widget
verticalBox starts ends = (childrenWidget aw) {render = rd, getBounds = gb} where
    awpb = starts ++ ends
    awp = if any isExpand $ fsts awpb then awpb else starts ++ [(Expand,emptyWidget)] ++ ends
    aw = snds awp
    gb = do
	(xs,ys) <- fmap unzip $ mapM getBounds aw
	return (maximum xs, sum ys)
    rd canvas = do
	let (_,yb) = bounds canvas
        --let ywo = snd $ widgetOrigin $ canvas
	(_,ys) <- gb
	let ne = length (filter isExpand $ fsts awp)
	    el = if yb > ys then splitSpace (yb - ys) ne else replicate ne 0
	    f ((e:es),canvas) (ps,w) | isExpand ps = do
		(_,wby) <- getBounds w
		if isFill ps then
		    renderChild w (boundCanvas Nothing (Just (wby + e)) canvas)
			else renderCentered w (boundCanvas Nothing (Just (wby + e)) canvas)
		return (es,canvasTranslate canvas (0,wby + e))
	    f (es,canvas) (_,w) = do
		(_,wby) <- getBounds w
		--renderCentered w (boundCanvas Nothing (Just wby) canvas)
		renderChild w (boundCanvas Nothing (Just wby) canvas)
		return (es,canvasTranslate canvas (0,wby))
	foldlM_ f (el,canvas) awp


horizontalBox :: [(Packing,Widget)] -> [(Packing,Widget)] -> Widget
horizontalBox starts ends = (childrenWidget aw) {render = rd, getBounds = gb } where
    awpb = starts ++ ends
    awp = if any isExpand $ fsts awpb then awpb else starts ++ [(Expand,emptyWidget)] ++ ends
    aw = snds awp
    gb = do
	(xs,ys) <- fmap unzip $ mapM getBounds aw
	return (sum xs, maximum ys)
    rd canvas = do
	let (xb,_) = bounds canvas
	(xs,_) <- gb
	let ne = length (filter isExpand $ fsts awp)
	    el = if xb > xs then splitSpace (xb - xs) ne else replicate ne 0
	    f ((e:es),canvas) (ps,w) | isExpand ps = do
		(wbx,_) <- getBounds w
		if isFill ps then
		    renderChild w (boundCanvas (Just (wbx + e)) Nothing canvas)
			else renderCentered w (boundCanvas (Just (wbx + e)) Nothing canvas)
		return (es,canvasTranslate canvas (wbx + e,0))
	    f (es,canvas) (_,w) = do
		(wbx,_) <- getBounds w
		--renderCentered w (boundCanvas (Just wbx) Nothing canvas)
		renderChild w (boundCanvas (Just wbx) Nothing canvas)
		return (es,canvasTranslate canvas (wbx,0))
	foldlM_ f (el,canvas) awp

homVerticalBox :: [(Packing,Widget)] -> Widget
homVerticalBox awp = (childrenWidget aw) {render = rd, getBounds = gb } where
    aw = snds awp
    gb = do
	(xs,ys) <- fmap unzip $ mapM getBounds aw
	return (maximum xs, (maximum ys) * length aw)
    rd canvas = do
	let (_,yb) = bounds canvas
	    el = splitSpace yb (length aw)
	    f ([],_) _ = error "no space to split"
	    f ((e:es),canvas) (ps,w)  = do
		if isFill ps then
		    renderChild w (boundCanvas Nothing (Just e) canvas)
			else renderCentered w (boundCanvas Nothing (Just e) canvas)
		return (es,canvasTranslate canvas (0,e))
	foldlM_ f (el,canvas) awp


homHorizontalBox :: [(Packing,Widget)] -> Widget
homHorizontalBox awp = (childrenWidget aw) {render = rd, getBounds = gb } where
    aw = snds awp
    gb = do
	(xs,ys) <- fmap unzip $ mapM getBounds aw
	return (maximum xs * length aw, maximum ys)
    rd canvas = do
	let (xb,_) = bounds canvas
	    el = splitSpace xb (length aw)
	    f ([],_) _ = error "no space to split"
	    f ((e:es),canvas) (ps,w)  = do
		if isFill ps then
		    renderChild w (boundCanvas (Just e) Nothing canvas)
			else renderCentered w (boundCanvas (Just e) Nothing canvas)
		return (es,canvasTranslate canvas (e,0))
	foldlM_ f (el,canvas) awp

splitSpace m n = f (m `mod` n) $ replicate n (m `div` n) where
    f 0 xs = xs
    f n (x:xs) = (x + 1) : f (n - 1) xs
    f n' [] = errorf "splitSpace %i %i (f %i []): this can't happen." [fi m, fi n, fi n']


	
newSVarWidget :: (Readable c ) => c a -> (a -> Widget) -> Widget
newSVarWidget sv f = emptyWidget {render = rw, getBounds = gb} where
    rw canvas = do
	v <- readVal sv
	render (f v) canvas
    gb = do
	v <- readVal sv
	getBounds (f v)

dynamicWidget :: IO Widget -> Widget
dynamicWidget w = emptyWidget {render = rw, getBounds = gb, processKey = pk {- , processEvent = pe -} } where
    rw canvas = do
	v <- w
	render v canvas
    gb = w >>= getBounds
    pk k = w >>= \x -> processKey x k
--    pe a b = w >>= \x -> processEvent x a b



stackedWidgets :: [Widget] -> Widget
stackedWidgets [] = widgetEmpty
stackedWidgets ws@(w:_) = widgetEmpty { render = rnd, processKey = \k -> processKey w k, getBounds = gb  } where
    rnd canvas = mapM_ (flip render canvas) (reverse ws)
    gb = do
	bs <- mapM getBounds ws
	return $ (liftT2 (maximum, maximum)) (unzip bs)

{-
newAlternate :: Widget -> IO (Widget, Alternate)
newAlternate w = do
    sig <- newSignal
    final <- connect (changedSignal w) (\() -> signal sig ())
    r <- newMVar (w,final)
    let w = emptyWidget {render = rw, getBounds = gb, processKey = pk }
	rw canvas = do
	    (w,_) <- readMVar r
	    renderChild w canvas
	gb = do
	    (w,_) <- readMVar r
	    getBounds w
	pk k = do
	    (w,_) <- readMVar r
	    processKey w k
    return (w,Alternate r sig)


switchAlternate :: Alternate -> Widget -> IO ()
switchAlternate (Alternate mv sig) w = (modifyMVar_ mv $ \(_,final) ->
	(final >> connect (changedSignal w) (\() -> signal sig ()) >>= \nf -> return (w,nf)))
	    >> signal sig ()
-}

keyCatcherWidget :: (Curses.Key -> IO Bool) -> Widget -> Widget
keyCatcherWidget kr w = w {processKey = kr'} where
    kr' k = do
	b <- processKey w k
	if b then return True else (kr k)

attrWidget :: Attr -> Widget -> Widget
attrWidget a w = w {render = nr} where
    nr canvas = Screen.withAttr canvas a (renderChild w  canvas)

{-
-- data Event = KeyEvent Key | MouseEvent MouseEvent

--data Justification = Justified | LeftJustified | RightJustified


data InlineBox = InlineBoxText String | InlineBoxAttr [Attr] [InlineBox] | InlineBoxChoice InlineBox InlineBox |
    InlineBoxLB | InlineBoxRight [InlineBox] -- | InlineBoxAction (Event -> IO Bool) [InlineBox]




inlineBoxWidget :: [InlineBox] -> Widget
inlineBoxWidget xs = emptyWidget {render = rnd, getBounds = gb } where
    rnd canvas = undefined
    gb = return (0,0)

-}
