module ClientEditor where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk


createMenuBar descr = do
  bar <- menuBarNew
  mapM_ (createMenu bar) descr
  return bar
  
  where
    createMenu bar (name,items) = do
      menu <- menuNew
      item <- menuItemNewWithLabelOrMnemonic name
      menuItemSetSubmenu item menu
      menuShellAppend bar item
      mapM_ (createMenuItem menu) items

    createMenuItem menu (name,action) = do
      item <- menuItemNewWithLabelOrMnemonic name
      menuShellAppend menu item
      case action of
        Just act -> on item menuItemActivate act
        Nothing  -> on item menuItemActivate (return ())
    
    menuItemNewWithLabelOrMnemonic name
      | elem '_' name = menuItemNewWithMnemonic name
      | otherwise     = menuItemNewWithLabel name

menuBarDescr = [ 
  ("_File", [ ("Open", Nothing)
            , ("Save", Nothing)
            , ("_Quit", Just mainQuit)
            ]
  )
  , ("Help", [ ("_Help", Nothing) ] )
  ]


startEditor :: IO ()
startEditor = do
  initGUI
  window <- windowNew
  menuBar <- createMenuBar menuBarDescr
  set window [ windowTitle := "Demo"
             , containerChild := menuBar
             ]
  on window objectDestroy mainQuit
  widgetShowAll window
mainGUI