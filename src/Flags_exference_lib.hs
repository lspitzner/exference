module Flags_exference_lib
  ( linkNodes
  , buildSearchTree
  )
where

linkNodes :: Bool
#if LINK_NODES
linkNodes = True
#else
linkNodes = False
#endif

buildSearchTree :: Bool
#if BUILD_SEARCH_TREE
buildSearchTree = True
#else
buildSearchTree = False
#endif
