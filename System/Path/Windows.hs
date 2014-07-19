{-# LANGUAGE EmptyDataDecls, PatternGuards, FlexibleInstances, Rank2Types, OverloadedStrings #-}
{-# LANGUAGE CPP #-}
#define MODULE_NAME     Windows
#define IS_WINDOWS      True

-- | This module provides type-safe access to filepath manipulations.
--
--   Normally you would import 'System.Path' (which will use the
--   default implementation for the host platform) instead of this.
--   However, importing this explicitly allows for manipulation of
--   non-native paths.
--
#ifdef __HADDOCK__
module System.Path.Windows where
#else
#include "Internal.hs"
#endif

