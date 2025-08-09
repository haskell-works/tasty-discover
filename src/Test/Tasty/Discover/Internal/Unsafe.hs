-- | Unsafe utility functions for internal use.
--
-- This module contains partial functions that are used internally
-- where we have strong invariants that guarantee they won't fail,
-- but we want to be explicit about their unsafe nature.

module Test.Tasty.Discover.Internal.Unsafe
  ( unsafeHead
  ) where

-- | Unsafe head function with descriptive error message.
--
-- This function is partial and will throw an error on empty lists.
-- It should only be used when there's a strong invariant guaranteeing
-- the list is non-empty.
--
-- __Why use this instead of a total function?__
--
-- * Preserves existing type signatures and caller simplicity
-- * Makes invariant violations fail fast with clear error messages
-- * Avoids pushing complexity up the call chain for conditions that should never occur
-- * Used specifically in 'getGenerators' where 'groupBy' never produces empty groups
--
-- __When to use:__
--
-- * Internal functions with strong invariants
-- * Performance-critical code where the invariant is guaranteed
-- * When converting to total functions would complicate the entire call chain
--
-- __When NOT to use:__
--
-- * Public APIs where callers might pass invalid input
-- * When the input domain genuinely includes edge cases
-- * When safety is more important than performance
unsafeHead :: [a] -> a
unsafeHead []    = error "unsafeHead: empty list"
unsafeHead (x:_) = x
