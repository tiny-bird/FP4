{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe       #-}

module Data.Batcher
  ( Sortable
  , SortableTrust
  , sort
  )
where

import           Data.Bits
  ( countLeadingZeros
  , finiteBitSize
  , shiftL
  , shiftR
  , (.&.)
  )
import qualified Foreign.Marshal.Alloc as FFI
import           Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import           Foreign.Storable
  ( Storable
  )
import qualified Foreign.Storable      as FFI

import           Control.Asynchronous
  ( ForeignThreadInterface
  , fork
  , sync
  )


class Monad m => FMEMI                  m
class FMEMI m => ForeignMemoryInterface m where
  malloc :: Storable a => Int   ->             m (Ptr a)
  free   :: Storable a => Ptr a ->             m (     )
  peek   :: Storable a => Ptr a -> Int ->      m      a
  poke   :: Storable a => Ptr a -> Int -> a -> m (     )


type Sortable a io =
  ( Storable a
  , Ord a
  , ForeignMemoryInterface io
  , ForeignThreadInterface io
  ) => [a] -> io [a]

type SortableTrust a =
  ( Storable a
  , Ord a
  ) => [a] -> [a]

type OffSet = Int
type Length = Int


instance FMEMI                  IO
instance ForeignMemoryInterface IO where
  malloc = FFI.mallocBytes
  free   = FFI.free
  peek   = FFI.peekElemOff
  poke   = FFI.pokeElemOff


sort :: Sortable a io
sort [       ] = pure []
sort xs@(hd:_) =
  malloc     m  >>= \ p  ->
  store  p 0 ys >>= \ _  -> -- initiate memory values to the list max value
  store  p 0 xs >>= \ _  ->
  sorter p o n  >>= \ _  ->
  query  p n    >>= \ zs ->
  free   p      >>= \ _  ->
  pure $ take l zs
  where
    l  = length xs
    n  = pow2 l -- Ensure that allocated memory is 2^i
    o  = FFI.sizeOf hd
    m  = n * o
    ys = take n $ cycle [ foldl1 max xs ]


query
  ::
    ( Storable a
    , ForeignMemoryInterface io
    )
  => Ptr a
  -> Length
  -> io [a]
query p n =
  aux 0
  where
    aux i
      | i   <   n =
        aux (i+1) >>= \ tl ->
        peek p i  >>= \ hd ->
        pure $ hd:tl
      | otherwise = pure []

store
  ::
    ( Storable a
    , ForeignMemoryInterface io
    )
  => Ptr a
  -> OffSet
  -> [a]
  -> io ()
store _ _ [    ] = pure ()
store p i (x:xs) = poke p i x >> store p (i+1) xs


pow2 :: Int -> Int
pow2 x =
  if x .&. (x - 1) == 0
  then x
  else 1 `shiftL` (b - z)
  where
    b = finiteBitSize     x
    z = countLeadingZeros x

next :: ( Storable a ) => Ptr a -> OffSet -> Ptr a
next p o =
  p `plusPtr` ( 1 * o) `asTypeOf` p

prev :: ( Storable a ) => Ptr a -> OffSet -> Ptr a
prev p o =
  p `plusPtr` (-1 * o) `asTypeOf` p


sorter
  ::
    ( Storable a
    , Ord a
    , ForeignMemoryInterface io
    , ForeignThreadInterface io
    )
  => Ptr a
  -> OffSet
  -> Length
  -> io ()
sorter p o n =
  (
    if 2 < n
    then
      fork (sorter p o m) >>= \ f ->
      fork (sorter q o m) >>= \ s ->
      sync [ f, s ]
    else
      pure ()
  ) >>= \ _ ->
  merger p o n
  where
    m = n `shiftR`   1
    q = p `plusPtr` (m * o) `asTypeOf` p

merger
  ::
    ( Storable a
    , Ord a
    , ForeignMemoryInterface io
    , ForeignThreadInterface io
    )
  => Ptr a
  -> OffSet
  -> Length
  -> io ()
merger p o n =
  w p (prev l o) >>= \ _ ->
  if 2 < n
  then
    fork (batcher p o m) >>= \ f ->
    fork (batcher q o m) >>= \ s ->
    sync [ f, s ]
  else
    pure ()
  where
    m = n `shiftR`   1
    q = p `plusPtr` (m * o) `asTypeOf` p
    l = p `plusPtr` (n * o) `asTypeOf` p
    w i j
      | i   <   q =
          fork (comparator i j)          >>= \ f ->
          fork (w (next i o) (prev j o)) >>= \ s ->
          sync [ f, s ]
      | otherwise = pure ()

comparator
  ::
    ( Storable a
    , Ord a
    , ForeignMemoryInterface io
    )
  => Ptr a
  -> Ptr a
  -> io ()
comparator p q =
  peek p 0           >>= \ i ->
  peek q 0           >>= \ j ->
  poke p 0 (min i j) >>= \ _ ->
  poke q 0 (max i j) >>= \ _ ->
  pure ()

batcher
  ::
    ( Storable a
    , Ord a
    , ForeignMemoryInterface io
    , ForeignThreadInterface io
    )
  => Ptr a
  -> OffSet
  -> Length
  -> io ()
batcher p o n =
  cleaner p o n >>= \ _ ->
  if 2 < n
  then
    fork (batcher p o m) >>= \ f ->
    fork (batcher q o m) >>= \ s ->
    sync [ f, s ]
  else
    pure ()
  where
    m = n `shiftR`   1
    q = p `plusPtr` (m * o) `asTypeOf` p

cleaner
  ::
    ( Storable a
    , Ord a
    , ForeignMemoryInterface io
    , ForeignThreadInterface io
    )
  => Ptr a
  -> OffSet
  -> Length
  -> io ()
cleaner p o n =
  w p q
  where
    m = n `shiftR`   1
    q = p `plusPtr` (m * o) `asTypeOf` p
    w i j
      | i   <   q =
          fork (comparator i j)          >>= \ f ->
          fork (w (next i o) (next j o)) >>= \ s ->
          sync [ f, s ]
      | otherwise = pure ()