{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
This module is copied verbatim (with some @hlint@ modifications) from
the [@time@](https://github.com/haskell/time/blob/b9e1506ddd2dadd45939a46795c5cf39ad333ec9/test/main/Test/Arbitrary.hs) package. As such, its license is reproduced:

TimeLib is Copyright (c) Ashley Yakeley and contributors, 2004-2021. All rights reserved.
Certain sections are Copyright 2004, The University Court of the University of Glasgow. All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Neither name of the copyright holders nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module Arbitrary where

import Control.Monad ( liftM2 )
import Data.Fixed ( Pico, mod' )
import Data.Ratio ( (%) )
import Data.Time
    ( UTCTime(UTCTime),
      Day(ModifiedJulianDay),
      DiffTime,
      CalendarDiffDays(CalendarDiffDays),
      CalendarDiffTime(CalendarDiffTime),
      LocalTime(LocalTime),
      TimeOfDay(TimeOfDay),
      TimeZone(TimeZone, timeZoneMinutes),
      ZonedTime(ZonedTime),
      utc,
      fromGregorian,
      toGregorian,
      localTimeToUT1,
      localTimeToUTC,
      ut1ToLocalTime,
      utcToLocalTime,
      timeOfDayToTime,
      timeToTimeOfDay,
      minutesToTimeZone,
      zonedTimeToUTC,
      DayOfWeek,
      NominalDiffTime,
      UniversalTime(ModJulianDate) )
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import Test.QuickCheck
    ( choose, oneof, Arbitrary(..), CoArbitrary(..) )
import System.Random ( Random )

instance Arbitrary DayOfWeek where
    arbitrary = toEnum <$> choose (1, 7)

deriving instance Random Day

instance Arbitrary Day where
    arbitrary = choose (fromGregorian (-9900) 1 1, fromGregorian 9999 12 31)
    shrink day =
        let (y, m, d) = toGregorian day
            dayShrink =
                [fromGregorian y m (d - 1) | d > 1]
            monthShrink =
                [fromGregorian y (m - 1) d | m > 1]
            yearShrink
              | y > 2000 = [fromGregorian (y - 1) m d]
              | y < 2000 = [fromGregorian (y + 1) m d]
              | otherwise = []
         in dayShrink ++ monthShrink ++ yearShrink

instance CoArbitrary Day where
    coarbitrary (ModifiedJulianDay d) = coarbitrary d

instance Arbitrary CalendarDiffDays where
    arbitrary = liftM2 CalendarDiffDays arbitrary arbitrary

instance Arbitrary DiffTime where
    arbitrary = oneof [intSecs, fracSecs] -- up to 1 leap second
      where
        intSecs = secondsToDiffTime' <$> choose (0, 86400)
        fracSecs = picosecondsToDiffTime' <$> choose (0, 86400 * 10 ^ (12 :: Int))
        secondsToDiffTime' :: Integer -> DiffTime
        secondsToDiffTime' = fromInteger
        picosecondsToDiffTime' :: Integer -> DiffTime
        picosecondsToDiffTime' x = fromRational (x % 10 ^ (12 :: Int))

instance CoArbitrary DiffTime where
    coarbitrary t = coarbitrary (fromEnum t)

instance Arbitrary NominalDiffTime where
    arbitrary = oneof [intSecs, fracSecs]
      where
        limit = 1000 * 86400
        picofactor = 10 ^ (12 :: Int)
        intSecs = secondsToDiffTime' <$> choose (negate limit, limit)
        fracSecs = picosecondsToDiffTime' <$> choose (negate limit * picofactor, limit * picofactor)
        secondsToDiffTime' :: Integer -> NominalDiffTime
        secondsToDiffTime' = fromInteger
        picosecondsToDiffTime' :: Integer -> NominalDiffTime
        picosecondsToDiffTime' x = fromRational (x % 10 ^ (12 :: Int))

instance CoArbitrary NominalDiffTime where
    coarbitrary t = coarbitrary (fromEnum t)

instance Arbitrary CalendarDiffTime where
    arbitrary = liftM2 CalendarDiffTime arbitrary arbitrary

reduceDigits :: Int -> Pico -> Maybe Pico
reduceDigits (-1) _ = Nothing
reduceDigits n x =
    let d :: Pico
        d = 10 ^^ negate n
        r = mod' x d
     in case r of
            0 -> reduceDigits (n - 1) x
            _ -> Just $ x - r

instance Arbitrary TimeOfDay where
    arbitrary = fmap timeToTimeOfDay arbitrary
    shrink (TimeOfDay h m s) =
        let shrinkInt 0 = []
            shrinkInt 1 = [0]
            shrinkInt _ = [0, 1]
            shrinkPico 0 = []
            shrinkPico 1 = [0]
            shrinkPico p =
                case reduceDigits 12 p of
                    Just p' -> [0, 1, p']
                    Nothing -> [0, 1]
         in [TimeOfDay h' m s | h' <- shrinkInt h]
            ++ [TimeOfDay h m' s | m' <- shrinkInt m]
                ++ [TimeOfDay h m s' | s' <- shrinkPico s]

instance CoArbitrary TimeOfDay where
    coarbitrary t = coarbitrary (timeOfDayToTime t)

instance Arbitrary LocalTime where
    arbitrary = liftM2 LocalTime arbitrary arbitrary
    shrink (LocalTime d tod) = [LocalTime d' tod | d' <- shrink d] ++ [LocalTime d tod' | tod' <- shrink tod]

instance CoArbitrary LocalTime where
    coarbitrary t = coarbitrary (floor (utcTimeToPOSIXSeconds (localTimeToUTC utc t)) :: Integer)

instance Arbitrary TimeZone where
    arbitrary = minutesToTimeZone <$> choose (-720, 720)
    shrink (TimeZone 0 _ _) = []
    shrink (TimeZone _ s n) = [TimeZone 0 s n]

instance CoArbitrary TimeZone where
    coarbitrary tz = coarbitrary (timeZoneMinutes tz)

instance Arbitrary ZonedTime where
    arbitrary = liftM2 ZonedTime arbitrary arbitrary
    shrink (ZonedTime d tz) = [ZonedTime d' tz | d' <- shrink d] ++ [ZonedTime d tz' | tz' <- shrink tz]

instance CoArbitrary ZonedTime where
    coarbitrary t = coarbitrary (floor (utcTimeToPOSIXSeconds (zonedTimeToUTC t)) :: Integer)

instance Arbitrary UTCTime where
    arbitrary = liftM2 UTCTime arbitrary arbitrary
    shrink t = fmap (localTimeToUTC utc) $ shrink $ utcToLocalTime utc t

instance CoArbitrary UTCTime where
    coarbitrary t = coarbitrary (floor (utcTimeToPOSIXSeconds t) :: Integer)

instance Arbitrary UniversalTime where
    arbitrary = (\n -> ModJulianDate $ n % k) <$> choose (-313698 * k, 2973483 * k) -- 1000-01-1 to 9999-12-31
      where
        k = 86400
    shrink t = fmap (localTimeToUT1 0) $ shrink $ ut1ToLocalTime 0 t

instance CoArbitrary UniversalTime where
    coarbitrary (ModJulianDate d) = coarbitrary d
