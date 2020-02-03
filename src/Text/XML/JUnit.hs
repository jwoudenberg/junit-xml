{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.XML.JUnit
  ( writeXmlReport,
    TestSuite,
    inSuite,
    TestCase,
    passed,
    skipped,
    failed,
    errored,
    stdout,
    stderr,
    time,
    failureMessage,
    failureStackTrace,
    errorMessage,
    errorStackTrace,
  )
where

import Data.Function ((&))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO
import GHC.Exts (fromList)
import qualified Text.XML as XML

-- | This function writes an xml report to the provided path.
writeXmlReport :: FilePath -> [TestSuite] -> IO ()
writeXmlReport out =
  Data.Text.Lazy.IO.writeFile out . XML.renderText XML.def . encode

-- Type that models the Junit XML format.
--
-- This is a stackoverflow question with some answers indicating the required
-- format.
-- https://stackoverflow.com/questions/4922867/what-is-the-junit-xml-format-specification-that-hudson-supports
--
-- Not all tags and attributes in the link above seem to get picked up in
-- Jenkins, so the types below are narrowed down to what Jenkins does display.
--
-- An example of the XML produced:
--
--     <?xml version="1.0" encoding="UTF-8"?>
--     <testsuites skipped="" errors="" failures="" tests="" time="">
--       <testsuite name="suite 1" tests="3">
--         <testcase name="test 1" classname="test.class" time="3">
--           <failure message="failure derscription">first failed</failure>
--           <system-out>stdout dumped here</system-out>
--           <system-err>stderr dumped here</system-err>
--         </testcase>
--         <testcase name="test 2" classname="test.class" time="2">
--           <error message="error description">second errored</error>
--           <system-out>stdout dumped here</system-out>
--           <system-err>stderr dumped here</system-err>
--         </testcase>
--       </testsuite>
--       <testsuite name="suite 2">
--         <testcase name="test 3" classname="test.class" time="1">
--           <system-out>stdout dumped here</system-out>
--           <system-err>stderr dumped here</system-err>
--         </testcase>
--       </testsuite>
--     </testsuites>
--
data TestSuite
  = TestSuite
      { suiteName :: T.Text,
        testCase :: XML.Element,
        counts :: Counts
      }

data TestCase outcome where
  TestCase ::
    Outcome outcome =>
    { testName' :: T.Text,
      outcome' :: outcome,
      stdout' :: Maybe T.Text,
      stderr' :: Maybe T.Text,
      time' :: Maybe Double
    } ->
    TestCase
      outcome

inSuite :: T.Text -> TestCase outcome -> TestSuite
inSuite name test@TestCase {outcome', time'} =
  TestSuite
    { suiteName = name,
      testCase = encodeTestCase test,
      counts = (outcomeCounter outcome') {cumTime = fromMaybe 0 time'}
    }

mapTest :: (a -> a) -> TestCase a -> TestCase a
mapTest f test = test {outcome' = f (outcome' test)}

stdout :: T.Text -> TestCase outcome -> TestCase outcome
stdout log test = test {stdout' = Just log}

stderr :: T.Text -> TestCase outcome -> TestCase outcome
stderr log test = test {stderr' = Just log}

time :: Double -> TestCase outcome -> TestCase outcome
time seconds test = test {time' = Just seconds}

passed :: T.Text -> TestCase Passed
passed name =
  TestCase
    { testName' = name,
      outcome' = Passed,
      stdout' = Nothing,
      stderr' = Nothing,
      time' = Nothing
    }

skipped :: T.Text -> TestCase Skipped
skipped name =
  TestCase
    { testName' = name,
      outcome' = Skipped,
      stdout' = Nothing,
      stderr' = Nothing,
      time' = Nothing
    }

failed :: T.Text -> TestCase Failed
failed name =
  TestCase
    { testName' = name,
      outcome' = Failure Nothing [],
      stdout' = Nothing,
      stderr' = Nothing,
      time' = Nothing
    }

errored :: T.Text -> TestCase Errored
errored name =
  TestCase
    { testName' = name,
      outcome' = Error Nothing [],
      stdout' = Nothing,
      stderr' = Nothing,
      time' = Nothing
    }

class Outcome a where

  outcomeToXML :: a -> Maybe XML.Element

  outcomeCounter :: a -> Counts

data Passed = Passed

instance Outcome Passed where

  outcomeToXML _ = Nothing

  outcomeCounter _ = mempty {cumTests = 1}

data Skipped = Skipped

instance Outcome Skipped where

  outcomeToXML _ = Just $ XML.Element "skipped" mempty []

  outcomeCounter _ = mempty {cumSkipped = 1, cumTests = 1}

data Failed
  = Failure
      { -- Warning: newlines in the failure message will look like spaces in the
        -- Jenkins UI!
        failureMessage' :: Maybe T.Text,
        failureStackTrace' :: [T.Text]
      }

instance Outcome Failed where

  outcomeToXML = Just . encodeFailure

  outcomeCounter _ = mempty {cumFailed = 1, cumTests = 1}

failureMessage :: T.Text -> TestCase Failed -> TestCase Failed
failureMessage msg test =
  mapTest (\outcome -> outcome {failureMessage' = Just msg}) test

failureStackTrace :: [T.Text] -> TestCase Failed -> TestCase Failed
failureStackTrace trace test =
  mapTest (\outcome -> outcome {failureStackTrace' = trace}) test

data Errored
  = Error
      { -- Warning: newlines in the failure message will look like spaces in the
        -- Jenkins UI!
        errorMessage' :: Maybe T.Text,
        errorStackTrace' :: [T.Text]
      }

instance Outcome Errored where

  outcomeToXML = Just . encodeError

  outcomeCounter _ = mempty {cumErrored = 1, cumTests = 1}

errorMessage :: T.Text -> TestCase Errored -> TestCase Errored
errorMessage msg test =
  mapTest (\outcome -> outcome {errorMessage' = Just msg}) test

errorStackTrace :: [T.Text] -> TestCase Errored -> TestCase Errored
errorStackTrace trace test =
  mapTest (\outcome -> outcome {errorStackTrace' = trace}) test

data Counts
  = Counts
      { cumTests :: Int,
        cumFailed :: Int,
        cumErrored :: Int,
        cumSkipped :: Int,
        cumTime :: Double
      }

instance Semigroup Counts where
  c1 <> c2 = Counts
    { cumTests = cumTests c1 + cumTests c2,
      cumFailed = cumFailed c1 + cumFailed c2,
      cumErrored = cumErrored c1 + cumErrored c2,
      cumSkipped = cumSkipped c1 + cumSkipped c2,
      cumTime = cumTime c1 + cumTime c2
    }

instance Monoid Counts where
  mempty = Counts 0 0 0 0 0

encode :: [TestSuite] -> XML.Document
encode suites =
  XML.Document prologue element []
  where
    prologue = XML.Prologue [] Nothing []
    (totalCounts, suiteElements) =
      foldMap
        (fmap (pure . XML.NodeElement) . encodeSuite)
        (NonEmpty.groupAllWith suiteName suites)
    element =
      XML.Element
        "testsuites"
        (fromList (countAttributes totalCounts))
        suiteElements

encodeSuite :: NonEmpty.NonEmpty TestSuite -> (Counts, XML.Element)
encodeSuite suite =
  (suiteCounts, element)
  where
    suiteCounts = foldMap counts suite
    element =
      XML.Element
        "testsuite"
        (fromList $ ("name", suiteName (NonEmpty.head suite)) : countAttributes suiteCounts)
        (NonEmpty.toList (XML.NodeElement . testCase <$> suite))

encodeTestCase :: TestCase a -> XML.Element
encodeTestCase TestCase {testName', outcome', stdout', stderr', time'} =
  XML.Element "testcase" attributes children
  where
    attributes =
      fromList $
        catMaybes
          [ Just $ ("name", testName'),
            (,) "time" . T.pack . show <$> time'
          ]
    children =
      XML.NodeElement
        <$> catMaybes
          [ outcomeToXML outcome',
            XML.Element "system-out" mempty . pure . XML.NodeContent <$> stdout',
            XML.Element "system-err" mempty . pure . XML.NodeContent <$> stderr'
          ]

encodeFailure :: Failed -> XML.Element
encodeFailure failure =
  XML.Element
    "failure"
    (maybe mempty (\v -> fromList [("message", v)]) (failureMessage' failure))
    [XML.NodeContent (T.unlines (failureStackTrace' failure))]

encodeError :: Errored -> XML.Element
encodeError err =
  XML.Element
    "error"
    (maybe mempty (\v -> fromList [("message", v)]) (errorMessage' err))
    [XML.NodeContent (T.unlines (errorStackTrace' err))]

countAttributes :: Counts -> [(XML.Name, T.Text)]
countAttributes counts =
  [ ("tests", T.pack (show (cumTests counts))),
    ("failures", T.pack (show (cumFailed counts))),
    ("errors", T.pack (show (cumErrored counts))),
    ("skipped", T.pack (show (cumSkipped counts))),
    ("time", T.pack (show (cumTime counts)))
  ]
