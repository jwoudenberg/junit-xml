{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.Function ((&))
import Test.Tasty
import Test.Tasty.Golden as Golden
import Text.XML.JUnit

main = defaultMain tests

tests :: TestTree
tests =
  Golden.goldenVsFile
    "Generate sample XML"
    "test/sample-report.xml"
    out
    (writeXmlReport out suites)
  where
    out = "/tmp/junit-xml-haskell-test.xml"
    suites =
      [ passed "Passed test"
          & stdout "passing stdout"
          & time 0.3
          & inSuite "suite 1",
        skipped "Skipped test"
          & inSuite "suite 1",
        failed "Failed test"
          & stdout "failing stdout"
          & stderr "failing stderr"
          & time 0.2
          & failureMessage "failing message"
          & failureStackTrace ["frame1", "frame2"]
          & inSuite "suite 1",
        errored "Errored test"
          & stdout "errored stdout"
          & stderr "errored stderr"
          & time 0.1
          & errorMessage "errored message"
          & errorStackTrace ["single error frame"]
          & inSuite "suite 2"
      ]
