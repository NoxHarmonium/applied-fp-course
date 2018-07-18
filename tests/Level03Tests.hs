{-# LANGUAGE OverloadedStrings #-}
module Level03Tests
  ( unitTests
  ) where

import           Test.Hspec
import           Test.Hspec.Wai

import           Data.String                (fromString)

import qualified System.Exit                as Exit

import qualified Data.ByteString.Lazy.Char8 as LBS8

import qualified Level03.Core               as Core

unitTests :: IO ()
unitTests = do
  -- We need to setup our Application.
  let app' = pure Core.app

  -- This sets up HSpec to use our application as the thing it executes before the tests are run
  hspec . with app' $ do
      -- Here is an example test for the 'ListRq' route.
      -- Start with a general description of what we're going to test.
      describe "List Route" $ do
        -- Individual test cases provide more precise information regarding
        -- what they are going to test.
        it "Should return a 'not implemented' message and 200 status" $
          -- Using the functions from ``Test.Hspec.Wai`` this actions a GET request
          -- on the "/list" route, and using an infix function, compares the result of
          -- that request to our expected result.

          -- There String literal here is being converted by the use of the
          -- ``IsString`` typeclass into a response type that Hspec.Wai can
          -- use. Check the documentation for more examples, but when given
          -- a string literal, it will assume that is the expected body of
          -- the request and also check for a 200 response code.
          get "/list" `shouldRespondWith` "List Request not implemented"

      describe "Add Route With Empty Topic" $ do
        it "Should respond with an error when given an empty comment" $
          post "/some_topic/add" "" `shouldRespondWith` "Empty Comment" {matchStatus = 400}

      describe "Add Route With Valid Topic" $ do
        it "Should return a 'Hello there!' message and 200 status" $
          post "/some_topic/add" "comment-text" `shouldRespondWith` "Hello there!"

      describe "View Route With Valid Topic" $ do
        it "Should return a 'not implemented' message and 200 status" $
          get "/some_topic/view" `shouldRespondWith` "View Request not implemented"

      describe "View Route With Empty Topic" $ do
        it "Should return a 'Empty Topic' message and 400 status" $
          get "//view" `shouldRespondWith` "Empty Topic" {matchStatus = 400}

      describe "Gibberish Route" $ do
        it "Should return a 'Unknown Route' message and 404 status" $
          get "sdfdsfsddf" `shouldRespondWith` "Unknown Route" {matchStatus = 404}
